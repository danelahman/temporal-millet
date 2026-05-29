module Error = Utils.Error
module Print = Utils.Print
module Ast = Language.Ast
module PrettyPrint = Language.PrettyPrint

module Make (GS : Language.GradeSystem.S) = struct
  module I = Interpreter.Make (GS)
  module ResourceGrade = GS.ResourceGrade
  open I

  (* A NUL byte is used as the redex marker because it cannot appear in any
     output produced by [Format] and so makes a single-character separator
     suitable for [String.split_on_char]. *)
  let tag_marker = '\x00'
  let print_mark ppf = Format.pp_print_as ppf 0 (String.make 1 tag_marker)

  let print_computation_redex ?max_level red c ppf =
    let print ?at_level = Print.print ?max_level ?at_level ppf in
    match (red, c) with
    | DoReturn, Ast.Do (c1, (pat, c2)) ->
        print ~at_level:2 "@[<v 0>%t@[<hov 2>let %t =@ %t@]%t in@,%t@]"
          print_mark (PP.print_pattern pat)
          (PP.print_computation ~max_level:1 c1)
          print_mark (PP.print_computation c2)
    | Box, Ast.Box (rho, e, (p, c)) ->
        let rho_pp = PrettyPrint.ResourceGradePrintParam.create () in
        print ~at_level:2 "@[<v 0>%t@[<hov 2>box %t %t as %t@]%t in@,%t@]"
          print_mark
          (PP.print_resource_grade rho_pp rho)
          (PP.print_expression ~max_level:0 e)
          (PP.print_pattern ~max_level:0 p)
          print_mark (PP.print_computation c)
    | Unbox, Ast.Unbox (e, (p, c)) ->
        print ~at_level:2 "@[<v 0>%t@[<hov 2>unbox %t as %t@]%t in@,%t@]"
          print_mark
          (PP.print_expression ~max_level:0 e)
          (PP.print_pattern p) print_mark (PP.print_computation c)
    | _, comp ->
        print "%t%t%t" print_mark
          (fun ppf -> PP.print_computation ?max_level comp ppf)
          print_mark

  let rec print_computation_reduction ?max_level red c ppf =
    let print ?at_level = Print.print ?max_level ?at_level ppf in
    match (red, c) with
    | DoCtx red, Ast.Do (c1, (Ast.PNonbinding, c2)) ->
        print ~at_level:2 "@[<v 0>%t;@,%t@]"
          (print_computation_reduction ~max_level:1 red c1)
          (PP.print_computation c2)
    | DoCtx red, Ast.Do (c1, (pat, c2)) ->
        print ~at_level:2 "@[<v 0>@[<hov 2>let %t =@ %t@] in@,%t@]"
          (PP.print_pattern pat)
          (print_computation_reduction ~max_level:1 red c1)
          (PP.print_computation c2)
    | HandleCtx red, Ast.Handle (c, h) ->
        print ~at_level:1 "@[<v 0>handle@;<1 2>%t@,with %t@]"
          (print_computation_reduction red c)
          (PP.print_expression ~max_level:0 h)
    | ComputationRedex redex, c ->
        print_computation_redex ?max_level redex c ppf
    | _, _ ->
        Error.runtime "internal: malformed reduction context in redex selector"

  let view_computation_with_redexes red comp =
    let rendered =
      match red with
      | None -> Format.asprintf "%t" (PP.print_computation comp)
      | Some red -> Format.asprintf "%t" (print_computation_reduction red comp)
    in
    match String.split_on_char tag_marker rendered with
    | [ code ] -> SyntaxHighlight.highlight_text code
    | [ pre; redex; post ] ->
        SyntaxHighlight.highlight_text pre
        @ [
            Vdom.elt "span"
              ~a:[ Vdom.class_ "active-redex" ]
              (SyntaxHighlight.highlight_text redex);
          ]
        @ SyntaxHighlight.highlight_text post
    | _ ->
        Error.runtime
          "internal: redex marker split produced an unexpected layout"
end
