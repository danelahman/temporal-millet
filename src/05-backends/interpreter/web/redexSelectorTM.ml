module Error = Utils.Error
module Print = Utils.Print
module Ast = Language.Ast
module PrettyPrint = Language.PrettyPrint

module Make (ResourceGrade : Language.ResourceGrade.Grade) = struct
  module I = Interpreter.Make (ResourceGrade)
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
          print_mark
          (PrettyPrint.print_pattern pat)
          (PrettyPrint.print_computation (module ResourceGrade) ~max_level:1 c1)
          print_mark
          (PrettyPrint.print_computation (module ResourceGrade) c2)
    | Box, Ast.Box (rho, e, (p, c)) ->
        let rho_pp = PrettyPrint.RhoPrintParam.create () in
        print ~at_level:2 "@[<v 0>%t@[<hov 2>box %t %t as %t@]%t in@,%t@]"
          print_mark
          (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho)
          (PrettyPrint.print_expression (module ResourceGrade) ~max_level:0 e)
          (PrettyPrint.print_pattern ~max_level:0 p)
          print_mark
          (PrettyPrint.print_computation (module ResourceGrade) c)
    | Unbox, Ast.Unbox (e, (p, c)) ->
        print ~at_level:2 "@[<v 0>%t@[<hov 2>unbox %t as %t@]%t in@,%t@]"
          print_mark
          (PrettyPrint.print_expression (module ResourceGrade) ~max_level:0 e)
          (PrettyPrint.print_pattern p)
          print_mark
          (PrettyPrint.print_computation (module ResourceGrade) c)
    | _, comp ->
        print "%t%t%t" print_mark
          (fun ppf ->
            PrettyPrint.print_computation
              (module ResourceGrade)
              ?max_level comp ppf)
          print_mark

  let rec print_computation_reduction ?max_level red c ppf =
    let print ?at_level = Print.print ?max_level ?at_level ppf in
    match (red, c) with
    | DoCtx red, Ast.Do (c1, (Ast.PNonbinding, c2)) ->
        print ~at_level:2 "@[<v 0>%t;@,%t@]"
          (print_computation_reduction ~max_level:1 red c1)
          (PrettyPrint.print_computation (module ResourceGrade) c2)
    | DoCtx red, Ast.Do (c1, (pat, c2)) ->
        print ~at_level:2 "@[<v 0>@[<hov 2>let %t =@ %t@] in@,%t@]"
          (PrettyPrint.print_pattern pat)
          (print_computation_reduction ~max_level:1 red c1)
          (PrettyPrint.print_computation (module ResourceGrade) c2)
    | HandleCtx red, Ast.Handle (c, h) ->
        print ~at_level:1 "@[<v 0>handle@;<1 2>%t@,with %t@]"
          (print_computation_reduction red c)
          (PrettyPrint.print_expression (module ResourceGrade) ~max_level:0 h)
    | ComputationRedex redex, c ->
        print_computation_redex ?max_level redex c ppf
    | _, _ ->
        Error.runtime "internal: malformed reduction context in redex selector"

  let view_computation_with_redexes red comp =
    let rendered =
      match red with
      | None ->
          Format.asprintf "%t"
            (PrettyPrint.print_computation (module ResourceGrade) comp)
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
