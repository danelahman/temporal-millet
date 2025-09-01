(** Generic pretty-printing functions *)

let message ?loc ~header fmt =
  match loc with
  | Some loc ->
      Format.fprintf Format.err_formatter
        ("%s (%t):@," ^^ fmt ^^ "@.")
        header (Location.print loc)
  | _ -> Format.fprintf Format.err_formatter ("%s: " ^^ fmt ^^ "@.") header

let error ?loc err_kind fmt = message ?loc ~header:err_kind fmt
let check ?loc fmt = message ?loc ~header:"Check" fmt
let warning ?loc fmt = message ?loc ~header:"Warning" fmt
let debug ?loc fmt = message ?loc ~header:"Debug" fmt

let print ?(at_level = min_int) ?(max_level = max_int) ppf =
  if at_level <= max_level then Format.fprintf ppf
  else fun fmt -> Format.fprintf ppf ("(" ^^ fmt ^^ ")")

let rec print_sequence sep pp vs ppf =
  match vs with
  | [] -> ()
  | [ v ] -> pp v ppf
  | v :: vs ->
      Format.fprintf ppf "%t%s@,%t" (pp v) sep (print_sequence sep pp vs)

let print_sequence_with_start_sep sep pp vs ppf =
  if List.length vs > 0 then
    Format.fprintf ppf "%s%t" sep (print_sequence sep pp vs)
  else print_sequence sep pp vs ppf

let rec print_cases pp vs ppf =
  match vs with
  | [] -> ()
  | [ v ] -> pp v ppf
  | v :: vs -> Format.fprintf ppf "%t@,| %t" (pp v) (print_cases pp vs)

let print_field fpp vpp (f, v) ppf = print ppf "%t = %t" (fpp f) (vpp v)

let print_tuple ?max_level pp lst ppf =
  match lst with
  | [] -> print ppf "()"
  | lst ->
      let print_elem x = pp ?max_level x in
      print ppf "(@[<hov>%t@])" (print_sequence ", " print_elem lst)

let print_record fpp vpp assoc ppf =
  print ppf "{@[<hov>%t@]}" (print_sequence "; " (print_field fpp vpp) assoc)
