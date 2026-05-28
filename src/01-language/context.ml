open Ast
open Exception
module Error = Utils.Error
module Symbol = Utils.Symbol

module type S = sig
  type var
  type base
  type 'a map_or_rho
  type 'a t

  val empty : 'a t
  val add_temp : base rho -> 'a t -> 'a t
  val add_variable : var -> 'a -> 'a t -> 'a t
  val find_variable : var -> 'a t -> 'a
  val find_variable_opt : var -> 'a t -> 'a option
  val abstract_rho_sum : 'a t -> base rho
  val eval_rho : base rho -> base
end

module Make
    (Variable : Symbol.S)
    (VariableMap : Map.S with type key = Variable.t)
    (Base : ResourceGrade.Grade) =
struct
  type var = Variable.t
  type base = Base.t
  type base_rho = base rho
  type 'a map_or_rho = (var, 'a VariableMap.t, base_rho) context_elem_ty
  type 'a t = (var, 'a VariableMap.t, base_rho) context

  let empty : 'a t = []

  let add_temp (n : base_rho) (lst : 'a t) : 'a t =
    match n with RhoConst z when z = Base.zero -> lst | _ -> Rho n :: lst

  let add_variable (key : var) (value : 'a) (lst : 'a t) : 'a t =
    match lst with
    | VarMap map :: rest ->
        let updated_map = VariableMap.add key value map in
        VarMap updated_map :: rest
    | _ ->
        let new_map = VariableMap.add key value VariableMap.empty in
        VarMap new_map :: lst

  let find_variable (key : var) (lst : 'a t) : 'a =
    let rec find = function
      | [] ->
          raise (VariableNotFound (Format.asprintf "%t" (Variable.print key)))
      | VarMap map :: rest -> (
          match VariableMap.find_opt key map with
          | Some v -> v
          | None -> find rest)
      | Rho _ :: rest -> find rest
    in
    find lst

  let find_variable_opt (key : var) (lst : 'a t) : 'a option =
    let rec find = function
      | [] -> None
      | VarMap map :: rest -> (
          match VariableMap.find_opt key map with
          | Some v -> Some v
          | None -> find rest)
      | Rho _ :: rest -> find rest
    in
    find lst

  let sum_rhos_added_after (key : var) (lst : 'a t) : base_rho =
    let rec go acc = function
      | [] ->
          raise (VariableNotFound (Format.asprintf "%t" (Variable.print key)))
      | Rho t :: rest -> go (Ast.RhoAdd (acc, t)) rest
      | VarMap map :: rest -> (
          match VariableMap.find_opt key map with
          | Some _ -> acc
          | None -> go acc rest)
    in
    go (Ast.RhoConst Base.zero) lst

  let abstract_rho_sum (lst : 'a t) : base_rho =
    let rec sum acc = function
      | [] -> acc
      | Rho t :: rest -> sum (RhoAdd (acc, t)) rest
      | VarMap _ :: rest -> sum acc rest
    in
    sum (RhoConst Base.zero) lst

  let rec eval_rho (t : base_rho) : base =
    match t with
    | RhoConst c -> c
    | RhoParam _ -> raise (RhoParamInEval "RhoParam not supported in eval_rho")
    | RhoAdd (t1, t2) -> Base.add (eval_rho t1) (eval_rho t2)
end
