open Ast
open Exception
module Error = Utils.Error
module Symbol = Utils.Symbol

module type S = sig
  type var
  type base
  type 'a map_or_resource_grade
  type 'a t

  val empty : 'a t
  val add_temp : base resource_grade -> 'a t -> 'a t
  val add_variable : var -> 'a -> 'a t -> 'a t
  val find_variable : var -> 'a t -> 'a
  val find_variable_opt : var -> 'a t -> 'a option
  val abstract_resource_grade_sum : 'a t -> base resource_grade
  val eval_resource_grade : base resource_grade -> base
end

module Make
    (Variable : Symbol.S)
    (VariableMap : Map.S with type key = Variable.t)
    (Base : Grade.S) =
struct
  type var = Variable.t
  type base = Base.t
  type base_resource_grade = base resource_grade

  type 'a map_or_resource_grade =
    (var, 'a VariableMap.t, base_resource_grade) context_elem_ty

  type 'a t = (var, 'a VariableMap.t, base_resource_grade) context

  let empty : 'a t = []

  let add_temp (n : base_resource_grade) (lst : 'a t) : 'a t =
    match n with
    | ResourceGradeConst z when z = Base.zero -> lst
    | _ -> ResourceGrade n :: lst

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
      | ResourceGrade _ :: rest -> find rest
    in
    find lst

  let find_variable_opt (key : var) (lst : 'a t) : 'a option =
    let rec find = function
      | [] -> None
      | VarMap map :: rest -> (
          match VariableMap.find_opt key map with
          | Some v -> Some v
          | None -> find rest)
      | ResourceGrade _ :: rest -> find rest
    in
    find lst

  let sum_resource_grades_added_after (key : var) (lst : 'a t) :
      base_resource_grade =
    let rec go acc = function
      | [] ->
          raise (VariableNotFound (Format.asprintf "%t" (Variable.print key)))
      | ResourceGrade t :: rest -> go (Ast.ResourceGradeAdd (acc, t)) rest
      | VarMap map :: rest -> (
          match VariableMap.find_opt key map with
          | Some _ -> acc
          | None -> go acc rest)
    in
    go (Ast.ResourceGradeConst Base.zero) lst

  let abstract_resource_grade_sum (lst : 'a t) : base_resource_grade =
    let rec sum acc = function
      | [] -> acc
      | ResourceGrade t :: rest -> sum (ResourceGradeAdd (acc, t)) rest
      | VarMap _ :: rest -> sum acc rest
    in
    sum (ResourceGradeConst Base.zero) lst

  let rec eval_resource_grade (t : base_resource_grade) : base =
    match t with
    | ResourceGradeConst c -> c
    | ResourceGradeParam _ ->
        raise
          (ResourceGradeParamInEval
             "ResourceGradeParam not supported in eval_resource_grade")
    | ResourceGradeAdd (t1, t2) ->
        Base.add (eval_resource_grade t1) (eval_resource_grade t2)
end
