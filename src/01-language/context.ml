open Ast
open Exception
module Error = Utils.Error
module Symbol = Utils.Symbol

module type S = sig
  type var
  type base
  type elapsed
  type barrier
  type 'a map_or_rho
  type 'a t

  val empty : 'a t
  val add_temp : elapsed -> 'a t -> 'a t
  val add_variable : var -> 'a -> 'a t -> 'a t
  val add_barrier : barrier -> 'a t -> 'a t
  val find_variable : var -> 'a t -> 'a
  val find_variable_opt : var -> 'a t -> 'a option
  val barrier_after : var -> 'a t -> barrier option
  val sum_rhos_added_after : var -> 'a t -> base rho
  val elapsed_after : var -> 'a t -> elapsed list
  val abstract_rho_sum : 'a t -> base rho
  val eval_rho : base rho -> base
end

(** A context is a stack of variable bindings interleaved with the resource
    grades accumulated between them. [Elapsed] is a payload the context stores
    as it is and only ever reads a grade out of: the interpreter needs the grade
    alone, while the typechecker also remembers where it came from, so that an
    error can point at the [delay], [perform] or sequenced computation that
    spent it. [Barrier] is a payload of the same kind, stored at the point an
    operation case restricts the context and never read by the context itself.
*)
module Make
    (Variable : Symbol.S)
    (VariableMap : Map.S with type key = Variable.t)
    (Base : ResourceGrade.Grade)
    (Elapsed : sig
      type t

      val rho : t -> Base.t rho
    end)
    (Barrier : sig
      type t
    end) =
struct
  type var = Variable.t
  type base = Base.t
  type base_rho = base rho
  type elapsed = Elapsed.t
  type barrier = Barrier.t
  type 'a map_or_rho = (var, 'a VariableMap.t, elapsed, barrier) context_elem_ty
  type 'a t = (var, 'a VariableMap.t, elapsed, barrier) context

  let empty : 'a t = []

  let add_temp (n : elapsed) (lst : 'a t) : 'a t =
    match Elapsed.rho n with
    | RhoConst z when z = Base.zero -> lst
    | _ -> Rho n :: lst

  let add_barrier (b : barrier) (lst : 'a t) : 'a t = Barrier b :: lst

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
      | (Rho _ | Barrier _) :: rest -> find rest
    in
    find lst

  let find_variable_opt (key : var) (lst : 'a t) : 'a option =
    let rec find = function
      | [] -> None
      | VarMap map :: rest -> (
          match VariableMap.find_opt key map with
          | Some v -> Some v
          | None -> find rest)
      | (Rho _ | Barrier _) :: rest -> find rest
    in
    find lst

  (** [barrier_after key lst] is the outermost barrier standing between the
      binding of [key] and the front of [lst], if any: walking front-to-binding
      meets it last. *)
  let barrier_after (key : var) (lst : 'a t) : barrier option =
    let rec go outermost = function
      | [] ->
          raise (VariableNotFound (Format.asprintf "%t" (Variable.print key)))
      | Barrier b :: rest -> go (Some b) rest
      | Rho _ :: rest -> go outermost rest
      | VarMap map :: rest -> (
          match VariableMap.find_opt key map with
          | Some _ -> outermost
          | None -> go outermost rest)
    in
    go None lst

  (** [sum_rhos_added_after key lst] is the sum of the resource grades recorded
      in [lst] since [key] was bound. The context is kept most-recent-first, so
      walking it from the front visits the grades newest-first and each one is
      added on the *left* of what has been accumulated so far: the sum reads
      chronologically, oldest first. This matters for the non-commutative
      (timed-trace) grades, where the order of the summands is the order the
      events happened in. *)
  let sum_rhos_added_after (key : var) (lst : 'a t) : base_rho =
    let rec go acc = function
      | [] ->
          raise (VariableNotFound (Format.asprintf "%t" (Variable.print key)))
      | Rho t :: rest -> go (Ast.RhoAdd (Elapsed.rho t, acc)) rest
      | Barrier _ :: rest -> go acc rest
      | VarMap map :: rest -> (
          match VariableMap.find_opt key map with
          | Some _ -> acc
          | None -> go acc rest)
    in
    go (Ast.RhoConst Base.zero) lst

  (** [elapsed_after key lst] are the entries recorded in [lst] since [key] was
      bound, oldest first: the summands of {!sum_rhos_added_after} with whatever
      else the client stored alongside them still attached. *)
  let elapsed_after (key : var) (lst : 'a t) : elapsed list =
    let rec go acc = function
      | [] ->
          raise (VariableNotFound (Format.asprintf "%t" (Variable.print key)))
      | Rho t :: rest -> go (t :: acc) rest
      | Barrier _ :: rest -> go acc rest
      | VarMap map :: rest -> (
          match VariableMap.find_opt key map with
          | Some _ -> acc
          | None -> go acc rest)
    in
    go [] lst

  (** [abstract_rho_sum lst] is the sum of all the resource grades recorded in
      [lst], oldest first, for the same reason as in {!sum_rhos_added_after}. *)
  let abstract_rho_sum (lst : 'a t) : base_rho =
    let rec sum acc = function
      | [] -> acc
      | Rho t :: rest -> sum (RhoAdd (Elapsed.rho t, acc)) rest
      | (VarMap _ | Barrier _) :: rest -> sum acc rest
    in
    sum (RhoConst Base.zero) lst

  let rec eval_rho (t : base_rho) : base =
    match t with
    | RhoConst c -> c
    | RhoParam _ | RhoRigid _ ->
        raise (RhoParamInEval "RhoParam not supported in eval_rho")
    | RhoAdd (t1, t2) -> Base.add (eval_rho t1) (eval_rho t2)
end
