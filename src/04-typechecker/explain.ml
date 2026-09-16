(** Turning a failed constraint into a diagnostic. The solver knows which
    constraint failed and, through its {!Language.Ast.reason}, which syntax
    asked for it; the sentence and the places to point at are decided here, so
    that [typechecker.ml] stays about inference and solving. *)

module Error = Utils.Error
module Diagnostic = Utils.Diagnostic
module Location = Utils.Location
module Ast = Language.Ast

module Make (ResourceGrade : Language.ResourceGrade.Grade) = struct
  type ty = ResourceGrade.t Ast.ty
  type rho = ResourceGrade.t Ast.rho
  type reason = ResourceGrade.t Ast.reason

  type printer = {
    ty : ty -> string;
    rho : rho -> string;
    is_zero : rho -> bool;
        (** Asked of the elapsed grades; a zero grade gets no label. *)
  }
  (** How one diagnostic renders types and grades. The printers number the
      parameters as they first meet them, so α and ρ₀ agree across a headline
      and its labels only while the same [printer] is used throughout. *)

  let label span text = { Diagnostic.span; text }

  (** A variable as a message may name it. A compiler-invented one is not named
      at all — the programmer cannot recognise a name they never wrote — and the
      message talks about the expression instead. *)
  let var_name x =
    if Ast.Variable.is_synthetic x then None
    else Some (Ast.Variable.string_of x)

  let op_name op = Ast.OpName.string_of op
  let label_name lbl = Ast.Label.string_of lbl

  (* "Variable x" or, for a variable the programmer never wrote, "This
     expression"; both start a sentence. *)
  let subject x =
    match var_name x with
    | Some name -> "Variable " ^ name
    | None -> "This expression"

  (* The same, for a sentence that continues with a verb: "x holds ...". *)
  let holder x =
    match var_name x with Some name -> name | None -> "This expression"

  let describe x =
    match var_name x with Some name -> name | None -> "this expression"

  (* One place that contributed to an elapsed grade, naming its share; the
     headline gives the total. A [delay n] states its grade in the source, so it
     is repeated only where the monoid renders it as other than the [n]. *)
  let elapsed_text p rho = function
    | Ast.Delayed n ->
        let g = p.rho rho in
        if g = string_of_int n then Printf.sprintf "delay %d elapses here" n
        else Printf.sprintf "delay %d elapses here (grade %s)" n g
    | Ast.Performed op ->
        Printf.sprintf "%s is performed here (grade %s)" (op_name op)
          (p.rho rho)
    | Ast.Sequenced ->
        Printf.sprintf "this computation runs here (grade %s)" (p.rho rho)
    | Ast.Boxed ->
        Printf.sprintf "the value is boxed here (grade %s ahead)" (p.rho rho)
    | Ast.Handled ->
        Printf.sprintf "the handled computation runs here (grade %s)"
          (p.rho rho)

  (* The context gets an entry for every computation a [let] sequences, whether
     or not it takes any time, and only the solved grade tells the two apart: a
     grade that solved to zero is no part of the explanation. *)
  let elapsed_labels p elapsed =
    List.filter_map
      (fun (rho, at, kind) ->
        if p.is_zero rho then None
        else Some (label at (elapsed_text p rho kind)))
      elapsed

  let binding_labels x bound_at =
    match bound_at with
    | None -> []
    | Some at ->
        [
          label at
            (match var_name x with
            | Some name -> name ^ " is bound here"
            | None -> "this value is bound here");
        ]

  (** The related places a reason contributes: where a variable was bound, where
      time passed since, where an operation was declared, and the chain back
      into the definition that needs the constraint.

      [scrutinee_ty] is the matched value's type; reasons carry no types, so
      only a caller holding the equation can supply it. [elapsed] is false on
      the type-equation path, where those grades are beside the point and, being
      unsubstituted, would print as parameters appearing nowhere else. *)
  let rec labels_of_reason p ?scrutinee_ty ?(elapsed = true) (reason : reason) =
    let elapsed_labels rhos = if elapsed then elapsed_labels p rhos else [] in
    match reason.why with
    | Ast.Application _ -> []
    | Ast.MatchScrutinee { scrutinee_at } -> (
        match scrutinee_ty with
        | None -> []
        | Some ty ->
            [ label scrutinee_at ("the matched value has type " ^ p.ty ty) ])
    | Ast.UseAfterTime { var; bound_at; elapsed } ->
        binding_labels var (Some bound_at) @ elapsed_labels elapsed
    | Ast.Unboxed { var; bound_at; elapsed } ->
        binding_labels var bound_at @ elapsed_labels elapsed
    | Ast.InstanceOf { var; defined_at; inner } ->
        let name = describe var in
        let definition =
          match defined_at with
          | None -> []
          | Some at -> [ label at (name ^ " is defined here") ]
        in
        definition
        @ [ label inner.at ("because of this use inside " ^ name) ]
        @ labels_of_reason p ~elapsed inner
    | Ast.HandlerCase { op; signature_at }
    | Ast.ContinuationGrade { op; signature_at }
    | Ast.PerformArgument { op; signature_at }
    | Ast.PerformContinuation { op; signature_at }
    | Ast.DefaultOf { op; signature_at } ->
        [ label signature_at ("operation " ^ op_name op ^ " is declared here") ]
    | Ast.MatchBranch | Ast.Annotation | Ast.PatternAnnotation
    | Ast.VariantArgument _ | Ast.BoxedValue | Ast.HandleWith
    | Ast.RecursiveDefinition _ | Ast.PureBody | Ast.Sequencing ->
        []

  let fail ~loc ~labels ~notes message =
    Error.typing ~loc ~labels ~notes "%s" message

  (* ------------------------------------------------------------------ *)
  (* Type equations                                                      *)
  (* ------------------------------------------------------------------ *)

  (** [ty_mismatch] reports an unsolvable equation between [lhs] and [rhs]. An
      equation is generated between whole types and then decomposed, so the
      construct that asked for it is several steps up, in [reason.path].

      Throughout [typechecker.ml] the demanded side is τ₁ — the parameter type,
      the operation's signature, the earlier branches — and the side the checked
      expression offers is τ₂, so a sentence can be read off the reason alone.
      [root] is the undecomposed equation, for the "while matching" note; [via]
      records which side arrived through a type parameter, whose deciding place
      is usually the real disagreement. *)
  let ty_mismatch p ~ty_reason ~lhs ~rhs ~(reason : reason) ~root ~via ~occurs =
    let t1 = p.ty lhs and t2 = p.ty rhs in
    let root_lhs, root_rhs = root in
    let r1 = p.ty root_lhs and r2 = p.ty root_rhs in
    let generic =
      Printf.sprintf "Type %s is not compatible with type %s" t1 t2
    in
    let at = reason.at in
    let primary, message, extra =
      match (reason.why, reason.path) with
      | Ast.Application { func_at; _ }, [] ->
          ( func_at,
            Printf.sprintf "This expression has type %s and cannot be applied"
              t1,
            [] )
      | Ast.Application { func_at; arg_at }, Ast.Argument :: _ ->
          ( arg_at,
            Printf.sprintf
              "This argument has type %s but the function expects %s" t2 t1,
            [ label func_at ("the function has type " ^ r1) ] )
      | Ast.Application _, Ast.Result :: _ ->
          (* The function fixes what the application really has, and the other
             side is what the surroundings ask for, so τ₁ and τ₂ read the other
             way round from the argument case. *)
          ( at,
            Printf.sprintf "The application has type %s but %s is expected here"
              t1 t2,
            [] )
      | Ast.MatchScrutinee _, _ ->
          ( at,
            Printf.sprintf
              "This pattern matches values of type %s but the matched value \
               has type %s"
              t2 t1,
            [] )
      | Ast.MatchBranch, _ ->
          ( at,
            Printf.sprintf
              "This branch has type %s but the earlier branches have type %s" t2
              t1,
            [] )
      | Ast.Annotation, _ ->
          ( at,
            Printf.sprintf
              "This expression has type %s but is annotated with %s" t2 t1,
            [] )
      | Ast.PatternAnnotation, _ ->
          ( at,
            Printf.sprintf "This pattern has type %s but is annotated with %s"
              t2 t1,
            [] )
      | Ast.VariantArgument lbl, _ ->
          ( at,
            Printf.sprintf
              "Constructor %s expects an argument of type %s but is given %s"
              (label_name lbl) t1 t2,
            [] )
      | Ast.BoxedValue, _ ->
          ( at,
            Printf.sprintf "The boxed value has type %s but is bound as %s" t1
              t2,
            [] )
      | Ast.Unboxed { var; _ }, [] ->
          ( at,
            Printf.sprintf
              "%s has type %s, which is not a boxed type, and cannot be unboxed"
              (subject var) t2,
            [] )
      | Ast.Unboxed { var; _ }, Ast.BoxContent :: _ ->
          ( at,
            Printf.sprintf "%s holds a value of type %s but it is bound as %s"
              (holder var) t2 t1,
            [] )
      | Ast.PerformArgument { op; _ }, _ ->
          ( at,
            Printf.sprintf
              "Operation %s takes an argument of type %s but is given %s"
              (op_name op) t2 t1,
            [] )
      | Ast.PerformContinuation { op; _ }, _ ->
          ( at,
            Printf.sprintf
              "The result of %s has type %s but the continuation binds it as %s"
              (op_name op) t2 t1,
            [] )
      | Ast.HandlerCase { op; _ }, path -> (
          (* The two equations of an operation case share a reason, and are
             told apart by their shape: only the second matches a pair. *)
          let op = op_name op in
          match (root_rhs, path) with
          | Ast.TyTuple _, [] ->
              ( at,
                Printf.sprintf
                  "The case for %s binds its argument and continuation as %s \
                   but they have types %s"
                  op t1 t2,
                [] )
          | Ast.TyTuple _, Ast.Component 1 :: _ ->
              ( at,
                Printf.sprintf
                  "The argument of %s has type %s but the case binds it as %s"
                  op t2 t1,
                [] )
          | Ast.TyTuple _, Ast.Component 2 :: _ ->
              ( at,
                Printf.sprintf
                  "The continuation of %s has type %s but the case binds it as \
                   %s"
                  op t2 t1,
                [] )
          | Ast.TyTuple _, _ -> (at, generic, [])
          | _ ->
              ( at,
                Printf.sprintf
                  "The case for %s returns %s but the return clause returns %s"
                  op t1 t2,
                [] ))
      | Ast.HandleWith, [] ->
          ( at,
            Printf.sprintf "This expression has type %s and is not a handler" t1,
            [] )
      | Ast.HandleWith, Ast.HandlerInput :: _ ->
          ( at,
            Printf.sprintf
              "This handler handles computations of type %s but the handled \
               computation has type %s"
              t1 t2,
            [] )
      | Ast.RecursiveDefinition f, _ ->
          ( at,
            Printf.sprintf
              "The recursive function %s is used at type %s but its definition \
               has type %s"
              (describe f) t1 t2,
            [] )
      | Ast.DefaultOf { op; _ }, Ast.Argument :: _ ->
          let op = op_name op in
          ( at,
            Printf.sprintf
              "The default implementation of %s binds its argument as %s but \
               %s takes %s"
              op t1 op t2,
            [] )
      | Ast.DefaultOf { op; _ }, Ast.Result :: _ ->
          let op = op_name op in
          ( at,
            Printf.sprintf
              "The default implementation of %s returns %s but %s returns %s" op
              t1 op t2,
            [] )
      | _ -> (at, generic, [])
    in
    let message =
      if occurs then
        Printf.sprintf "Cannot construct the infinite type %s = %s" t1 t2
      else message
    in
    let labels =
      extra @ labels_of_reason p ~scrutinee_ty:root_lhs ~elapsed:false reason
    in
    (* Label the place a type parameter was decided, unless it is already
       pointed at as the primary or by a more specific label. *)
    let via_label a shown =
      match ty_reason a with
      | Some (r : reason)
        when (not (Location.equal r.at primary))
             && not
                  (List.exists
                     (fun (l : Diagnostic.label) -> Location.equal l.span r.at)
                     labels) ->
          [ label r.at (shown ^ " was inferred here") ]
      | _ -> []
    in
    let via_labels =
      (match fst via with Some a -> via_label a t1 | None -> [])
      @ match snd via with Some a -> via_label a t2 | None -> []
    in
    let notes =
      if r1 = t1 && r2 = t2 then []
      else [ Printf.sprintf "while matching %s against %s" r1 r2 ]
    in
    fail ~loc:primary ~labels:(labels @ via_labels) ~notes message

  (* ------------------------------------------------------------------ *)
  (* Grade equations                                                     *)
  (* ------------------------------------------------------------------ *)

  let matching_note p = function
    | None -> []
    | Some (lhs, rhs) ->
        [ Printf.sprintf "while matching %s against %s" (p.ty lhs) (p.ty rhs) ]

  (** The grade unifier can make no more progress. Every stuck equation is
      shown: which one to change is exactly what it could not decide. *)
  let rho_stuck p unsolved =
    match unsolved with
    | [] -> assert false
    | (lhs, rhs, (reason : reason), root) :: rest ->
        let show (lhs, rhs, _, _) =
          Printf.sprintf "%s = %s" (p.rho lhs) (p.rho rhs)
        in
        let message =
          "Cannot determine the grades: "
          ^ String.concat ", "
              (List.map show ((lhs, rhs, reason, root) :: rest))
        in
        fail ~loc:reason.at
          ~labels:
            (labels_of_reason p reason
            @ List.map
                (fun ((_, _, (r : reason), _) as eq) ->
                  label r.at ("and here: " ^ show eq))
                rest)
          ~notes:(matching_note p root) message

  let rigid_required_equal p ~rigid ~other ~(reason : reason) ~root =
    fail ~loc:reason.at
      ~labels:(labels_of_reason p reason)
      ~notes:(matching_note p root)
      (Printf.sprintf
         "The grade %s of a handler continuation may be any grade, but here it \
          is required to equal %s"
         (p.rho rigid) (p.rho other))

  (* ------------------------------------------------------------------ *)
  (* Inequalities and eternality                                         *)
  (* ------------------------------------------------------------------ *)

  (* A refuting instance of the universally quantified continuation grades. *)
  let witness_text rho1 rho2 = function
    | None -> ""
    | Some w ->
        let rigid =
          Ast.RhoParamSet.union (Ast.rigid_rhos rho1) (Ast.rigid_rhos rho2)
        in
        Printf.sprintf ", already when the continuation grade%s %s %s"
          (if Ast.RhoParamSet.cardinal rigid > 1 then "s" else "")
          (if Ast.RhoParamSet.cardinal rigid > 1 then "are" else "is")
          (ResourceGrade.show w)

  let ineq_text p rho1 rho2 witness =
    Printf.sprintf "the resource inequality %s %s %s does not hold%s"
      (p.rho rho1) ResourceGrade.is_sub_rho_symbol (p.rho rho2)
      (witness_text rho1 rho2 witness)

  (** An inequality between grades that cannot hold. Only the reason knows which
      promise it breaks. *)
  let ineq_failed p rho1 rho2 (reason : reason) witness =
    let g1 = p.rho rho1 and g2 = p.rho rho2 in
    let specific, message =
      match reason.why with
      | Ast.Annotation ->
          ( true,
            Printf.sprintf
              "This function's body has grade %s, which does not match its \
               annotated grade %s"
              g1 g2 )
      | Ast.ContinuationGrade { op; _ } ->
          ( true,
            Printf.sprintf
              "The case for %s has grade %s, which does not match the grade %s \
               of %s followed by its continuation"
              (op_name op) g1 g2 (op_name op) )
      | Ast.Unboxed { var; _ } ->
          ( true,
            if p.is_zero rho1 then
              Printf.sprintf
                "%s is unboxed before any grade has elapsed, but its box grade \
                 is %s"
                (subject var) g2
            else
              Printf.sprintf
                "%s is unboxed after grade %s has elapsed, which does not \
                 match its box grade %s"
                (subject var) g1 g2 )
      | Ast.DefaultOf { op; _ } ->
          ( true,
            Printf.sprintf
              "The default implementation of %s has grade %s, which does not \
               match the declared grade %s of %s"
              (op_name op) g1 g2 (op_name op) )
      | _ ->
          ( false,
            Printf.sprintf "The resource inequality %s %s %s does not hold" g1
              ResourceGrade.is_sub_rho_symbol g2 )
    in
    (* The note spells out the constraint, which the specific headlines do not,
       and carries the refuting instance. The generic headline already is the
       constraint, so there the note only earns its place with a witness. *)
    let notes =
      if specific || witness <> None then [ ineq_text p rho1 rho2 witness ]
      else []
    in
    fail ~loc:reason.at ~labels:(labels_of_reason p reason) ~notes message

  let not_eternal p ty (reason : reason) =
    let t = p.ty ty in
    let message =
      match reason.why with
      | Ast.UseAfterTime { var; _ } ->
          Printf.sprintf
            "%s has type %s, which is not eternal, but is used after a grade \
             has elapsed"
            (subject var) t
      | Ast.InstanceOf { var; _ } ->
          Printf.sprintf "Type %s is not eternal, as required by the type of %s"
            t (describe var)
      | _ -> Printf.sprintf "Type %s is not eternal" t
    in
    fail ~loc:reason.at ~labels:(labels_of_reason p reason) ~notes:[] message

  (** A disjunction [eternal τ ∨ ρ₁ ≾ ρ₂] with both sides refuted. *)
  let eternal_or_ineq_failed p ty rho1 rho2 (reason : reason) witness =
    let t = p.ty ty in
    let message, notes =
      match reason.why with
      | Ast.UseAfterTime { var; _ } ->
          ( Printf.sprintf
              "%s is used after grade %s has elapsed, but its type %s is not \
               eternal"
              (subject var) (p.rho rho1) t,
            [ ineq_text p rho1 rho2 witness ] )
      | Ast.InstanceOf { var; _ } ->
          ( Printf.sprintf
              "Type %s is not eternal, as required by the type of %s" t
              (describe var),
            [ ineq_text p rho1 rho2 witness ] )
      | _ ->
          ( Printf.sprintf "Type %s is not eternal and %s" t
              (ineq_text p rho1 rho2 witness),
            [] )
    in
    fail ~loc:reason.at ~labels:(labels_of_reason p reason) ~notes message

  (** The same disjunction, its inequality left open rather than refuted. *)
  let eternal_or_ineq_unknown p ty rho1 rho2 (reason : reason) =
    let t = p.ty ty in
    let g1 = p.rho rho1 and g2 = p.rho rho2 in
    let message, notes =
      match reason.why with
      | Ast.UseAfterTime { var; _ } ->
          ( Printf.sprintf
              "%s is used after grade %s has elapsed, but its type %s is not \
               eternal and grade %s cannot be compared with %s"
              (subject var) g1 t g1 g2,
            [] )
      | Ast.InstanceOf { var; _ } ->
          ( Printf.sprintf
              "Type %s is not eternal, as required by the type of %s" t
              (describe var),
            [ Printf.sprintf "grade %s cannot be compared with %s" g1 g2 ] )
      | _ ->
          ( Printf.sprintf
              "Type %s is not eternal and cannot compare non-ground resource \
               values %s and %s"
              t g1 g2,
            [] )
    in
    fail ~loc:reason.at ~labels:(labels_of_reason p reason) ~notes message

  (** An inequality that survived solving with unknown grades still in it.
      Inequalities are not carried into schemes, so it cannot be deferred. *)
  let non_ground_ineq p rho1 rho2 (reason : reason) =
    let rigid =
      Ast.RhoParamSet.union (Ast.rigid_rhos rho1) (Ast.rigid_rhos rho2)
    in
    let rigid_clause =
      match Ast.RhoParamSet.elements rigid with
      | [] -> ""
      | [ r ] ->
          Printf.sprintf
            ", where %s is the grade of a handler continuation and may be any \
             grade"
            (p.rho (Ast.RhoRigid r))
      | rs ->
          Printf.sprintf
            ", where %s are grades of handler continuations and may be any \
             grades"
            (String.concat ", " (List.map (fun r -> p.rho (Ast.RhoRigid r)) rs))
    in
    fail ~loc:reason.at
      ~labels:(labels_of_reason p reason)
      ~notes:[]
      (Printf.sprintf "Cannot compare non-ground resource values %s and %s%s"
         (p.rho rho1) (p.rho rho2) rigid_clause)
end
