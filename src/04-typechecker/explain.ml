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

  (** A fragment of the user's code inside a sentence, marked as {!Diagnostic}
      marks it: this is the one place a diagnostic's backticks come from. *)
  let code text = "`" ^ text ^ "`"

  type printer = {
    ty : ty -> string;  (** the type as a code fragment *)
    rho : rho -> string;  (** the grade as a code fragment *)
    ty_raw : ty -> string;
    rho_raw : rho -> string;
        (** Unmarked, for a sentence that spells out a larger fragment — an
            equation or an inequality — and marks that as a whole. *)
    is_zero : rho -> bool;
        (** Asked of the elapsed grades; a zero grade gets no label. *)
  }
  (** How one diagnostic renders types and grades. The printers number the
      parameters as they first meet them, so α and ρ₀ agree across a headline
      and its labels only while the same [printer] is used throughout. *)

  let label span text = { Diagnostic.span; text }

  (* A label that points at its own span says so; how it is worded is the
     renderer's, since only it knows whether the span is shown alongside. *)
  let here = Diagnostic.place

  (** A variable as a message may name it, as a code fragment. A
      compiler-invented one is not named at all — the programmer cannot
      recognise a name they never wrote — and the message talks about the
      expression instead, in prose. *)
  let var_name x =
    if Ast.Variable.is_synthetic x then None
    else Some (code (Ast.Variable.string_of x))

  let op_name op = code (Ast.OpName.string_of op)
  let label_name lbl = code (Ast.Label.string_of lbl)

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

  (* What a definition needs eternal, as "f needs ... to be eternal" continues.
     A compiler-invented variable has no name to give, so the phrase says what
     the definition holds on to instead. *)
  let needed_eternal x =
    match var_name x with
    | Some name -> "the type of " ^ name
    | None -> "the type it keeps across a delay"

  (* ------------------------------------------------------------------ *)
  (* Universally quantified continuation grades                          *)
  (* ------------------------------------------------------------------ *)

  type rigids = Ast.rigid_origin Ast.RhoParamMap.t
  (** Where each rigid grade a message may print was introduced. One that
      reaches a message without an origin is described as before. *)

  (* The continuation of a case, as the subject of a sentence and as a noun
     phrase; it is named only when the case binds it to a variable. *)
  let continuation_subject (o : Ast.rigid_origin) =
    match Option.map var_name o.continuation with
    | Some (Some name) -> name
    | _ -> "the continuation"

  let continuation_phrase (o : Ast.rigid_origin) =
    match Option.map var_name o.continuation with
    | Some (Some name) -> "the continuation " ^ name
    | _ -> "the continuation"

  (** The rigid grades of [rhos] whose origin is known, outermost case first:
      [Location.compare] puts a containing span before the span it contains. *)
  let rigids_of rigids rhos =
    List.fold_left
      (fun acc rho -> Ast.RhoParamSet.union acc (Ast.rigid_rhos rho))
      Ast.RhoParamSet.empty rhos
    |> Ast.RhoParamSet.elements
    |> List.filter_map (fun r ->
        Option.map (fun o -> (r, o)) (Ast.RhoParamMap.find_opt r rigids))
    |> List.sort (fun (_, o1) (_, o2) ->
        Location.compare o1.Ast.case_at o2.Ast.case_at)

  (** The fixed form the messages quantify with. It renders the grades, so a
      sentence that opens with it numbers them from the outermost case. *)
  let for_every p rs =
    "for every "
    ^ String.concat " and every "
        (List.map
           (fun (r, o) ->
             Printf.sprintf "grade %s %s may have" (p.rho (Ast.RhoRigid r))
               (continuation_phrase o))
           rs)

  (* A rigid grade on first mention, where the sentence has room for it. *)
  let rigid_description p (r, (o : Ast.rigid_origin)) =
    Printf.sprintf "%s, the grade of %s in the case for %s"
      (p.rho (Ast.RhoRigid r)) (continuation_phrase o) (op_name o.op)

  let rigid_labels p rs =
    List.map
      (fun (r, (o : Ast.rigid_origin)) ->
        label o.continuation_at
          (Printf.sprintf "%s may have any grade %s" (continuation_subject o)
             (p.rho (Ast.RhoRigid r))))
      rs

  (* The refuting instance of the quantified grades, spelled out as the
     inequality it turns the constraint into. *)
  let witness_clause p rs rho1 rho2 = function
    | None -> ""
    | Some w ->
        Printf.sprintf ": for %s it becomes %s"
          (String.concat " and "
             (List.map
                (fun (r, _) ->
                  code
                    (Printf.sprintf "%s = %s"
                       (p.rho_raw (Ast.RhoRigid r))
                       (ResourceGrade.show w)))
                rs))
          (code
             (Printf.sprintf "%s %s %s"
                (p.rho_raw (Ast.instantiate_rigid w rho1))
                ResourceGrade.is_sub_rho_symbol
                (p.rho_raw (Ast.instantiate_rigid w rho2))))

  (* A fragment whose sides mention rigid grades states what it claims of every
     one of them, so it carries its quantifier; one without them stands as it
     is. Both the inequalities and the equations go through here. *)
  let quantified p rs text =
    match rs with
    | [] -> code text
    | rs ->
        code
          (Printf.sprintf "∀%s. %s"
             (String.concat " "
                (List.map (fun (r, _) -> p.rho_raw (Ast.RhoRigid r)) rs))
             text)

  (* One place that contributed to an elapsed grade, naming its share; the
     headline gives the total. A [delay n] states its grade in the source, so it
     is repeated only where the monoid renders it as other than the [n]. *)
  let elapsed_text p rho = function
    | Ast.Delayed n ->
        let g = p.rho_raw rho in
        if g = string_of_int n then
          Printf.sprintf "%s elapses %s"
            (code (Printf.sprintf "delay %d" n))
            here
        else
          Printf.sprintf "%s elapses %s (grade %s)"
            (code (Printf.sprintf "delay %d" n))
            here (code g)
    | Ast.Performed op ->
        Printf.sprintf "%s is performed %s (grade %s)" (op_name op) here
          (p.rho rho)
    | Ast.Sequenced ->
        Printf.sprintf "this computation runs %s (grade %s)" here (p.rho rho)
    | Ast.Boxed ->
        Printf.sprintf "the value is boxed %s (grade %s ahead)" here (p.rho rho)
    | Ast.Handled ->
        Printf.sprintf "the handled computation runs %s (grade %s)" here
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
            | Some name -> name ^ " is bound " ^ here
            | None -> "this value is bound " ^ here);
        ]

  (* The use an eternality obligation began at, as [(use_at, var, bound_at,
     elapsed)]: a scheme's eternality obligation always starts at a use after
     time, and the [InstanceOf] levels only record which definitions it came
     through. *)
  let rec use_after_time (reason : reason) =
    match reason.why with
    | Ast.InstanceOf { inner; _ } -> use_after_time inner
    | Ast.UseAfterTime { var; bound_at; elapsed } ->
        Some (reason.at, var, bound_at, elapsed)
    | _ -> None

  (* The grades of [elapsed] as one sum, in source order, or [None] when there
     are none; the printer simplifies it. *)
  let total_elapsed = function
    | [] -> None
    | (rho, _, _) :: rest ->
        Some
          (List.fold_left
             (fun acc (rho, _, _) -> Ast.RhoAdd (acc, rho))
             rho rest)

  (** The story of a use after time, in source order: where the variable was
      bound, where the grade was spent, and the use itself. The use says what it
      needs, because a headline built on it speaks of the definition instead. *)
  let use_after_time_labels p ~elapsed:show_elapsed ~use_at var bound_at elapsed
      =
    let spent = if show_elapsed then elapsed_labels p elapsed else [] in
    let total = if show_elapsed then total_elapsed elapsed else None in
    let use =
      match total with
      | Some g when not (p.is_zero g) ->
          Printf.sprintf
            "%s is used %s after grade %s has elapsed, which only an eternal \
             type allows"
            (describe var) here (p.rho g)
      | _ -> Printf.sprintf "%s is used %s" (describe var) here
    in
    List.stable_sort
      (fun (l1 : Diagnostic.label) (l2 : Diagnostic.label) ->
        Location.compare l1.span l2.span)
      (binding_labels var (Some bound_at) @ spent @ [ label use_at use ])

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
        let definition =
          match defined_at with
          | None -> []
          | Some at -> [ label at (describe var ^ " is defined " ^ here) ]
        in
        (* A chain of definitions lists them outermost first and then tells the
           story of the use that started it all; the recursion does both. *)
        let rest =
          match inner.why with
          | Ast.UseAfterTime { var; bound_at; elapsed = spent } ->
              use_after_time_labels p ~elapsed ~use_at:inner.at var bound_at
                spent
          | _ -> labels_of_reason p ~elapsed inner
        in
        definition @ rest
    | Ast.HandlerCase { op; signature_at }
    | Ast.ContinuationGrade { op; signature_at }
    | Ast.PerformArgument { op; signature_at }
    | Ast.PerformContinuation { op; signature_at }
    | Ast.DefaultOf { op; signature_at } ->
        [
          label signature_at ("operation " ^ op_name op ^ " is declared " ^ here);
        ]
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
        Printf.sprintf "Cannot construct the infinite type %s"
          (code (Printf.sprintf "%s = %s" (p.ty_raw lhs) (p.ty_raw rhs)))
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
          [ label r.at (shown ^ " was inferred " ^ here) ]
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
  let rho_stuck p ~rigids unsolved =
    match unsolved with
    | [] -> assert false
    | (lhs, rhs, (reason : reason), root) :: rest ->
        let show (lhs, rhs, _, _) =
          quantified p
            (rigids_of rigids [ lhs; rhs ])
            (Printf.sprintf "%s = %s" (p.rho_raw lhs) (p.rho_raw rhs))
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
                  label r.at ("and " ^ here ^ ": " ^ show eq))
                rest)
          ~notes:(matching_note p root) message

  let rigid_required_equal p ~rigids ~rigid ~other ~(reason : reason) ~root =
    let rs = rigids_of rigids [ rigid; other ] in
    let named =
      match rigid with
      | Ast.RhoRigid r ->
          Option.map (fun o -> (r, o)) (Ast.RhoParamMap.find_opt r rigids)
      | _ -> None
    in
    let message =
      match named with
      | None ->
          Printf.sprintf
            "The grade %s of a handler continuation may be any grade, but here \
             it is required to equal %s"
            (p.rho rigid) (p.rho other)
      | Some (_, (o : Ast.rigid_origin)) ->
          let who = String.capitalize_ascii (continuation_phrase o) in
          let g = p.rho rigid in
          Printf.sprintf
            "%s in the case for %s may have any grade %s, but here %s is \
             required to equal %s"
            who (op_name o.op) g g (p.rho other)
    in
    fail ~loc:reason.at
      ~labels:(labels_of_reason p reason @ rigid_labels p rs)
      ~notes:(matching_note p root) message

  (* ------------------------------------------------------------------ *)
  (* Inequalities and eternality                                         *)
  (* ------------------------------------------------------------------ *)

  (* The two sides a headline names, with the rigid grades it must quantify
     over: the inequality as it was generated, unless the cancellation dropped
     one of those grades, in which case only the cancelled form can be said. *)
  let stated_sides ~rigids (reason : reason) rho1 rho2 =
    let rs = rigids_of rigids [ rho1; rho2 ] in
    let same s1 s2 =
      List.map fst (rigids_of rigids [ s1; s2 ]) = List.map fst rs
    in
    match reason.Ast.stated with
    | Some (s1, s2) when same s1 s2 -> (s1, s2, rs)
    | Some _ | None -> (rho1, rho2, rs)

  (* The inequality is one fragment of the user's code, so its two sides come
     in unmarked and the whole of it is marked here. *)
  let ineq_code p rs g1 g2 =
    quantified p rs
      (Printf.sprintf "%s %s %s" g1 ResourceGrade.is_sub_rho_symbol g2)

  let plain_ineq p rs g1 g2 =
    Printf.sprintf "the resource inequality %s does not hold"
      (ineq_code p rs g1 g2)

  (* The constraint as a note. With rigid grades in it the note also carries
     the quantification and, when there is one, the refuting instance. *)
  let ineq_text p ~rigids rho1 rho2 witness =
    let rs = rigids_of rigids [ rho1; rho2 ] in
    let quantified = match rs with [] -> "" | rs -> for_every p rs in
    let g1 = p.rho_raw rho1 in
    let g2 = p.rho_raw rho2 in
    match rs with
    | [] -> plain_ineq p [] g1 g2
    | rs ->
        Printf.sprintf
          "%s, the resource inequality %s must hold, but does not%s" quantified
          (ineq_code p rs g1 g2)
          (witness_clause p rs rho1 rho2 witness)

  (* An inequality whose headline the reason picks; the quantification, if
     any, goes into the note. *)
  let ineq_failed_generic p ~rigids ~rs ~stated:(s1, s2) rho1 rho2
      (reason : reason) witness =
    let g1 = p.rho s1 and g2 = p.rho s2 in
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
            if p.is_zero s1 then
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
            Printf.sprintf "The resource inequality %s does not hold"
              (ineq_code p rs (p.rho_raw s1) (p.rho_raw s2)) )
    in
    (* The note spells out the constraint, which the specific headlines do not,
       and carries the quantification and the refuting instance; the generic
       headline already is the constraint, so there it needs one of those. *)
    let notes =
      if specific || witness <> None || rs <> [] || (s1, s2) <> (rho1, rho2)
      then [ ineq_text p ~rigids rho1 rho2 witness ]
      else []
    in
    fail ~loc:reason.at
      ~labels:(labels_of_reason p reason @ rigid_labels p rs)
      ~notes message

  (** An inequality between grades that cannot hold. Only the reason knows which
      promise it breaks. *)
  let ineq_failed p ~rigids rho1 rho2 (reason : reason) witness =
    let s1, s2, rs = stated_sides ~rigids reason rho1 rho2 in
    match (reason.why, rs) with
    | Ast.ContinuationGrade { op; _ }, _ :: _ ->
        (* The case's own constraint: the headline carries the quantification,
           the note the inequality and its refuting instance. *)
        let quantified = String.capitalize_ascii (for_every p rs) in
        let g1 = p.rho s1 in
        let g2 = p.rho s2 in
        fail ~loc:reason.at
          ~labels:(labels_of_reason p reason @ rigid_labels p rs)
          ~notes:
            [
              plain_ineq p rs (p.rho_raw rho1) (p.rho_raw rho2)
              ^ witness_clause p rs rho1 rho2 witness;
            ]
          (Printf.sprintf
             "%s, the case for %s must have a grade matching %s, but its grade \
              %s does not"
             quantified (op_name op) g2 g1)
    | _ ->
        ineq_failed_generic p ~rigids ~rs ~stated:(s1, s2) rho1 rho2 reason
          witness

  (* An obligation inherited from a definition: the headline names the
     definition it came from and what that definition needs eternal, and leaves
     the constraint itself to the labels. *)
  let instance_eternal p ty var inner =
    match use_after_time inner with
    | Some (_, x, _, _) ->
        Printf.sprintf "%s is not eternal, but %s needs %s to be eternal"
          (p.ty ty) (describe var) (needed_eternal x)
    | None ->
        Printf.sprintf "Type %s is not eternal, as required by the type of %s"
          (p.ty ty) (describe var)

  let not_eternal p ty (reason : reason) =
    let t = p.ty ty in
    let message =
      match reason.why with
      | Ast.UseAfterTime { var; _ } ->
          Printf.sprintf
            "%s has type %s, which is not eternal, but is used after a grade \
             has elapsed"
            (subject var) t
      | Ast.InstanceOf { var; inner; _ } -> instance_eternal p ty var inner
      | _ -> Printf.sprintf "Type %s is not eternal" t
    in
    fail ~loc:reason.at ~labels:(labels_of_reason p reason) ~notes:[] message

  (** A disjunction [eternal τ ∨ ρ₁ ≾ ρ₂] with both sides refuted. *)
  let eternal_or_ineq_failed p ~rigids ty rho1 rho2 (reason : reason) witness =
    let t = p.ty ty in
    let s1, _, rs = stated_sides ~rigids reason rho1 rho2 in
    let message, notes =
      match reason.why with
      | Ast.UseAfterTime { var; _ } ->
          ( Printf.sprintf
              "%s is used after grade %s has elapsed, but its type %s is not \
               eternal"
              (subject var) (p.rho s1) t,
            [ ineq_text p ~rigids rho1 rho2 witness ] )
      | Ast.InstanceOf { var; inner; _ } ->
          ( instance_eternal p ty var inner,
            [ ineq_text p ~rigids rho1 rho2 witness ] )
      | _ ->
          ( Printf.sprintf "Type %s is not eternal and %s" t
              (ineq_text p ~rigids rho1 rho2 witness),
            [] )
    in
    fail ~loc:reason.at
      ~labels:(labels_of_reason p reason @ rigid_labels p rs)
      ~notes message

  (** The same disjunction, its inequality left open rather than refuted. *)
  let eternal_or_ineq_unknown p ~rigids ty rho1 rho2 (reason : reason) =
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
      | Ast.InstanceOf { var; inner; _ } ->
          ( instance_eternal p ty var inner,
            [ Printf.sprintf "grade %s cannot be compared with %s" g1 g2 ] )
      | _ ->
          ( Printf.sprintf
              "Type %s is not eternal and cannot compare non-ground resource \
               values %s and %s"
              t g1 g2,
            [] )
    in
    fail ~loc:reason.at
      ~labels:
        (labels_of_reason p reason
        @ rigid_labels p (rigids_of rigids [ rho1; rho2 ]))
      ~notes message

  (** An inequality that survived solving with unknown grades still in it.
      Inequalities are not carried into schemes, so it cannot be deferred. *)
  let non_ground_ineq p ~rigids rho1 rho2 (reason : reason) =
    let rs = rigids_of rigids [ rho1; rho2 ] in
    let message =
      match rs with
      | [] ->
          Printf.sprintf "Cannot compare non-ground resource values %s and %s"
            (p.rho rho1) (p.rho rho2)
      | rs ->
          Printf.sprintf "Cannot decide the resource inequality %s %s"
            (ineq_code p rs (p.rho_raw rho1) (p.rho_raw rho2))
            (for_every p rs)
    in
    fail ~loc:reason.at
      ~labels:(labels_of_reason p reason @ rigid_labels p rs)
      ~notes:[] message

  (** A rigid grade in the type a definition or a run exports. [described] is
      that type, rendered with [p] so that its grades are already numbered. *)
  let rigid_escape p ~rigids ~loc ~described r =
    match Ast.RhoParamMap.find_opt r rigids with
    | None ->
        fail ~loc ~labels:[] ~notes:[]
          (Printf.sprintf
             "The grade %s of a handler continuation may be any grade and \
              cannot occur in %s"
             (p.rho (Ast.RhoRigid r)) described)
    | Some o ->
        fail ~loc
          ~labels:(rigid_labels p [ (r, o) ])
          ~notes:[]
          (Printf.sprintf
             "%s mentions %s, which may be any grade and so cannot occur in it"
             (String.capitalize_ascii described)
             (rigid_description p (r, o)))
end
