  $ for f in *.mlt
  > do
  >   echo "======================================================================"
  >   echo $f
  >   echo "======================================================================"
  >   case $f in
  >     time_intervals.mlt) ../temporal-millet --resources time-interval $f;;
  >     time_upper.mlt) ../temporal-millet --resources time-upper-bound $f;;
  >     comp_type_annotation_upper*.mlt) ../temporal-millet --resources time-upper-bound $f;;
  >     eternal_lower.mlt) ../temporal-millet $f;;
  >     eternal_*.mlt) ../temporal-millet --resources time-upper-bound $f;;
  >     noneternal_lower.mlt) ../temporal-millet $f;;
  >     noneternal*.mlt) ../temporal-millet --resources time-upper-bound $f;;
  >     continuation_discard_reject_lower.mlt) ../temporal-millet $f;;
  >     continuation_twice_lower.mlt) ../temporal-millet $f;;
  >     continuation_*.mlt) ../temporal-millet --resources time-upper-bound $f;;
  >     error_use_after_delay.mlt) ../temporal-millet --resources time-upper-bound $f;;
  >     traces_lower.mlt) ../temporal-millet --resources traces-lower-bound $f;;
  >     3dprint_traces.mlt) ../temporal-millet --resources traces-interval $f;;
  >     traces_intervals.mlt) ../temporal-millet --resources traces-interval $f;;
  >     traces_intervals_bounds.mlt) ../temporal-millet --resources traces-interval $f;;
  >     traces_intervals_default_bounds.mlt) ../temporal-millet --resources traces-interval $f;;
  >     traces_*.mlt) ../temporal-millet --resources traces-upper-bound $f;;
  >     *) ../temporal-millet $f;;
  >   esac
  >   :  # this command is here to suppress potential non-zero exit codes in the output
  > done
  ======================================================================
  3dprint_traces.mlt
  ======================================================================
  === Run 1 ===
  return (Mounted (Printed (Cooled (Extruded (Heated (Model "Sword"))))))
  State: [
    ({1},{1}),
    { resource_0 ↦ Epoxy # ({8},{11}),
      resource_2 ↦
        fun op_var ↦
          handle
            let printed = return op_var in
            delay 2 (return ());
            unbox printed as p in
            unbox resource_0 as g in
            perform Mount (p, g) (op_var. return op_var)
          with printer
        # ({Heat; Extrude; Cool},{Heat; Extrude; Cool})
    },
    ({1},{1}),
    ({3},{3}),
    { resource_3 ↦ Extruded (Heated (Model "Sword")) # ({2},{2}) },
    ({2},{2}),
    { resource_4 ↦
        Printed (Cooled (Extruded (Heated (Model "Sword"))))
        # ({2},{8})
    },
    ({2},{2}),
    ({1},{1})
  ]
  
  === Run 2 ===
  return ("Sword #1", Printed (Cooled (Extruded (Heated (Model "Sword")))))
  State: [
    { resource_1 ↦
        fun op_var ↦
          handle
            let printed = return op_var in
            delay 2 (return ());
            unbox printed as p in
            return ("Sword #1", p)
          with printer
        # ({Heat; Extrude; Cool},{Heat; Extrude; Cool})
    },
    ({1},{1}),
    ({3},{3}),
    { resource_2 ↦ Extruded (Heated (Model "Sword")) # ({2},{2}) },
    ({2},{2}),
    { resource_3 ↦
        Printed (Cooled (Extruded (Heated (Model "Sword"))))
        # ({2},{8})
    },
    ({2},{2})
  ]
  
  === Run 3 ===
  return (Model "Sword")
  State: [
    ({1},{1})
  ]
  
  === Run 4 ===
  return Epoxy
  State: []
  
  ======================================================================
  comp_type_annotation.mlt
  ======================================================================
  === Run 1 ===
  return 1
  State: [
    3
  ]
  
  === Run 2 ===
  return 4
  State: [
    2
  ]
  
  === Run 3 ===
  return 42
  State: [
    1
  ]
  
  === Run 4 ===
  return 1
  State: [
    3
  ]
  
  ======================================================================
  comp_type_annotation_reject.mlt
  ======================================================================
  File "comp_type_annotation_reject.mlt", line 3, characters 9-31:
  3 | let f () : int # 5 = delay 3; 1
               ^^^^^^^^^^^^^^^^^^^^^^
  Typing error: This function's body has grade 3, which does not match its annotated grade 5
    Note: the resource inequality 3 >= 5 does not hold
  ======================================================================
  comp_type_annotation_upper.mlt
  ======================================================================
  === Run 1 ===
  return 1
  State: [
    3
  ]
  
  ======================================================================
  comp_type_annotation_upper_reject.mlt
  ======================================================================
  File "comp_type_annotation_upper_reject.mlt", line 3, characters 9-31:
  3 | let f () : int # 2 = delay 3; 1
               ^^^^^^^^^^^^^^^^^^^^^^
  Typing error: This function's body has grade 3, which does not match its annotated grade 2
    Note: the resource inequality 3 <= 2 does not hold
  ======================================================================
  continuation_discard_reject_lower.mlt
  ======================================================================
  File "continuation_discard_reject_lower.mlt", line 9, characters 27-38:
  9 | let h = handler | x -> x | Op p k -> 5
                                 ^^^^^^^^^^^
  Typing error: The case for Op has grade 0, which does not match the grade ρ₀ + 1 of Op followed by its continuation
    File "continuation_discard_reject_lower.mlt", line 5, characters 0-31:
    5 | operation Op : unit ~> unit # 1
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    operation Op is declared here
    Note: the resource inequality 0 >= ρ₀ + 1 does not hold, already when the continuation grade is 0
  ======================================================================
  continuation_discard_upper.mlt
  ======================================================================
  === Run 1 ===
  return 5
  State: [
    { resource_1 ↦
        fun op_var ↦ handle
                       return op_var;
                       delay 5 (return ());
                       return 3
                     with h
        # 1
    }
  ]
  
  ======================================================================
  continuation_escape_reject.mlt
  ======================================================================
  File "continuation_escape_reject.mlt", line 9, characters 0-71:
  9 | let h g = handler | x -> x | Op p k -> g k; delay 1; continue k with ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: The grade ρ₀ of a handler continuation may be any grade and cannot occur in the type ([1](unit → α # ρ₀) → β) → α # ρ₁ ⇒ α # 0 of h
  ======================================================================
  continuation_fixed_reject.mlt
  ======================================================================
  File "continuation_fixed_reject.mlt", line 9, characters 39-95:
  9 | let h = handler | x -> (fun () -> x) | Op p k -> (fun () -> let f = continue k with () in f ())
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: The grade ρ₀ of a handler continuation may be any grade, but here it is required to equal 0
    File "continuation_fixed_reject.mlt", line 5, characters 0-31:
    5 | operation Op : unit ~> unit # 1
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    operation Op is declared here
    Note: while matching unit → β # ρ₁ + ρ₂ against unit → α
  ======================================================================
  continuation_twice_lower.mlt
  ======================================================================
  === Run 1 ===
  return 3
  State: [
    { resource_1 ↦ fun op_var ↦ handle
                                  return op_var;
                                  return 3
                                with h # 0 }
  ]
  
  ======================================================================
  continuation_twice_reject_upper.mlt
  ======================================================================
  File "continuation_twice_reject_upper.mlt", line 9, characters 27-85:
  9 | let h = handler | x -> x | Op p k -> let a = continue k with () in continue k with ()
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: The case for Op has grade ρ₀, which does not match the grade 1 of Op followed by its continuation
    File "continuation_twice_reject_upper.mlt", line 5, characters 0-31:
    5 | operation Op : unit ~> unit # 1
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    operation Op is declared here
    Note: the resource inequality ρ₀ <= 1 does not hold, already when the continuation grade is 2
  ======================================================================
  default_ops.mlt
  ======================================================================
  === Run 1 ===
  return 7
  State: [
    { resource_1 ↦
        fun op_var ↦
          handle
            let v = return op_var in
            (let b = (let b = (+) v in
                      b 1) in
             perform Set b (op_var. return op_var));
            return v
          with h
        # 3
    },
    3,
    1,
    1
  ]
  
  === Run 2 ===
  return 0
  State: [
    3,
    1
  ]
  
  ======================================================================
  default_reject_bounds.mlt
  ======================================================================
  File "default_reject_bounds.mlt", line 7, characters 0-24:
  7 | default Get () = delay 2
      ^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: The default implementation of Get has grade 2, which does not match the declared grade 3 of Get
    File "default_reject_bounds.mlt", line 5, characters 0-32:
    5 | operation Get : unit ~> unit # 3
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    operation Get is declared here
    Note: the resource inequality 2 >= 3 does not hold
  ======================================================================
  default_reject_duplicate.mlt
  ======================================================================
  File "default_reject_duplicate.mlt", line 8, characters 0-25:
  8 | default Log msg = delay 2
      ^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: operation Log already has a default implementation
  ======================================================================
  default_reject_type.mlt
  ======================================================================
  File "default_reject_type.mlt", line 7, characters 0-32:
  7 | default Get () = delay 3; "zero"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: The default implementation of Get returns string but Get returns int
    File "default_reject_type.mlt", line 5, characters 0-31:
    5 | operation Get : unit ~> int # 3
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    operation Get is declared here
  ======================================================================
  default_reject_unknown.mlt
  ======================================================================
  File "default_reject_unknown.mlt", line 5, characters 0-25:
  5 | default Log msg = delay 1
      ^^^^^^^^^^^^^^^^^^^^^^^^^
  Syntax error: Unknown name Log
  ======================================================================
  duplicate_variant_tydef_sum.mlt
  ======================================================================
  File "duplicate_variant_tydef_sum.mlt", line 3, characters 0-39:
  3 | type cow = Horn of int | Horn of string
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Syntax error: Label Horn defined multiple times
  ======================================================================
  error_apply_arg.mlt
  ======================================================================
  File "error_apply_arg.mlt", line 11, characters 8-9:
  11 |   f "one"
               ^
  Typing error: This argument has type string but the function expects int
    File "error_apply_arg.mlt", line 11, characters 2-3:
    11 |   f "one"
           ^
    the function has type int → int # ρ₀ + ρ₁
    File "error_apply_arg.mlt", line 9, characters 10-29:
    9 |   let f = id (fun n -> n + 1) in
                  ^^^^^^^^^^^^^^^^^^^
    int was inferred here
    Note: while matching int → int # ρ₀ + ρ₁ against string → α # ρ₂
  ======================================================================
  error_handler_case.mlt
  ======================================================================
  File "error_handler_case.mlt", line 9, characters 4-20:
  9 |   | Op p k -> "done"
          ^^^^^^^^^^^^^^^^
  Typing error: The case for Op returns string but the return clause returns int
    File "error_handler_case.mlt", line 5, characters 0-31:
    5 | operation Op : unit ~> unit # 1
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    operation Op is declared here
  ======================================================================
  error_unbox_nonvariable.mlt
  ======================================================================
  File "error_unbox_nonvariable.mlt", line 7, characters 10-12:
  7 | run unbox 42 as v in v
                ^^
  Typing error: Only a variable can be unboxed
  ======================================================================
  error_use_after_delay.mlt
  ======================================================================
  File "error_use_after_delay.mlt", line 21, characters 2-3:
  21 |   t
         ^
  Typing error: Variable t is used after grade 3 has elapsed, but its type token is not eternal
    File "error_use_after_delay.mlt", line 16, characters 6-7:
    16 |   let t = Token in
               ^
    t is bound here
    File "error_use_after_delay.mlt", line 17, characters 2-9:
    17 |   delay 2;
           ^^^^^^^
    delay 2 elapses here
    File "error_use_after_delay.mlt", line 19, characters 2-17:
    19 |   perform Ping ();
           ^^^^^^^^^^^^^^^
    Ping is performed here (grade 1)
    Note: the resource inequality 3 <= 0 does not hold
  ======================================================================
  error_variant_arity.mlt
  ======================================================================
  File "error_variant_arity.mlt", line 12, characters 4-9:
  12 | run Red 1
           ^^^^^
  Typing error: Constructor Red takes no argument but is given one
  
  File "error_variant_arity.mlt", line 14, characters 4-8:
  14 | run Wrap
           ^^^^
  Typing error: Constructor Wrap takes an argument but is given none
  
  File "error_variant_arity.mlt", line 17, characters 6-11:
  17 |     | Red x -> 0
             ^^^^^
  Typing error: Constructor Red takes no argument but is given one
  
  File "error_variant_arity.mlt", line 21, characters 6-10:
  21 |     | Wrap -> 0
             ^^^^
  Typing error: Constructor Wrap takes an argument but is given none
  ======================================================================
  errors_multiple.mlt
  ======================================================================
  File "errors_multiple.mlt", line 12, characters 25-26:
  12 | let first (n : string) = n + 1
                                ^
  Typing error: This argument has type string but the function expects int
    File "errors_multiple.mlt", line 12, characters 27-28:
    12 | let first (n : string) = n + 1
                                    ^
    the function has type int → int → int
    Note: while matching int → int → int against string → int → α # ρ₀ # ρ₁
  
  File "errors_multiple.mlt", line 15, characters 14-36:
  15 |   let slow () : int # 5 = delay 3; 1 in
                     ^^^^^^^^^^^^^^^^^^^^^^
  Typing error: This function's body has grade 3, which does not match its annotated grade 5
    Note: the resource inequality 3 >= 5 does not hold
  
  File "errors_multiple.mlt", line 18, characters 15-24:
  18 | let second n = first n + "two"
                      ^^^^^^^^^
  Typing error: The application has type int but string is expected here
    Note: while matching int → int → int against int → string → α # ρ₀ # ρ₁
  ======================================================================
  eternal_lower.mlt
  ======================================================================
  === Run 1 ===
  return (fun () ↦ return ())
  State: [
    1
  ]
  
  === Run 2 ===
  return (fun () ↦ return ())
  State: [
    2
  ]
  
  ======================================================================
  eternal_types.mlt
  ======================================================================
  === Run 1 ===
  return 5
  State: [
    1
  ]
  
  === Run 2 ===
  return (Stamp 1)
  State: [
    1
  ]
  
  === Run 3 ===
  return Tag
  State: [
    1
  ]
  
  === Run 4 ===
  return 5
  State: [
    2
  ]
  
  === Run 5 ===
  return (fun () ↦ return ())
  State: []
  
  === Run 6 ===
  return 2
  State: [
    { resource_1 ↦
        fun op_var ↦
          handle
            return op_var;
            return 1
          with handler
               | return y ↦ return y
               | Tick (p, k) ↦
                        let r = (unbox k as unbox_var in
                                 unbox_var ()) in
                        return 2
        # 1
    }
  ]
  
  === Run 7 ===
  return (Stamp 42)
  State: [
    3
  ]
  
  === Run 8 ===
  return (Ticket Token)
  State: []
  
  ======================================================================
  eternal_tyvars.mlt
  ======================================================================
  === Run 1 ===
  return 5
  State: [
    1
  ]
  
  === Run 2 ===
  return Tag
  State: [
    1
  ]
  
  === Run 3 ===
  return 5
  State: [
    2
  ]
  
  === Run 4 ===
  return (fun () ↦ return ())
  State: []
  
  === Run 5 ===
  return (1, "two")
  State: [
    1
  ]
  
  === Run 6 ===
  return true
  State: [
    1,
    1
  ]
  
  === Run 7 ===
  return 6
  State: [
    { resource_1 ↦
        fun op_var ↦
          handle
            return op_var;
            return 5
          with handler
               | return y ↦ return y
               | Op (p, k) ↦
                      let r = (unbox k as unbox_var in
                               unbox_var ()) in
                      return 6
        # 1
    }
  ]
  
  ======================================================================
  eternal_tyvars_reject_function.mlt
  ======================================================================
  File "eternal_tyvars_reject_function.mlt", line 9, characters 4-8:
  9 | run keep (fun () -> ())
          ^^^^
  Typing error: Type unit → unit is not eternal, as required by the type of keep
    File "eternal_tyvars_reject_function.mlt", line 5, characters 0-23:
    5 | let keep x = delay 1; x
        ^^^^^^^^^^^^^^^^^^^^^^^
    keep is defined here
    File "eternal_tyvars_reject_function.mlt", line 5, characters 22-23:
    5 | let keep x = delay 1; x
                              ^
    because of this use inside keep
    File "eternal_tyvars_reject_function.mlt", line 5, characters 9-10:
    5 | let keep x = delay 1; x
                 ^
    x is bound here
    File "eternal_tyvars_reject_function.mlt", line 5, characters 13-20:
    5 | let keep x = delay 1; x
                     ^^^^^^^
    delay 1 elapses here
  ======================================================================
  eternal_tyvars_reject_handler.mlt
  ======================================================================
  File "eternal_tyvars_reject_handler.mlt", line 12, characters 48-49:
  12 | run handle (perform Op (); (fun () -> ())) with h (fun () -> ())
                                                       ^
  Typing error: Type unit → unit is not eternal, as required by the type of h
    File "eternal_tyvars_reject_handler.mlt", line 9, characters 0-70:
    9 | let h x = handler | y -> y | Op p k -> let r = continue k with () in x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    h is defined here
    File "eternal_tyvars_reject_handler.mlt", line 9, characters 69-70:
    9 | let h x = handler | y -> y | Op p k -> let r = continue k with () in x
                                                                             ^
    because of this use inside h
    File "eternal_tyvars_reject_handler.mlt", line 9, characters 6-7:
    9 | let h x = handler | y -> y | Op p k -> let r = continue k with () in x
              ^
    x is bound here
    File "eternal_tyvars_reject_handler.mlt", line 9, characters 47-65:
    9 | let h x = handler | y -> y | Op p k -> let r = continue k with () in x
                                                       ^^^^^^^^^^^^^^^^^^
    this computation runs here (grade ρ₀)
  ======================================================================
  eternal_tyvars_reject_higher_order.mlt
  ======================================================================
  File "eternal_tyvars_reject_higher_order.mlt", line 9, characters 4-9:
  9 | run after (fun () -> delay 2) (fun () -> ())
          ^^^^^
  Typing error: Type unit → unit is not eternal, as required by the type of after
    File "eternal_tyvars_reject_higher_order.mlt", line 5, characters 0-23:
    5 | let after g x = g (); x
        ^^^^^^^^^^^^^^^^^^^^^^^
    after is defined here
    File "eternal_tyvars_reject_higher_order.mlt", line 5, characters 22-23:
    5 | let after g x = g (); x
                              ^
    because of this use inside after
    File "eternal_tyvars_reject_higher_order.mlt", line 5, characters 12-13:
    5 | let after g x = g (); x
                    ^
    x is bound here
    File "eternal_tyvars_reject_higher_order.mlt", line 5, characters 16-20:
    5 | let after g x = g (); x
                        ^^^^
    this computation runs here (grade 2)
    Note: the resource inequality 2 <= 0 does not hold
  ======================================================================
  eternal_tyvars_reject_noneternal.mlt
  ======================================================================
  File "eternal_tyvars_reject_noneternal.mlt", line 11, characters 4-8:
  11 | run keep Token
           ^^^^
  Typing error: Type token is not eternal, as required by the type of keep
    File "eternal_tyvars_reject_noneternal.mlt", line 7, characters 0-23:
    7 | let keep x = delay 1; x
        ^^^^^^^^^^^^^^^^^^^^^^^
    keep is defined here
    File "eternal_tyvars_reject_noneternal.mlt", line 7, characters 22-23:
    7 | let keep x = delay 1; x
                              ^
    because of this use inside keep
    File "eternal_tyvars_reject_noneternal.mlt", line 7, characters 9-10:
    7 | let keep x = delay 1; x
                 ^
    x is bound here
    File "eternal_tyvars_reject_noneternal.mlt", line 7, characters 13-20:
    7 | let keep x = delay 1; x
                     ^^^^^^^
    delay 1 elapses here
  ======================================================================
  invalid_match_type.mlt
  ======================================================================
  File "invalid_match_type.mlt", line 6, characters 6-7:
  6 |     | B -> ()
            ^
  Typing error: This pattern matches values of type b but the matched value has type a list
    File "invalid_match_type.mlt", line 5, characters 8-9:
    5 |   match a with
                ^
    the matched value has type a list
    File "invalid_match_type.mlt", line 4, characters 8-9:
    4 | run let a = [A] in
                ^
    a list was inferred here
  ======================================================================
  iterative_unbox.mlt
  ======================================================================
  File "iterative_unbox.mlt", line 7, characters 30-57:
  7 |   fold_left (fun acc value -> unbox value as v in acc + v) 0 boxed
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: Variable value is unboxed before any grade has elapsed, but its box grade is 3
    File "iterative_unbox.mlt", line 7, characters 21-26:
    7 |   fold_left (fun acc value -> unbox value as v in acc + v) 0 boxed
                             ^^^^^
    value is bound here
    Note: the resource inequality 0 >= 3 does not hold
  ======================================================================
  less_than_function.mlt
  ======================================================================
  Runtime error: Incomparable expression (fun x ↦ return x)
  ======================================================================
  lexer.mlt
  ======================================================================
  === Run 1 ===
  return 10
  State: []
  
  === Run 2 ===
  return 20
  State: []
  
  === Run 3 ===
  return 30
  State: []
  
  === Run 4 ===
  return 40
  State: []
  
  === Run 5 ===
  return -1000000000
  State: []
  
  === Run 6 ===
  return 42
  State: []
  
  === Run 7 ===
  return -42
  State: []
  
  === Run 8 ===
  return 42
  State: []
  
  === Run 9 ===
  return 42
  State: []
  
  === Run 10 ===
  return 11259375
  State: []
  
  === Run 11 ===
  return 11259375
  State: []
  
  === Run 12 ===
  return 32072
  State: []
  
  === Run 13 ===
  return 32072
  State: []
  
  === Run 14 ===
  return 3.141592
  State: []
  
  === Run 15 ===
  return 4.141592
  State: []
  
  === Run 16 ===
  return -5.1592
  State: []
  
  === Run 17 ===
  return 6.1592
  State: []
  
  === Run 18 ===
  return -3.14
  State: []
  
  ======================================================================
  malformed_type_application.mlt
  ======================================================================
  File "malformed_type_application.mlt", line 4, characters 0-25:
  4 | type bar = (int, int) foo
      ^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: Type foo expects 1 argument but is given 2
  ======================================================================
  nat.mlt
  ======================================================================
  === Run 1 ===
  return 42
  State: []
  
  ======================================================================
  non_linear_pattern.mlt
  ======================================================================
  File "non_linear_pattern.mlt", line 3, characters 8-13:
  3 | run let (a,a) = (10, 20) in a
              ^^^^^
  Syntax error: Variable a defined multiple times
  ======================================================================
  noneternal_lower.mlt
  ======================================================================
  === Run 1 ===
  return Token
  State: [
    1
  ]
  
  ======================================================================
  noneternal_reject_after_delay.mlt
  ======================================================================
  File "noneternal_reject_after_delay.mlt", line 13, characters 2-3:
  13 |   t
         ^
  Typing error: Variable t is used after grade 1 has elapsed, but its type token is not eternal
    File "noneternal_reject_after_delay.mlt", line 11, characters 6-7:
    11 |   let t = Token in
               ^
    t is bound here
    File "noneternal_reject_after_delay.mlt", line 12, characters 2-9:
    12 |   delay 1;
           ^^^^^^^
    delay 1 elapses here
    Note: the resource inequality 1 <= 0 does not hold
  ======================================================================
  noneternal_reject_alias.mlt
  ======================================================================
  File "noneternal_reject_alias.mlt", line 5, characters 0-29:
  5 | noneternal type seconds = int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: type seconds is an alias and cannot be declared noneternal; wrap it in a constructor, as in 'noneternal type seconds = Seconds of ...'
  ======================================================================
  noneternal_reject_unknown_grade.mlt
  ======================================================================
  File "noneternal_reject_unknown_grade.mlt", line 21, characters 2-3:
  21 |   t
         ^
  Typing error: Variable t is used after grade ρ₀ + 1 has elapsed, but its type token is not eternal and grade ρ₀ + 1 cannot be compared with 0
    File "noneternal_reject_unknown_grade.mlt", line 18, characters 6-7:
    18 |   let t = Token in
               ^
    t is bound here
    File "noneternal_reject_unknown_grade.mlt", line 19, characters 10-14:
    19 |   let r = g () in
                   ^^^^
    this computation runs here (grade ρ₀)
    File "noneternal_reject_unknown_grade.mlt", line 20, characters 2-9:
    20 |   delay 1;
           ^^^^^^^
    delay 1 elapses here
  
  File "noneternal_reject_unknown_grade.mlt", line 34, characters 19-23:
  34 | let hold_token g = hold g Token
                          ^^^^
  Typing error: Type token is not eternal, as required by the type of hold
    File "noneternal_reject_unknown_grade.mlt", lines 26-30, characters 0-3:
    26 | let hold g x =
         ^^^^^^^^^^^^^^
    hold is defined here
    File "noneternal_reject_unknown_grade.mlt", line 30, characters 2-3:
    30 |   y
           ^
    because of this use inside hold
    File "noneternal_reject_unknown_grade.mlt", line 27, characters 6-7:
    27 |   let y = x in
               ^
    y is bound here
    File "noneternal_reject_unknown_grade.mlt", line 28, characters 10-14:
    28 |   let r = g () in
                   ^^^^
    this computation runs here (grade ρ₀)
    File "noneternal_reject_unknown_grade.mlt", line 29, characters 2-9:
    29 |   delay 1;
           ^^^^^^^
    delay 1 elapses here
    Note: grade ρ₀ + 1 cannot be compared with 0
  ======================================================================
  noneternal_type.mlt
  ======================================================================
  === Run 1 ===
  return (Ticket Token)
  State: []
  
  === Run 2 ===
  return (Stamp 42)
  State: [
    3
  ]
  
  ======================================================================
  occurs_check.mlt
  ======================================================================
  File "occurs_check.mlt", line 1, characters 14-19:
  1 | run let rec f x = f in f
                    ^^^^^
  Typing error: Cannot construct the infinite type α = β → α
  ======================================================================
  orelse_andalso.mlt
  ======================================================================
  ======================================================================
  patterns.mlt
  ======================================================================
  === Run 1 ===
  return 5
  State: []
  
  === Run 2 ===
  return (1, 2)
  State: []
  
  === Run 3 ===
  return (1, 2::3::4::[])
  State: []
  
  === Run 4 ===
  return (2::3::4::[])
  State: []
  
  === Run 5 ===
  return 10
  State: []
  
  === Run 6 ===
  return (10, Moo 10)
  State: []
  
  === Run 7 ===
  return (42, 42, 42)
  State: []
  
  === Run 8 ===
  return (1, 2, 3, (1, 2, 3))
  State: []
  
  === Run 9 ===
  return ("foo", "foo", "bar")
  State: []
  
  ======================================================================
  polymorphism.mlt
  ======================================================================
  === Run 1 ===
  return (5, "foo")
  State: []
  
  === Run 2 ===
  return (4, "foo")
  State: []
  
  === Run 3 ===
  return (1::u, "foo"::u)
  State: []
  
  === Run 4 ===
  return ([]::v, (2::[])::v)
  State: []
  
  === Run 5 ===
  return (fun x ↦
            let h = return (fun t ↦ return (fun u ↦ return u)) in
            let b = h x in
            b x)
  State: []
  
  === Run 6 ===
  return (fun x ↦
            let h = return (fun t ↦ return (fun u ↦ return t)) in
            let b = h x in
            b x)
  State: []
  
  ======================================================================
  polymorphism_id_id.mlt
  ======================================================================
  File "polymorphism_id_id.mlt", line 3, characters 17-18:
  3 |     (v 42, v "foo")
                       ^
  Typing error: This argument has type string but the function expects int
    File "polymorphism_id_id.mlt", line 3, characters 11-12:
    3 |     (v 42, v "foo")
                   ^
    the function has type int → int
    File "polymorphism_id_id.mlt", line 2, characters 12-15:
    2 | run let v = u u in
                    ^^^
    int was inferred here
    Note: while matching int → int against string → α # ρ₀
  ======================================================================
  recursion.mlt
  ======================================================================
  === Run 1 ===
  return 5
  State: []
  
  ======================================================================
  shadow_label.mlt
  ======================================================================
  File "shadow_label.mlt", line 2, characters 0-41:
  2 | type bull = Tail of string | Horn of bull
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Syntax error: Label Horn defined multiple times
  ======================================================================
  shadow_type.mlt
  ======================================================================
  File "shadow_type.mlt", line 3, characters 0-23:
  3 | type cow = Hoof of bool
      ^^^^^^^^^^^^^^^^^^^^^^^
  Syntax error: Type cow defined multiple times
  ======================================================================
  test_equality.mlt
  ======================================================================
  === Run 1 ===
  return true
  State: []
  
  === Run 2 ===
  return false
  State: []
  
  === Run 3 ===
  return true
  State: []
  
  === Run 4 ===
  return false
  State: []
  
  === Run 5 ===
  return false
  State: []
  
  === Run 6 ===
  return true
  State: []
  
  ======================================================================
  test_less_then.mlt
  ======================================================================
  === Run 1 ===
  return false
  State: []
  
  === Run 2 ===
  return true
  State: []
  
  === Run 3 ===
  return false
  State: []
  
  === Run 4 ===
  return false
  State: []
  
  === Run 5 ===
  return true
  State: []
  
  === Run 6 ===
  return false
  State: []
  
  === Run 7 ===
  return false
  State: []
  
  === Run 8 ===
  return "composite values"
  State: []
  
  === Run 9 ===
  return true
  State: []
  
  === Run 10 ===
  return false
  State: []
  
  ======================================================================
  test_mocked_ops.mlt
  ======================================================================
  === Run 1 ===
  return (Complete (UvCured (Cooled (Fresh (Model "Sword")))), 
          Complete (UvCured (Cooled (Fresh (Model "Hammer")))))
  State: [
    7,
    { resource_0 ↦ Fresh (Model "Sword") # 5 },
    7,
    { resource_1 ↦ Fresh (Model "Hammer") # 5 },
    5,
    5
  ]
  
  ======================================================================
  test_op_handling.mlt
  ======================================================================
  === Run 1 ===
  return (Complete (UvCured (Cooled (Fresh (Model "Sword")))), 
          Complete (UvCured (Cooled (Fresh (Model "Hammer")))))
  State: [
    { resource_1 ↦
        fun op_var ↦
          handle
            let freshSword = return op_var in
            let freshHammer =
              perform PrintResinModel (Model "Hammer") (op_var. return op_var) in
            unbox freshSword as cooledSword in
            let curedSword =
              perform UvCure (Cooled cooledSword) (op_var. return op_var) in
            unbox freshHammer as cooledHammer in
            let curedHammer =
              perform UvCure (Cooled cooledHammer) (op_var. return op_var) in
            return (Complete curedSword, Complete curedHammer)
          with h
        # 7
    },
    7,
    { resource_2 ↦ Fresh (Model "Sword") # 5,
      resource_4 ↦
        fun op_var ↦
          handle
            let freshHammer = return op_var in
            unbox resource_2 as cooledSword in
            let curedSword =
              perform UvCure (Cooled cooledSword) (op_var. return op_var) in
            unbox freshHammer as cooledHammer in
            let curedHammer =
              perform UvCure (Cooled cooledHammer) (op_var. return op_var) in
            return (Complete curedSword, Complete curedHammer)
          with h
        # 7
    },
    7,
    { resource_5 ↦ Fresh (Model "Hammer") # 5,
      resource_7 ↦
        fun op_var ↦
          handle
            let curedSword = return op_var in
            unbox resource_5 as cooledHammer in
            let curedHammer =
              perform UvCure (Cooled cooledHammer) (op_var. return op_var) in
            return (Complete curedSword, Complete curedHammer)
          with h
        # 5
    },
    5,
    { resource_9 ↦
        fun op_var ↦
          handle
            let curedHammer = return op_var in
            return (Complete (UvCured (Cooled (Fresh (Model "Sword")))), 
                    Complete curedHammer)
          with h
        # 5
    },
    5
  ]
  
  ======================================================================
  test_precedence_and_associativity.mlt
  ======================================================================
  === Run 1 ===
  return 1
  State: []
  
  === Run 2 ===
  return 2
  State: []
  
  === Run 3 ===
  return 5
  State: []
  
  === Run 4 ===
  return 1
  State: []
  
  === Run 5 ===
  return 5
  State: []
  
  === Run 6 ===
  return 3
  State: []
  
  === Run 7 ===
  return 27.
  State: []
  
  === Run 8 ===
  return true
  State: []
  
  === Run 9 ===
  return 22
  State: []
  
  ======================================================================
  test_stdlib.mlt
  ======================================================================
  === Run 1 ===
  return "test less"
  State: []
  
  === Run 2 ===
  return true
  State: []
  
  === Run 3 ===
  return false
  State: []
  
  === Run 4 ===
  return false
  State: []
  
  === Run 5 ===
  return "test equal"
  State: []
  
  === Run 6 ===
  return true
  State: []
  
  === Run 7 ===
  return true
  State: []
  
  === Run 8 ===
  return "test tilda_minus"
  State: []
  
  === Run 9 ===
  return -1
  State: []
  
  === Run 10 ===
  return -3.14159
  State: []
  
  === Run 11 ===
  return -1.
  State: []
  
  === Run 12 ===
  return "test integer operations"
  State: []
  
  === Run 13 ===
  return 4
  State: []
  
  === Run 14 ===
  return 4
  State: []
  
  === Run 15 ===
  return 19
  State: []
  
  === Run 16 ===
  return 65
  State: []
  
  === Run 17 ===
  return 33
  State: []
  
  === Run 18 ===
  return 0
  State: []
  
  === Run 19 ===
  return 2
  State: []
  
  === Run 20 ===
  return 0
  State: []
  
  === Run 21 ===
  return "test float operations"
  State: []
  
  === Run 22 ===
  return 8.
  State: []
  
  === Run 23 ===
  return 5.84
  State: []
  
  === Run 24 ===
  return 8.478
  State: []
  
  === Run 25 ===
  return 0.44
  State: []
  
  === Run 26 ===
  return 1.16296296296
  State: []
  
  === Run 27 ===
  return infinity
  State: []
  
  === Run 28 ===
  return "13"
  State: []
  
  === Run 29 ===
  return "(1, 2, 3)::[]"
  State: []
  
  === Run 30 ===
  return "(1, 2, 3)"
  State: []
  
  === Run 31 ===
  return "fun x \226\134\166 return x"
  State: []
  
  === Run 32 ===
  return "test some and none"
  State: []
  
  === Run 33 ===
  return None
  State: []
  
  === Run 34 ===
  return (Some 3)
  State: []
  
  === Run 35 ===
  return "test ignore"
  State: []
  
  === Run 36 ===
  return ()
  State: []
  
  === Run 37 ===
  return "test not"
  State: []
  
  === Run 38 ===
  return false
  State: []
  
  === Run 39 ===
  return "test compare"
  State: []
  
  === Run 40 ===
  return true
  State: []
  
  === Run 41 ===
  return true
  State: []
  
  === Run 42 ===
  return true
  State: []
  
  === Run 43 ===
  return true
  State: []
  
  === Run 44 ===
  return true
  State: []
  
  === Run 45 ===
  return "test range"
  State: []
  
  === Run 46 ===
  return (4::5::6::7::8::9::[])
  State: []
  
  === Run 47 ===
  return "test map"
  State: []
  
  === Run 48 ===
  return (1::4::9::16::25::[])
  State: []
  
  === Run 49 ===
  return "test take"
  State: []
  
  === Run 50 ===
  return 5
  State: []
  
  === Run 51 ===
  return (2::5::8::11::14::17::20::23::26::29::32::35::38::41::44::47::50::53::56::59::62::[])
  State: []
  
  === Run 52 ===
  return "test fold_left and fold_right"
  State: []
  
  === Run 53 ===
  return 89
  State: []
  
  === Run 54 ===
  return 161
  State: []
  
  === Run 55 ===
  return "test forall, exists and mem"
  State: []
  
  === Run 56 ===
  return false
  State: []
  
  === Run 57 ===
  return true
  State: []
  
  === Run 58 ===
  return false
  State: []
  
  === Run 59 ===
  return "test filter"
  State: []
  
  === Run 60 ===
  return (4::5::[])
  State: []
  
  === Run 61 ===
  return "test complement and intersection"
  State: []
  
  === Run 62 ===
  return (1::3::5::6::[])
  State: []
  
  === Run 63 ===
  return (2::4::[])
  State: []
  
  === Run 64 ===
  return "test zip and unzip"
  State: []
  
  === Run 65 ===
  return ((1, "a")::(2, "b")::(3, "c")::[])
  State: []
  
  === Run 66 ===
  return (1::2::3::[], "a"::"b"::"c"::[])
  State: []
  
  === Run 67 ===
  return "test reverse"
  State: []
  
  === Run 68 ===
  return (5::4::3::2::1::[])
  State: []
  
  === Run 69 ===
  return "test concatenate lists"
  State: []
  
  === Run 70 ===
  return (1::2::3::4::5::6::[])
  State: []
  
  === Run 71 ===
  return "test length, hd and tl"
  State: []
  
  === Run 72 ===
  return 5
  State: []
  
  === Run 73 ===
  return 1
  State: []
  
  === Run 74 ===
  return (2::3::4::[])
  State: []
  
  === Run 75 ===
  return "test abs, min and max"
  State: []
  
  === Run 76 ===
  return (5, 5, 5)
  State: []
  
  === Run 77 ===
  return 1
  State: []
  
  === Run 78 ===
  return 2
  State: []
  
  === Run 79 ===
  return "test gcd and lcm"
  State: []
  
  === Run 80 ===
  return 4
  State: []
  
  === Run 81 ===
  return 24
  State: []
  
  === Run 82 ===
  return "test odd and even"
  State: []
  
  === Run 83 ===
  return false
  State: []
  
  === Run 84 ===
  return true
  State: []
  
  === Run 85 ===
  return "test id"
  State: []
  
  === Run 86 ===
  return 5
  State: []
  
  === Run 87 ===
  return id
  State: []
  
  === Run 88 ===
  return "test compose and reverse apply"
  State: []
  
  === Run 89 ===
  return 196
  State: []
  
  === Run 90 ===
  return 7
  State: []
  
  === Run 91 ===
  return "test fst and snd"
  State: []
  
  === Run 92 ===
  return "foo"
  State: []
  
  === Run 93 ===
  return 4
  State: []
  
  ======================================================================
  test_temporal.mlt
  ======================================================================
  === Run 1 ===
  return 0
  State: []
  
  === Run 2 ===
  return 0
  State: [
    3
  ]
  
  === Run 3 ===
  return 1
  State: [
    9,
    2,
    3
  ]
  
  === Run 4 ===
  return 11
  State: [
    5
  ]
  
  === Run 5 ===
  return 11
  State: [
    10,
    5
  ]
  
  === Run 6 ===
  return ()
  State: [
    24,
    24,
    24,
    23,
    23,
    42
  ]
  
  === Run 7 ===
  return resource_0
  State: [
    5,
    { resource_0 ↦ 11 # 3 },
    3
  ]
  
  === Run 8 ===
  return 42
  State: [
    { resource_0 ↦ 42 # 3 },
    3
  ]
  
  === Run 9 ===
  return (43, "test")
  State: [
    2,
    10,
    { resource_0 ↦ (43, 99, "test") # 3 },
    3
  ]
  
  ======================================================================
  time_fold_delays.mlt
  ======================================================================
  === Run 1 ===
  return 42
  State: [
    { resource_0 ↦ 42 # 7,
      resource_2 ↦
        fun op_var ↦
          handle
            return op_var;
            unbox resource_0 as x in
            return x
          with h
        # 7
    },
    3,
    4
  ]
  
  ======================================================================
  time_intervals.mlt
  ======================================================================
  === Run 1 ===
  return 1
  State: [
    { resource_0 ↦ 1 # (1,4) },
    (1,1),
    (2,2)
  ]
  
  === Run 2 ===
  return 1
  State: [
    { resource_0 ↦ 1 # (1,4) },
    (1,1),
    (2,2)
  ]
  
  === Run 3 ===
  return 4
  State: [
    { resource_0 ↦ 1 # (1,4) },
    (1,1),
    (2,2)
  ]
  
  === Run 4 ===
  return 8
  State: [
    { resource_0 ↦ 1 # (1,4) },
    (1,1),
    (2,2)
  ]
  
  === Run 5 ===
  return 15
  State: [
    { resource_0 ↦ 7 # (2,5), resource_1 ↦ 1 # (1,4) },
    (1,1),
    (2,2)
  ]
  
  ======================================================================
  time_reject_within.mlt
  ======================================================================
  File "time_reject_within.mlt", line 6, characters 0-47:
  6 | operation Heat : unit ~> unit # 2 within (1, 2)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: runtime bounds are only used by the timed-trace grading monoids; under 'time-lower-bound' the operation grade already carries them
  ======================================================================
  time_upper.mlt
  ======================================================================
  === Run 1 ===
  return 1
  State: [
    { resource_0 ↦ 1 # 3 },
    1,
    2
  ]
  
  === Run 2 ===
  return 1
  State: [
    { resource_0 ↦ 1 # 3 },
    2
  ]
  
  ======================================================================
  traces_annotation.mlt
  ======================================================================
  === Run 1 ===
  return ()
  State: [
    {2}
  ]
  
  ======================================================================
  traces_default.mlt
  ======================================================================
  === Run 1 ===
  return (Fresh (Model "Sword"))
  State: [
    { resource_1 ↦
        fun op_var ↦ handle
                       return op_var
                     with printer
        # {Heat; Extrude; Cool}
    },
    {1},
    {3},
    {2}
  ]
  
  ======================================================================
  traces_intervals.mlt
  ======================================================================
  === Run 1 ===
  return (Mounted (Printed (Cooled (Extruded (Heated (Model "Sword"))))))
  State: [
    ({1},{1}),
    { resource_0 ↦ Epoxy # ({8},{11}),
      resource_2 ↦
        fun op_var ↦
          handle
            let printed = return op_var in
            delay 2 (return ());
            unbox printed as p in
            unbox resource_0 as g in
            perform Mount (p, g) (op_var. return op_var)
          with printer
        # ({Heat; Extrude; Cool},{Heat; Extrude; Cool})
    },
    ({1},{1}),
    ({3},{3}),
    { resource_3 ↦ Extruded (Heated (Model "Sword")) # ({2},{2}) },
    ({2},{2}),
    { resource_4 ↦
        Printed (Cooled (Extruded (Heated (Model "Sword"))))
        # ({2},{8})
    },
    ({2},{2}),
    ({1},{1})
  ]
  
  ======================================================================
  traces_intervals_bounds.mlt
  ======================================================================
  === Run 1 ===
  return 1
  State: []
  
  ======================================================================
  traces_intervals_default_bounds.mlt
  ======================================================================
  File "traces_intervals_default_bounds.mlt", line 9, characters 0-28:
  9 | default Extrude () = delay 1
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: The default implementation of Extrude has grade ({1},{1}), which does not match the declared grade ({3},{5}) of Extrude
    File "traces_intervals_default_bounds.mlt", line 7, characters 0-71:
    7 | operation Extrude : unit ~> unit # ({Extrude}, {Extrude}) within (3, 5)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    operation Extrude is declared here
    Note: the resource inequality ({1},{1}) <= ({3},{5}) does not hold
  ======================================================================
  traces_lower.mlt
  ======================================================================
  === Run 1 ===
  return (Mounted (Printed (Cooled (Extruded (Heated (Model "Sword"))))))
  State: [
    {1},
    { resource_0 ↦ Epoxy # {8},
      resource_2 ↦
        fun op_var ↦
          handle
            let printed = return op_var in
            delay 2 (return ());
            unbox printed as p in
            unbox resource_0 as g in
            perform Mount (p, g) (op_var. return op_var)
          with printer
        # {Heat; Extrude; Cool}
    },
    {1},
    {3},
    { resource_3 ↦ Extruded (Heated (Model "Sword")) # {2} },
    {2},
    { resource_4 ↦ Printed (Cooled (Extruded (Heated (Model "Sword")))) # {2} },
    {2},
    {1}
  ]
  
  ======================================================================
  traces_normalise.mlt
  ======================================================================
  === Run 1 ===
  return 7
  State: [
    { resource_0 ↦ 42 # {Heat; 4 | 4; Heat} }
  ]
  
  ======================================================================
  traces_reject_allowance.mlt
  ======================================================================
  File "traces_reject_allowance.mlt", lines 10-11, characters 2-3:
  10 |   unbox r as x in
         ^^^^^^^^^^^^^^^
  Typing error: Variable r is unboxed after grade {Heat} has elapsed, which does not match its box grade {1}
    File "traces_reject_allowance.mlt", line 8, characters 14-15:
    8 |   box 1 42 as r in
                      ^
    r is bound here
    File "traces_reject_allowance.mlt", line 9, characters 2-17:
    9 |   perform Heat ();
          ^^^^^^^^^^^^^^^
    Heat is performed here (grade {Heat})
    Note: the resource inequality {Heat} <= {1} does not hold
  ======================================================================
  traces_reject_bounds.mlt
  ======================================================================
  File "traces_reject_bounds.mlt", line 4, characters 0-52:
  4 | operation Heat : unit ~> unit # {Heat} within (3, 0)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: the runtime bounds of operation Heat must satisfy lo <= hi
  ======================================================================
  traces_reject_bounds_declared.mlt
  ======================================================================
  File "traces_reject_bounds_declared.mlt", line 6, characters 0-61:
  6 | operation Send : string ~> unit # {Tx | Tx; Tx} within (2, 6)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: operation Send is compound, so its runtime bounds follow from its grade {Tx | Tx; Tx} and must not be declared
  ======================================================================
  traces_reject_default_bounds.mlt
  ======================================================================
  File "traces_reject_default_bounds.mlt", line 8, characters 0-28:
  8 | default Extrude () = delay 6
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: The default implementation of Extrude has grade {6}, which does not match the declared grade {5} of Extrude
    File "traces_reject_default_bounds.mlt", line 6, characters 0-58:
    6 | operation Extrude : unit ~> unit # {Extrude} within (3, 5)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    operation Extrude is declared here
    Note: the resource inequality {6} <= {5} does not hold
  ======================================================================
  traces_reject_default_nonatomic.mlt
  ======================================================================
  File "traces_reject_default_nonatomic.mlt", line 14, characters 0-39:
  14 | default PrintModel m = delay 6; Fresh m
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: a default implementation may only be given for an atomic operation, but the grade of PrintModel is {Heat; Extrude; Cool}; handle it with a handler in terms of the operations it names
  ======================================================================
  traces_reject_missing_within.mlt
  ======================================================================
  File "traces_reject_missing_within.mlt", line 5, characters 0-38:
  5 | operation Heat : unit ~> unit # {Heat}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: atomic operation Heat needs runtime bounds `within (lo, hi)` under the 'traces-upper-bound' grading monoid
  ======================================================================
  traces_reject_order.mlt
  ======================================================================
  File "traces_reject_order.mlt", lines 16-20, characters 4-31:
  16 |   | PrintModel m k ->
           ^^^^^^^^^^^^^^^^^
  Typing error: The case for PrintModel has grade {Cool; Extrude; Heat}, which does not match the grade {Heat; Extrude; Cool} of PrintModel followed by its continuation
    File "traces_reject_order.mlt", line 11, characters 0-61:
    11 | operation PrintModel : model ~> fresh # {Heat; Extrude; Cool}
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    operation PrintModel is declared here
    Note: the resource inequality {Cool; Extrude; Heat} <= {Heat; Extrude; Cool} does not hold
  ======================================================================
  traces_reject_self_retry.mlt
  ======================================================================
  File "traces_reject_self_retry.mlt", line 6, characters 0-53:
  6 | operation Send : string ~> unit # {Send | Send; Send}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: compound operation Send may not name itself in its grade {Send | Send; Send}
  ======================================================================
  traces_reject_unknown_event.mlt
  ======================================================================
  File "traces_reject_unknown_event.mlt", line 6, characters 0-53:
  6 | operation PrintModel : unit ~> unit # {Heat; Extrude}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Typing error: unknown event 'Extrude' in the grade of operation PrintModel
  ======================================================================
  traces_upper.mlt
  ======================================================================
  === Run 1 ===
  return (Receipt "telemetry")
  State: [
    { resource_0 ↦ Receipt "telemetry" # {6},
      resource_2 ↦
        fun op_var ↦
          handle
            return op_var;
            unbox resource_0 as r in
            return r
          with send_retry
        # {Tx | Tx; Tx}
    },
    {2},
    {2}
  ]
  
  ======================================================================
  tydef.mlt
  ======================================================================
  === Run 1 ===
  return Tail
  State: []
  
  === Run 2 ===
  return (Node (10, Empty, Node (20, Empty, Empty)))
  State: []
  
  ======================================================================
  type_annotations.mlt
  ======================================================================
  === Run 1 ===
  return (fun y ↦ return (fun z ↦ let b = (let b = z y in
                                           b true) in
                                  return b))
  State: []
  
  ======================================================================
  typing.mlt
  ======================================================================
  === Run 1 ===
  return (fun y ↦ return y)
  State: []
  
  === Run 2 ===
  return h
  State: []
  
  ======================================================================
  use_undefined_type.mlt
  ======================================================================
  File "use_undefined_type.mlt", line 1, characters 18-21:
  1 | type foo = One of bar | Two of int
                        ^^^
  Syntax error: Unknown name bar

The options: typechecking only reports errors and runs nothing, and the
single-dash form of the help option is not accepted.

  $ ../temporal-millet --typecheck-only nat.mlt
  $ ../temporal-millet --typecheck-only comp_type_annotation_reject.mlt
  File "comp_type_annotation_reject.mlt", line 3, characters 9-31:
  3 | let f () : int # 5 = delay 3; 1
               ^^^^^^^^^^^^^^^^^^^^^^
  Typing error: This function's body has grade 3, which does not match its annotated grade 5
    Note: the resource inequality 3 >= 5 does not hold
  [1]
  $ ../temporal-millet -help
  ../temporal-millet: unknown option '-help'.
  Run Temporal Millet as '../temporal-millet [filename.mlt] ...'
    --debug           Show final internal state and top level typing results after execution
    --help            Display this list of options
    --no-stdlib       Do not load the standard library
    --resources       Type of resource grades to use (default: time-lower-bound). Accepted: 'time-lower-bound', 'time-upper-bound', 'time-interval', 'traces-lower-bound', 'traces-upper-bound', 'traces-interval'
    --typecheck-only  Typecheck the files without running them
  [2]
