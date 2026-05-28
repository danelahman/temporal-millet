  $ for f in *.mlt
  > do
  >   echo "======================================================================"
  >   echo $f
  >   echo "======================================================================"
  >   case $f in
  >     interval.mlt) ../cli.exe --resources time-interval $f;;
  >     upper-bound.mlt) ../cli.exe --resources time-upper-bound $f;;
  >     *) ../cli.exe $f;;
  >   esac
  >   :  # this command is here to suppress potential non-zero exit codes in the output
  > done
  ======================================================================
  duplicate_variant_tydef_sum.mlt
  ======================================================================
  Syntax error (file "duplicate_variant_tydef_sum.mlt", line 3, char 1):
  Label Horn defined multiple times.
  ======================================================================
  interval.mlt
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
  invalid_match_type.mlt
  ======================================================================
  Typing error: Cannot unify types a list = b
  ======================================================================
  iterative_unbox.mlt
  ======================================================================
  Typing error: Comparing resource inequality 0 >= 3 failed
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
  Typing error: Type foo expects 1 arguments but got 2.
  ======================================================================
  nat.mlt
  ======================================================================
  === Run 1 ===
  return 42
  State: []
  
  ======================================================================
  non_linear_pattern.mlt
  ======================================================================
  Syntax error (file "non_linear_pattern.mlt", line 3, char 9):
  Variable a defined multiple times.
  ======================================================================
  occurs_check.mlt
  ======================================================================
  Typing error: Cannot unify types α = β → α
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
  Typing error: Cannot unify types int = string
  ======================================================================
  recursion.mlt
  ======================================================================
  === Run 1 ===
  return 5
  State: []
  
  ======================================================================
  shadow_label.mlt
  ======================================================================
  Syntax error (file "shadow_label.mlt", line 2, char 1):
  Label Horn defined multiple times.
  ======================================================================
  shadow_type.mlt
  ======================================================================
  Syntax error (file "shadow_type.mlt", line 3, char 1):
  Type cow defined multiple times.
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
  Typing error: Type α → β # ρ₀ is not eternal and cannot compare non-ground resource values 0 and 
  ρ₀ + ρ₀
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
  upper-bound.mlt
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
  use_undefined_type.mlt
  ======================================================================
  Syntax error (file "use_undefined_type.mlt", line 1, char 19):
  Unknown name --bar--
