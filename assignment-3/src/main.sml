structure Main =
struct
local
  open Patterns
  open Testing
in

(*  some code to make it easier to test *)


infix 9 ++
infix 9 --
infix 7 == 

val tt = emptyTree ++ 4 ++ 2 ++ 1 ++ 5 ++ 1 ++ 10

(*
each of the following pairs is  a set of tests. One per item in the assignment.

the #1 of the pair is the name of the test
    #2 is a list of tests. 
    Each element of this list is a boolean expression that tests certain
    functionality. All boolean expressions should evaluate to true

add tests to test you code
  
*)

val test_tree_insert =
    ("T1. test tree insert", [
      (tree_root emptyTree) = NONE,
      (tree_root tt) = SOME 4
    ])

val test_tree_delete =
    ("T2. test tree delete", [
      tt--4 = (emptyTree ++ 2 ++ 1 ++ 5 ++ 1 ++ 10)
    ])

val test_tree_height =
    ("T3. test tree height", [
      (tree_height tt) = 4
    ])

val test_tree_fold =
    ("T4. test tree_fold_pre_order", [
      (tree_fold_pre_order (op +) 0 tt) = 23
    ])

val test_tree_max =
    ("T5. test tree_max", [
      (tree_max tt) = SOME 10      
    ])

val test_tree_to_list =
    ("T6. test tree_to_list", [
      (tree_to_list tt) = [4,2, 1,1,5,10]  
    ])

val test_tree_filter =
    ("T7 test tree_filter", [
      (tree_filter (fn x => x> 4) tt) == (emptyTree ++ 5 ++10)
    ])

val test_tree_sum_even =
    ("T8 test tree_sum_even", [
      (tree_sum_even tt) = 16
    ])

(* test pattern matching *)

val test_first_answer =
    ("P1. test first_answer", [
      first_answer (fn x => if String.size(x) = 3 then SOME x else NONE) ["this", "is", "the", "end", "of", "the", "world"] = "the"
    ]);
        


val test_all_answers =
    ("P2. all_answers",
     [
       all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,2,1,2,1,2] = NONE,
       all_answers (fn x => if (x < 0) then NONE else SOME ([1])) [] = SOME []]
    );

val test_check_pattern =
    ("3. check_pattern", [
      check_pattern (TupleP [Wildcard,Variable "cat",
                         Variable "pp",TupleP[Variable "tt"],
                         Wildcard,ConstP 3,
                         ConstructorP("cony",Variable "pp")]) = false
    ]);


val test_match =
    ("test_match",
         [match(Unit, UnitP) = SOME [],
          match(Unit, Variable "cat") = SOME [("cat", Unit)],
          match(Tuple [Unit, Const 8], TupleP [Variable "cat", Variable "dog"]) = SOME [("cat", Unit),("dog", Const 8)],
          match(Tuple [Unit, Tuple [Unit, Unit]],
                        TupleP [Variable "cat", TupleP [Variable "dog", Variable "rat"]]) = SOME [("cat", Unit), ("dog", Unit),  ("rat", Unit)],
          match(Tuple[Const 7, Const 6, Unit, Const 7],
                        TupleP[
                          Variable "a",
                          Variable "ab",
                          Variable "abc",
                          Variable "abcd"]) = SOME [("a",Const 7), ("ab",Const 6), ("abc",Unit), ("abcd",Const 7)
          ]
    ]);


val test_first_match =
    ("test_first_match",
         [
           first_match Unit [UnitP] = SOME [],
           first_match Unit [Variable "cat"] = SOME [("cat", Unit)],
           first_match Unit [ConstP 3] = NONE,
           first_match (Tuple []) [TupleP [Wildcard]] = NONE,
           first_match (Tuple [Unit]) [TupleP []] = NONE,
           first_match (Tuple [Unit]) [TupleP [Variable "cat"]] = SOME [("cat", Unit)],
           first_match (Const 7) [Wildcard ] = SOME [],
           first_match (Tuple[Const 7, Const 6, Unit, Const 8])
                               [TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]] = SOME [("cat",Const 6)],
           first_match (Tuple[Const 7, Const 6, Unit, Const 7])
                               [TupleP[Variable "a", Variable "ab",
                                       Variable "abc", Variable "abcd"]] = SOME [
              ("a",Const 7),
              ("ab",Const 6),
              ("abc",Unit),
              ("abcd",Const 7)
           ]
    ]);

fun main (prog_name, args) =
    let
      val tree_tests = [
        test_tree_insert,
        test_tree_delete,
        test_tree_height,
        test_tree_fold,
        test_tree_max,
        test_tree_to_list,
        test_tree_filter,
        test_tree_sum_even
      ]

      val pattern_tests = [
        test_first_answer,
        test_all_answers,
        test_check_pattern,
        test_match,
        test_first_match]

      val all_tests = tree_tests @ pattern_tests
      val tests_done = map do_test_set all_tests
      val passed = (foldr (fn (x, acc) => if x then acc + 1 else acc) 0 tests_done)
      val total = length(tests_done)
    in
      print("Passed: " ^ Int.toString(passed) ^
            " Total: " ^ Int.toString(total) ^ "\nFinished testing\n");
      total - passed
    end

                                
end
end    
