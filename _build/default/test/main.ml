open Rml
open OUnit2

let make_test (name : string) (expr_str : string) (val_str : string) =
  name >:: fun _ ->
  assert_equal val_str
    (Main.interp_expr Checker.Context.empty Eval.initial_env expr_str
     |> function
     | Ok x | Error (ParseError x) | Error (TypeError x) -> x)
    ~printer:(fun x -> x)

let tests =
  [
    make_test "unit" {|()|} {|()|};
    make_test "true" "true" "true";
    make_test "false" "false" "false";
    make_test "zero" "0" "0";
    make_test "if statement" "if true then 0 else 1" "0";
    make_test "else statement" "if 4=5 then 0 else 1" "1";
    make_test "negative" "-1" "-1";
    make_test "positive" "5" "5";
    make_test "forty-two" {| 42 |} {|42|};
    make_test "string" {|"zardoz" |} {|"zardoz"|};
    make_test "pair of ints" "(1,4)" "(1, 4)";
    make_test "second binding is prioritiy"
      "let ((a, a) : int * int) = (2, 3) in a" "3";
    (* make_test "self" "self" "<handle>"; *)
    make_test "int identity function" "fun (x : int) -> x" "<function>";
    make_test "application" "(fun (x : int) -> x + 5) 37" "42";
    make_test "uop negate" "- (5 + 5)" "-10";
    make_test "bop add" "5 +5 +5 " "15";
    make_test "bop mod" "42 % 37" "5";
    make_test "bop and" "true && false" "false";
    make_test "bop pipe" "5 |> (fun (x: int) -> x +37)" "42";
    make_test "cons" "5 :: 3 :: 2 :: 1 :: []" "<list>";
    make_test "basic sequence" "(); 5" "5";
    make_test "uop ref" "ref 0" "<ref>";
    make_test "uop not" "not false" "true";
    make_test "deref" "!ref 4" "4"
    (* make_test "match bool true" *)
    (* "match 5 = 5 with | true -> 42 | false -> 0 end" "42"; *);
  ]

let suite = "suite" >::: tests
let () = run_test_tt_main suite
