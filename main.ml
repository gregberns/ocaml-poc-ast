(* 
Below is OCaml code that defines an AST (ML style) and does two things:
* Takes the AST and prints it as a string
* Evaluates the AST expressions down where possible

Tests cover most of the functionality and some glaring edge cases

Note: I couldn't figure out how importing libraries worked (ounit), so I just wrote a custom assert function

Run it:
ocamlc -c main.ml && ocamlc -o main main.cmo && ./main 
*)

(* 
References:
https://www.cs.cornell.edu/courses/cs3110/2020fa/textbook/interp/simpl_subst_model.html

Let AST: https://www.cs.cornell.edu/courses/cs3110/2014sp/hw/4/doc/Ast.html
*)

type bop =
| Add
| Mult
| Lte
| And
| Or

type uniop =
| Not

type id = string

type associative_type = 
| AssocLeft
| AssocRight
| AssocNone

type expr = 
| Var of string
| Int of int
| Bool of bool
| Uniop of uniop * expr
| Binop of bop * expr * expr
| If of expr * expr * expr
| Let of id * expr * expr
| Err of string

let rec to_string e : string = match e with
| Var s -> s
| Int i -> string_of_int i
| Bool bool -> 
    (match bool with
    | true -> "true"
    | false -> "false")
| Uniop (op, e) ->
    (match op with
    | Not -> "!" ^ (to_string e))
| Binop (op, e1, e2) ->
    let f o = match o with
    | Add -> "+"
    | Mult -> "*"
    | Lte -> "<"
    | And -> "&&"
    | Or -> "||"
    in
    (to_string e1) ^ " " ^ (f op) ^ " " ^ (to_string e2)
| If (c, t, e) ->
    "if " ^ (to_string c) ^ " then " ^ (to_string t) ^ " else " ^ (to_string e)
| Let (id, e1, e2) ->
    "let " ^ id ^ " = " ^ (to_string e1) ^ " in " ^ (to_string e2)
| Err e -> e
;;

let rec replace_val v repl expr =
    if (expr = v) then 
        repl
    else
        match expr with
        | Binop (op, e1, e2) -> 
            Binop (op, (replace_val v repl e1), (replace_val v repl e2))
        | If (c, t, e) -> 
            If ((replace_val v repl c), (replace_val v repl t), (replace_val v repl e))
        | Let (id, e1, e2) -> 
            Let (id, (replace_val v repl e1), (replace_val v repl e2))
        | e -> e
    ;;

let rec eval (e: expr) : expr = match e with
| Var s -> Var s
| Int i -> Int i
| Bool b -> Bool b
| Uniop (op, e) ->
    (match eval e with 
    | (Int i) -> 
        (match op with
        | _ -> Err ("Error - Invalid expr: " ^ (to_string (Uniop (op, e)))))
    | (Bool b) -> 
        (match op with
        | Not -> Bool (not b))
    | (Var v) -> Uniop (op, (Var v))
    | i -> Err ("Error - Invalid expr: " ^ (to_string (Uniop (op, i)))))
| Binop (op, e1, e2) ->
    (match ((eval e1), (eval e2)) with 
    | (Int i1, Int i2) -> 
        (match op with
        | Add -> Int (i1 + i2)
        | Mult -> Int (i1 * i2)
        | Lte -> (if (i1 < i2) then (Bool true) else (Bool false))
        | _ -> Err ("Error - Invalid expr: " ^ (to_string (Binop (op, e1, e2))))
        )
    | (Bool b1, Bool b2) -> 
        (match op with
        | And -> (Bool (b1 && b2))
        | Or -> (Bool (b1 || b2))
        | _ -> Err ("Error - Invalid expr: " ^ (to_string (Binop (op, e1, e2))))
        )
    | (Var v1, Var v2) -> Binop (op, Var v1, Var v2) 
    | (Var v1, e2) ->  Binop (op, Var v1, e2) 
    | (e1, Var v2) -> Binop (op, e1, Var v2) 
    | (i, j) -> Err ("Error - Invalid expr: " ^ (to_string (Binop (op, i, j))))
    )
| If (c, t, e) ->
    (match (eval c) with
    | Bool b -> if b then (eval t) else (eval e)
    | _ -> Err ("Error - Invalid if clause: " ^ (to_string (If (c, t, e))))
    )
| Let (id, e1, e2) -> 
    (* Err ("Error - Not Implemented") *)
    let x = (eval e1) in eval (replace_val (Var id) x e2)
    (* search e2 for (Val id) and replace it  *)
| Err e -> Err e
;;


let assert_equal_expr (msg: string) (a: expr) (b: expr) =
    (if a = b then 
        ()
    else 
        print_endline ("Failed Assert :: (" ^ msg);
        print_endline ("a: " ^ (to_string a));
        print_endline ("a: " ^ (to_string b));
        ()
    );;
let assert_equal_str (msg: string) (a: string) (b: string) =
    (if a = b then 
        ()
    else 
        (print_endline ("Failed Assert :: (" ^ msg ^ ")");
        print_endline ("a: " ^ a);
        print_endline ("b: " ^ b);
        ())
    );;

(* 
Refactoring/Simplification functions
!expr && !expr ~> expr || expr

Func
let f x = x  -->  Let (Var "f", Fun ("x", Var "x") 

Option
List - let x = 3 in [1,2,x]
 *)


(* 
Boolean Simplificaiton
http://www.gti.bh/Library/assets/part2-s9luuiwx.pdf

For example, the complement of the variable A is A'. If A = 1, then A' = 0. If A = 0, then A' = 1.
Boolean addition is equivalent to the OR operation
Boolean multiplication is equivalent to the AND operation

Idempotent Laws
- AA = A
- A+A = A

Commutative Laws: 
- A+B = B+A 
- AB = BA

Associative Laws:
- A + (B + C) = (A + B) + C 
- A(BC) = (AB)C

Distributive Law::
- A(B + C) = AB + AC 
- AB + AC = A(B + C)

Complement Laws
- AA' = F
- A+A' = T

1.  A + 0 = A
2.  A + 1 = 1
3.  A * 0 = 0
4.  A * 1 = A
5.  A + A = A
6.  A + A' = 1
7.  A * A = A
8.  A * A' = 0
9.  A'' = A
10. A + AB = A
11. A + A'B = A + B
12. (A + B)(A + C) = A + BC

*)

let rec simplify e =
    let has_associative_type e : associative_type =
        match e with
        | Binop (o, Binop (o2, e1, e2), e3) -> AssocRight
        | Binop (o, e1, Binop (o2, e2, e3)) -> AssocLeft
        | _ -> AssocNone
    in
    (* A left-associative operator is one for which x A y A z is parsed as (x A y) A z. *)
    let helper_left_associative_operator e = 
        match e with
        | Binop (o, Var a, Binop (o2, Var b, Var c)) ->
            Binop (o, Binop (o2, Var a, Var b), Var c)
        | _ -> e
    in
    (* A right-associative operator is one for which x A y A z is parsed as x A (y A z). *)
    let helper_right_associative_operator e =
        match e with
        | Binop (o, Binop (o2, Var a, Var b), Var c) ->
            Binop (o, Var a, Binop (o2, Var b, Var c))
        | _ -> e
    in
    let helper_left_commutative_operator e =
        match e with
        | Binop (o, Var a, Binop (o2, Var b, Var c)) ->
            Binop (o, Binop (o2, Var a, Var b), Var c)
        | _ -> e
    in
    let helper_right_commutative_operator e =
        match e with
        | Binop (o, Binop (o2, Var a, Var b), Var c) ->
            Binop (o, Var a, Binop (o2, Var b, Var c))
        | _ -> e
    in
    (* Idempotent - AA = A *)
    (* Idempotent - A+A = A *)
    let rule_idempotent e = 
        match e with
        | Binop (o, e1, e2) -> 
            if e1 = e2 then e1 else e
        | _ -> e
    in
    (* Associative :: A + (B + C) = (A + B) + C  *)
    let rule_associative (simplify: expr -> expr) e =
        (* let t = has_associative_type e in *)
        let f = simplify (helper_left_associative_operator e) in
        let g = simplify (helper_right_associative_operator f) in
        match g with
        | Binop(_, Var _, Var _) -> g
        | _ -> e
    in
    (* Commutative :: A+B = B+A  *)
    let rule_commutative simplify e = 
        let f = simplify (helper_left_commutative_operator e) in
        let g = simplify (helper_right_commutative_operator f) in
        match g with
        | Binop(_, Var _, Var _) -> g
        | _ -> e
    in
    
    let rec walk f e =
        (* print_endline (to_string e); *)
        match e with 
        | Binop (op, e1, e2) ->
            let e1r = walk f e1 in
            let e2r = walk f e2 in
            f (Binop (op, e1r, e2r))
        | _ -> e
    in
    let run_rules (e: expr) : expr = 
        rule_idempotent e
    in
    let rec simpl (e: expr) : expr =
        let a = run_rules e in
        let b = (rule_associative run_rules) a in
        let c = (rule_commutative run_rules) b in
        if e = c then e else simpl c
    in
    walk simpl e
in

(* 
New Strategies:
* Sort variables, then they can be compared more reliably
* Standardize grouping - transform `(a&&b)&&(a&&b)` to `((a&&b)&&a)&&b`
* Use a quick check style strategy to figure out what rule order to use and simplify the tree
*)

let single_test () = 
    (* B && A && A  *)
    let e = Binop (And, Binop (And, Var "b", Var "a"), Var "a") in
    assert_equal_str "Associative: b && a && a" ("b && a") (to_string (simplify e));
in
single_test ();

let all_tests () =

    (* A && A && B  *)
    let e = Binop (And, Binop (And, Var "a", Var "a"), Var "b") in
    assert_equal_str "Associative: a && a && b" ("a && b") (to_string (simplify e));
   
    (* B && A && A  *)
    let e = Binop (And, Binop (And, Var "b", Var "a"), Var "a") in
    assert_equal_str "Associative: b && a && a" ("b && a") (to_string (simplify e));
    (* A && A && B  *)
    let e = Binop (And, Var "a", Binop (And, Var "a", Var "b")) in
    assert_equal_str "Associative: a && a && b" ("a && b") (to_string (simplify e));
    (* A && B && A  *)
    let e = Binop (And, Binop (And, Var "a", Var "b"), Var "a") in
    assert_equal_str "Commutative: a && b && a" ("b && a") (to_string (simplify e));
    (* A && B || A  *)
    let e = Binop (Or, Binop (And, Var "a", Var "b"), Var "a") in
    assert_equal_str "Commutative: a && b || a" ("b || a") (to_string (simplify e));
    (* (A && B) && (A && B) *)
    let e = Binop (And, Binop (And, Var "a", Var "b"), Binop (And, Var "a", Var "b")) in
    assert_equal_str "(a && b) && (a && b)" ("a && b") (to_string (simplify e));
    (* (A && B) && (B && A) *)
    (* let e = Binop (And, Binop (And, Var "a", Var "b"), Binop (And, Var "b", Var "a"));
    assert_equal_str "(a && b) && (b && a)" ("a && b") (to_string (simplify e));; *)

    (* 11. A + A'B = A + B *)


    (* Idempotent *)
    (* AA = A *)
    let e = Binop (And, Var "a", Var "a") in
    assert_equal_str "a && a == a" ("a") (to_string (simplify e));
    (* A+A = A *)
    let e = Binop (Or, Var "a", Var "a") in
    assert_equal_str "a || a == a" ("a") (to_string (simplify e));
    (* A+A+A = A *)
    let e = Binop (Or, Binop (Or, Var "a", Var "a"), Var "a") in
    assert_equal_str "a || a || a == a" ("a") (to_string (simplify e));
    (* A+A+A+A = A *)
    let e = Binop (Or, Binop (Or, Var "a", Binop (Or, Var "a", Var "a")), Var "a") in
    assert_equal_str "a || a || a || a == a" ("a") (to_string (simplify e));


    (* !true *)
    let e = Uniop (Not, Bool true) in
    assert_equal_str "!true" ("!true") (to_string e);
    assert_equal_str "!true" ("false") (to_string (eval e));

    (* !true && true *)
    let e = Binop (And, Uniop (Not, Bool true), Bool true) in
    assert_equal_str "!true && true" ("!true && true") (to_string e);
    assert_equal_str "eval !true && true" ("false") (to_string (eval e));

    (* replace_val v repl expr *)
    let e = Binop (And, Var "x", Bool true) in
    assert_equal_str "replace_val true && true" ("true && true") (to_string (replace_val (Var "x") (Bool true) e));

    (* x && y || z *)
    let e = Binop (Or, Binop(And, Var "x", Var "y"), Var "z") in
    assert_equal_str "x && y || z" ("x && y || z") (to_string e);
    assert_equal_str "eval x && y || z" ("x && y || z") (to_string (eval e));


    (* let x = true in x *)
    let e = Let ("x", Bool true, Var "x") in
    assert_equal_str "let x = true in x" ("let x = true in x") (to_string e);
    assert_equal_str "eval let x = true in x" ("true") (to_string (eval e));

    (* let x = 1 + 4 in x * 3  *)
    let e = Let ("x", Binop (Add, Int 1, Int 4), Binop (Mult, Var "x", Int 3)) in
    assert_equal_str "let x = 1 + 4 in x * 3" ("let x = 1 + 4 in x * 3") (to_string e);
    assert_equal_str "eval let x = 1 + 4 in x * 3" ("15") (to_string (eval e));

    (* let x = true in let y = false in x && y *)
    let e = Let ("x", Bool true, Let ("y", Bool false, Binop (And, Var "x", Var "y"))) in
    assert_equal_str "let x = true in let y = false in x && y" ("let x = true in let y = false in x && y") (to_string e);
    assert_equal_str "eval let x = true in let y = false in x && y" ("false") (to_string (eval e));


    (* if true && true then true || false else false && false *)
    let e = If (
        Binop (And, Bool true, Bool true), 
        Binop (Or, Bool true, Bool false),
        Binop (And, Bool false, Bool false)) in
    assert_equal_str "if true && true then true || false else false && false" 
                        ("if true && true then true || false else false && false") (to_string e);
    assert_equal_str "eval if true && true then true || false else false && false" ("true") (to_string (eval e));

    (* if true then true else false *)
    let e = If (Bool true, Bool true, Bool false) in
    assert_equal_str "if true then true else false" ("if true then true else false") (to_string e);
    assert_equal_str "eval if true then true else false" ("true") (to_string (eval e));

    (* if false then true else false *)
    let e = If (Bool false, Bool true, Bool false) in
    assert_equal_str "if false then true else false" ("if false then true else false") (to_string e);
    assert_equal_str "eval if false then true else false" ("false") (to_string (eval e));

    (* if 1 then 1 else 2 *)
    let e = If (Int 1, Bool true, Bool false) in
    assert_equal_str "if 1 then true else false" ("if 1 then true else false") (to_string e);
    assert_equal_str "eval if 1 then true else false" ("Error - Invalid if clause: if 1 then true else false") (to_string (eval e));

    (* if false then 1 else 2 *)
    let e = If (Bool true, Int 1, Int 2) in
    assert_equal_str "if true then 1 else 2" ("if true then 1 else 2") (to_string e);
    assert_equal_str "eval if true then 1 else 2" ("1") (to_string (eval e));

    (* true && true *)
    let e = Binop (And, Bool true, Bool true) in
    assert_equal_str "true and true" ("true && true") (to_string e);
    assert_equal_str "eval true and true" ("true") (to_string (eval e));

    (* true && true && true *)
    let e = Binop (And, Bool true, Binop (And, Bool true, Bool true)) in
    assert_equal_str "true and true and true" ("true && true && true") (to_string e);
    assert_equal_str "eval true and true and true" ("true") (to_string (eval e));

    (* false || true *)
    let e = Binop (Or, Bool false, Bool true) in
    assert_equal_str "false or true" ("false || true") (to_string e);
    assert_equal_str "eval false or true" ("true") (to_string (eval e));

    (* false || false || true *)
    let e = Binop (Or, Bool false, Binop (Or, Bool false, Bool true)) in
    assert_equal_str "false or false or true" ("false || false || true") (to_string e);
    assert_equal_str "eval false or false or true" ("true") (to_string (eval e));

    (* Error: 1 + true *)
    let e = Binop (Add, Int 1, Bool true) in
    assert_equal_str "1 + true" ("1 + true") (to_string e);
    assert_equal_str "eval 1 + true" ("Error - Invalid expr: 1 + true") (to_string (eval e));

    (* 1 + 2 *)
    let e = Binop (Add, (Int 1), (Int 2)) in
    assert_equal_str "1 + 2" ("1 + 2") (to_string e);
    assert_equal_str "eval 1 + 2" ("3") (to_string (eval e));

    (* 1 < 2 *)
    let e = Binop (Lte, (Int 1), (Int 2)) in
    assert_equal_str "1 < 2" ("1 < 2") (to_string e);
    assert_equal_str "eval 1 < 2" ("true") (to_string (eval e));
in
all_tests ();

print_endline "Pass.";
