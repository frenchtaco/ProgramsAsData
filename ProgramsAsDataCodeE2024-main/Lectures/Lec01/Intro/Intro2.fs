(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [ ("a", 3); ("c", 78); ("baf", 666); ("b", 111) ];;

let emptyenv = [] (* the empty environment *)

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: r -> if x = y then v else lookup r x

let cvalue = lookup env "c"


(* Object language expressions with variables *)

type expr =
    | CstI of int
    | Var of string
    | Prim of string * expr * expr
    | If of expr * expr * expr;;
    
    
//note: We changed the names of Var and CstI as to not get issues between the two types of expressions.
type aexpr =
    | Const of int
    | AVar of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Sub of aexpr * aexpr
    

let e1 = CstI 17

let e2 = Prim("min", CstI 3, Var "a");;

let e3 = Prim("min", Prim("*", Var "b", CstI 9), Var "a")

let e4 = Prim("max", CstI 2, CstI 5)
let e5 = Prim("min", CstI 2, CstI 5)
let e6 = Prim("==", CstI 2, CstI 2)
let e7 = Prim("==", CstI 2, CstI 5)

let e8 = If(Var "a", CstI 11, CstI 22)


// 1.2.ii
let (e9 : aexpr) = Sub(AVar "v", Add(AVar "w", AVar "z"))


//2∗(v−(w+z))
let (e10 : aexpr) = Mul(Const 2, Sub(AVar "v", Add(AVar "w", AVar "z")))

//x + y + z + v.
let (e11 : aexpr) = Add(AVar "x", Add(AVar "y", Add(AVar "z", AVar "v")))


let (e12 : aexpr) = Add(AVar "x", Const 0);;
let (e13 : aexpr) = Mul(Const 0, AVar "x");;

let (e14 : aexpr) = Sub(Const 10, Const 10);;

let (e15 : aexpr) = Sub(Const 10, Const 0);;

let (e16 : aexpr) = Mul(Const 2, Const 5);;

let (e17 : aexpr) = Mul(Const 2, Sub(Const 10, Const 0));;


(* Evaluation within an environment *)

let rec eval e (env: (string * int) list) : int =
    match e with
    | CstI i -> i
    | Var x -> lookup env x
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "+" -> i1 + i2
        | "-" -> i1 - i2
        | "*" -> i1 * i2
        | "max" -> if i1 > i2 then i1 else i2
        | "min" -> if i1 < i2 then i1 else i2
        | "==" -> if i1 = i2 then 1 else 0
        | _ -> failwith"operator doesn't exist in expr"
    | If(e1, e2, e3) ->
        let val1 = eval e1 env
        let val2 = eval e2 env
        let val3 = eval e3 env
        if val1 <> 0 then val2 else val3
    | Prim _ -> failwith "unknown primitive"
    

let e1v = eval e1 env
let e2v1 = eval e2 env
let e2v2 = eval e2 [ ("a", 314) ]
let e3v = eval e3 env

let rec fmt (e : aexpr) : string =
    match e with
    | Const x -> string x
    | AVar x -> string x
    | Add (e1, e2) -> "(" + fmt e1 + " + " + fmt e2 + ")"
    | Sub (e1, e2) -> "(" + fmt e1 + " - " + fmt e2 + ")"
    | Mul (e1, e2) -> "(" + fmt e1 + " * " + fmt e2 + ")"
    
    
let rec simplify (e : aexpr) : aexpr =
    match e with
    | Const x -> Const x
    | Add(e1, e2) -> match (e1, e2) with
                     | Const 0, e2 -> e2
                     | e1, Const 0 -> e1
                     | e1, e2 -> Add(simplify e1, simplify e2)
    | Sub(e1, e2) -> match (e1, e2) with
                     | e1, Const 0 -> e1
                     | e1, e2 when e1 = e2 ->  Const 0
                     | e1, e2 -> Sub(simplify e1, simplify e2)
    | Mul(e1, e2) -> match (e1, e2) with
                     | Const 1, e2 -> e2
                     | e1, Const 1 ->  e1
                     | _, Const 0 -> Const 0
                     | Const 0, _ -> Const 0
                     | e1, e2 -> Mul(simplify e1, simplify e2)
                     
                     
                     
let rec diff (exp : aexpr) var : aexpr=
    match exp with
    | Const _ -> Const 0
    | AVar x ->if x = var then Const 1 else Const 0
    | Add(e1, e2) -> Add(diff e1 var, diff e2 var)
    | Sub(e1, e2) -> Sub(diff e1 var, diff e2 var)
    | Mul(e1, e2) -> Mul(diff e1 var, diff e2 var)
