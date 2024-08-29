(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int                       // constant integer
  | Var of string                     // variable
  | Prim of string * expr * expr      // primitive operation
  | If of expr * expr * expr;;        // If-statement expression
  
let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

let e4 = Prim("max", CstI 40, CstI 20);

let eq1 = Prim("==", CstI 40, CstI 40);

let eq2 = Prim("==", CstI 20, CstI 40)

let ifE = If(Var "a", CstI 11, CstI 22)


//////////////////////////
//                      //
//     Exercise 1.1     //
//                      //
//////////////////////////
let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i                -> i
    | Var x                 -> lookup env x 
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env

        match ope with
        | "+" -> i1 + i2
        | "*" -> i1 + i2
        | "-" -> i1 + i2
        | "max" ->  if (i1 < i2) then i2 else i1
        | "min" ->  if (i1 < i2) then i1 else i2
        | "==" ->   if (i1 = i2) then 1 else 0
        | _ -> failwith "unspecified operator"
        
    | If(e1, e2, e3) ->
        let i1 = eval e1 env
        
        if i1 >= 0 then
            eval e2 env
        else
            eval e3 env
        
    | Prim _            -> failwith "unknown primitive";;

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

//////////////////////////
//                      //
//     Exercise 1.2     //
//                      //
//////////////////////////
type aexpr =
    | CstI of int
    | Var of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Sub of aexpr * aexpr;;
    
let ae1 = Mul(Var "x", Add(Var "y", CstI 3))

let ae2 = Sub(Var "v", Add(Var "w", Var "z"))

let ae3 = Mul((CstI 2), Sub(Var "v", Add(Var "w", Var "z")))

let ae4 = Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))


let addTest1 = Add(Var "x", CstI 0)                                 // Result

let subTest1 = Sub(Var "x", Var "x")                                // Result

let mulTest1 = Mul(Var "x", CstI 0)                                 // Result

let mulTest2 = Mul(Var "y", Sub(Var "x", Var "x"))                  // Result

let mulTest3 = Mul(Add(CstI 1, CstI 0), Add(Var "x", CstI 0))       // Result 

let rec fmt (ae : aexpr) : string =
    match ae with
    | CstI i_Val -> string i_Val
    | Var v_Val  -> string v_Val
    | Add (e1, e2) -> "(" + fmt e1 + " + " + fmt e2 + ")"
    | Sub (e1, e2) -> "(" + fmt e1 + " - " + fmt e2 + ")"
    | Mul (e1, e2) -> "(" + fmt e1 + " * " + fmt e2 + ")"
            
let rec simplify (ae : aexpr) : aexpr =
    match ae with
    | CstI x -> CstI x
    | Var x -> Var x
    | Add (e1, e2) ->
        match (e1, e2) with
        | CstI 0, e2 -> simplify e2
        | e1, CstI 0 -> simplify e1
        | e1, e2 -> Add(simplify e1, simplify e2)
        
    | Sub (e1, e2) ->
        match (e1, e2) with
        | e1, CstI 0 -> simplify e1
        | e1, e2 when e1 = e2 -> CstI 0
        | e1, e2 -> Sub (simplify e1, simplify e2)
        
    | Mul (e1, e2) ->
        match (e1, e2) with
        | CstI 1, e2 -> simplify e2
        | e1, CstI 1 -> simplify e1
        | CstI 0, _ -> CstI 0
        | _, CstI 0 -> CstI 0
        | e1, e2 -> Mul(simplify e1, simplify e2)
        
        
//////////////////////////
//                      //
//     Exercise 1.4     //
//                      //
//////////////////////////
