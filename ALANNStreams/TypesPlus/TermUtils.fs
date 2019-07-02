 (*
 * The MIT License
 *
 * Copyright 2018 The ALANN2018 authors.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

module TermUtils

open Types
open Evidence
open System.Linq

// Get sub terms to a recursive depth of Params.TERM_DEPTH
//let terms term depth = 
//    let rec loop level term = seq {
//        if level < depth then
//            match term with
//            | Word "_" | Var(_, _) | Interval _ -> ()

//            | Word _ -> 
//                yield term

//            | Term(_, x) -> 
//                yield term
//                for t in x do
//                    yield! loop (level + 1) t
//            | _ -> failwith "Unexpected Temporal term in terms()"
//        else
//            ()
//    }

//    loop 0 term |> Seq.toList |> List.distinct

let terms term = 
    let rec loop acc term =
        match term with
        | Word "_" | Var(_, _) | Interval _ -> acc
        | Word _ -> term::acc
        | Term(_, lst) -> term::List.fold loop acc lst
        | _ -> failwith "Unexpected Temporal term in terms()"

    loop [] term |> Seq.toList |> List.distinct

let rec synComp st =
    match st with
    | Term(_, lst) -> 1 + List.fold (fun sum t -> sum + synComp(t)) 0 lst
    | Var( _) -> 2
    | Word _ -> 1
    | Temporal _ -> 1
    | Interval _ -> 1
    | _ -> failwith "Unexpected Term Type in syntacticComplexity()"

let noCommonSubterm s p =    
    let rec flatten acc term =
        match term with
        | Word("_") -> acc
        | Word(_) -> term::acc
        | Term(_, lst) -> List.fold flatten acc lst
        | _ -> acc 

    (flatten [] s).Intersect(flatten [] p).Count() = 0

let inline sort lst = lst |> List.sort

let isExtSet = function | Term(ExtSet, _) -> true | _ -> false
let isIntSet = function | Term(IntSet, _) -> true | _ -> false
let isAtomic = function | Word _ -> true | _ -> false
let isSet t = isExtSet t || isIntSet t
let isSetOrAtomic t = isSet t || isAtomic t
let isImp = function | Term(Imp, _) | Term(PreImp, _) | Term(ConImp, _) | Term(RetImp, _) -> true | _ -> false
let isEqu = function | Term(Equ, _) | Term(PreEqu, _) | Term(ConEqu, _) -> true | _ -> false
let isTemporal = function | Term(ConEqu, _) | Term(PreEqu, _) | Term(PreImp, _) | Term(ConImp, _) | Term(RetImp, _) | Term(Par, _) | Term(Seq, _) -> true | _ -> false
let isPredictiveTemporal = function | Term(PreEqu, _) | Term(PreImp, _) | Term(ConImp, _) | Term(RetImp, _) -> true | _ -> false
let isImplicationOp = function Imp |PreImp | ConImp | RetImp -> true | _ -> false
let isTemporalOp = function | PreImp | ConImp | RetImp | ConEqu | PreEqu -> true | _ -> false
let isCopula = function | Inh | Sim | Imp | Equ | PreImp | ConImp | RetImp | ConEqu | PreEqu -> true | _ -> false
let isHypothesis = function | Term(op, _) when isImplicationOp op -> true | _ -> false
let isNotImpOrEqu s = not(isImp s || isEqu s)
let isVar = function | Var(_) -> true | _ -> false
let notSet a = not (isExtSet a || isIntSet a)
let union a b = Set.toList <| Set.union (Set.ofList a) (Set.ofList b) |> sort
let intersection a b = Set.toList <| Set.intersect (Set.ofList a) (Set.ofList b) |> sort
let difference a b = Set.toList <| Set.difference (Set.ofList a) (Set.ofList b) |> sort
let isMember s lst = List.contains s lst 
let listLess s lst = List.except [s] lst
let from s lst = match listLess s lst with | [a] -> a | _ -> failwith "Failure in from() - Non binary term" 
 
let rec containsVars = function
    | Var(_, _) -> true
    | Word _ | Interval _ -> false
    | Term(_, lst) -> List.exists containsVars lst
    | _ -> failwith "Unexpected Temporal term in containsVars()"

let rec containsQueryVars = function
    | Var(QVar, _) -> true
    | Var(_, _) -> false
    | Word _ | Interval _ -> false
    | Term(_, lst) -> List.exists containsQueryVars lst
    | _ -> failwith "Unexpected Temporal term in containsQueryVars()"

let reduce = function
    | Term(IntInt, hd::tl) when tl = [] ->  hd
    | Term(ExtInt, hd::tl) when tl = [] ->  hd
    | Term(And, hd::tl) when tl = [] -> hd
    | Term(Or, hd::tl) when tl = [] -> hd
    | Term(Seq, hd::tl) when tl = [] -> hd
    | Term(Par, hd::tl) when tl = [] -> hd
    | term -> term

let isSelective t = containsQueryVars t

let isRevisable (b1 : Belief) (b2 : Belief) = nonOverlap b1.Stamp.Evidence b2.Stamp.Evidence    
