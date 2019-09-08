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

// Return all sub terms including parent term
let terms term = 
    let rec loop acc term =
        match term with
        | Word "_" | Var(_, _) -> acc
        | Word _ -> term::acc
        | TemporalTerm(_, lst, _) | Term(_, lst) -> term::List.fold loop acc lst

    loop [] term |> Seq.toList |> List.distinct

// Calculate syntactic complexity of term
let rec synComp st =
    match st with
    | TemporalTerm(_, lst, _) | Term(_, lst) -> 1 + List.fold (fun sum t -> sum + synComp(t)) 0 lst
    | Var( _) -> 2
    | Word _ -> 1

// Test for no common subterms in term
let noCommonSubterm s p =    
    let rec flatten acc term =
        match term with
        | Word("_") -> acc
        | Word(_) -> term::acc
        | Term(_, lst) -> List.fold flatten acc lst
        | _ -> acc 

    (flatten [] s).Intersect(flatten [] p).Count() = 0

// General utils
let inline sort lst = lst |> List.sort

// Revision related utils
let isRevisable (b1 : Belief) (b2 : Belief) = nonOverlap b1.Stamp.Evidence b2.Stamp.Evidence    

// Term related utils
let isAtomic = function | Word _ -> true | _ -> false
let isImp = function | Term(Imp, _) | TemporalTerm(PreImp, _, _) | TemporalTerm(ConImp, _, _) | TemporalTerm(RetImp, _, _) -> true | _ -> false
let isEqu = function | Term(Equ, _) | TemporalTerm(PreEqu, _, _) | TemporalTerm(ConEqu, _, _) -> true | _ -> false
let isTemporal = function | TemporalTerm _ -> true | _ -> false
let isTemporalImplication = function |PreImp | ConImp | RetImp -> true | _ -> false
let isHypothesis = function | Term(Imp, _) -> true | TemporalTerm(op, _, _) when isTemporalImplication op -> true | _ -> false
let isNotImpOrEqu s = not(isImp s || isEqu s)

// set related utils
let isExtSet = function | Term(ExtSet, _) -> true | _ -> false
let isIntSet = function | Term(IntSet, _) -> true | _ -> false
let isSet t = isExtSet t || isIntSet t
let notSet a = not (isExtSet a || isIntSet a)
let union a b = Set.toList <| Set.union (Set.ofList a) (Set.ofList b) |> sort
//let intersection a b = Set.toList <| Set.intersect (Set.ofList a) (Set.ofList b) |> sort
let difference a b = Set.toList <| Set.difference (Set.ofList a) (Set.ofList b) |> sort

let intersection (a: 'a seq) (b: 'b seq) = a.Intersect(b) |> Seq.toList |> sort


let reduce = function
    | Term(IntInt, hd::tl) when tl = [] ->  hd
    | Term(ExtInt, hd::tl) when tl = [] ->  hd
    | Term(And, hd::tl) when tl = [] -> hd
    | Term(Or, hd::tl) when tl = [] -> hd
    | TemporalTerm(Seq, hd::tl, _) when tl = [] -> hd
    | TemporalTerm(Par, hd::tl, _) when tl = [] -> hd
    | term -> term

// List related utils
let isMember s lst = List.contains s lst 
let listLess s lst = List.except [s] lst
let from s lst = match listLess s lst with | [a] -> a | _ -> failwith "Failure in from() - Non binary term" 
 
// Var related utils
let isVar = function | Var(_) -> true | _ -> false

let rec containsVars = function
    | Var(_, _) -> true
    | Word _ -> false
    | TemporalTerm(_, lst, _) | Term(_, lst) -> List.exists containsVars lst

let rec containsQueryVars = function
    | Var(QVar, _) -> true
    | Var(_, _) -> false
    | Word _ -> false
    | TemporalTerm(_, lst, _) | Term(_, lst) -> List.exists containsQueryVars lst

let rec containsDepVars = function
| Var(DVar, _) -> true
| Var(_, _) -> false
| Word _ -> false
| TemporalTerm(_, lst, _) | Term(_, lst) -> List.exists containsDepVars lst

let isSelective t = containsQueryVars t

