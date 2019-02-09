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

// Get sub terms to a recursive depth of Params.TERM_DEPTH
let terms term = 
    let rec loop level term = seq {
        if level < Params.TERM_DEPTH then
            match term with
            | Var(_, _) | Word "_" -> 
                ()

            | Word _ -> 
                yield term

            | Term(_, x) -> 
                yield term
                for t in x do
                    yield! loop (level + 1) t
            | _ -> failwith "Unexpected Temporal term in terms()"
        else
            ()
    }

    loop 0 term |> Seq.toList |> List.distinct

let rec syntacticComplexity st =
    match st with
    | Term(_, lst) -> 1 + List.fold (fun sum t -> sum + syntacticComplexity(t)) 0 lst
    | Var( _) -> 2
    | Word _ -> 1
    | _ -> failwith "Unexpected Temporal term in syntacticComplexity()"

let noCommonSubterm s p =    
    let rec flatten acc term =
        let rec flattenList acc lst =
            match lst with
            | [] -> acc
            | x::rest -> (flatten acc x)@flattenList acc rest

        match term with
        | Word(c) when c <> "_" -> term::acc
        | Term(_, lst) -> flattenList acc lst
        | _ -> acc       
    
    Set.intersect (Set.ofList (flatten [] s)) (Set.ofList (flatten [] p)) |> Set.toList |> (=) []

let isMainTerm host = function
    | term when term = host -> true
    | Term(_, [t1; _]) when t1 = host -> true
    | Term(_, [_; t2]) when t2 = host -> true
    | Term(_, [t1]) when t1 = host -> true
    | _ -> false

let isTarget host = function
    | term when term = host -> true
    | Term(_, [_; t2]) when t2 = host -> true
    | Term(_, [t1]) when t1 = host -> true
    | _ -> false

let isSimilarComplexity sc1 sc2  = float32(abs(sc1 - sc2)) < 0.1f

let inline sort lst = lst |> List.sort

let inline replace lst a b = lst |> Seq.map (fun x -> if x = a then b else x)

let isExtSet = function | Term(ExtSet, _) -> true | _ -> false
let isIntSet = function | Term(IntSet, _) -> true | _ -> false
let isAtomic = function | Word _ -> true | _ -> false
let isSet t = isExtSet t || isIntSet t
let isSetOrAtomic t = isSet t || isAtomic t
let isImp = function | Term(Imp, _) | Term(PreImp, _) | Term(ConImp, _) | Term(RetImp, _) -> true | _ -> false
let isEqu = function | Term(Equ, _) | Term(PreEqu, _) | Term(ConEqu, _) -> true | _ -> false
let isTemporal = function | Term(ConEqu, _) | Term(PreEqu, _) | Term(PreImp, _) | Term(ConImp, _) | Term(RetImp, _) | Term(Par, _) | Term(Seq, _) -> true | _ -> false
let isPredictiveTemporal = function | Term(PreEqu, _) | Term(PreImp, _) | Term(ConImp, _) | Term(RetImp, _) -> true | _ -> false
let isImplicationOp = function Imp |ConImp | RetImp -> true | _ -> false
let isTemporalOp = function | PreImp | ConImp | RetImp | ConEqu | PreEqu -> true | _ -> false
let isSequenceable = function | Term(Inh, [Word _; Word _]) | Term(Par, _) | Term(Seq, _) -> true | _ -> false
let isCopula = function | Inh | Sim | Imp | Equ | PreImp | ConImp | RetImp | ConEqu | PreEqu -> true | _ -> false
let isSuperTerm term = function | Term(op, [t1; t2]) when (term = t1 || term = t2) && isImplicationOp op -> true | _ -> false

let isNotImpOrEqu s = not(isImp s || isEqu s)
let isVar = function | Var(_) -> true | _ -> false
let notSet a = not (isExtSet a || isIntSet a)
let getTerms = function | Term(_, lst) -> lst | _ -> []
let union a b = Set.toList <| Set.union (Set.ofList a) (Set.ofList b)
let intersection a b = Set.toList <| Set.intersect (Set.ofList a) (Set.ofList b)
let difference a b = Set.toList <| Set.difference (Set.ofList a) (Set.ofList b)
let isMember s lst = Set.contains s (Set.ofList lst)
let listLess s lst = Set.toList <| Set.difference (Set.ofList lst) (Set.ofList [s])
let isBefore (s : Stamp) (p : Stamp) = s.Created < p.Created && abs(s.Created - p.Created) < Params.PAST_TENSE_OFFSET
let isConcurrent (s : Stamp) (p : Stamp) = abs (p.Created - s.Created) < Params.CONCURRENCY_DURATION
let isAfter (s : Stamp) (p : Stamp) = s.Created > p.Created && abs(s.Created - p.Created) < Params.FUTURE_TENSE_OFFSET
let isQuoted = function | Word s -> s.Chars(s.Length - 1) = ''' | _ -> false
let isBelief e = e.EventType = EventType.Belief
 
let rec containsVars = function
    | Var(_, _) -> true
    | Word _ -> false
    | Term(_, lst) -> List.exists containsVars lst
    | _ -> failwith "Unexpected Temporal term in containsVars()"

let rec containsQueryVars = function
    | Var(QVar, _) -> true
    | Var(_, _) -> false
    | Word _ -> false
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

let isFirstOrder = function | Term(Inh, _) | Term(Sim, _) -> true | _ -> false

let isRevisble (b1 : Belief) (b2 : Belief) = nonOverlap b1.Stamp.Evidence b2.Stamp.Evidence    

// Helper functions
let makeKey (b : Belief) = b.Term
let makeKeyFromEvent (e : Event) = e.Term
