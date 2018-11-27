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

let terms term = 

    let rec loop level term = seq {
        if level <= Params.TERM_DEPTH then
            match term with
            | Var(_, _) | Word "_" -> 
                ()

            | Word _ -> 
                yield term

            | Term(_, x) -> 
                yield term
                for t in x do
                    yield! loop (level + 1) t
        else
            ()
    }

    loop 1 term |> Seq.toList |> List.distinct

let rec syntacticComplexity st =
    match st with
    | Term(_, lst) -> 1 + List.fold (fun sum t -> sum + syntacticComplexity(t)) 0 lst
    | Var( _) -> 2
    | Word _ -> 1

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
       
    
    let res = Set.intersect (Set.ofList (flatten [] s)) (Set.ofList (flatten [] p)) |> Set.toList |> (=) []
    res

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
let isSequenceable = function | Term(Inh, [Word _; Word _]) | Term(Par, _) | Term(Seq, _) -> true | _ -> false
let isNotImpOrEqu s = not(isImp s || isEqu s)
let isVar = function | Var(_) -> true | _ -> false
let notSet a = not (isExtSet a || isIntSet a)
let getTerms = function | Term(_, lst) -> lst | _ -> []
let union a b = Set.toList <| Set.union (Set.ofList a) (Set.ofList b)
let intersection a b = Set.toList <| Set.intersect (Set.ofList a) (Set.ofList b)
let difference a b = Set.toList <| Set.difference (Set.ofList a) (Set.ofList b)
let isMember s lst = Set.contains s (Set.ofList lst)
let listLess s lst = Set.toList <| Set.difference (Set.ofList lst) (Set.ofList [s])
let isBefore s p = s.Created < p.Created && abs(s.Created - p.Created) < 1000L
let isConcurrent s p = abs (p.Created - s.Created) < Params.CONCURRENCY_DURATION
let isAfter s p = s.Created > p.Created && abs(s.Created - p.Created) < 1000L
let isQuoted = function | Word s -> s.Chars(s.Length - 1) = ''' | _ -> false

let rec containsVars = function
    | Var(op, lst) -> true
    | Term(op, hd::tl) when tl = [] -> containsVars hd
    | Term(op, lst) -> List.exists containsVars lst
    | Word _ -> false

let reduce = function
    | Term(IntInt, hd::tl) when tl = [] ->  hd
    | Term(ExtInt, hd::tl) when tl = [] ->  hd
    | Term(And, hd::tl) when tl = [] -> hd
    | Term(Or, hd::tl) when tl = [] -> hd
    | Term(Seq, hd::tl) when tl = [] -> hd
    | Term(Par, hd::tl) when tl = [] -> hd
    | term -> term

let isSelective t = containsVars t
