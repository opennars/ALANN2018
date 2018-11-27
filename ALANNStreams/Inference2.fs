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

module Inference2


open Types
open TruthFunctions
open Evidence
open TermUtils
open Unify
open TermFormatters
open Factories
//open ActivePatterns

type Postcondition = | AllowBackward | Swap | QuestionOnly | BeliefFromQuestion
type InferenceFunction = (Term * Term) -> (Term * (TV * TV -> TV) * (TV * TV -> TV) option * Postcondition list) list

let inf f eb =
    let matchToEvent = function
        | (term, tf1, tf2, conds) ->
            match eb.Event.EventType with
            | Belief when not(List.contains QuestionOnly conds) -> [makeEvent eb (term, (tf1(eb.Event.TV.Value, eb.Belief.TV)))]
            | Question when List.contains BeliefFromQuestion conds -> [makeEvent eb (term, tf1(eb.Belief.TV, eb.Belief.TV))]
            | Question when List.contains QuestionOnly conds -> [makeQuestionEvent eb term]
            | Question when List.contains AllowBackward conds -> [makeQuestionEvent eb term]
            | Goal when Option.isSome tf2 -> [makeEvent eb (term, tf2.Value(eb.Event.TV.Value, eb.Belief.TV))]
            | Quest when List.contains AllowBackward conds -> [makeQuestEvent eb term]
            | _ -> []

    let matches = 
        f (eb.Event.Term, eb.Belief.Term)
        |> List.map matchToEvent
        |> List.concat

    matches

let questionAnswerCheck eb =
    let ebToE eb =
        {EventType = EventType.Belief
         Term = eb.Belief.Term
         TV = Some eb.Belief.TV
         AV = {eb.AV with STI = eb.AV.STI * (1.0f - eb.Belief.TV.C)}
         Stamp = {eb.Belief.Stamp with LastUsed = SystemTime()}}

    let result =
        match eb.Event.Term, eb.Belief.Term with
        | t1, t2 when t1 = t2 -> true
        | t1, t2 -> unifies t1 t2
        | _ -> false

    if result then
        if eb.Event.Stamp.Source = User then
            printfn "Unifies %s %s %s %f %A" (ft eb.Event.Term) (ft eb.Belief.Term) (truth eb.Belief.TV) eb.AV.STI eb.Belief.Stamp.Evidence
        [ebToE eb]
    else
        []


let firstOrderSyllogisitic = function
    | Term(Inh, [a; b1]), Term(Inh, [b2; c]) when b1 = b2 && a <> c && noCommonSubterm a c -> [(Term(Inh, [a; c]), ded, Some strong, [AllowBackward; Swap])
                                                                                               (Term(Inh, [c; a]), exe, Some weak, [AllowBackward; Swap])]
    | Term(Inh, [a1; b]), Term(Inh, [a2; c]) when a1 = a2 && b <> c && noCommonSubterm b c -> [(Term(Inh, [c; b]), abd, Some weak, [AllowBackward; Swap])]
    | Term(Inh, [a; c1]), Term(Inh, [b; c2]) when c1 = c2 && a <> b && noCommonSubterm a b -> [(Term(Inh, [b; a]), ind, Some weak, [AllowBackward; Swap])]
    | _ -> []    

let (immediate : InferenceFunction) = function
    | Term(Inh, [s; p]), _ when s <> p -> [(Term(Inh, [p; s]), cnv, None, [])]
    | _ -> []

let similaritySyllogisitic = function
    | Term(Inh, [p; m1]), Term(Inh, [s; m2]) when m1 = m2 && s <> p && noCommonSubterm s p -> [(Term(Sim, [s; p]), com, None, [AllowBackward])]
    | Term(Inh, [m1; p]), Term(Inh, [m2; s]) when m1 = m2 && s <> p && noCommonSubterm s p -> [(Term(Sim, [s; p]), com, None, [AllowBackward])]
    | Term(Inh, [m1; p]), Term(Sim, [s; m2]) when m1 = m2 && s <> p && noCommonSubterm s p -> [(Term(Inh, [s; p]), ana, None, [AllowBackward])]
    | Term(Inh, [p; m1]), Term(Sim, [s; m2]) when m1 = m2 && s <> p && noCommonSubterm s p -> [(Term(Inh, [s; p]), ana, None, [AllowBackward])]
    | Term(Sim, [m1; p]), Term(Sim, [s; m2]) when m1 = m2 && s <> p && noCommonSubterm s p -> [(Term(Sim, [s; p]), res, None, [AllowBackward])]
    | _ -> []

let similarityFromInheritance = function
    | Term(Inh, [s1; p1]), Term(Inh, [p2; s2]) when p1 = p2 && s1 = s2 && noCommonSubterm s1 p1 -> [(Term(Sim, [s1; p1]), int, None, [AllowBackward])]
    //| Term(Inh, []), Term(Inh, []) when p1 = p2 && s1 = s2 && noCommonSubterm s1 p1 -> [(Term(Inh, [s1; p1]), ???(tv1, tv2))]
    | _ -> []

let setIntersectionComprehension = function
    | Term(Inh, [c1; Term(ExtSet, a)]), Term(Inh, [c2; Term(ExtSet, b)]) when c1 = c2 && a <> b && intersection a b <> [] -> [(Term(Inh, [c1; Term(IntSet, intersection a b)]), int, None, [AllowBackward])]
    | Term(Inh, [c1; Term(IntSet, a)]), Term(Inh, [c2; Term(IntSet, b)]) when c1 = c2 && a <> b && intersection a b <> [] -> [(Term(Inh, [c1; Term(IntSet, intersection a b)]), uni, None, [AllowBackward])] 
    | Term(Inh, [Term(ExtSet, a); c1]), Term(Inh, [Term(ExtSet, b); c2]) when c1 = c2 && a <> b && intersection a b <> [] -> [(Term(Inh, [Term(IntSet, intersection a b); c1]), uni, None, [AllowBackward])]
    | Term(Inh, [Term(IntSet, a); c1]), Term(Inh, [Term(IntSet, b); c2]) when c1 = c2 && a <> b && intersection a b <> [] -> [(Term(Inh, [Term(IntSet, intersection a b); c1]), int, None, [AllowBackward])]
    | _ -> []

let setUnionComprehension = function
    | Term(Inh, [c1; Term(ExtSet, a)]), Term(Inh, [c2; Term(ExtSet, b)]) when c1 = c2 && a <> b -> [(Term(Inh, [c1; Term(ExtSet, union a b)]), uni, None, [AllowBackward])]
    | Term(Inh, [c1; Term(IntSet, a)]), Term(Inh, [c2; Term(IntSet, b)]) when c1 = c2 && a <> b -> [(Term(Inh, [c1; Term(IntSet, union a b)]), int, None, [AllowBackward])]
    | Term(Inh, [Term(ExtSet, a); c1]), Term(Inh, [Term(ExtSet, b); c2]) when c1 = c2 && a <> b -> [(Term(Inh, [Term(ExtSet, union a b); c1]), int, None, [AllowBackward])]
    | Term(Inh, [Term(IntSet, a); c1]), Term(Inh, [Term(IntSet, b); c2]) when c1 = c2 && a <> b -> [(Term(Inh, [Term(IntSet, union a b); c1]), uni, None, [AllowBackward])]
    | _ -> []

let setUnionDifComprehension = function
    | Term(Inh, [c1; Term(ExtSet, a)]), Term(Inh, [c2; Term(ExtSet, b)]) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [c1; Term(ExtSet, difference a b)]), dif, None, [AllowBackward])]
    | Term(Inh, [c1; Term(IntSet, a)]), Term(Inh, [c2; Term(IntSet, b)]) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [c1; Term(IntSet, difference a b)]), dif, None, [AllowBackward])]
    | Term(Inh, [Term(ExtSet, a); c1]), Term(Inh, [Term(ExtSet, b); c2]) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [Term(ExtSet, difference a b); c1]), dif, None, [AllowBackward])]
    | Term(Inh, [Term(IntSet, a); c1]), Term(Inh, [Term(IntSet, b); c2]) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [Term(IntSet, difference a b); c1]), dif, None, [AllowBackward])]
    | _ -> []

let setDifferenceComprehension = function
    | Term(Inh, [c1; Term(ExtSet, a)]), Term(Inh, [c2; Term(ExtSet, b)]) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [c1; Term(ExtSet, difference a b)]), dif, None, [AllowBackward])]
    | Term(Inh, [c1; Term(IntSet, a)]), Term(Inh, [c2; Term(IntSet, b)]) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [c1; Term(IntSet, difference a b)]), dif, None, [AllowBackward])]
    | Term(Inh, [Term(ExtSet, a); c1]), Term(Inh, [Term(ExtSet, b); c2]) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [Term(ExtSet, difference a b); c1]), dif, None, [AllowBackward])]
    | Term(Inh, [Term(IntSet, a); c1]), Term(Inh, [Term(IntSet, b); c2]) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [Term(IntSet, difference a b); c1]), dif, None, [AllowBackward])]
    | _ -> []

let setDecomposition = function
    | Term(Inh, [s; m1]),  Term(Inh, [Term(IntInt, lst); m2]) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [Term(IntInt, sort <| listLess s lst); m1]), pnn, None, [])]
    | Term(Inh, [s; m1]),  Term(Inh, [Term(ExtInt, lst); m2]) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [Term(ExtInt, sort <| listLess s lst); m1]), npp, None, [])]
    | Term(Inh, [s1; m1]), Term(Inh, [Term(IntDif, [s2; p]); m2]) when m1 = m2 && s1 = s2 -> [(Term(Inh, [p; m1]), pnp, None, [])]
    | Term(Inh, [s1; m1]), Term(Inh, [Term(IntDif, [p; s2]); m2]) when m1 = m2 && s1 = s2 -> [(Term(Inh, [p; m1]), nnn, None, [])]
    | Term(Inh, [m1; s]),  Term(Inh, [m2; Term(ExtInt, lst)]) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [m1; Term(ExtInt, sort <| listLess s lst)]), pnn, None, [])]
    | Term(Inh, [m1; s]),  Term(Inh, [m2; Term(IntInt, lst)]) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [Term(IntInt, sort <| listLess s lst); m1]), npp, None, [])]
    | Term(Inh, [m1; s1]), Term(Inh, [m2; Term(ExtDif, [s2; p])]) when m1 = m2 && s1 = s2 -> [(Term(Inh, [m1; p]), pnp, None, [])]
    | Term(Inh, [m1; s1]), Term(Inh, [m2; Term(ExtDif, [p; s2])]) when m1 = m2 && s1 = s2 -> [(Term(Inh, [m1; p]), nnn, None, [])]
    | _ -> []

let InheritanceSetComprehension = function
    | Term(Inh, [p; m1]), Term(Inh, [s; m2]) when m1 = m2 && p <> s && notSet s && notSet p && noCommonSubterm s p -> [(Term(Inh, [Term(IntInt, sort [s; p]); m1]), int, None, [])
                                                                                                                       (Term(Inh, [Term(ExtInt, sort [s; p]); m1]), uni, None, [])
                                                                                                                       (Term(Inh, [Term(IntDif, [p; s]); m1]), dif, None, [])]
    | Term(Inh, [m1; p]), Term(Inh, [m2; s]) when m1 = m2 && s <> p && notSet s && notSet p && noCommonSubterm s p -> [(Term(Inh, [m1; Term(ExtInt, sort [p; s])]), int, None, [])
                                                                                                                       (Term(Inh, [m1; Term(IntInt, sort [p; s])]), uni, None, [])
                                                                                                                       (Term(Inh, [m1; Term(ExtDif, [p; s])]), dif, None, [])]
    | _ -> []

let (Nal1_3_EquivalenceAndImplication : InferenceFunction) = function
    | Term(Sim, [s1; Term(ExtSet, [p])]), s2 when s1 = s2 && s1 <> p -> [(Term(Inh, [s1; Term(ExtSet, [p])]), identity, None, [AllowBackward])]
    | Term(Sim, [s; Term(ExtSet, [p1])]), Term(ExtSet, [p2]) when p1 = p2 && s <> p1 -> [(Term(Inh, [s; Term(ExtSet, [p1])]), identity, None, [AllowBackward])]
    | Term(Sim, [Term(IntSet, [s1]); p]), Term(IntSet, [s2]) when s1 = s2 && s1 <> p -> [(Term(Inh, [Term(IntSet, [s1]); p]), identity, None, [AllowBackward])]
    | Term(Sim, [Term(IntSet, [s]); p1]), p2 when p1 = p2 && s <> p1 -> [(Term(Inh, [Term(IntSet, [s]); p1]), identity, None, [])]
    | Term(Sim, [Term(ExtSet, [s1]); Term(ExtSet, [p])]), Term(ExtSet, [s2]) when s1 = s2 && s1 <> p -> [(Term(Inh, [Term(ExtSet, [p]); Term(ExtSet, [s1])]), identity, None, [AllowBackward])]
    | Term(Sim, [Term(ExtSet, [s]); Term(ExtSet, [p1])]), Term(ExtSet, [p2]) when p1 = p2 && s <> p1 -> [(Term(Inh, [Term(ExtSet, [p1]); Term(ExtSet, [s])]), identity, None, [AllowBackward])]
    | Term(Sim, [Term(IntSet, [s1]); Term(IntSet, [p])]), Term(IntSet, [s2]) when s1 = s2 && s1 <> p -> [(Term(Inh, [Term(IntSet, [p]); Term(IntSet, [s1])]), identity, None, [AllowBackward])]
    | Term(Sim, [Term(IntSet, [s]); Term(IntSet, [p1])]), Term(IntSet, [p2]) when p1 = p2 && s <> p1 -> [(Term(Inh, [Term(IntSet, [p1]); Term(IntSet, [s])]), identity, None, [AllowBackward])]
    | _ -> []

let (setDefinitionUnwrap : InferenceFunction) = function
    | Term(Sim, [Term(ExtSet, [s1]); Term(ExtSet, [p])]), Term(ExtSet, [s2]) when s1 = s2 && s1 <> p -> [(Term(Sim, [s1; p]), identity, Some d_id, [AllowBackward])]
    | Term(Sim, [Term(ExtSet, [s]); Term(ExtSet, [p1])]), Term(ExtSet, [p2]) when p1 = p2 && s <> p1 -> [(Term(Sim, [s; p1]), identity, Some d_id, [AllowBackward])]
    | Term(Sim, [Term(IntSet, [s1]); Term(IntSet, [p])]), Term(IntSet, [s2]) when s1 = s2 && s1 <> p -> [(Term(Sim, [s1; p]), identity, Some d_id, [AllowBackward])]
    | Term(Sim, [Term(IntSet, [s]); Term(IntSet, [p1])]), Term(IntSet, [p2]) when p1 = p2 && s <> p1 -> [(Term(Sim, [s; p1]), identity, Some d_id, [AllowBackward])]
    | Term(Inh, [s1; Term(ExtSet, [p])]), s2 when s1 = s2 && s1 <> p -> [(Term(Sim, [s1; Term(ExtSet, [p])]), identity, Some d_id, [AllowBackward])]
    | Term(Inh, [s; Term(ExtSet, [p1])]), Term(ExtSet, [p2]) when p1 = p2 && s <> p1 -> [(Term(Sim, [s; Term(ExtSet, [p1])]), identity, Some d_id, [AllowBackward])]
    | Term(Inh, [Term(IntSet, [s1]); p]), Term(IntSet, [s2]) when s1 = s2 && s1 <> p -> [(Term(Sim, [Term(IntSet, [s1]); p]), identity, Some d_id, [AllowBackward])]
    | Term(Inh, [Term(IntSet, [s]); p1]), p2 when p1 = p2 && s <> p1 -> [(Term(Sim, [Term(IntSet, [s]); p1]), identity, Some d_id, [AllowBackward])]
    | _ -> []

let (structuralInference : InferenceFunction) = function
    | Term(Inh, [Term(Prod, [a; b]); m]), _ when (isAtomic m) -> [(Term(Inh, [a; Term(ExtImg, [m; Word "_"; b])]), identity, None, [])
                                                                  (Term(Inh, [b; Term(ExtImg, [m; a; Word "_"])]), identity, None, [])]
    | Term(Inh, [m; Term(Prod, [a; b])]), _ when (isAtomic m)  -> [(Term(Inh, [Term(IntImg, [m; Word "_"; b]); a]), identity, None, [])
                                                                   (Term(Inh, [Term(IntImg, [m; a; Word "_"]); b]), identity, None, [])]
    | Term(Inh, [ai; Term(ExtImg, [m; a; Word "_"])]), _ -> [(Term(Inh, [Term(Prod, [a; ai]); m]), identity, None, [])]
    | Term(Inh, [ai; Term(ExtImg, [m; Word "_"; b])]), _ when (isAtomic ai) -> [(Term(Inh, [Term(Prod, [ai; b]); m]), identity, None, [])]
    | Term(Inh, [Term(IntImg, [m; a; Word "_"]); ai]), _ when (isAtomic ai)  -> [(Term(Inh, [m; Term(Prod, [a; ai])]), identity, None, [])]
    | Term(Inh, [Term(IntImg, [m; Word "_"; b]); ai]), _ when (isAtomic ai)  -> [(Term(Inh, [m; Term(Prod, [ai; b])]), identity, None, [])]
    | _ -> []

let structuralInference2 = function
    | Term(Inh, [a1; c]), Term(Inh, [a2; d]) when a1 = a2 && c <> d -> [(Term(Inh, [Term(Prod, [a1; a2]); Term(Prod, [c; d])]), int, None, [])]
    | Term(Inh, [a; c1]), Term(Inh, [b; c2]) when c1 = c2 && a <> b -> [(Term(Inh, [Term(Prod, [a; b]); Term(Prod, [c1; c2])]), int, None, [])]
    | Term(Inh, [Term(ExtSet, [a1]); c]), Term(Inh, [a2; d]) when a1 = a2 && c <> d -> [(Term(Inh, [Term(Prod, [Term(ExtSet, [a1]); a2]); Term(Prod, [c; d])]), int, None, [])]
    | Term(Inh, [a1; c]), Term(Inh, [Term(ExtSet, [a2]); d]) when a1 = a2 && c <> d -> [(Term(Inh, [Term(Prod, [a1; Term(ExtSet, [a2])]); Term(Prod, [c; d])]), int, None, [])]
    | Term(Inh, [a; Term(IntSet, [c1])]), Term(Inh, [b; c2]) when c1 = c2 && a <> b -> [(Term(Inh, [Term(Prod, [a; b]); Term(Prod, [Term(IntSet, [c1]); c2])]), int, None, [])]
    | Term(Inh, [a; c1]), Term(Inh, [b; Term(IntSet, [c2])]) when c1 = c2 && a <> b -> [(Term(Inh, [Term(Prod, [a; b]); Term(Prod, [c1; Term(IntSet, [c2])])]), int, None, [])]
    | _ -> []


let backwardDrivenForwardInference = function
    | Term(Inh, [Term(Prod, [b1; p]); z]), Term(Inh, [b2; a]) when b1 = b2 && a <> z && not(isVar p) -> [(Term(Inh, [Term(Prod, [b2; p]); Term(Prod, [a; p])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Term(Inh, [Term(Prod, [p; b1]); z]), Term(Inh, [b2; a]) when b1 = b2 && a <> z && not(isVar p) -> [(Term(Inh, [Term(Prod, [p; b1]); Term(Prod, [p; a])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Term(Sim, [Term(Prod, [b1; p]); z]), Term(Sim, [b2; a]) when b1 = b2 && a <> z && not(isVar p) -> [(Term(Sim, [Term(Prod, [b1; p]); Term(Prod, [a; p])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Term(Sim, [Term(Prod, [p; b1]); z]), Term(Sim, [b2; a]) when b1 = b2 && a <> z && not(isVar p) -> [(Term(Sim, [Term(Prod, [p; b1]); Term(Prod, [p; a])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Term(Inh, [Term(IntImg, [n1; a; Word "_"]); z]), Term(Inh, [n2; r]) when n1 = n2 && z <> r && not(isVar a) -> [(Term(Inh, [Term(IntImg, [n1; a; Word "_"]); Term(IntImg, [r; a; Word "_"])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Term(Inh, [Term(ExtImg, [n; Word "_"; b1]); z]), Term(Inh, [s; b2]) when b1 = b2 && z <> s && not(isVar n) -> [(Term(Inh, [Term(ExtImg, [n; Word "_"; b1]); Term(ExtImg, [n; Word "_"; s])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | _ -> []
    
let backwardOnlyInference = function
    | Term(Inh, [a; s1]), Term(Inh, [b; s2]) when s1 = s2 && a <> b -> [(Term(Inh, [a; b]), identity, None, [QuestionOnly])
                                                                        (Term(Inh, [b; a]), identity, None, [QuestionOnly])
                                                                        (Term(Sim, [a; b]), identity, None, [QuestionOnly])]
    | _ -> []

