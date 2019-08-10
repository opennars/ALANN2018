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
 
module HigherOrderInference

open Types
open TruthFunctions
open TermUtils
open Unify
open ActivePatterns
open InferenceUtils

let Nal5_conversion_contrapostion_negation : InferenceFunction = function
    // conversion
    | Imp(p1, s1), Imp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(Imp, [p1; s1]), cnv, None, [BeliefFromQuestion])]
    //| ConImp(p1, s1), ConImp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(ConImp, [p1; s1]), cnv, None, [BeliefFromQuestion])]
    //| RetImp(p1, s1), PreImp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(RetImp, [p1; s1]), cnv, None, [BeliefFromQuestion])]
    //| PreImp(p1, s1), RetImp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(PreImp, [p1; s1]), cnv, None, [BeliefFromQuestion])]

    // contraposition
    | Imp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(Imp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward; Structural])]
    | Imp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(Imp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward; Structural])]
    //| ConImp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(ConImp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward; Structural])]
    //| ConImp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(ConImp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward; Structural])]
    //| PreImp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(RetImp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward; Structural])]
    //| PreImp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(RetImp, [Term(Not, [p]); s1]), cnt, None, [Structural])]
    //| RetImp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(PreImp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward; Structural])]
    //| RetImp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(PreImp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward; Structural])]

    // negation
    | Imp(a1, b), a2 when a1 = a2 && a1 <> b -> [(Term(Not, [Term(Imp, [a1; b])]), neg, Some d_neg, [Structural; AllowBackward])]
    | Imp(a, b1), b2 when b1 = b2 && a <> b1 -> [(Term(Not, [Term(Imp, [a; b1])]), neg, Some d_neg, [Structural; AllowBackward])]
    | Not(Imp(a1, b)), a2 when a1 = a2 && a1 <> b -> [(Term(Imp, [a1; b]), neg, Some d_neg, [Structural; AllowBackward])]
    | Not(Imp(a, b1)), b2 when b1 = b2 && a <> b1 -> [(Term(Imp, [a; b1]), neg, Some d_neg, [Structural; AllowBackward])]

    | Equ(a1, b), a2 when a1 = a2 && a1 <> b -> [(Term(Not, [Term(Equ, [a1; b])]), neg, Some d_neg, [Structural; AllowBackward])]
    | Equ(a, b1), b2 when b1 = b2 && a <> b1 -> [(Term(Not, [Term(Equ, [a; b1])]), neg, Some d_neg, [Structural; AllowBackward])]
    | Not(Equ(a1, b)), a2 when a1 = a2 && a1 <> b -> [(Term(Equ, [a1; b]), neg, Some d_neg, [Structural; AllowBackward])]
    | Not(Equ(a, b1)), b2 when b1 = b2 && a <> b1 -> [(Term(Equ, [a; b1]), neg, Some d_neg, [Structural; AllowBackward])]
    | _ -> []

let nal_5_implication_based_syllogism_Imp : InferenceFunction = function
    | Imp(m1, p), Imp(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ded, None, [AllowBackward])]
    | ConImp(m1, p), ConImp(s, m2) when m1 = m2 && s <> p -> [(TemporalTerm(ConImp, [s; p], 0s), ded, None, [AllowBackward; IsTemporal])
                                                              (TemporalTerm(Par, sort [s; p], 0s), int, None, [AllowBackward; IsTemporal])]
    //| RetImp(m1, p, i1), RetImp(s, m2, i2) when m1 = m2 && s <> p && i1 < i2 -> [(TemporalTerm(RetImp, [s; p], i1 + i2), ded, None, [AllowBackward; IsTemporal])]
    //| PreImp(m1, p, i1), PreImp(s, m2, i2) when m1 = m2 && s <> p && i2 < i1 -> [(TemporalTerm(PreImp, [s; p], i1+i2), ded, None, [AllowBackward; IsTemporal])]
    //| PreImp(m1, p, i1), ConImp(s, m2) when m1 = m2 && s <> p -> [(TemporalTerm(PreImp, [s; p], i1), ded, None, [AllowBackward; IsTemporal])]

    | Imp(p, m1), Imp(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), abd, None, [AllowBackward])]
    | ConImp(p, m1), ConImp(s, m2) when m1 = m2 && s <> p -> [(TemporalTerm(ConImp, [s; p], 0s), abd, None, [AllowBackward; IsTemporal])]
    //| PreImp(p, m1, i1), PreImp(s, m2, i2) when m1 = m2 && s <> p && i2 < i1 -> [(TemporalTerm(ConImp, [s; p], i1 + i2), abd, None, [AllowBackward; IsTemporal])]
    //| RetImp(p, m1, i1), RetImp(s, m2, i2) when m1 = m2 && s <> p && i1 < i2 -> [(TemporalTerm(ConImp, [s; p], i1 + i2), abd, None, [AllowBackward; IsTemporal])]

    | Imp(m1, p), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ind, None, [AllowBackward])]
    //| PreImp(m1, p, i1), PreImp(m2, s, i2) when m1 = m2 && s <> p -> [(TemporalTerm(ConImp, [s; p], i1 + i2), ind, None, [AllowBackward; IsTemporal])]
    | ConImp(m1, p), ConImp(m2, s) when m1 = m2 && s <> p -> [(TemporalTerm(ConImp, [s; p], 0s), ind, None, [AllowBackward; IsTemporal])]
    //| RetImp(m1, p, i1), RetImp(m2, s, i2) when m1 = m2 && s <> p -> [(TemporalTerm(ConImp, [s; p], i1 + i2), ind, None, [AllowBackward; IsTemporal])]

    | Imp(p, m1), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), exe, None, [AllowBackward])]
    //| PreImp(p, m1, i1), PreImp(m2, s, i2) when m1 = m2 && s <> p && i2 < i1 -> [(TemporalTerm(RetImp, [s; p], i1 + i2), exe, None, [AllowBackward; IsTemporal])]
    //| RetImp(p, m1, i1), RetImp(m2, s, i2) when m1 = m2 && s <> p && i1 < i2 -> [(TemporalTerm(PreImp, [s; p], i1 + i2), exe, None, [AllowBackward; IsTemporal])]
    | ConImp(p, m1), ConImp(m2, s) when m1 = m2 && s <> p -> [(TemporalTerm(ConImp, [s; p], 0s), exe, None, [AllowBackward; IsTemporal])]
    | _ -> []

let nal_5_implication_based_syllogism_Equ1 : InferenceFunction = function
    // Implication to Equivalence
    | Imp(s1, p1), Imp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(Equ, [s1; p1]), int, None, [AllowBackward])]
    //| ConImp(s1, p1), ConImp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(ConEqu, [s1; p1]), int, None, [AllowBackward])]
    //| PreImp(s1, p1), RetImp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(PreEqu, [s1; p1]), int, None, [AllowBackward])]
    //| RetImp(s1, p1), PreImp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(PreEqu, [p1; s1]), int, None, [AllowBackward])]

    // Equivalence based syllogism
    | Imp(p, m1), Imp(s, m2) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), com, None, [AllowBackward; Swap])]
    //| PreImp(p, m1), PreImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])
    //                                                          (Term(PreEqu, [s; p]), com, None, [AllowBackward; Swap])
    //                                                          (Term(PreEqu, [p; s]), com, None, [AllowBackward; Swap])]

    //| ConImp(p, m1), ConImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])]
    //| RetImp(p, m1), RetImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])
    //                                                          (Term(PreEqu, [s; p]), com, None, [AllowBackward; Swap])
    //                                                          (Term(PreEqu, [p; s]), com, None, [AllowBackward; Swap])]

    | Imp(m1, p), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), com, None, [AllowBackward; Swap])]
    //| PreImp(m1, p), PreImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])
    //                                                          //(Term(PreEqu, [s; p]), com, None, [AllowBackward; Swap])
    //                                                          //(Term(PreEqu, [p; s]), com, None, [AllowBackward; Swap])
    //                                                          ]
    //| ConImp(m1, p), ConImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])]
    | _ -> []

let nal_5_implication_based_syllogism_Equ2 : InferenceFunction = function
    | Imp(m1, p), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]
    | Imp(m1, p), Equ(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]

    //| PreImp(m1, p, i1), PreEqu(s, m2, i2) when m1 = m2 && s <> p -> [(TemporalTerm(PreImp, [s; p], i1), ana, None, [AllowBackward])]
    //| PreImp(m1, p, i1), PreEqu(m2, s, i2) when m1 = m2 && s <> p -> [(TemporalTerm(PreImp, [s; p], i1), ana, None, [AllowBackward])]

    //| PreImp(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ana, None, [AllowBackward])]
    //| PreImp(m1, p), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ana, None, [AllowBackward])]

    //| ConImp(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ana, None, [AllowBackward])]
    //| ConImp(m1, p), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ana, None, [AllowBackward])]

    //| RetImp(m1, p), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]
    //| RetImp(m1, p), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]

    //| RetImp(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]
    //| RetImp(m1, p), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]

    | Imp(p, m1), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]
    | Imp(p, m1), Equ(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]

    //| PreImp(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]
    //| PreImp(p, m1), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]

    //| ConImp(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [p; s]), ana, None, [AllowBackward])]
    //| ConImp(p, m1), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [p; s]), ana, None, [AllowBackward])]

    //| RetImp(p, m1), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]
    //| RetImp(p, m1), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]

    //| RetImp(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]
    //| RetImp(p, m1), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]

    | Equ(m1, p), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), res, None, [AllowBackward])]
    | Equ(m1, p), Equ(m2, s) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), res, None, [AllowBackward])]
    | Equ(p, m1), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), res, None, [AllowBackward])]
    | Equ(p, m1), Equ(m2, s) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), res, None, [AllowBackward])]

    //| PreEqu(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]
    //| PreEqu(m1, p), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]
    //| PreEqu(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]
    //| PreEqu(p, m1), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]

    //| ConEqu(m1, p), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]
    //| ConEqu(m1, p), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]
    //| ConEqu(p, m1), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]
    //| ConEqu(p, m1), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]

    | _ -> []

let nal5_implication_based_composition = function 
    | Imp(p, m1), Imp(s, m2) when m1 = m2 && s <> p ->                          [(Term(Imp, [Term(Or, sort [p; s]); m1]), int, None, [])
                                                                                 (Term(Imp, [Term(And, sort [p; s]); m1]), uni, None, [])]
    | ConImp(p, m1), ConImp(s, m2) when m1 = m2 && s <> p ->                    [(TemporalTerm(ConImp, [Term(Or, sort [p; s]); m1], 0s), int, None, [])
                                                                                 (TemporalTerm(ConImp, [TemporalTerm(Par, sort [p; s], 0s); m1], 0s), uni, None, [IsTemporal])]
    //| PreImp(p, m1, i1), PreImp(s, m2, i2) when m1 = m2 && s <> p && i2 < i1 -> [(TemporalTerm(PreImp, [Term(Or, sort [p; s]); m1], i1 + i2), int, None, [])
    //                                                                             (TemporalTerm(PreImp, [TemporalTerm(Par, sort [p; s], 0s); m1], i1 + i2), uni, None, [IsTemporal])]                                                        
    //| RetImp(p, m1, i1), RetImp(s, m2, i2) when m1 = m2 && s <> p && i2 > i1 -> [(TemporalTerm(RetImp, [Term(Or, sort [p; s]); m1], i1 + i2), int, None, [])
    //                                                                             (TemporalTerm(RetImp, [TemporalTerm(Par, sort [p; s], 0s); m1], i1 + i2), uni, None, [IsTemporal])]

    | Imp(m1, p), Imp(m2, s) when m1 = m2 && s <> p ->                          [(Term(Imp, [m1; Term(And, [p; s])]), int, None, [])
                                                                                 (Term(Imp, [m1; Term(Or, sort [p; s])]), uni, None, [])]
    //| PreImp(m1, p, i1), PreImp(m2, s, i2) when m1 = m2 && s <> p && i2 < i1 -> [(TemporalTerm(PreImp, [m1; TemporalTerm(Par, sort [p; s], 0s)], i1 + i2), int, None, [IsTemporal])
    //                                                                             (TemporalTerm(PreImp, [m1; Term(Or, sort [p; s])], i1 + i2), uni, None, [])]
    | ConImp(m1, p), ConImp(m2, s) when m1 = m2 && s <> p ->                    [(TemporalTerm(ConImp, [m1; TemporalTerm(Par, sort [p; s], 0s)], 0s), int, None, [IsTemporal])
                                                                                 (TemporalTerm(ConImp, [m1; Term(Or, sort [p; s])], 0s), uni, None, [])]
    //| RetImp(m1, p, i1), RetImp(m2, s, i2) when m1 = m2 && s <> p && i1 < i2 -> [(TemporalTerm(RetImp, [m1; TemporalTerm(Par, sort [p; s], 0s)], i1 + i2), int, None, [IsTemporal])
    //                                                                             (TemporalTerm(RetImp, [m1; Term(Or, sort [p; s])], i1 + i2), uni, None, [])]
    //| PreImp(d1, r, i1), RetImp(d2, k, i2) when d1 = d2 && r <> k ->            [(TemporalTerm(PreImp, [k; r], i1 + i2), abd, None, [IsTemporal])
    //                                                                             (TemporalTerm(RetImp, [k; r], i1 + i2), ind, None, [IsTemporal])
    //                                                                             (TemporalTerm(PreEqu, [k; r], i1 + i2), com, None, [IsTemporal])
                                                              //]
    | _ -> []

let nal5_nal8_implication_based_decomposition1 (t1, t2) = 
    if not(containsVars t1) && not(containsVars t2) then
        match t1, t2 with
        | Imp(s, m1), Imp(Or(lst), m2) when m1 = m2 && m1 <> s && isMember s lst -> [Term(Imp, [reduce <| Term(Or, sort (listLess s lst)); m1]), pnn, None, []]
        //| PreImp(s, m1, i), Imp(Or(lst), m2) when m1 = m2 && isMember s lst ->      [TemporalTerm(PreImp, [reduce <| Term(Or, sort (listLess s lst)); m1], i), pnn, None, [IsTemporal]]
        //| ConImp(s, m1), Imp(Or(lst), m2) when m1 = m2 && isMember s lst ->         [TemporalTerm(ConImp, [reduce <| Term(Or, sort (listLess s lst)); m1], 0s), pnn, None, [IsTemporal]]
        //| RetImp(s, m1, i), Imp(Or(lst), m2) when m1 = m2 && isMember s lst ->      [TemporalTerm(RetImp, [reduce <| Term(Or, sort (listLess s lst)); m1], i), pnn, None, [IsTemporal]]
        | _ -> []
    else []

let nal5_nal8_implication_based_decomposition2 (t1, t2) = 
    if not(containsVars t1) && not(containsVars t2) then
        match t1, t2 with
        | Imp(s, m1), Imp(And(lst), m2) when m1 = m2 && isMember s lst -> [Term(Imp, [reduce <| Term(And, listLess s lst); m1]), npp, None, []]
        //| PreImp(s, m1), PreImp(And(lst), m2) when m1 = m2 && isMember s lst -> [Term(PreImp, [reduce <| Term(And, listLess s lst); m1]), npp, None, []]
        //| ConImp(s, m1), ConImp(And(lst), m2) when m1 = m2 && isMember s lst -> [Term(ConImp, [reduce <| Term(And, listLess s lst); m1]), npp, None, []]
        //| RetImp(s, m1), RetImp(And(lst), m2) when m1 = m2 && isMember s lst -> [Term(RetImp, [reduce <| Term(And, listLess s lst); m1]), npp, None, []]
        | _ -> []
    else []

let nal5_nal8_implication_based_decomposition3 (t1, t2) = 
    if not(containsVars t1) && not(containsVars t2) then
        match t1, t2 with
        | Imp(m1, s), Imp(m2, And(lst)) when m1 = m2 && isMember s lst -> [(Term(Imp, [m1; reduce <| Term(And, listLess s lst)]), pnn, None, [])]
        //| PreImp(m1, s), PreImp(m2, And(lst)) when m1 = m2 && isMember s lst -> [(Term(PreImp, [m1; reduce <| Term(And, listLess s lst)]), pnn, None, [])]
        //| ConImp(m1, s), ConImp(m2, And(lst)) when m1 = m2 && isMember s lst -> [(Term(ConImp, [m1; reduce <| Term(And, listLess s lst)]), pnn, None, [])]
        //| RetImp(m1, s), RetImp(m2, And(lst)) when m1 = m2 && isMember s lst -> [(Term(RetImp, [m1; reduce <| Term(And, listLess s lst)]), pnn, None, [])]
        | _ -> []
    else []

let nal5_nal8_implication_based_decomposition4 (t1, t2) = 
    if not(containsVars t1) && not(containsVars t2) then
        match t1, t2 with    
        | Imp(m1, s), Imp(m2, Or(lst)) when m1 = m2 && isMember s lst -> [(Term(Imp, [m1; reduce <| Term(Or, sort (listLess s lst))]), npp, None, [])]
        //| PreImp(m1, s), PreImp(m2, Or(lst)) when m1 = m2 && isMember s lst -> [(Term(PreImp, [m1; reduce <| Term(Or, sort (listLess s lst))]), npp, None, [])]
        //| ConImp(m1, s), ConImp(m2, Or(lst)) when m1 = m2 && isMember s lst -> [(Term(ConImp, [m1; reduce <| Term(Or, sort (listLess s lst))]), npp, None, [])]
        //| RetImp(m1, s), RetImp(m2, Or(lst)) when m1 = m2 && isMember s lst -> [(Term(RetImp, [m1; reduce <| Term(Or, sort (listLess s lst))]), npp, None, [])]
        | _ -> []
    else []

let nal5_nal8_implication_based_decomposition5 (t1, t2) = 
    if not(containsVars t1) && not(containsVars t2) then
        match t1, t2 with    
        // conditional syllogism
        | m1, Imp(m2, p) when m1 = m2 && m1 <> p -> [(p, ded, Some weak, [])]                                                 
        //| m1, PreImp(m2, p, i1) when m1 = m2 && m1 <> p -> [(p, ded, Some weak, [])
        //                                                    (TemporalTerm(Seq, [m1; p], i1), ded, Some weak, [])]
    
        | m1, ConImp(m2, p) when m1 = m2 && m1 <> p -> [(p, ded, Some weak, [])
                                                        (TemporalTerm(Par, sort [m1; p], 0s), ded, Some weak, [IsTemporal])]
    
        //| m1, RetImp(m2, p) when m1 = m2 && m1 <> p -> [(p, ded, Some weak, [])
        //                                                (Term(Seq, [p; m1]), ded, Some weak, [])]
                                                                  
        | m1, Imp(p, m2) when m1 = m2 && m1 <> p -> [(p, abd, Some strong, [])]
        //| m1, PreImp(p, m2, i1) when m1 = m2 && m1 <> p -> [(p, abd, Some strong, [])
        //                                                    (TemporalTerm(Seq, [p; m1], i1), abd, Some strong, [])]
        //| m1, ConImp(p, m2) when m1 = m2 && m1 <> p -> [(p, ana, Some strong, [])
        //                                                (TemporalTerm(Par, [p; m1], 0s), ana, Some strong, [IsTemporal])]
        //| m1, RetImp(p, m2) when m1 = m2 && m1 <> p -> [(p, ana, Some strong, [])
        //                                                (Term(Seq, [p; m1]), ana, Some strong, [])]
    
        | m1, Equ(s, m2) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]
        | m1, Equ(m2, s) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]
    
        //| m1, ConEqu(s, m2) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]
        //| m1, ConEqu(m2, s) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]
    
        //| m1, PreEqu(s, m2) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]
        //| m1, PreEqu(m2, s) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]
    
        | _ -> []
    else []

let nal5_nal8_implication_based_decomposition6 (t1, t2) = 
    if not(containsVars t1) && not(containsVars t2) then
        match t1, t2 with    
        // conjunction decompose
        | And([a; b] as lst), s when a <> b && isMember s lst -> [(from s lst, ded, None, [])]
        | Or([a; b] as lst), s when a <> b && isMember s lst -> [(from s lst, ded, None, [])]
        //| Seq([a; b] as lst, i), s when a <> b && isMember s lst -> [(from s lst, ded, None, [])]
        //| Par([a; b] as lst), s when a <> b && isMember s lst -> [(from s lst, ded, None, [])]
        | _ -> []
    else []

let nal5_nal8_implication_based_decomposition7 (t1, t2) = 
    if not(containsVars t1) && not(containsVars t2) then
        match t1, t2 with    
        // propositional decomposition on one premise
        //| Seq([a; b], i), _ -> [(a, structuralDed, None, [Structural])
        //                        (b, structuralDed, None, [Structural])]
    
        //| Par([a; b]), _ -> [(a, structuralDed, None, [Structural])
        //                     (b, structuralDed, None, [Structural])]
    
        | And([a; b]), _ -> [(a, structuralDed, None, [Structural])
                             (b, structuralDed, None, [Structural])]
    
        | Or([a; b]), _  ->[(a, structuralDed, None, [Structural])
                            (b, structuralDed, None, [Structural])]
    
        | _ -> []
    else []

let nal5_nal8_implication_based_decomposition8 (t1, t2) = 
    if not(containsVars t1) && not(containsVars t2) then
        match t1, t2 with    
        //| s, Seq(lst, i) when isMember (Term(Not, [s])) lst -> [(reduce (TemporalTerm(Seq, listLess (Term(Not, [s])) lst, i)), nnn, None, [])]
        //| s, Par(lst) when isMember (Term(Not, [s])) lst -> [(reduce (TemporalTerm(Par, listLess (Term(Not, [s])) lst, 0s)), nnn, None, [])]
        | s, And(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(And, sort (listLess (Term(Not, [s])) lst))), nnn, None, [])]
        | s, Or(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(Or, sort (listLess (Term(Not, [s])) lst))), nnn, None, [])]
        | _ -> []
    else []

let nal5_multi_conditional_syllogism_1 : InferenceFunction = function
    // conditional deduction
    | Imp(a, m1), Imp(And(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(Imp, [Term(And, sort (a::tl)); c]), ded, None, [])]
    | Imp(a, m1), Imp(And(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, sort [a; hd]); c]), ded, None, [])]

    // conditional abduction
    | Imp(m1, c1), Imp(And(m2::[tl]), c2) when unifies m1 m2 && c1 = c2 -> [(tl, abd, None, [])]
    | Imp(m1, c1), Imp(And(hd::[m2]), c2) when unifies  m1 m2 && c1 = c2 -> [(hd, abd, None, [])]

    | y, Equ(And1(x::z::_), b)  when x <> b && unifies y x -> [(Term(Equ, [z; b]), ded, None, [])]
    | y, Equ(b, (And(x::z::_))) when x <> b && unifies y x -> [(Term(Equ, [b; z]), ded, None, [])]
    | y, Equ(And(z::x::_), b)   when x <> b && unifies y x -> [(Term(Equ, [z; b]), ded, None, [])]
    | y, Equ(b, (And(z::x::_))) when x <> b && unifies y x -> [(Term(Equ, [b; z]), ded, None, [])]

    //| y, ConEqu((Par(x::z::_)), b) when x <> b && unifies y x -> [(Term(ConEqu, [z; b]), ded, None, [])]
    //| y, ConEqu((Par(z::x::_)), b) when x <> b && unifies y x -> [(Term(ConEqu, [z; b]), ded, None, [])]
    //| y, PreEqu(Par(x::z::_), b)   when x <> b && unifies y x -> [(Term(PreEqu, [z; b]), ded, None, [])]
    //| y, PreEqu(Par(z::x::_), b)   when x <> b && unifies y x -> [(Term(PreEqu, [z; b]), ded, None, [])]
    //| y, PreEqu(b, (Par(x::z::_))) when x <> b && unifies y x -> [(Term(PreEqu, [b; z]), ded, None, [])]
    //| y, PreEqu(b, (Par(z::x::_))) when x <> b && unifies y x -> [(Term(PreEqu, [b; z]), ded, None, [])]

    | y, Imp(And(x::z::_), b) when x <> b && unifies y x    -> [(Term(Imp,    [z; b]), ded, None, [])]
    | y, Imp(And(z::x::_), b) when x <> b && unifies y x    -> [(Term(Imp,    [z; b]), ded, None, [])]
    //| y, PreImp(Seq(x::z::_, i1), b, i2) when x <> b && unifies y x -> [(TemporalTerm(PreImp, [z; b], i1), ded, None, [])]
    //| y, PreImp(Seq(z::x::_, i1), b, i2) when x <> b && unifies y x -> [(TemporalTerm(PreImp, [z; b], i1), ded, None, [])]
    //| y, ConImp(Par(x::z::_), b) when x <> b && unifies y x -> [(Term(ConImp, [z; b]), ded, None, [])]
    //| y, ConImp(Par(z::x::_), b) when x <> b && unifies y x -> [(Term(ConImp, [z; b]), ded, None, [])]
    //| y, ConImp(Par(x, z, _), b) when x <> b && unifies y x -> [(TemporalTerm(ConImp, [z; b], 0s), ded, None, [])]
    //| y, ConImp(Par(z, x, _), b) when x <> b && unifies y x -> [(TemporalTerm(ConImp, [z; b], 0s), ded, None, [])]

    //| y, RetImp(Seq(x::z::_), b) when x <> b && unifies y x -> [(Term(RetImp, [z; b]), ded, None, [])]
    //| y, RetImp(Seq(z::x::_), b) when x <> b && unifies y x -> [(Term(RetImp, [z; b]), ded, None, [])]
    
    | Imp(And(m1::a1::_), c1), Imp(And([e1; e2] as lst), c2) when e1 <> e2 && isMember m1 lst && c1 = c2 -> [(Term(Imp, [a1; from m1 lst]), ind, None, [])]
    | Imp(And(a1::m1::_), c1), Imp(And([e1; e2] as lst), c2) when e1 <> e2 && isMember m1 lst && c1 = c2 -> [(Term(Imp, [a1; from m1 lst]), ind, None, [])]

    //| PreImp(Seq(m::_, i1) as a, c1, i2), PreImp(Seq(lstB, i3) as b, c2, i4) when c1 = c2 && unifies a b    -> [(substUnify m a b, abd, None, [])]
    //| PreImp(Seq(_::m::_, i1) as a, c1, i2), PreImp(Seq(lstB, i3) as b, c2, i4) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]
    //| ConImp(Par(m::_) as a, c1), ConImp(Par(lstB) as b, c2) when c1 = c2 && unifies a b    -> [(substUnify m a b, abd, None, [])]
    //| ConImp(Par(_::m::_) as a, c1), ConImp(Par(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]
    //| RetImp(Seq(m::_) as a, c1), RetImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b    -> [(substUnify m a b, abd, None, [])]
    //| RetImp(Seq(_::m::_) as a, c1), RetImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]

    | Equ(a, m1), Imp(And(m2::[tl]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, a::[tl]); c]), ana, None, [])]
    | Equ(a, m1), Imp(And(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, hd::[a]); c]), ana, None, [])]
    
    //| ConEqu(a, m1), ConImp(Par(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(ConImp, [Term(Par, a::tl); c]), ana, None, [])]
    //| ConEqu(a, m1), ConImp(Par(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, hd::[a]); c]), ana, None, [])]
    //| PreEqu(a, m1), ConImp(Par(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(ConImp, [Term(Par, a::tl); c]), ana, None, [])]
    //| PreEqu(a, m1), ConImp(Par(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, hd::[a]); c]), ana, None, [])]
    //| PreEqu(m1, a), ConImp(Par(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(ConImp, [Term(Par, a::tl); c]), ana, None, [])]
    //| PreEqu(m1, a), ConImp(Par(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, hd::[a]); c]), ana, None, [])]

    //| PreImp(a, m1, i1), PreImp(Seq(m2::tl, i2), c, i3) when unifies m1 m2 && a <> c && m1 <> c   -> [(TemporalTerm(PreImp, [TemporalTerm(Seq, a::m1::tl, i1); c], i1), ded, None, [])]
    //| PreImp(a, m1, i1), PreImp(Seq(hd::[m2], i2), c, i3) when unifies m1 m2 && a <> c && m1 <> c -> [(TemporalTerm(PreImp, [TemporalTerm(Seq, [a; hd; m1], i1); c], i1), ded, None, [])]
    //| ConImp(a, m1), ConImp(Seq(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(ConImp, [Term(Par, a::m1::tl); c]), ded, None, [])]
    //| ConImp(a, m1), ConImp(Seq(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, a::hd::[m1]); c]), ded, None, [])]
    //| RetImp(a, m1), RetImp(Seq(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(RetImp, [Term(Seq, a::m1::tl); c]), ded, None, [])]
    //| RetImp(a, m1), RetImp(Seq(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(RetImp, [Term(Seq, [a; hd; m1]); c]), ded, None, [])]

    | Imp(And([m; u]), a), Imp(w, b) when a <> b && unifies w u -> [(Term(Imp, [Term(And, [m; b]); a]), ind, None, [])]
    | Imp(And([u; m]), a), Imp(w, b) when a <> b && unifies w u -> [(Term(Imp, [Term(And, [m; b]); a]), ind, None, [])]
    | _ -> []


let nal5_multi_conditional_syllogism_2 : InferenceFunction = function
    | Imp(And(lst), c), m when c <> m && isNotImpOrEqu m && not (isMember m lst) -> [(Term(Imp, [reduce (Term(And, listLess m lst)); c]), ind, None, [])]

    | y, Imp(And([x1; x2]), b) when unifies y x1 && b <> y && isNotImpOrEqu y -> [(substUnify (Term(Imp, [x2; b])) y x1, ded, None, [])]
    | y, Imp(And([x1; x2]), b) when unifies y x2 && b <> y && isNotImpOrEqu y -> [(substUnify (Term(Imp, [x1; b])) y x2, ded, None, [])]
                                                                                                            
    | y, Equ(And([x1; x2]), b) when unifies y x1 && b <> y && isNotImpOrEqu y -> [(substUnify (Term(Equ, [x2; b])) y x1, ded, None, [])]
    | y, Equ(And([x1; x2]), b) when unifies y x2 && b <> y && isNotImpOrEqu y -> [(substUnify (Term(Equ, [x1; b])) y x2, ded, None, [])]

    | Imp(a, c), m when a <> c && c <> m && isNotImpOrEqu m -> [(Term(Imp, [Term(And, [m; a]); c]), ind, None, [])]

    | _ -> []

let nal6_relational_symmetry_variable_introduction = function
    | Inh(Prod(a1, b1), m1), Inh(Prod(b2, a2), m2) when m1 = m2 && a1 = a2 && b1 = b2 -> [(Term(Equ, [Term(Inh, [Term(Prod, [Var(IVar, "1"); Var(IVar, "2")]); m1]); Term(Inh, [Term(Prod, [Var(IVar, "2"); Var(IVar, "1")]); m1])]), com, None, [])]
    | Inh(m1, Prod(a1, b1)), Inh(m2, Prod(b2, a2)) when m1 = m2 && a1 = a2 && b1 = b2 -> [(Term(Equ, [Term(Inh, [Term(Prod, [m1; Var(IVar, "1"); Var(IVar, "2")])]); Term(Inh, [Term(Prod, [m1; Var(IVar, "2"); Var(IVar, "1")])])]), com, None, [])]
    | _ -> []

let nal6_variable_introduction = function
    | Inh(s, m1), Inh(p, m2) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), abd, None, [BeliefOnly])
                                                        (Term(Imp, [Term(Inh, [s; Var(IVar, "1")]); Term(Inh, [p; Var(IVar, "1")])]), ind, None, [BeliefOnly])
                                                        (Term(Equ, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), com, None, [BeliefOnly])
                                                        (Term(And, [Term(Inh, [p; Var(DVar, "1")]); Term(Inh, [s; Var(DVar, "1")])]), int, None, [BeliefOnly])
                                                        
                                                        //(Term(ConImp, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), abd, None, [BeliefOnly])
                                                        //(Term(ConImp, [Term(Inh, [s; Var(IVar, "1")]); Term(Inh, [p; Var(IVar, "1")])]), ind, None, [BeliefOnly])
                                                        //(Term(ConEqu, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), com, None, [BeliefOnly])
                                                        //(Term(Par, [Term(Inh, [p; Var(DVar, "1")]); Term(Inh, [s; Var(DVar, "")])]), int, None, [BeliefOnly])

                                                        //(Term(PreImp, [Term(Inh, [s; Var(IVar, "1")]); Term(Inh, [p; Var(IVar, "1")])]), abd, None, [BeliefOnly])
                                                        //(Term(RetImp, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), ind, None, [BeliefOnly])
                                                        //(Term(Seq, [Term(Inh, [s; Var(DVar, "1")]); Term(Inh, [p; Var(DVar, "1")])]), int, None, [BeliefOnly])

                                                        //(Term(PreImp, [Term(Inh, [s; Var(IVar, "1")]); Term(Inh, [p; Var(IVar, "1")])]), abd, None, [BeliefOnly])
                                                        //(Term(RetImp, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), ind, None, [BeliefOnly])
                                                        //(Term(Seq, [Term(Inh, [s; Var(DVar, "1")]); Term(Inh, [p; Var(DVar, "1")])]), int, None, [BeliefOnly])
                                                        ]

    | Inh(m1, s), Inh(m2, p) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), ind, None, [BeliefOnly])
                                                        (Term(Imp, [Term(Inh, [Var(IVar, "1"); p]); Term(Inh, [Var(IVar, "1"); s])]), abd, None, [BeliefOnly])
                                                        (Term(Equ, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), com, None, [BeliefOnly])
                                                        (Term(And, [Term(Inh, [Var(DVar, "1"); s]); Term(Inh, [Var(DVar, "1"); p])]), int, None, [BeliefOnly])
                                                        
                                                        //(Term(ConImp, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), ind, None, [BeliefOnly])
                                                        //(Term(ConImp, [Term(Inh, [Var(IVar, "1"); p]); Term(Inh, [Var(IVar, "1"); s])]), abd, None, [BeliefOnly])
                                                        //(Term(ConEqu, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), com, None, [BeliefOnly])
                                                        //(Term(Par, [Term(Inh, [Var(DVar, "1"); s]); Term(Inh, [Var(DVar, "1"); p])]), int, None, [BeliefOnly])

                                                        //(Term(PreImp, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), ind, None, [BeliefOnly])
                                                        //(Term(RetImp, [Term(Inh, [Var(IVar, "1"); p]); Term(Inh, [Var(IVar, "1"); s])]), abd, None, [BeliefOnly])
                                                        //(Term(Seq, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), int, None, [BeliefOnly])

                                                        //(Term(PreImp, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), ind, None, [BeliefOnly])
                                                        //(Term(RetImp, [Term(Inh, [Var(IVar, "1"); p]); Term(Inh, [Var(IVar, "1"); s])]), abd, None, [BeliefOnly])
                                                        //(Term(Seq, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), int, None, [BeliefOnly])
                                                        ]


    | _ -> []

let nal6_variable_syllogisms = function
    | Imp(Inh(a, r1), z1), Imp(And([Inh(Var(DVar, "1"), b); Inh(Var(DVar, "1"), r2)]), z2) when r1 = r2 && z1 = z2 && a <> b -> [Term(Inh, [a; b]), abd, None, []]  
    | Imp(Inh(a, r1), z1), Imp(And([Inh(Var(DVar, "1"), r2); Inh(Var(DVar, "1"), b)]), z2) when r1 = r2 && z1 = z2 && a <> b -> [Term(Inh, [a; b]), abd, None, []]  

    | Inh(u, l1), Imp(And([Inh(Var(DVar, "1"), l2); Inh(Var(DVar, "1"), r)]), z) when l1 = l2 && u <> z && z <> l1 -> [(Term(Imp, [Term(Inh, [u; r]); z]), ded, None, [])]
    | Inh(u, l1), Imp(And([Inh(Var(DVar, "1"), r); Inh(Var(DVar, "1"), l2)]), z) when l1 = l2 && u <> z && z <> l1 -> [(Term(Imp, [Term(Inh, [u; r]); z]), ded, None, [])]

    | _ -> []

let nal6_multiple_variable_introduction = function
    //| Imp(a, Inh(m1, p)), Inh(m2, s) when m1 = m2 && s <> p && a <> Term(Inh, [m1; s], None) -> [(Term(Imp, [Term(And, [a; Term(Inh, [Var(IVar, "1"); s], None)], None); Term(Inh, [Var(IVar, "1"); p], None)], None), ind, None, [])
    | Imp(a, Inh(m1, p)), Inh(m2, s) when m1 = m2 && s <> p && a <> Term(Inh, [m1; s]) -> [(Term(Imp, [Term(And, [a; Term(Inh, [Var(IVar, "1"); s])]); Term(Inh, [Var(IVar, "1"); p])]), ind, None, [])
                                                                                           (Term(And, [Term(Imp, [a; Term(Inh, [Var(DVar, "1"); p])]); Term(Inh, [Var(DVar, "1"); s])]), int, None, [])]
    | And(Inh(m1, p)::lst), Inh(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Inh, [Var(IVar, "1"); s]); Term(And, Term(Inh, [Var(IVar, "1"); p])::lst)]), ind, None, [])
                                                                  (Term(And, Term(Inh, [Var(DVar, "1"); s])::Term(Inh, [Var(DVar, "1"); p])::lst), ind, None, [])]

    | Imp(a, Inh(p, m1)), Inh(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [Term(And, [a; Term(Inh, [s; Var(IVar, "1")])]); Term(Inh, [p; Var(IVar, "1")])]), abd, None, [])
                                                                (Term(And, [Term(Imp, [a; Term(Inh, [p; Var(DVar, "1")])]); Term(Inh, [s; Var(DVar, "1")])]), int, None, [])]

    | And(Inh(p, m1)::lst), Inh(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Inh, [s; Var(IVar, "1")]); Term(And, Term(Inh, [p; Var(IVar, "1")])::lst)]), ind, None, [])
                                                                  (Term(And, Term(Inh, [s; Var(DVar, "1")])::Term(Inh, [p; Var(DVar, "1")])::lst), ind, None, [])]

    | Inh(a1, l), Imp(Inh(a2, s), r) when a1 = a2 && s<> r && l <> s -> [(Term(Imp, [Term(Inh, [Var(DVar, "1"); s]); r]), ind, None, [])]

    // #R[(A --> L) ((&& (A --> S) :list/A) ==> R) |- ((&& (#X --> L) (#X --> S) :list/A) ==> R) :pre ((:substitute A #X)) :post (:t/induction)]
    | Inh(a1, l), Imp(And(Inh(a2, s)::lst), r) when a1 = a2 && s<> r && l <> s -> [(makeSubstitutions (Term(Imp, [Term(And, Term(Inh, [Var(DVar, "1"); l])::Term(Inh, [Var(DVar, "1"); s])::lst); r])) [(a1, Var(DVar, "1"))], ind, None, [])]
    | Inh(a1, l), Imp(And(_::[Inh(a2, s)] as lst), r) when a1 = a2 && s<> r && l <> s -> [(makeSubstitutions (Term(Imp, [Term(And, Term(Inh, [Var(DVar, "1"); l])::Term(Inh, [Var(DVar, "1"); s])::lst); r])) [(a1, Var(DVar, "1"))], ind, None, [])]

    | _ -> []

let nal6_variable_elimination = function
    | b, And([a; c]) when unifies b a -> [(substUnify c b a, anon_ana, Some strong, [BeliefOnly])]
    | b, And([c; a]) when unifies b a -> [(substUnify c b a, anon_ana, Some strong, [BeliefOnly])]

    | And([c; a]), b when unifies b a -> [(substUnify c b a, ded, Some strong, [GoalOnly])]
    | And([a; c]), b when unifies b a -> [(substUnify c b a, ded, Some strong, [GoalOnly])]

    | b, Imp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [])]
    //| b, PreImp(a, c, i) when unifies b a -> [(substUnify c b a, ded, Some ind, [IsTemporal])]
    //| b, ConImp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [IsTemporal])]
    //| b, RetImp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [IsTemporal])]

    | b, Imp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [])]
    //| b, PreImp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [IsTemporal])]
    //| b, ConImp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [IsTemporal])]
    //| b, RetImp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [IsTemporal])]

    | b, Equ(a, c) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]
    //| b, ConEqu(a, c) when unifies b a -> [(substUnify c b a, ana, Some ded, [IsTemporal])]
    //| b, PreEqu(a, c) when unifies b a -> [(substUnify c b a, ana, Some ded, [IsTemporal])]
    
    | b, Equ(c, a) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]
    //| b, ConEqu(c, a) when unifies b a -> [(substUnify c b a, ana, Some ded, [IsTemporal])]
    //| b, PreEqu(c, a) when unifies b a -> [(substUnify c b a, ana, Some ded, [IsTemporal])]

    | _ -> []

let nal7_temporal_inference  = function
    //| Seq(a1, c, i), a2 when a1 = a2 && isNotImpOrEqu a1 && isNotImpOrEqu c -> [(TemporalTerm(PreImp, [a1; c], i), structuralInt, None, [IsTemporal])]
    //| Seq(a, c1, i), c2 when c1 = c2 && isNotImpOrEqu a && isNotImpOrEqu c1 -> [(TemporalTerm(PreImp, [a; c1], i), structuralInt, None, [IsTemporal])]
    | Par(a1, c, _), a2 when a1 = a2 && isNotImpOrEqu a1 -> [(TemporalTerm(ConImp, [a1; c], 0s), structuralInt, None, [IsTemporal])]
    | Par(a, c1, _), c2 when c1 = c2 && isNotImpOrEqu a -> [(TemporalTerm(ConImp, [a; c1], 0s), structuralInt, None, [IsTemporal])]

    | Par(b2, c, _), Par(a, b1, _) when b1 = b2 -> [(TemporalTerm(ConImp, [a; c], 0s), ded, None, [IsTemporal])]

    | ConImp(a, c1), c2 when c1 = c2 -> [(TemporalTerm(Par, sort [a; c1], 0s), structuralInt, None, [IsTemporal])]
    | ConImp(a1, c), a2 when a1 = a2 -> [(TemporalTerm(Par, sort [a1; c], 0s), structuralInt, None, [IsTemporal])]

    //| s, p when s <> p && isNotImpOrEqu p && isNotImpOrEqu s -> [
    //                                                               //(Term(ConImp, [s; p]), temporal_ind, None, [IsTemporal])
    //                                                               //(Term(ConImp, [p; s]), temporal_abd, None, [IsTemporal])
    //                                                               //(Term(ConEqu, [s; p]), temporal_com, None, [IsTemporal])
    //                                                               (TemporalTerm(PreImp, [s; p], 0s), temporal_ind, None, [])
    //                                                               //(Term(RetImp, [p; s]), temporal_abd, None, [IsTemporal])
    //                                                               //(Term(PreEqu, [s; p]), temporal_com, None, [IsTemporal])
    //                                                               //(Term(PreImp, [p; s]), temporal_ind, None, [IsTemporal])
    //                                                               ////(Term(RetImp, [s; p]), temporal_abd, None, [IsTemporal])
    //                                                               //(Term(PreEqu, [p; s]), temporal_com, None, [IsTemporal])
    //                                                               ]

    //| s, p when isNotImpOrEqu p && isNotImpOrEqu s  -> [(TemporalTerm(Par, [s; p], 0s), temporal_int, None, [IsTemporal])
    //                                                    (TemporalTerm(Seq, [s; p], 0s), temporal_int, None, [IsTemporal])
    //                                                    (TemporalTerm(Seq, [p; s], 0s), temporal_int, None, [IsTemporal])]

    | _ -> []
