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
    | ConImp(p1, s1), ConImp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(ConImp, [p1; s1]), cnv, None, [BeliefFromQuestion])]
    | RetImp(p1, s1), PreImp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(RetImp, [p1; s1]), cnv, None, [BeliefFromQuestion])]
    | PreImp(p1, s1), RetImp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(PreImp, [p1; s1]), cnv, None, [BeliefFromQuestion])]

    // contraposition
    | Imp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(Imp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward; Structural])]
    | Imp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(Imp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward; Structural])]
    | ConImp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(ConImp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward; Structural])]
    | ConImp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(ConImp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward; Structural])]
    | PreImp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(RetImp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward; Structural])]
    | PreImp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(RetImp, [Term(Not, [p]); s1]), cnt, None, [Structural])]
    | RetImp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(PreImp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward; Structural])]
    | RetImp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(PreImp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward; Structural])]

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
    | ConImp(m1, p), ConImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ded, None, [AllowBackward; IsConcurrent])]
    | RetImp(m1, p), RetImp(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ded, None, [AllowBackward; IsBefore])]
    | PreImp(m1, p), PreImp(s, m2) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ded, None, [AllowBackward; IsAfter])]

    | Imp(p, m1), Imp(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), abd, None, [AllowBackward])]
    | ConImp(p, m1), ConImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), abd, None, [AllowBackward; IsConcurrent])]
    | PreImp(p, m1), PreImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), abd, None, [AllowBackward; IsConcurrent])]
    | RetImp(p, m1), RetImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), abd, None, [AllowBackward; IsConcurrent])]

    | Imp(m1, p), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ind, None, [AllowBackward])]
    | PreImp(m1, p), PreImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ind, None, [AllowBackward; IsConcurrent])]
    | ConImp(m1, p), ConImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ind, None, [AllowBackward; IsConcurrent])]
    | RetImp(m1, p), RetImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ind, None, [AllowBackward; IsConcurrent])]

    | Imp(p, m1), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), exe, None, [AllowBackward])]
    | PreImp(p, m1), PreImp(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), exe, None, [AllowBackward; IsAfter])]
    | RetImp(p, m1), RetImp(m2, s) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), exe, None, [AllowBackward; IsBefore])]
    | ConImp(p, m1), ConImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), exe, None, [AllowBackward; IsConcurrent])]
    | _ -> []

let nal_5_implication_based_syllogism_Equ1 : InferenceFunction = function
    // Implication to Equivalence
    | Imp(s1, p1), Imp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(Equ, [s1; p1]), int, None, [AllowBackward])]
    | ConImp(s1, p1), ConImp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(ConEqu, [s1; p1]), int, None, [AllowBackward])]
    | PreImp(s1, p1), RetImp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(PreEqu, [s1; p1]), int, None, [AllowBackward])]
    | RetImp(s1, p1), PreImp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(PreEqu, [p1; s1]), int, None, [AllowBackward])]

    // Equivalence based syllogism
    | Imp(p, m1), Imp(s, m2) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), com, None, [AllowBackward; Swap])]
    | PreImp(p, m1), PreImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])
                                                              (Term(PreEqu, [s; p]), com, None, [AllowBackward; Swap])
                                                              (Term(PreEqu, [p; s]), com, None, [AllowBackward; Swap])]

    | ConImp(p, m1), ConImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])]
    | RetImp(p, m1), RetImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])
                                                              (Term(PreEqu, [s; p]), com, None, [AllowBackward; Swap])
                                                              (Term(PreEqu, [p; s]), com, None, [AllowBackward; Swap])]

    | Imp(m1, p), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), com, None, [AllowBackward; Swap])]
    | PreImp(m1, p), PreImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])
                                                              (Term(PreEqu, [s; p]), com, None, [AllowBackward; Swap])
                                                              (Term(PreEqu, [p; s]), com, None, [AllowBackward; Swap])]
    | ConImp(m1, p), ConImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward; Swap])]
    | _ -> []

let nal_5_implication_based_syllogism_Equ2 : InferenceFunction = function
    | Imp(m1, p), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]
    | Imp(m1, p), Equ(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]

    | PreImp(m1, p), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ana, None, [AllowBackward])]
    | PreImp(m1, p), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ana, None, [AllowBackward])]

    | PreImp(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ana, None, [AllowBackward])]
    | PreImp(m1, p), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ana, None, [AllowBackward])]

    | ConImp(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ana, None, [AllowBackward])]
    | ConImp(m1, p), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ana, None, [AllowBackward])]

    | RetImp(m1, p), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]
    | RetImp(m1, p), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]

    | RetImp(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]
    | RetImp(m1, p), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]

    | Imp(p, m1), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]
    | Imp(p, m1), Equ(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]

    | PreImp(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]
    | PreImp(p, m1), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]

    | ConImp(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [p; s]), ana, None, [AllowBackward])]
    | ConImp(p, m1), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [p; s]), ana, None, [AllowBackward])]

    | RetImp(p, m1), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]
    | RetImp(p, m1), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]

    | RetImp(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]
    | RetImp(p, m1), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]

    | Equ(m1, p), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), res, None, [AllowBackward])]
    | Equ(m1, p), Equ(m2, s) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), res, None, [AllowBackward])]
    | Equ(p, m1), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), res, None, [AllowBackward])]
    | Equ(p, m1), Equ(m2, s) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), res, None, [AllowBackward])]

    | PreEqu(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward; IsAfter])]
    | PreEqu(m1, p), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward; IsAfter])]
    | PreEqu(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward; IsAfter])]
    | PreEqu(p, m1), ConEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward; IsAfter])]

    | ConEqu(m1, p), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward; IsAfter])]
    | ConEqu(m1, p), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward; IsAfter])]
    | ConEqu(p, m1), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward; IsAfter])]
    | ConEqu(p, m1), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward; IsAfter])]

    | _ -> []

let nal5_implication_based_composition = function 
    | Imp(p, m1), Imp(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Or, [p; s]); m1]), int, None, [])
                                                        (Term(Imp, [Term(And, [p; s]); m1]), uni, None, [])]
    | ConImp(p, m1), ConImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [Term(Or, [p; s]); m1]), int, None, [])
                                                              (Term(ConImp, [Term(Par, [p; s]); m1]), uni, None, [])]
    | PreImp(p, m1), PreImp(s, m2) when m1 = m2 && s <> p -> [(Term(PreImp, [Term(Or, [p; s]); m1]), int, None, [])
                                                              (Term(PreImp, [Term(Par, [p; s]); m1]), uni, None, [])]                                                        
    | RetImp(p, m1), RetImp(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [Term(Or, [p; s]); m1]), int, None, [])
                                                              (Term(RetImp, [Term(Par, [p; s]); m1]), uni, None, [])]

    | Imp(m1, p), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [m1; Term(And, [p; s])]), int, None, [])
                                                        (Term(Imp, [m1; Term(Or, [p; s])]), uni, None, [])]
    | PreImp(m1, p), PreImp(m2, s) when m1 = m2 && s <> p -> [(Term(PreImp, [m1; Term(Par, [p; s])]), int, None, [])
                                                              (Term(PreImp, [m1; Term(Or, [p; s])]), uni, None, [])]
    | ConImp(m1, p), ConImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [m1; Term(Par, [p; s])]), int, None, [])
                                                              (Term(ConImp, [m1; Term(Or, [p; s])]), uni, None, [])]
    | RetImp(m1, p), RetImp(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [m1; Term(Par, [p; s])]), int, None, [])
                                                              (Term(RetImp, [m1; Term(Or, [p; s])]), uni, None, [])]
    | PreImp(d1, r), RetImp(d2, k) when d1 = d2 && r <> k -> [(Term(PreImp, [k; r]), abd, None, [])
                                                              (Term(RetImp, [k; r]), ind, None, [])
                                                              (Term(PreEqu, [k; r]), com, None, [])]
    | _ -> []

let nal5_nal8_implication_based_decomposition1 = function
    | Imp(s, m1), Imp(Or(lst), m2) when m1 = m2 && m1 <> s && isMember s lst -> [Term(Imp, [reduce <| Term(Or, listLess s lst); m1]), pnn, None, []]
    | PreImp(s, m1), Imp(Or(lst), m2) when m1 = m2 && isMember s lst -> [Term(PreImp, [reduce <| Term(Or, listLess s lst); m1]), pnn, None, []]
    | ConImp(s, m1), Imp(Or(lst), m2) when m1 = m2 && isMember s lst -> [Term(ConImp, [reduce <| Term(Or, listLess s lst); m1]), pnn, None, []]
    | RetImp(s, m1), Imp(Or(lst), m2) when m1 = m2 && isMember s lst -> [Term(RetImp, [reduce <| Term(Or, listLess s lst); m1]), pnn, None, []]
    | _ -> []

let nal5_nal8_implication_based_decomposition2 = function
    | Imp(s, m1), Imp(And(lst), m2) when m1 = m2 && isMember s lst -> [Term(Imp, [reduce <| Term(And, listLess s lst); m1]), npp, None, []]
    | PreImp(s, m1), PreImp(And(lst), m2) when m1 = m2 && isMember s lst -> [Term(PreImp, [reduce <| Term(And, listLess s lst); m1]), npp, None, []]
    | ConImp(s, m1), ConImp(And(lst), m2) when m1 = m2 && isMember s lst -> [Term(ConImp, [reduce <| Term(And, listLess s lst); m1]), npp, None, []]
    | RetImp(s, m1), RetImp(And(lst), m2) when m1 = m2 && isMember s lst -> [Term(RetImp, [reduce <| Term(And, listLess s lst); m1]), npp, None, []]
    | _ -> []

let nal5_nal8_implication_based_decomposition3 = function
    | Imp(m1, s), Imp(m2, And(lst)) when m1 = m2 && isMember s lst -> [(Term(Imp, [m1; reduce <| Term(And, listLess s lst)]), pnn, None, [])]
    | PreImp(m1, s), PreImp(m2, And(lst)) when m1 = m2 && isMember s lst -> [(Term(PreImp, [m1; reduce <| Term(And, listLess s lst)]), pnn, None, [])]
    | ConImp(m1, s), ConImp(m2, And(lst)) when m1 = m2 && isMember s lst -> [(Term(ConImp, [m1; reduce <| Term(And, listLess s lst)]), pnn, None, [])]
    | RetImp(m1, s), RetImp(m2, And(lst)) when m1 = m2 && isMember s lst -> [(Term(RetImp, [m1; reduce <| Term(And, listLess s lst)]), pnn, None, [])]
    | _ -> []

let nal5_nal8_implication_based_decomposition4 = function
    | Imp(m1, s), Imp(m2, Or(lst)) when m1 = m2 && isMember s lst -> [(Term(Imp, [m1; reduce <| Term(Or, listLess s lst)]), npp, None, [])]
    | PreImp(m1, s), PreImp(m2, Or(lst)) when m1 = m2 && isMember s lst -> [(Term(PreImp, [m1; reduce <| Term(Or, listLess s lst)]), npp, None, [])]
    | ConImp(m1, s), ConImp(m2, Or(lst)) when m1 = m2 && isMember s lst -> [(Term(ConImp, [m1; reduce <| Term(Or, listLess s lst)]), npp, None, [])]
    | RetImp(m1, s), RetImp(m2, Or(lst)) when m1 = m2 && isMember s lst -> [(Term(RetImp, [m1; reduce <| Term(Or, listLess s lst)]), npp, None, [])]
    | _ -> []

let nal5_nal8_implication_based_decomposition5 = function
    // conditional syllogism
    | m1, Imp(m2, p) when m1 = m2 && m1 <> p -> [(p, ded, Some weak, [])]                                                 
    | m1, PreImp(m2, p) when m1 = m2 && m1 <> p -> [(p, ded, Some weak, [])
                                                    (Term(Seq, [m1; p]), ded, Some weak, [])]

    | m1, ConImp(m2, p) when m1 = m2 && m1 <> p -> [(p, ded, Some weak, [])
                                                    (Term(Par, [m1; p]), ded, Some weak, [])]

    | m1, RetImp(m2, p) when m1 = m2 && m1 <> p -> [(p, ded, Some weak, [])
                                                    (Term(Seq, [p; m1]), ded, Some weak, [])]
                                                              
    | m1, Imp(p, m2) when m1 = m2 && m1 <> p -> [(p, abd, Some strong, [])]
    | m1, PreImp(p, m2) when m1 = m2 && m1 <> p -> [(p, abd, Some strong, [])
                                                    (Term(Seq, [p; m1]), abd, Some strong, [])]
    | m1, ConImp(p, m2) when m1 = m2 && m1 <> p -> [(p, ana, Some strong, [])
                                                    (Term(Par, [p; m1]), ana, Some strong, [])]
    | m1, RetImp(p, m2) when m1 = m2 && m1 <> p -> [(p, ana, Some strong, [])
                                                    (Term(Seq, [p; m1]), ana, Some strong, [])]

    | m1, Equ(s, m2) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]
    | m1, Equ(m2, s) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]

    | m1, ConEqu(s, m2) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]
    | m1, ConEqu(m2, s) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]

    | m1, PreEqu(s, m2) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]
    | m1, PreEqu(m2, s) when m1 = m2 && m1 <> s -> [(s, ana, Some strong, [])]

    | _ -> []

let nal5_nal8_implication_based_decomposition6 = function
    // conjunction decompose
    | And([a; b] as lst), s when a <> b && isMember s lst -> [(from s lst, ded, None, [])]
    | Or([a; b] as lst), s when a <> b && isMember s lst -> [(from s lst, ded, None, [])]
    | Seq([a; b] as lst), s when a <> b && isMember s lst -> [(from s lst, ded, None, [IsConcurrent])]
    | Par([a; b] as lst), s when a <> b && isMember s lst -> [(from s lst, ded, None, [IsConcurrent])]
    | _ -> []

let nal5_nal8_implication_based_decomposition7 = function
    // propositional decomposition on one premise
    | Seq([a; b]), _ -> [(a, structuralDed, None, [Structural])
                         (b, structuralDed, None, [Structural])]

    | Par([a; b]), _ -> [(a, structuralDed, None, [Structural])
                         (b, structuralDed, None, [Structural])]

    | And([a; b]), _ -> [(a, structuralDed, None, [Structural])
                         (b, structuralDed, None, [Structural])]

    | Or([a; b]), _  ->[(a, structuralDed, None, [Structural])
                        (b, structuralDed, None, [Structural])]

    | _ -> []

let nal5_nal8_implication_based_decomposition8 = function
    | s, Seq(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(Seq, listLess (Term(Not, [s])) lst)), nnn, None, [])]
    | s, Par(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(Par, listLess (Term(Not, [s])) lst)), nnn, None, [])]
    | s, And(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(And, listLess (Term(Not, [s])) lst)), nnn, None, [])]
    | s, Or(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(Or, listLess (Term(Not, [s])) lst)), nnn, None, [])]
    | _ -> []

let nal5_multi_conditional_syllogism_1 : InferenceFunction = function
    // conditional deduction
    | Imp(a, m1), Imp(And(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(Imp, [Term(And, a::tl); c]), ded, None, [])]
    | Imp(a, m1), Imp(And(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, [a; hd]); c]), ded, None, [])]

    // conditional abduction
    | Imp(m1, c1), Imp(And(m2::[tl]), c2) when unifies m1 m2 && c1 = c2 -> [(tl, abd, None, [])]
    | Imp(m1, c1), Imp(And(hd::[m2]), c2) when unifies  m1 m2 && c1 = c2 -> [(hd, abd, None, [])]

    | y, Equ(And1(x::z::_), b)  when x <> b && unifies y x -> [(Term(Equ, [z; b]), ded, None, [])]
    | y, Equ(b, (And(x::z::_))) when x <> b && unifies y x -> [(Term(Equ, [b; z]), ded, None, [])]
    | y, Equ(And(z::x::_), b)   when x <> b && unifies y x -> [(Term(Equ, [z; b]), ded, None, [])]
    | y, Equ(b, (And(z::x::_))) when x <> b && unifies y x -> [(Term(Equ, [b; z]), ded, None, [])]

    | y, ConEqu((Par(x::z::_)), b) when x <> b && unifies y x -> [(Term(ConEqu, [z; b]), ded, None, [IsConcurrent])]
    | y, ConEqu((Par(z::x::_)), b) when x <> b && unifies y x -> [(Term(ConEqu, [z; b]), ded, None, [IsConcurrent])]
    | y, PreEqu(Par(x::z::_), b)   when x <> b && unifies y x -> [(Term(PreEqu, [z; b]), ded, None, [IsConcurrent])]
    | y, PreEqu(Par(z::x::_), b)   when x <> b && unifies y x -> [(Term(PreEqu, [z; b]), ded, None, [IsConcurrent])]
    | y, PreEqu(b, (Par(x::z::_))) when x <> b && unifies y x -> [(Term(PreEqu, [b; z]), ded, None, [IsConcurrent])]
    | y, PreEqu(b, (Par(z::x::_))) when x <> b && unifies y x -> [(Term(PreEqu, [b; z]), ded, None, [IsConcurrent])]

    | y, Imp(And(x::z::_), b) when x <> b && unifies y x    -> [(Term(Imp,    [z; b]), ded, None, [])]
    | y, Imp(And(z::x::_), b) when x <> b && unifies y x    -> [(Term(Imp,    [z; b]), ded, None, [])]
    | y, PreImp(Seq(x::z::_), b) when x <> b && unifies y x -> [(Term(PreImp, [z; b]), ded, None, [IsConcurrent])]
    | y, PreImp(Seq(z::x::_), b) when x <> b && unifies y x -> [(Term(PreImp, [z; b]), ded, None, [IsConcurrent])]
    | y, ConImp(Par(x::z::_), b) when x <> b && unifies y x -> [(Term(ConImp, [z; b]), ded, None, [IsConcurrent])]
    | y, ConImp(Par(z::x::_), b) when x <> b && unifies y x -> [(Term(ConImp, [z; b]), ded, None, [IsConcurrent])]
    | y, RetImp(Seq(x::z::_), b) when x <> b && unifies y x -> [(Term(RetImp, [z; b]), ded, None, [IsConcurrent])]
    | y, RetImp(Seq(z::x::_), b) when x <> b && unifies y x -> [(Term(RetImp, [z; b]), ded, None, [IsConcurrent])]
    
    | Imp(And(m1::a1::_), c1), Imp(And([e1; e2] as lst), c2) when e1 <> e2 && isMember m1 lst && c1 = c2 -> [(Term(Imp, [a1; from m1 lst]), ind, None, [])]
    | Imp(And(a1::m1::_), c1), Imp(And([e1; e2] as lst), c2) when e1 <> e2 && isMember m1 lst && c1 = c2 -> [(Term(Imp, [a1; from m1 lst]), ind, None, [])]

    | PreImp(Seq(m::_) as a, c1), PreImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b    -> [(substUnify m a b, abd, None, [])]
    | PreImp(Seq(_::m::_) as a, c1), PreImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]
    | ConImp(Par(m::_) as a, c1), ConImp(Par(lstB) as b, c2) when c1 = c2 && unifies a b    -> [(substUnify m a b, abd, None, [IsConcurrent])]
    | ConImp(Par(_::m::_) as a, c1), ConImp(Par(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [IsConcurrent])]
    | RetImp(Seq(m::_) as a, c1), RetImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b    -> [(substUnify m a b, abd, None, [])]
    | RetImp(Seq(_::m::_) as a, c1), RetImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]

    | Equ(a, m1), Imp(And(m2::[tl]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, a::[tl]); c]), ana, None, [])]
    | Equ(a, m1), Imp(And(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, hd::[a]); c]), ana, None, [])]
    
    | ConEqu(a, m1), ConImp(Par(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(ConImp, [Term(Par, a::tl); c]), ana, None, [IsConcurrent])]
    | ConEqu(a, m1), ConImp(Par(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, hd::[a]); c]), ana, None, [IsConcurrent])]
    | PreEqu(a, m1), ConImp(Par(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(ConImp, [Term(Par, a::tl); c]), ana, None, [IsConcurrent])]
    | PreEqu(a, m1), ConImp(Par(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, hd::[a]); c]), ana, None, [IsConcurrent])]
    | PreEqu(m1, a), ConImp(Par(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(ConImp, [Term(Par, a::tl); c]), ana, None, [IsConcurrent])]
    | PreEqu(m1, a), ConImp(Par(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, hd::[a]); c]), ana, None, [IsConcurrent])]

    | PreImp(a, m1), PreImp(Seq(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(PreImp, [Term(Seq, a::m1::tl); c]), ded, None, [])]
    | PreImp(a, m1), PreImp(Seq(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(PreImp, [Term(Seq, [a; hd; m1]); c]), ded, None, [])]
    | ConImp(a, m1), ConImp(Seq(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(ConImp, [Term(Par, a::m1::tl); c]), ded, None, [IsConcurrent])]
    | ConImp(a, m1), ConImp(Seq(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, a::hd::[m1]); c]), ded, None, [IsConcurrent])]
    | RetImp(a, m1), RetImp(Seq(m2::tl), c) when unifies m1 m2 && a <> c && m1 <> c   -> [(Term(RetImp, [Term(Seq, a::m1::tl); c]), ded, None, [])]
    | RetImp(a, m1), RetImp(Seq(hd::[m2]), c) when unifies m1 m2 && a <> c && m1 <> c -> [(Term(RetImp, [Term(Seq, [a; hd; m1]); c]), ded, None, [])]

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


let nal6_variable_introduction = function
    | Inh(s, m1), Inh(p, m2) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), abd, None, [BeliefOnly])
                                                        (Term(Imp, [Term(Inh, [s; Var(IVar, "1")]); Term(Inh, [p; Var(IVar, "1")])]), ind, None, [BeliefOnly])
                                                        (Term(Equ, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), com, None, [BeliefOnly])
                                                        (Term(And, [Term(Inh, [p; Var(DVar, "1")]); Term(Inh, [s; Var(DVar, "1")])]), int, None, [BeliefOnly])
                                                        
                                                        (Term(ConImp, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), abd, None, [BeliefOnly; IsConcurrent])
                                                        (Term(ConImp, [Term(Inh, [s; Var(IVar, "1")]); Term(Inh, [p; Var(IVar, "1")])]), ind, None, [BeliefOnly; IsConcurrent])
                                                        (Term(ConEqu, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), com, None, [BeliefOnly; IsConcurrent])
                                                        (Term(Par, [Term(Inh, [p; Var(DVar, "1")]); Term(Inh, [s; Var(DVar, "")])]), int, None, [BeliefOnly; IsConcurrent])

                                                        (Term(PreImp, [Term(Inh, [s; Var(IVar, "1")]); Term(Inh, [p; Var(IVar, "1")])]), abd, None, [BeliefOnly; IsBefore])
                                                        (Term(RetImp, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), ind, None, [BeliefOnly; IsBefore])
                                                        (Term(Seq, [Term(Inh, [s; Var(DVar, "1")]); Term(Inh, [p; Var(DVar, "1")])]), int, None, [BeliefOnly; IsBefore])

                                                        (Term(PreImp, [Term(Inh, [s; Var(IVar, "1")]); Term(Inh, [p; Var(IVar, "1")])]), abd, None, [BeliefOnly; IsAfter])
                                                        (Term(RetImp, [Term(Inh, [p; Var(IVar, "1")]); Term(Inh, [s; Var(IVar, "1")])]), ind, None, [BeliefOnly; IsAfter])
                                                        (Term(Seq, [Term(Inh, [s; Var(DVar, "1")]); Term(Inh, [p; Var(DVar, "1")])]), int, None, [BeliefOnly; IsAfter])]

    | Inh(m1, s), Inh(m2, p) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), ind, None, [BeliefOnly])
                                                        (Term(Imp, [Term(Inh, [Var(IVar, "1"); p]); Term(Inh, [Var(IVar, "1"); s])]), abd, None, [BeliefOnly])
                                                        (Term(Equ, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), com, None, [BeliefOnly])
                                                        (Term(And, [Term(Inh, [Var(DVar, "1"); s]); Term(Inh, [Var(DVar, "1"); p])]), int, None, [BeliefOnly])
                                                        
                                                        (Term(ConImp, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), ind, None, [BeliefOnly; IsConcurrent])
                                                        (Term(ConImp, [Term(Inh, [Var(IVar, "1"); p]); Term(Inh, [Var(IVar, "1"); s])]), abd, None, [BeliefOnly; IsConcurrent])
                                                        (Term(ConEqu, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), com, None, [BeliefOnly; IsConcurrent])
                                                        (Term(Par, [Term(Inh, [Var(DVar, "1"); s]); Term(Inh, [Var(DVar, "1"); p])]), int, None, [BeliefOnly; IsConcurrent])

                                                        (Term(PreImp, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), ind, None, [BeliefOnly; IsBefore])
                                                        (Term(RetImp, [Term(Inh, [Var(IVar, "1"); p]); Term(Inh, [Var(IVar, "1"); s])]), abd, None, [BeliefOnly; IsBefore])
                                                        (Term(Seq, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), int, None, [BeliefOnly; IsBefore])

                                                        (Term(PreImp, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), ind, None, [BeliefOnly; IsAfter])
                                                        (Term(RetImp, [Term(Inh, [Var(IVar, "1"); p]); Term(Inh, [Var(IVar, "1"); s])]), abd, None, [BeliefOnly; IsAfter])
                                                        (Term(Seq, [Term(Inh, [Var(IVar, "1"); s]); Term(Inh, [Var(IVar, "1"); p])]), int, None, [BeliefOnly; IsAfter])]


    | _ -> []

let nal6_variable_syllogisms = function
    | Imp(Inh(a, r1), z1), Imp(And([Inh(Var(DVar, "1"), b); Inh(Var(DVar, "1"), r2)]), z2) when r1 = r2 && z1 = z2 && a <> b -> [Term(Inh, [a; b]), abd, None, []]  
    | Imp(Inh(a, r1), z1), Imp(And([Inh(Var(DVar, "1"), r2); Inh(Var(DVar, "1"), b)]), z2) when r1 = r2 && z1 = z2 && a <> b -> [Term(Inh, [a; b]), abd, None, []]  

    | Inh(u, l1), Imp(And([Inh(Var(DVar, "1"), l2); Inh(Var(DVar, "1"), r)]), z) when l1 = l2 && u <> z && z <> l1 -> [(Term(Imp, [Term(Inh, [u; r]); z]), ded, None, [])]
    | Inh(u, l1), Imp(And([Inh(Var(DVar, "1"), r); Inh(Var(DVar, "1"), l2)]), z) when l1 = l2 && u <> z && z <> l1 -> [(Term(Imp, [Term(Inh, [u; r]); z]), ded, None, [])]

    | _ -> []

let nal6_multiple_variable_introduction = function
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
    | b, PreImp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [IsConcurrent])]
    | b, ConImp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [IsConcurrent])]
    | b, RetImp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [IsAfter])]

    | b, Imp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [])]
    | b, PreImp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [IsAfter])]
    | b, ConImp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [IsConcurrent])]
    | b, RetImp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [IsBefore])]

    | b, Equ(a, c) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]
    | b, ConEqu(a, c) when unifies b a -> [(substUnify c b a, ana, Some ded, [IsConcurrent])]
    | b, PreEqu(a, c) when unifies b a -> [(substUnify c b a, ana, Some ded, [IsAfter])]
    
    | b, Equ(c, a) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]
    | b, ConEqu(c, a) when unifies b a -> [(substUnify c b a, ana, Some ded, [IsAfter])]
    | b, PreEqu(c, a) when unifies b a -> [(substUnify c b a, ana, Some ded, [IsAfter])]

    | _ -> []

let nal7_temporal_inference  = function
    | s, p when s <> p &&  isNotImpOrEqu p && isNotImpOrEqu s  -> [(Term(ConImp, [s; p]), temporal_ind, None, [IsConcurrent])
                                                                   (Term(ConImp, [p; s]), temporal_abd, None, [IsConcurrent])
                                                                   (Term(ConEqu, [s; p]), temporal_com, None, [IsConcurrent])
                                                                   (Term(PreImp, [s; p]), temporal_ind, None, [IsBefore])
                                                                   (Term(RetImp, [p; s]), temporal_abd, None, [IsBefore])
                                                                   (Term(PreEqu, [s; p]), temporal_com, None, [IsBefore])
                                                                   (Term(PreImp, [p; s]), temporal_ind, None, [IsBefore])
                                                                   (Term(RetImp, [s; p]), temporal_abd, None, [IsBefore])
                                                                   (Term(PreEqu, [p; s]), temporal_com, None, [IsBefore])]

    | s, p when isNotImpOrEqu p && isNotImpOrEqu s  -> [(Term(Par,    [s; p]), temporal_int, None, [IsConcurrent])
                                                        (Term(Seq,    [s; p]), temporal_int, None, [IsBefore])
                                                        (Term(Seq,    [p; s]), temporal_int, None, [IsBefore])]

    | _ -> []
