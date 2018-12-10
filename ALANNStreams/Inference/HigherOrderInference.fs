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
open Factories
open ActivePatterns
open InferenceUtils

let revision = function
    | a, b when a = b -> [(a, rev, None, [])]
    | _ -> []

let Nal5_conversion_contrapostion_negation : InferenceFunction = function
    // conversion
    | Imp(p1, s1), Imp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(Imp, [p1; s1]), cnv, None, [BeliefFromQuestion])]
    | ConImp(p1, s1), ConImp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(ConImp, [p1; s1]), cnv, None, [BeliefFromQuestion])]
    | RetImp(p1, s1), PreImp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(RetImp, [p1; s1]), cnv, None, [BeliefFromQuestion])]
    | PreImp(p1, s1), RetImp(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(PreImp, [p1; s1]), cnv, None, [BeliefFromQuestion])]

    // contraposition
    | Imp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(Imp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward])]
    | Imp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(Imp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward])]
    | ConImp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(ConImp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward])]
    | ConImp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(ConImp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward])]
    | PreImp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(RetImp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward])]
    | PreImp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(RetImp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward])]
    | RetImp(Not(s), p1), p2 when p1 = p2 && s <> p1 -> [(Term(PreImp, [Term(Not, [p1]); s]), cnt, None, [AllowBackward])]
    | RetImp(Not(s1), p), Not(s2) when s1 = s2 && s1 <> p -> [(Term(PreImp, [Term(Not, [p]); s1]), cnt, None, [AllowBackward])]

    // negation
    | Not(Inh(a1, b)), a2 when a1 = a2 && a1 <> b -> [(Term(Inh, [a1; b]), neg, Some d_neg, [AllowBackward])]
    | Not(Inh(a, b1)), b2 when b1 = b2 && a <> b1 -> [(Term(Inh, [a; b1]), neg, Some d_neg, [AllowBackward])]

    | Not(Sim(a1, b)), a2 when a1 = a2 && a1 <> b -> [(Term(Sim, [a1; b]), neg, Some d_neg, [AllowBackward])]
    | Not(Sim(a, b1)), b2 when b1 = b2 && a <> b1 -> [(Term(Sim, [a; b1]), neg, Some d_neg, [AllowBackward])]

    | Imp(a1, b), a2 when a1 = a2 && a1 <> b -> [(Term(Not, [Term(Imp, [a1; b])]), neg, Some d_neg, [AllowBackward])]
    | Imp(a, b1), b2 when b1 = b2 && a <> b1 -> [(Term(Not, [Term(Imp, [a; b1])]), neg, Some d_neg, [AllowBackward])]
    | Not(Imp(a1, b)), a2 when a1 = a2 && a1 <> b -> [(Term(Imp, [a1; b]), neg, Some d_neg, [AllowBackward])]
    | Not(Imp(a, b1)), b2 when b1 = b2 && a <> b1 -> [(Term(Imp, [a; b1]), neg, Some d_neg, [AllowBackward])]

    | Equ(a1, b), a2 when a1 = a2 && a1 <> b -> [(Term(Not, [Term(Equ, [a1; b])]), neg, Some d_neg, [AllowBackward])]
    | Equ(a, b1), b2 when b1 = b2 && a <> b1 -> [(Term(Not, [Term(Equ, [a; b1])]), neg, Some d_neg, [AllowBackward])]
    | Not(Equ(a1, b)), a2 when a1 = a2 && a1 <> b -> [(Term(Equ, [a1; b]), neg, Some d_neg, [AllowBackward])]
    | Not(Equ(a, b1)), b2 when b1 = b2 && a <> b1 -> [(Term(Equ, [a; b1]), neg, Some d_neg, [AllowBackward])]
    | _ -> []

let nal_5_implication_based_syllogism_Imp : InferenceFunction = function
    | Imp(m1, p), Imp(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ded, None, [AllowBackward])]
    | ConImp(m1, p), ConImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ded, None, [AllowBackward])]
    | RetImp(m1, p), RetImp(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ded, None, [AllowBackward])]
    | PreImp(m1, p), PreImp(s, m2) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ded, None, [AllowBackward])]

    | Imp(p, m1), Imp(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), abd, None, [AllowBackward])]
    | ConImp(p, m1), ConImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ind, None, [AllowBackward])]
    | PreImp(p, m1), PreImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ind, None, [AllowBackward])]
    | RetImp(p, m1), RetImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ind, None, [AllowBackward])]

    | Imp(m1, p), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), abd, None, [AllowBackward])]
    | PreImp(m1, p), PreImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), abd, None, [AllowBackward])]
    | ConImp(m1, p), ConImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), abd, None, [AllowBackward])]
    | RetImp(m1, p), RetImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), abd, None, [AllowBackward])]

    | Imp(p, m1), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), exe, None, [AllowBackward])]
    | PreImp(p, m1), PreImp(m2, s) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), exe, None, [AllowBackward])]
    | RetImp(p, m1), RetImp(m2, s) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), exe, None, [AllowBackward])]
    | ConImp(p, m1), ConImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), exe, None, [AllowBackward])]
    | _ -> []

let nal_5_implication_based_syllogism_Equ1 : InferenceFunction = function
    // Implication to Equivalence
    | Imp(s1, p1), Imp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(Equ, [s1; p1]), int, None, [AllowBackward])]
    | ConImp(s1, p1), ConImp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(ConEqu, [s1; p1]), int, None, [AllowBackward])]
    | PreImp(s1, p1), RetImp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(PreEqu, [s1; p1]), int, None, [AllowBackward])]
    | RetImp(s1, p1), PreImp(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(PreEqu, [p1; s1]), int, None, [AllowBackward])]

    // Equivalence based syllogism
    | Imp(p, m1), Imp(s, m2) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), com, None, [AllowBackward])]
    | PreImp(p, m1), PreImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), ind, None, [AllowBackward])
                                                              (Term(PreEqu, [s; p]), ind, None, [AllowBackward])
                                                              (Term(PreEqu, [p; s]), ind, None, [AllowBackward])]
    | ConImp(p, m1), ConImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), ind, None, [AllowBackward])]
    | RetImp(p, m1), RetImp(s, m2) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), ind, None, [AllowBackward])
                                                              (Term(PreEqu, [s; p]), ind, None, [AllowBackward])
                                                              (Term(PreEqu, [p; s]), ind, None, [AllowBackward])]

    | Imp(m1, p), Imp(m2, s) when m1 = m2 && s <> p -> [(Term(Equ, [s; p]), com, None, [AllowBackward])]
    | PreImp(m1, p), PreImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward])
                                                              (Term(PreEqu, [s; p]), com, None, [AllowBackward])
                                                              (Term(PreEqu, [p; s]), com, None, [AllowBackward])]
    | ConImp(m1, p), ConImp(m2, s) when m1 = m2 && s <> p -> [(Term(ConEqu, [s; p]), com, None, [AllowBackward])]
    | _ -> []

let nal_5_implication_based_syllogism_Equ2 : InferenceFunction = function
    | Imp(m1, p), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]
    | PreImp(m1, p), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ana, None, [AllowBackward])]
    | PreImp(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreImp, [s; p]), ana, None, [AllowBackward])]
    | ConImp(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [s; p]), ana, None, [AllowBackward])]
    | RetImp(m1, p), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]
    | RetImp(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [s; p]), ana, None, [AllowBackward])]

    | Imp(p, m1), Equ(s, m2) when m1 = m2 && s <> p -> [(Term(Imp, [s; p]), ana, None, [AllowBackward])]
    | PreImp(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]
    | ConImp(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(ConImp, [p; s]), ana, None, [AllowBackward])]
    | RetImp(p, m1), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]
    | RetImp(p, m1), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(RetImp, [p; s]), ana, None, [AllowBackward])]

    | PreEqu(m1, p), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]
    | ConEqu(m1, p), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Term(PreEqu, [s; p]), res, None, [AllowBackward])]
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
    | m1, Imp(m2, p) when m1 = m2 && m1 <> p -> [(p, ind, None, [])]
    | m1, PreImp(m2, p) when m1 = m2 && m1 <> p -> [(p, ind, None, [])]
    | m1, ConImp(m2, p) when m1 = m2 && m1 <> p -> [(p, ind, None, [])]
    | m1, RetImp(m2, p) when m1 = m2 && m1 <> p -> [(p, ind, None, [])]

    | m1, Imp(p, m2) when m1 = m2 && m1 <> p -> [(p, ind, None, [])]
    | m1, PreImp(p, m2) when m1 = m2 && m1 <> p -> [(p, ind, None, [])]
    | m1, ConImp(p, m2) when m1 = m2 && m1 <> p -> [(p, ind, None, [])]
    | m1, RetImp(p, m2) when m1 = m2 && m1 <> p -> [(p, ind, None, [])]

    | m1, Equ(s, m2) when m1 = m2 && m1 <> s -> [(s, ana, None, [])]
    | m1, ConEqu(s, m2) when m1 = m2 && m1 <> s -> [(s, ana, None, [])]
    | m1, PreEqu(s, m2) when m1 = m2 && m1 <> s -> [(s, ana, None, [])]

    | m1, Equ(m2, s) when m1 = m2 && m1 <> s -> [(s, ana, None, [])]
    | m1, ConEqu(m2, s) when m1 = m2 && m1 <> s -> [(s, ana, None, [])]
    | m1, PreEqu(m2, s) when m1 = m2 && m1 <> s -> [(s, ana, None, [])]
    | _ -> []

let nal5_nal8_implication_based_decomposition6 = function
    // conjunction decompose
    | And(lst), s when isMember s lst -> [(s, ded, Some strong, [])]
    | Seq(lst), s when isMember s lst -> [(s, ded, Some strong, [])]
    | Par(lst), s when isMember s lst -> [(s, ded, Some strong, [])]
    //| And(lst), s when isMember s lst -> [(s, ded, strong, [])]   // TODO review this rule and complete
    | _ -> []

let nal5_nal8_implication_based_decomposition7 = function
    // propositional decomposition
    | s, Seq(lst) when isMember s lst -> [(reduce (Term(Seq, (listLess s lst))), pnn, None, [])]
    | s, Par(lst) when isMember s lst -> [(reduce (Term(Par, (listLess s lst))), pnn, None, [])]
    | s, And(lst) when isMember s lst -> [(reduce (Term(And, (listLess s lst))), pnn, None, [])]
    | s, Or(lst) when isMember s lst -> [(reduce (Term(Or, (listLess s lst))), pnn, None, [])]
    | _ -> []

let nal5_nal8_implication_based_decomposition8 = function
    | s, Seq(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(Seq, listLess (Term(Not, [s])) lst)), nnn, None, [])]
    | s, Par(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(Par, listLess (Term(Not, [s])) lst)), nnn, None, [])]
    | s, And(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(And, listLess (Term(Not, [s])) lst)), nnn, None, [])]
    | s, Or(lst) when isMember (Term(Not, [s])) lst -> [(reduce (Term(Or, listLess (Term(Not, [s])) lst)), nnn, None, [])]
    | _ -> []

let nal5_multi_conditional_syllogism : InferenceFunction = function
    | y, Equ(And1(x::lst) as c, b) when y <> x && x <> b && unifies y x    ->  [(Term(Equ, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, Equ(b, (And(x::lst) as c)) when y <> x && x <> b && unifies y x    -> [(Term(Equ, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, Equ(And(_::x::lst) as c, b) when y <> x && x <> b && unifies y x -> [(Term(Equ, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, Equ(b, (And(_::x::lst) as c)) when y <> x && x <> b && unifies y x -> [(Term(Equ, [substUnify c y x; substUnify b y x]), ded, None, [])]

    | y, ConEqu((Par(x::lst) as c), b) when y <> x && x <> b && unifies y x    -> [(Term(ConEqu, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, ConEqu((Par(_::x::lst) as c), b) when y <> x && x <> b && unifies y x -> [(Term(ConEqu, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, PreEqu(Par(x::lst) as c, b) when y <> x && x <> b && unifies y x    -> [(Term(PreEqu, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, PreEqu(Par(_::x::lst) as c, b) when y <> x && x <> b && unifies y x -> [(Term(PreEqu, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, PreEqu(b, (Par(x::lst) as c)) when y <> x && x <> b && unifies y x    -> [(Term(PreEqu, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, PreEqu(b, (Par(_::x::lst) as c)) when y <> x && x <> b && unifies y x -> [(Term(PreEqu, [substUnify c y x; substUnify b y x]), ded, None, [])]

    | y, Imp(And(x::lst) as c, b) when y <> x && x <> b && unifies y x    -> [(Term(Imp, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, Imp(And(_::x::lst) as c, b) when y <> x && x <> b && unifies y x -> [(Term(Imp, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, PreImp(Seq(x::lst) as c, b) when y <> x && x <> b && unifies y x    -> [(Term(PreImp, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, PreImp(Seq(_::x::lst) as c, b) when y <> x && x <> b && unifies y x -> [(Term(PreImp, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, ConImp(Par(x::lst) as c, b) when y <> x && x <> b && unifies y x    -> [(Term(ConImp, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, ConImp(Par(_::x::lst) as c, b) when y <> x && x <> b && unifies y x -> [(Term(ConImp, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, RetImp(Seq(x::lst) as c, b) when y <> x && x <> b && unifies y x    -> [(Term(RetImp, [substUnify c y x; substUnify b y x]), ded, None, [])]
    | y, RetImp(Seq(_::x::lst) as c, b) when y <> x && x <> b && unifies y x -> [(Term(RetImp, [substUnify c y x; substUnify b y x]), ded, None, [])]
    
    | Imp(And(m::lstA) as a, c1), Imp(And(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]
    | PreImp(Seq(m::lstA) as a, c1), PreImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]
    | PreImp(Seq(_::m::lstA) as a, c1), PreImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]
    | ConImp(Par(m::lstA) as a, c1), ConImp(Par(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]
    | ConImp(Par(_::m::lstA) as a, c1), ConImp(Par(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]
    | RetImp(Seq(m::lstA) as a, c1), RetImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]
    | RetImp(Seq(_::m::lstA) as a, c1), RetImp(Seq(lstB) as b, c2) when c1 = c2 && unifies a b -> [(substUnify m a b, abd, None, [])]

    | Imp(And([m; u]), c1), Imp(w, c2) when c1 = c2 && m <> w && unifies w u -> [(substUnify m w u, abd, None, [])]

    | Imp(And(lst), c), m when c <> m -> [(Term(Imp, [Term(And, m::lst); c]), ind, None, [])]

    | Imp(a, c), m when a <> c && c <> m -> [(Term(Imp, [Term(And, [m; a]); c]), ind, None, [])]

    | Equ(a, m1), Imp(And(m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, a::lst); c]), ana, None, [])]
    | Equ(a, m1), Imp(And(_::m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, a::lst); c]), ana, None, [])]
    | ConEqu(a, m1), ConImp(Par(m2::lst), c) when m1 = m2 && a <> c && m1 <> c    -> [(Term(ConImp, [Term(Par, a::lst); c]), ana, None, [])]
    | ConEqu(a, m1), ConImp(Par(_::m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, a::lst); c]), ana, None, [])]
    | PreEqu(a, m1), ConImp(Par(m2::lst), c) when m1 = m2 && a <> c && m1 <> c    -> [(Term(ConImp, [Term(Par, a::lst); c]), ana, None, [])]
    | PreEqu(a, m1), ConImp(Par(_::m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, a::lst); c]), ana, None, [])]
    | PreEqu(m1, a), ConImp(Par(m2::lst), c) when m1 = m2 && a <> c && m1 <> c    -> [(Term(ConImp, [Term(Par, a::lst); c]), ana, None, [])]
    | PreEqu(m1, a), ConImp(Par(_::m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, a::lst); c]), ana, None, [])]

    | Imp(a, m1), Imp(And(m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, a::m1::lst); c]), ded, None, [])]
    | Imp(a, m1), Imp(And(_::m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(Imp, [Term(And, a::m1::lst); c]), ded, None, [])]
    | PreImp(a, m1), PreImp(Seq(m2::lst), c) when m1 = m2 && a <> c && m1 <> c    -> [(Term(PreImp, [Term(Seq, a::m1::lst); c]), ded, None, [])]
    | PreImp(a, m1), PreImp(Seq(_::m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(PreImp, [Term(Seq, a::m1::lst); c]), ded, None, [])]
    | ConImp(a, m1), ConImp(Seq(m2::lst), c) when m1 = m2 && a <> c && m1 <> c    -> [(Term(ConImp, [Term(Par, a::m1::lst); c]), ded, None, [])]
    | ConImp(a, m1), ConImp(Seq(_::m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(ConImp, [Term(Par, a::m1::lst); c]), ded, None, [])]
    | RetImp(a, m1), RetImp(Seq(m2::lst), c) when m1 = m2 && a <> c && m1 <> c    -> [(Term(RetImp, [Term(Seq, a::m1::lst); c]), ded, None, [])]
    | RetImp(a, m1), RetImp(Seq(_::m2::lst), c) when m1 = m2 && a <> c && m1 <> c -> [(Term(RetImp, [Term(Seq, a::m1::lst); c]), ded, None, [])]
    | _ -> []

//let nal6_variable_introduction = function
//    | Inh(s, m1), Inh(p, m2) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Inh, [p; Var(IVar, "X")]); Term(Inh, [s; Var(IVar, "X")])]), abd, None, [BeliefOnly])
//                                                        (Term(Imp, [Term(Inh, [s; Var(IVar, "X")]); Term(Inh, [p; Var(IVar, "X")])]), ind, None, [BeliefOnly])
//                                                        (Term(Equ, [Term(Inh, [p; Var(IVar, "X")]); Term(Inh, [s; Var(IVar, "X")])]), com, None, [BeliefOnly])
//                                                        (Term(And, [Term(Inh, [p; Var(DVar, "Y")]); Term(Inh, [s; Var(DVar, "Y")])]), int, None, [BeliefOnly])
                                                        
//                                                        (Term(ConImp, [Term(Inh, [p; Var(IVar, "X")]); Term(Inh, [s; Var(IVar, "X")])]), abd, None, [BeliefOnly])
//                                                        (Term(ConImp, [Term(Inh, [s; Var(IVar, "X")]); Term(Inh, [p; Var(IVar, "X")])]), ind, None, [BeliefOnly])
//                                                        (Term(ConImp, [Term(Inh, [p; Var(IVar, "X")]); Term(Inh, [s; Var(IVar, "X")])]), ind, None, [BeliefOnly])
//                                                        (Term(ConEqu, [Term(Inh, [p; Var(IVar, "X")]); Term(Inh, [s; Var(IVar, "X")])]), com, None, [BeliefOnly])
//                                                        (Term(Par, [Term(Inh, [p; Var(DVar, "Y")]); Term(Inh, [s; Var(DVar, "Y")])]), int, None, [BeliefOnly])]

//                                                        // interval variants ommitted here as not required

//    | Inh(m1, s), Inh(m2, p) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Inh, [Var(IVar, "X"); s]); Term(Inh, [Var(IVar, "X"); p])]), ind, None, [BeliefOnly])
//                                                        (Term(Imp, [Term(Inh, [Var(IVar, "X"); p]); Term(Inh, [Var(IVar, "X"); s])]), abd, None, [BeliefOnly])
//                                                        (Term(Equ, [Term(Inh, [Var(IVar, "X"); s]); Term(Inh, [Var(IVar, "X"); p])]), com, None, [BeliefOnly])
//                                                        (Term(And, [Term(Inh, [Var(DVar, "Y"); s]); Term(Inh, [Var(DVar, "Y"); p])]), int, None, [BeliefOnly])
                                                        
//                                                        (Term(ConImp, [Term(Inh, [Var(IVar, "X"); s]); Term(Inh, [Var(IVar, "X"); p])]), ind, None, [BeliefOnly])
//                                                        (Term(ConImp, [Term(Inh, [Var(IVar, "X"); p]); Term(Inh, [Var(IVar, "X"); s])]), abd, None, [BeliefOnly])
//                                                        (Term(ConImp, [Term(Inh, [Var(IVar, "X"); s]); Term(Inh, [Var(IVar, "X"); p])]), ind, None, [BeliefOnly])
//                                                        (Term(ConEqu, [Term(Inh, [Var(IVar, "X"); s]); Term(Inh, [Var(IVar, "X"); p])]), com, None, [BeliefOnly])
//                                                        (Term(Par, [Term(Inh, [Var(DVar, "Y"); s]); Term(Inh, [Var(DVar, "Y"); p])]), int, None, [BeliefOnly])]
//    | _ -> []

let nal6_variable_syllogisms = function
    | Imp(Inh(a, r1), z1), Imp(And([Inh(Var(DVar, "Y"), b); Inh(Var(DVar, "Y"), r2)]), z2) when r1 = r2 && z1 = z2 && a <> b -> [Term(Inh, [a; b]), abd, None, []]  
    | Imp(Inh(a, r1), z1), Imp(And([Inh(Var(DVar, "Y"), r2); Inh(Var(DVar, "Y"), b)]), z2) when r1 = r2 && z1 = z2 && a <> b -> [Term(Inh, [a; b]), abd, None, []]  

    | Inh(u, l1), Imp(And([Inh(Var(DVar, "Y"), l2); Inh(Var(DVar, "Y"), r)]), z) when l1 = l2 && u <> z && z <> l1 -> [(Term(Imp, [Term(Inh, [u; r]); z]), ded, None, [])]
    | Inh(u, l1), Imp(And([Inh(Var(DVar, "Y"), r); Inh(Var(DVar, "Y"), l2)]), z) when l1 = l2 && u <> z && z <> l1 -> [(Term(Imp, [Term(Inh, [u; r]); z]), ded, None, [])]

    | _ -> []

let nal6_multiple_variable_introduction = function
    | Imp(a, Inh(m1, p)), Inh(m2, s) when m1 = m2 && s <> p && a <> Term(Inh, [m1; s]) -> [(Term(Imp, [Term(And, [a; Term(Inh, [Var(IVar, "X"); s])]); Term(Inh, [Var(IVar, "X"); p])]), ind, None, [])
                                                                                           (Term(And, [Term(Imp, [a; Term(Inh, [Var(DVar, "Y"); p])]); Term(Inh, [Var(DVar, "Y"); s])]), int, None, [])]
    | And(Inh(m1, p)::lst), Inh(m2, s) when m1 = m2 && s <> p -> [(Term(Imp, [Term(Inh, [Var(IVar, "Y"); s]); Term(And, Term(Inh, [Var(IVar, "Y"); p])::lst)]), ind, None, [])
                                                                  (Term(And, Term(Inh, [Var(DVar, "Y"); s])::Term(Inh, [Var(DVar, "Y"); p])::lst), ind, None, [])]
    | _ -> []

let nal6_variable_elimination = function
    | b, And([a; c]) when unifies b a -> [(substUnify c b a, anon_ana, Some strong, [])]
    | b, And([c; a]) when unifies b a -> [(substUnify c b a, anon_ana, Some strong, [])]

    | And([c; a]), b when unifies b a -> [(substUnify c b a, ded, Some strong, [])]
    | And([a; c]), b when unifies b a -> [(substUnify c b a, ded, Some strong, [])]

    | b, Imp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [])]
    | b, PreImp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [])]
    | b, ConImp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [])]
    | b, RetImp(a, c) when unifies b a -> [(substUnify c b a, ded, Some ind, [])]

    | b, Imp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [])]
    | b, PreImp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [])]
    | b, ConImp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [])]
    | b, RetImp(c, a) when unifies b a -> [(substUnify c b a, abd, Some ded, [])]

    | b, Equ(a, c) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]
    | b, ConEqu(a, c) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]
    | b, PreEqu(a, c) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]
    
    | b, Equ(c, a) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]
    | b, ConEqu(c, a) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]
    | b, PreEqu(c, a) when unifies b a -> [(substUnify c b a, ana, Some ded, [])]

    | _ -> []

let nal7_temporal_conjunction = function
    | Par([c1; a]), Par([c2; b]) when c1 = c2 && a <> b -> [(Term(Par, [c1; Term(Par, [a; b])]), temporal_int, None, [IsConcurrent])
                                                            (Term(Par, [c1; Term(Seq, [a; b])]), temporal_int, None, [IsBefore])
                                                            (Term(Par, [c1; Term(Seq, [b; a])]), temporal_int, None, [IsAfter])]
    | Par([a; c1]), Par([c2; b]) when c1 = c2 && a <> b -> [(Term(Par, [c1; Term(Par, [a; b])]), temporal_int, None, [IsConcurrent])
                                                            (Term(Par, [c1; Term(Seq, [a; b])]), temporal_int, None, [IsBefore])
                                                            (Term(Par, [c1; Term(Seq, [b; a])]), temporal_int, None, [IsAfter])]

    | Seq([c1; a]), Par([c2; b]) when c1 = c2 && a <> b -> [(Term(Seq, [c1; Term(Par, [a; b])]), temporal_int, None, [IsConcurrent])
                                                            (Term(Seq, [c1; Term(Seq, [a; b])]), temporal_int, None, [IsBefore])
                                                            (Term(Seq, [c1; Term(Seq, [b; a])]), temporal_int, None, [IsAfter])]
    | Seq([a; c1]), Par([c2; b]) when c1 = c2 && a <> b -> [(Term(Seq, [c1; Term(Par, [a; b])]), temporal_int, None, [IsConcurrent])
                                                            (Term(Seq, [c1; Term(Seq, [a; b])]), temporal_int, None, [IsBefore])
                                                            (Term(Seq, [c1; Term(Seq, [b; a])]), temporal_int, None, [IsAfter])]

    // TODO Missing case goes here Par(), Seq()
    | _ -> []

let nal7_temporal_inference  = function
    | p, s when isNotImpOrEqu p && isNotImpOrEqu s  -> [(Term(ConImp, [s; p]), temporal_ind, None, [IsConcurrent])
                                                        (Term(ConImp, [p; s]), temporal_abd, None, [IsConcurrent])
                                                        (Term(ConEqu, [s; p]), temporal_com, None, [IsConcurrent])
                                                        (Term(Par,    [s; p]), temporal_int, None, [IsConcurrent])
                                                        (Term(PreImp, [s; p]), temporal_ind, None, [IsBefore])
                                                        (Term(RetImp, [p; s]), temporal_abd, None, [IsBefore])
                                                        (Term(PreEqu, [s; p]), temporal_com, None, [IsBefore])
                                                        (Term(Seq,    [s; p]), temporal_int, None, [IsBefore])
                                                        (Term(PreImp, [p; s]), temporal_ind, None, [IsAfter])
                                                        (Term(RetImp, [s; p]), temporal_abd, None, [IsAfter])
                                                        (Term(PreEqu, [p; s]), temporal_com, None, [IsAfter])
                                                        (Term(Seq,    [p; s]), temporal_int, None, [IsAfter])]
    | _ -> []
