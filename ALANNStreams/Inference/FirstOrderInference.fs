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

module FirstOrderInference

open Types
open TruthFunctions
open TermUtils
open ActivePatterns
open InferenceUtils
    
let firstOrderSyllogisitic = function
    | Inh(a, b1), Inh(b2, c) when b1 = b2 && a <> c -> [(Term(Inh, [a; c]), ded, Some strong, [AllowBackward; Swap])
                                                        (Term(Inh, [c; a]), exe, Some weak, [AllowBackward; Swap])]
    | Inh(a1, b), Inh(a2, c) when a1 = a2 && b <> c -> [(Term(Inh, [c; b]), abd, Some weak, [AllowBackward; Swap])]
    | Inh(a, c1), Inh(b, c2) when c1 = c2 && a <> b -> [(Term(Inh, [b; a]), ind, Some weak, [AllowBackward; Swap])]
    | _ -> []    

let similaritySyllogisitic = function
    | Inh(p, m1), Inh(s, m2) when m1 = m2 && s <> p -> [(Term(Sim, [s; p]), com, Some weak, [AllowBackward; Swap])]
    | Inh(m1, p), Inh(m2, s) when m1 = m2 && s <> p -> [(Term(Sim, [s; p]), com, Some weak, [AllowBackward; Swap])]

    | Inh(m1, p), Sim(s, m2) when m1 = m2 && s <> p -> [(Term(Inh, [s; p]), ana, Some strong, [AllowBackward; Swap])]
    | Inh(m1, p), Sim(m2, s) when m1 = m2 && s <> p -> [(Term(Inh, [s; p]), ana, Some strong, [AllowBackward; Swap])]

    | Inh(p, m1), Sim(s, m2) when m1 = m2 && s <> p -> [(Term(Inh, [p; s]), ana, Some strong, [AllowBackward; Swap])]
    | Inh(p, m1), Sim(m2, s) when m1 = m2 && s <> p -> [(Term(Inh, [p; s]), ana, Some strong, [AllowBackward; Swap])]

    | Sim(m1, p), Sim(s, m2) when m1 = m2 && s <> p -> [(Term(Sim, [s; p]), res, Some strong, [AllowBackward; Swap])]
    | Sim(m1, p), Sim(m2, s) when m1 = m2 && s <> p -> [(Term(Sim, [s; p]), res, Some strong, [AllowBackward; Swap])]
    | Sim(p, m1), Sim(s, m2) when m1 = m2 && s <> p -> [(Term(Sim, [s; p]), res, Some strong, [AllowBackward; Swap])]
    | Sim(p, m1), Sim(m2, s) when m1 = m2 && s <> p -> [(Term(Sim, [s; p]), res, Some strong, [AllowBackward; Swap])]

    | _ -> []

let similarityFromInheritance = function
    | Inh(s1, p1), Inh(p2, s2) when p1 = p2 && s1 = s2 && p1 <> s1 -> [(Term(Sim, [s1; p1]), int, None, [AllowBackward])]
    | _ -> []

let setIntersectionComprehension = function
    | Inh(c1, ExtSet(a)), Inh(c2, ExtSet(b)) when c1 = c2 && a <> b && intersection a b <> [] -> [(Term(Inh, [c1; Term(ExtSet, intersection a b)]), int, None, [])]
    | Inh(c1, IntSet(a)), Inh(c2, IntSet(b)) when c1 = c2 && a <> b && intersection a b <> [] -> [(Term(Inh, [c1; Term(IntSet, intersection a b)]), uni, None, [])] 
    | Inh(ExtSet(a), c1), Inh(ExtSet(b), c2) when c1 = c2 && a <> b && intersection a b <> [] -> [(Term(Inh, [Term(ExtSet, intersection a b); c1]), uni, None, [])]
    | Inh(IntSet(a), c1), Inh(IntSet(b), c2) when c1 = c2 && a <> b && intersection a b <> [] -> [(Term(Inh, [Term(IntSet, intersection a b); c1]), int, None, [])]
    | _ -> []

let setUnionComprehension = function
    | Inh(c1, ExtSet(a)), Inh(c2, ExtSet(b)) when c1 = c2 && a <> b -> [(Term(Inh, [c1; Term(ExtSet, union a b)]), uni, None, [])]
    | Inh(c1, IntSet(a)), Inh(c2, IntSet(b)) when c1 = c2 && a <> b -> [(Term(Inh, [c1; Term(IntSet, union a b)]), int, None, [])]
    | Inh(ExtSet(a), c1), Inh(ExtSet(b), c2) when c1 = c2 && a <> b -> [(Term(Inh, [Term(ExtSet, union a b); c1]), int, None, [])]
    | Inh(IntSet(a), c1), Inh(IntSet(b), c2) when c1 = c2 && a <> b -> [(Term(Inh, [Term(IntSet, union a b); c1]), uni, None, [])]
    | _ -> []

let setDifferenceComprehension = function
    | Inh(c1, ExtSet(a)), Inh(c2, ExtSet(b)) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [c1; Term(ExtSet, difference a b)]), dif, None, [Swap])]
    | Inh(c1, IntSet(a)), Inh(c2, IntSet(b)) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [c1; Term(IntSet, difference a b)]), dif, None, [Swap])]
    | Inh(ExtSet(a), c1), Inh(ExtSet(b), c2) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [Term(ExtSet, difference a b); c1]), dif, None, [Swap])]
    | Inh(IntSet(a), c1), Inh(IntSet(b), c2) when c1 = c2 && a <> b && difference a b <> [] -> [(Term(Inh, [Term(IntSet, difference a b); c1]), dif, None, [Swap])]
    | _ -> []

let setDecomposition = function
    | Inh(s, m1), Inh(IntInt(lst), m2) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [reduce(Term(IntInt, listLess s lst)); m1]), pnn, None, [])]
    | Inh(s, m1), Inh(ExtInt(lst), m2) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [reduce(Term(ExtInt, listLess s lst)); m1]), npp, None, [])]
    | Inh(s1, m1), Inh(IntDif(s2, p), m2) when m1 = m2 && s1 = s2 && p <> m1 -> [(Term(Inh, [p; m1]), pnp, None, [])]
    | Inh(s1, m1), Inh(IntDif(p, s2), m2) when m1 = m2 && s1 = s2 && p <> m1 -> [(Term(Inh, [p; m1]), nnn, None, [])]
    | Inh(m1, s), Inh(m2, ExtInt(lst)) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [m1; reduce(Term(ExtInt, listLess s lst))]), pnn, None, [])]
    | Inh(m1, s), Inh(m2, IntInt(lst)) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [m1; reduce(Term(IntInt, listLess s lst))]), npp, None, [])]
    | Inh(m1, s1), Inh(m2, ExtDif(s2, p)) when m1 = m2 && s1 = s2 && p <> m1 -> [(Term(Inh, [m1; p]), pnp, None, [])]
    | Inh(m1, s1), Inh(m2, ExtDif(p, s2)) when m1 = m2 && s1 = s2 && p <> m1 -> [(Term(Inh, [m1; p]), nnn, None, [])]
    | _ -> []

let InheritanceSetComprehension = function
    | Inh(p, m1), Inh(s, m2) when m1 = m2 && p <> s && notSet s && notSet p && noCommonSubterm s p -> [(Term(Inh, [Term(IntInt, sort <| [s; p]); m1]), int, None, [])
                                                                                                       (Term(Inh, [Term(ExtInt, sort <| [s; p]); m1]), uni, None, [])
                                                                                                       (Term(Inh, [Term(IntDif, [p; s]); m1]), dif, None, [])]
    | Inh(m1, p), Inh(m2, s) when m1 = m2 && s <> p && notSet s && notSet p && noCommonSubterm s p -> [(Term(Inh, [m1; Term(ExtInt, sort <| [p; s])]), int, None, [])
                                                                                                       (Term(Inh, [m1; Term(IntInt, sort <| [p; s])]), uni, None, [])
                                                                                                       (Term(Inh, [m1; Term(ExtDif, [p; s])]), dif, None, [])]
    | _ -> []

let (Nal1_3_EquivalenceAndImplication : InferenceFunction) = function
    // Similarity to inheritance
    | Inh(s1, p1), Sim(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(Inh, [s1; p1]), structuralInt, None, [BeliefFromQuestion])]
    | Inh(s1, p1), Sim(p2, s2) when s1 = s2 && p1 = p2 -> [(Term(Inh, [s1; p1]), structuralInt, None, [BeliefFromQuestion])]
    // Inheritance to similarity
    | Sim(s1, p1), Inh(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(Sim, [s1; p1]), structuralAbd, None, [BeliefFromQuestion])]
    | Sim(p1, s1), Inh(s2, p2) when s1 = s2 && p1 = p2 -> [(Term(Sim, [p1; s1]), structuralAbd, None, [BeliefFromQuestion])]
    // Set Definition Similarity to Inheritance
    | Sim(s1, ExtSet([p])), s2 when s1 = s2 && s1 <> p -> [(Term(Inh, [s1; Term(ExtSet, [p])]), identity, Some identity, [AllowBackward])]
    | Sim(s, ExtSet([p1])), ExtSet([p2]) when p1 = p2 && s <> p1 -> [(Term(Inh, [s; Term(ExtSet, [p1])]), identity, Some identity, [AllowBackward])]
    | Sim(IntSet([s1]), p), IntSet([s2]) when s1 = s2 && s1 <> p -> [(Term(Inh, [Term(IntSet, [s1]); p]), identity, Some identity, [AllowBackward])]
    | Sim(IntSet([s]), p1), p2 when p1 = p2 && s <> p1 -> [(Term(Inh, [Term(IntSet, [s]); p1]), identity, Some identity, [])]

    | Sim(ExtSet([s1]), ExtSet([p])), ExtSet([s2]) when s1 = s2 && s1 <> p -> [(Term(Inh, [Term(ExtSet, [p]); Term(ExtSet, [s1])]), identity, Some identity, [AllowBackward])]
    | Sim(ExtSet([s]), ExtSet([p1])), ExtSet([p2]) when p1 = p2 && s <> p1 -> [(Term(Inh, [Term(ExtSet, [p1]); Term(ExtSet, [s])]), identity, Some identity, [AllowBackward])]
    | Sim(IntSet([s1]), IntSet([p])), IntSet([s2]) when s1 = s2 && s1 <> p -> [(Term(Inh, [Term(IntSet, [p]); Term(IntSet, [s1])]), identity, Some identity, [AllowBackward])]
    | Sim(IntSet([s]), IntSet([p1])), IntSet([p2]) when p1 = p2 && s <> p1 -> [(Term(Inh, [Term(IntSet, [p1]); Term(IntSet, [s])]), identity, Some identity, [AllowBackward])]
    // Set Definition Unwrap
    | Sim(ExtSet([s1]), ExtSet([p])), ExtSet([s2]) when s1 = s2 && s1 <> p -> [(Term(Inh, [Term(ExtSet, [s1]); Term(ExtSet, [p])]), identity, Some identity, [AllowBackward])]
    | Sim(ExtSet([s]), ExtSet([p1])), ExtSet([p2]) when p1 = p2 && s <> p1 -> [(Term(Inh, [Term(ExtSet, [s]); Term(ExtSet, [p1])]), identity, Some identity, [AllowBackward])]
    | Sim(IntSet([s1]), IntSet([p])), IntSet([s2]) when s1 = s2 && s1 <> p -> [(Term(Inh, [Term(IntSet, [s1]); Term(IntSet, [p])]), identity, Some identity, [AllowBackward])]
    | Sim(IntSet([s]), IntSet([p1])), IntSet([p2]) when p1 = p2 && s <> p1 -> [(Term(Inh, [Term(IntSet, [s]); Term(IntSet, [p1])]), identity, Some identity, [AllowBackward])]
    // Nothing is more specific than a instance so it's similar
    | Inh(s1, ExtSet([p])), s2 when s1 = s2 && s1 <> p -> [(Term(Sim, [s1; Term(ExtSet, [p])]), identity, Some identity, [AllowBackward])]
    | Inh(s, ExtSet([p1])), ExtSet([p2]) when p1 = p2 && s <> p1 -> [(Term(Sim, [s; Term(ExtSet, [p1])]), identity, Some identity, [AllowBackward])]
    // nothing is more general than a property so it's similar
    | Inh(IntSet([s1]), p), IntSet([s2]) when s1 = s2 && s1 <> p -> [(Term(Sim, [Term(IntSet, [s1]); p]), identity, Some identity, [])]
    | Inh(IntSet([s]), p1), p2 when p1 = p2 && s <> p1 -> [(Term(Sim, [Term(IntSet, [s]); p1]), identity, Some identity, [])]
    | _ -> []

let Nal1_4_conversion_contrapostion_negation : InferenceFunction = function
    // conversion
    | Inh(p1, s1), Inh(s2, p2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(Term(Inh, [p1; s1]), cnv, None, [BeliefFromQuestion])]

    | Inh(s1, p1), _ when s1 <> p1 -> [(Term(Inh, [p1; s1]), cnv, None, [Structural])]

    // negation
    | Inh(a1, b), a2 when a1 = a2 && a1 <> b -> [(Term(Not, [Term(Inh, [a1; b])]), neg, Some d_neg, [Structural; AllowBackward])]
    | Inh(a, b1), b2 when b1 = b2 && a <> b1 -> [(Term(Not, [Term(Inh, [a; b1])]), neg, Some d_neg, [Structural; AllowBackward])]
    
    | Not(Inh(a1, b)), a2 when a1 = a2 && a1 <> b -> [(Term(Inh, [a1; b]), neg, Some d_neg, [Structural; AllowBackward])]
    | Not(Inh(a, b1)), b2 when b1 = b2 && a <> b1 -> [(Term(Inh, [a; b1]), neg, Some d_neg, [Structural; AllowBackward])]

    | Sim(a1, b), a2 when a1 = a2 && a1 <> b -> [(Term(Not, [Term(Sim, [a1; b])]), neg, Some d_neg, [Structural; AllowBackward])]
    | Sim(a, b1), b2 when b1 = b2 && a <> b1 -> [(Term(Not, [Term(Sim, [a; b1])]), neg, Some d_neg, [Structural; AllowBackward])]
    
    | Not(Sim(a1, b)), a2 when a1 = a2 && a1 <> b -> [(Term(Sim, [a1; b]), neg, Some d_neg, [Structural; AllowBackward])]
    | Not(Sim(a, b1)), b2 when b1 = b2 && a <> b1 -> [(Term(Sim, [a; b1]), neg, Some d_neg, [Structural; AllowBackward])]

    | _ -> []

let (setDefinitionUnwrap : InferenceFunction) = function
    | Sim(ExtSet([s1]), ExtSet([p])), ExtSet([s2]) when s1 = s2 && s1 <> p -> [(Term(Sim, [s1; p]), identity, Some d_id, [AllowBackward])]
    | Sim(ExtSet([s]), ExtSet([p1])), ExtSet([p2]) when p1 = p2 && s <> p1 -> [(Term(Sim, [s; p1]), identity, Some d_id, [AllowBackward])]
    | Sim(IntSet([s1]), IntSet([p])), IntSet([s2]) when s1 = s2 && s1 <> p -> [(Term(Sim, [s1; p]), identity, Some d_id, [AllowBackward])]
    | Sim(IntSet([s]), IntSet([p1])), IntSet([p2]) when p1 = p2 && s <> p1 -> [(Term(Sim, [s; p1]), identity, Some d_id, [AllowBackward])]
    | Inh(s1, ExtSet([p])), s2 when s1 = s2 && s1 <> p -> [(Term(Sim, [s1; Term(ExtSet, [p])]), identity, Some d_id, [AllowBackward])]
    | Inh(s, ExtSet([p1])), ExtSet([p2]) when p1 = p2 && s <> p1 -> [(Term(Sim, [s; Term(ExtSet, [p1])]), identity, Some d_id, [AllowBackward])]
    | Inh(IntSet([s1]), p), IntSet([s2]) when s1 = s2 && s1 <> p -> [(Term(Sim, [Term(IntSet, [s1]); p]), identity, Some d_id, [AllowBackward])]
    | Inh(IntSet([s]), p1), p2 when p1 = p2 && s <> p1 -> [(Term(Sim, [Term(IntSet, [s]); p1]), identity, Some d_id, [AllowBackward])]
    | _ -> []

let structuralInference : InferenceFunction = function
    // NAL 3 single premise inference
    | Inh(IntInt([a; b]), m1), m2 when m1 = m2 -> [(Term(Inh, [a; m1]), beliefStructuralDed, None, [Structural])
                                                   (Term(Inh, [b; m1]), beliefStructuralDed, None, [Structural])]
    | Inh(m1, ExtInt([a; b])), m2 when m1 = m2 -> [(Term(Inh, [m1; a]), beliefStructuralDed, None, [Structural])
                                                   (Term(Inh, [m1; b]), beliefStructuralDed, None, [Structural])]

    | Inh(IntDif(a, b), m1), m2 when m1 = m2 -> [(Term(Inh, [a; m1]), beliefStructuralDed, None, [Structural])]
    | Inh(m1, ExtDif(a, b)), m2 when m1 = m2 -> [(Term(Inh, [m1; a]), beliefStructuralDed, None, [Structural])]

    | Inh(Prod(a, b), m), _ when a <> Word "_" && b <> Word "_" -> [(Term(Inh, [a; Term(ExtImg, [m; Word "_"; b])]), identity, None, [Structural])   
                                                                    (Term(Inh, [b; Term(ExtImg, [m; a; Word "_"])]), identity, None, [Structural])]
    | Inh(m, Prod(a, b)), _ when a <> Word "_" && b <> Word "_" -> [(Term(Inh, [Term(IntImg, [m; Word "_"; b]); a]), identity, None, [Structural])   
                                                                    (Term(Inh, [Term(IntImg, [m; a; Word "_"]); b]), identity, None, [Structural])]
    | Inh(ai, ExtImg(m, a, Word "_")), _ when a <> Word "_" -> [(Term(Inh, [Term(Prod, [a; ai]); m]), identity, None, [Structural])
                                                                (Term(Inh, [a; Term(ExtImg, [m; Word "_"; ai])]), identity, None, [Structural])   ]
    | Inh(ai, ExtImg(m, Word "_", b)), _ when b <> Word "_" -> [(Term(Inh, [Term(Prod, [ai; b]); m]), identity, None, [Structural])
                                                                (Term(Inh, [b; Term(ExtImg, [m; ai; Word "_"])]), identity, None, [Structural])   ]
    | Inh(IntImg(m, a, Word "_"), ai), _ -> [(Term(Inh, [m; Term(Prod, [a; ai])]), identity, None, [Structural])
                                             (Term(Inh, [Term(IntImg, [m; Word "_"; ai]); a]), identity, None, [Structural])]
    | Inh(IntImg(m, Word "_", b), ai), _ -> [(Term(Inh, [m; Term(Prod, [ai; b])]), identity, None, [Structural])
                                             (Term(Inh, [Term(IntImg, [m; ai; Word "_"]); b]), identity, None, [Structural])]     
    | _ -> []

let structuralInference2 = function
    | Inh(a1, c), Inh(a2, d) when a1 = a2 && c <> d && a1 <> c -> [(Term(Inh, [Term(Prod, [a1; a2]); Term(Prod, [c; d])]), int, None, [])]
    | Inh(a, c1), Inh(b, c2) when c1 = c2 && a <> b && c1 <> a -> [(Term(Inh, [Term(Prod, [a; b]); Term(Prod, [c1; c2])]), int, None, [])]
    | Inh(ExtSet([a1]), c), Inh(a2, d) when a1 = a2 && c <> d && a1 <> c  -> [(Term(Inh, [Term(Prod, [Term(ExtSet, [a1]); a2]); Term(Prod, [c; d])]), int, None, [])]
    | Inh(a1, c), Inh(ExtSet([a2]), d) when a1 = a2 && c <> d && a1 <> c  -> [(Term(Inh, [Term(Prod, [a1; Term(ExtSet, [a2])]); Term(Prod, [c; d])]), int, None, [])]
    | Inh(a, IntSet([c1])), Inh(b, c2) when c1 = c2 && a <> b && a <> c1  -> [(Term(Inh, [Term(Prod, [a; b]); Term(Prod, [Term(IntSet, [c1]); c2])]), int, None, [])]
    | Inh(a, c1), Inh(b, IntSet([c2])) when c1 = c2 && a <> b && a <> c1  -> [(Term(Inh, [Term(Prod, [a; b]); Term(Prod, [c1; Term(IntSet, [c2])])]), int, None, [])]
    | _ -> []

//let RFTRules = function
//    // mutual entailment
//    //| Inh(Prod(a, b), r), _ -> [(Term(Inh, [Term(Prod, [b; a]); Term(Prod, [b; Term(ExtImg, [r; Word "_"; b])])]), identity, None, [Structural])]
//    // Combinatorial mutual entailment
//    | Inh(Prod(a, m1), rx), Inh(Prod(m2, b), ry) when m1 = m2 ->  [(Term(Inh, [Term(Prod, [a; b]); Term(Prod, [rx; ry])]), identity, None, [Structural; AllowBackward])
//                                                                   (Term(Inh, [Term(Prod, [b; a]); Term(Prod, [b; Term(ExtImg, [Term(Prod, [rx; ry]); Word "_"; b])])]), identity, None, [Structural; AllowBackward])]
//    | Inh(Prod(m1, a), rx), Inh(Prod(m2, b), ry) when m1 = m2 ->  [(Term(Inh, [Term(Prod, [a; b]); Term(Prod, [rx; ry])]), identity, None, [Structural; AllowBackward])
//                                                                   (Term(Inh, [Term(Prod, [b; a]); Term(Prod, [b; Term(ExtImg, [Term(Prod, [rx; ry]); Word "_"; b])])]), identity, None, [Structural; AllowBackward])]
//    | Inh(Prod(a, m1), rx), Inh(Prod(b, m2), ry) when m1 = m2 ->  [(Term(Inh, [Term(Prod, [a; b]); Term(Prod, [rx; ry])]), identity, None, [Structural; AllowBackward])
//                                                                   (Term(Inh, [Term(Prod, [b; a]); Term(Prod, [b; Term(ExtImg, [Term(Prod, [rx; ry]); Word "_"; b])])]), identity, None, [Structural; AllowBackward])]

//    | _ -> []


let backwardDrivenForwardInference = function
    // Composition on both sides of a statement
    | Inh(ExtInt(set1), ExtInt(set2)), Inh(a, b) when isMember a set1 && isMember b set2 -> [(Term(Inh, [Term(ExtInt, set1); Term(ExtInt, set2)]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(IntInt(set1), IntInt(set2)), Inh(a, b) when isMember a set1 && isMember b set2 -> [(Term(Inh, [Term(IntInt, set1); Term(IntInt, set2)]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(ExtDif(s1, a1), ExtDif(s2, b1)), Inh(b2, a2) when s1 = s2 && a1 = a2 -> [(Term(Inh, [Term(ExtDif, [s1; a1]); Term(ExtDif, [s2; b1])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(IntDif(s1, a1), IntDif(s2, b1)), Inh(b2, a2) when s1 = s2 && a1 = a2 -> [(Term(Inh, [Term(IntDif, [s1; a1]); Term(IntDif, [s2; b1])]), beliefStructuralDed, None, [BeliefFromQuestion])]

    //composition on one side of a statement
    | Inh(w1, IntInt(set1)), Inh(w2, b) when w1 = w2 && isMember b set1 -> [(Term(Inh, [w1; Term(IntInt, set1)]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(ExtInt(set1), w1), Inh(b, w2) when w1 = w2 && isMember b set1 -> [(Term(Inh, [Term(ExtInt, set1); w1]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(w1, ExtDif(s, b1)), Inh(w2, b2) when w1 = w2 && b1 = b2 -> [(Term(Inh, [w1; Term(ExtDif, [s; b1])]), beliefStructuralDif, None, [BeliefFromQuestion])]
    | Inh(IntDif(s, b1), w1), Inh(b2, w2) when w1 = w2 && b1 = b2 -> [(Term(Inh, [Term(IntDif, [s; b1]); w1]), beliefStructuralDif, None, [BeliefFromQuestion])]


    | Inh(Prod(b1, p), z), Inh(b2, a) when b1 = b2 && a <> z && not(isVar p) -> [(Term(Inh, [Term(Prod, [b1; p]); Term(Prod, [a; p])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(Prod(p, b1), z), Inh(b2, a) when b1 = b2 && a <> z && not(isVar p) -> [(Term(Inh, [Term(Prod, [p; b1]); Term(Prod, [p; a])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Sim(Prod(b1, p), z), Sim(b2, a) when b1 = b2 && a <> z && not(isVar p) -> [(Term(Sim, [Term(Prod, [b1; p]); Term(Prod, [a; p])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Sim(Prod(p, b1), z), Sim(b2, a) when b1 = b2 && a <> z && not(isVar p) -> [(Term(Sim, [Term(Prod, [p; b1]); Term(Prod, [p; a])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(IntImg(n1, a, Word "_"), z), Inh(n2, r) when n1 = n2 && z <> r && not(isVar a) -> [(Term(Inh, [Term(IntImg, [n1; a; Word "_"]); Term(IntImg, [r; a; Word "_"])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(ExtImg(n, Word "_", b1), z), Inh(s, b2) when b1 = b2 && z <> s && not(isVar n) -> [(Term(Inh, [Term(ExtImg, [n; Word "_"; b1]); Term(ExtImg, [n; Word "_"; s])]), beliefStructuralDed, None, [BeliefFromQuestion])]
    
    // Backward driven forward set decomposition
    | Inh(s, m1), Inh(IntInt(lst), m2) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [reduce(Term(IntInt, listLess s lst)); m1]), beliefStructuralDed, None, [BeliefFromQuestion])
                                                                                                  (Term(Inh, [s; m1]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(s, m1), Inh(ExtInt(lst), m2) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [reduce(Term(ExtInt, listLess s lst)); m1]), beliefStructuralDed, None, [BeliefFromQuestion])
                                                                                                  (Term(Inh, [s; m1]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(s1, m1), Inh(IntDif(s2, p), m2) when m1 = m2 && s1 = s2 && p <> m1 -> [(Term(Inh, [p; m1]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(s1, m1), Inh(IntDif(p, s2), m2) when m1 = m2 && s1 = s2 && p <> m1 -> [(Term(Inh, [p; m1]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(m1, s), Inh(m2, ExtInt(lst)) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [m1; reduce(Term(ExtInt, listLess s lst))]), beliefStructuralDed, None, [BeliefFromQuestion])
                                                                                                  (Term(Inh, [m1; s]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(m1, s), Inh(m2, IntInt(lst)) when m1 = m2 && isMember s lst && listLess s lst <> [] -> [(Term(Inh, [m1; reduce(Term(IntInt, listLess s lst))]), beliefStructuralDed, None, [BeliefFromQuestion])
                                                                                                  (Term(Inh, [m1; s]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(m1, s1), Inh(m2, ExtDif(s2, p)) when m1 = m2 && s1 = s2 && p <> m1 -> [(Term(Inh, [m1; p]), beliefStructuralDed, None, [BeliefFromQuestion])]
    | Inh(m1, s1), Inh(m2, ExtDif(p, s2)) when m1 = m2 && s1 = s2 && p <> m1 -> [(Term(Inh, [m1; p]), beliefStructuralDed, None, [BeliefFromQuestion])]

    // compound composition one premise
    | Or(lst), s when isMember s lst -> [(Term(Or, lst), ded, None, [BeliefFromQuestion])]

    | _ -> []
    
let backwardOnlyInference = function
    | Inh(a, s1), Inh(b, s2) when s1 = s2 && a <> b -> [(Term(Inh, [a; b]), identity, None, [QuestionOnly])
                                                        (Term(Inh, [b; a]), identity, None, [QuestionOnly])
                                                        (Term(Sim, [a; b]), identity, None, [QuestionOnly])]
    | _ -> []



