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

module InferenceTests

//open Types
//open Parser
//open InferenceUtils
//open FirstOrderInference
//open HigherOrderInference
//open Expecto
//open TermFormatters
//open Factories
//open TruthFunctions
//open System.Threading

//let parseTerm str = 
//    match testp pterm str with
//    | Some x -> x
//    | None -> failwith "Parsing Term error"

//let parseEvent str = 
//    match testp pevent str with
//    | Some x -> x
//    | None -> failwith "Parsing Term error"

//let makeTestEventBelief e b =
//    {Attention = 1.0f; Depth = SearchDepth.Deep; Answer = false; Event = e; Belief = makeBeliefFromEvent b}

//let testInfFunc (f : InferenceFunction) (e1 : Event) (e2 : Event) =
//    let matcher = function
//        | {Event.Term = t1; TV = Some tv1} -> (ft t1, Some tv1)
//        | {Event.Term = t1; TV = None} -> (ft t1, None)

//    List.map matcher (inf (f, NoSwap) (makeTestEventBelief e1 e2))

////let testTemporalInfFunc t1 t2 =
////    let matcher = function
////        | {Event.Term = t1; TV = Some tv1} -> (ft t1, Some tv1)
////        | {Event.Term = t1; TV = None} -> (ft t1, None)

////    List.map matcher (temporalInf t1 t2)

//let TemporalDistance = int32(Params.CONCURRENCY_DURATION + 10L)

//[<Tests>]
//let test1 =
//    testList "Inference" [
//        testCase "Nal1_5_conversion_contrapostion_negation: conversion1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p --> s>." + truth tv1
//            let t2 = parseEvent <|  "<s --> p>." + truth tv1         
//            let expected = [("<p --> s>", Some <| cnv(tv1, tv2))]
//            Expect.equal (testInfFunc Nal1_4_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: conversion1 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: conversion2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p ==> s>." + truth tv1
//            let t2 = parseEvent <|  "<s ==> p>." + truth tv1         
//            let expected = [("<p ==> s>", Some <| cnv(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: conversion2 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: conversion3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =|> s>." + truth tv1
//            let t2 = parseEvent <|  "<s =|> p>." + truth tv1         
//            let expected = [("<p =|> s>", Some <| cnv(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: conversion3 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: conversion4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =-> s>." + truth tv1
//            let t2 = parseEvent <|  "<s =+> p>." + truth tv1         
//            let expected = [("<p =-> s>", Some <| cnv(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: conversion4 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: conversion5" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =+> s>." + truth tv1
//            let t2 = parseEvent <|  "<s =-> p>." + truth tv1         
//            let expected = [("<p =+> s>", Some <| cnv(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: conversion5 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: contrapostion1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<--s ==> p>." + truth tv1
//            let t2 = parseEvent <|  "p . " + truth tv1          
//            let expected = [("<--p ==> s>", Some <| cnt(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: contrapostion1 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: contrapostion2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<--s ==> p>." + truth tv1
//            let t2 = parseEvent <|  "--s ." + truth tv1          
//            let expected = [("<--p ==> s>", Some <| cnt(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: contrapostion2 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: contrapostion3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<--s =|> p>." + truth tv1
//            let t2 = parseEvent <|  "p . " + truth tv1          
//            let expected = [("<--p =|> s>", Some <| cnt(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: contrapostion3 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: contrapostion4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<--s =|> p>." + truth tv1
//            let t2 = parseEvent <|  "--s ." + truth tv1          
//            let expected = [("<--p =|> s>", Some <| cnt(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: contrapostion4 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: contrapostion5" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<--s =+> p>." + truth tv1
//            let t2 = parseEvent <|  "p . " + truth tv1          
//            let expected = [("<--p =-> s>", Some <| cnt(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: contrapostion5 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: contrapostion6" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<--s =+> p>." + truth tv1
//            let t2 = parseEvent <|  "--s ." + truth tv1          
//            let expected = [("<--p =-> s>", Some <| cnt(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: contrapostion6 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: contrapostion7" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<--s =-> p>." + truth tv1
//            let t2 = parseEvent <|  "p . " + truth tv1          
//            let expected = [("<--p =+> s>", Some <| cnt(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: contrapostion7 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: contrapostion8" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<--s =-> p>." + truth tv1
//            let t2 = parseEvent <|  "--s ." + truth tv1           
//            let expected = [("<--p =+> s>", Some <| cnt(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: contrapostion8 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<a --> b>." + truth tv1
//            let t2 = parseEvent <|  "a ." + truth tv1       
//            let expected = [("--<a --> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal1_4_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation1 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<a --> b>." + truth tv1
//            let t2 = parseEvent <|  "b ." + truth tv1         
//            let expected = [("--<a --> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal1_4_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation2 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "--<a --> b>." + truth tv1
//            let t2 = parseEvent <|  "a ." + truth tv1        
//            let expected = [("<a --> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation3 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "--<a --> b>." + truth tv1
//            let t2 = parseEvent <|  "b ." + truth tv1         
//            let expected = [("<a --> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation4 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation5" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<a <-> b>." + truth tv1
//            let t2 = parseEvent <|  "a ." + truth tv1        
//            let expected = [("--<a <-> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal1_4_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation5 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation6" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<a <-> b>." + truth tv1
//            let t2 = parseEvent <|  "b ." + truth tv1        
//            let expected = [("--<a <-> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal1_4_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation6 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation7" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "--<a <-> b>." + truth tv1
//            let t2 = parseEvent <|  "a ." + truth tv1        
//            let expected = [("<a <-> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation7 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation8" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "--<a <-> b>." + truth tv1
//            let t2 = parseEvent <|  "b ." + truth tv1         
//            let expected = [("<a <-> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation8 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation9" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<a ==> b>." + truth tv1
//            let t2 = parseEvent <|  "a ." + truth tv1        
//            let expected = [("--<a ==> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation9 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation10" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<a ==> b>." + truth tv1
//            let t2 = parseEvent <|  "b ." + truth tv1         
//            let expected = [("--<a ==> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation10 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation11" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "--<a ==> b>." + truth tv1
//            let t2 = parseEvent <|  "a ." + truth tv1         
//            let expected = [("<a ==> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation11 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation12" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "--<a ==> b>." + truth tv1
//            let t2 = parseEvent <|  "b ." + truth tv1         
//            let expected = [("<a ==> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation12 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation13" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<a <=> b>." + truth tv1
//            let t2 = parseEvent <|  "a ." + truth tv1          
//            let expected = [("--<a <=> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation13 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation14" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<a <=> b>." + truth tv1
//            let t2 = parseEvent <|  "b ." + truth tv1          
//            let expected = [("--<a <=> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation14 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation15" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "--<a <=> b>." + truth tv1
//            let t2 = parseEvent <|  "a ." + truth tv1    
//            let expected = [("<a <=> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation15 failed"

//        testCase "Nal1_5_conversion_contrapostion_negation: negation16" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "--<a <=> b>." + truth tv1
//            let t2 = parseEvent <|  "b . " + truth tv1
//            let expected = [("<a <=> b>", Some <| neg(tv1, tv2))]
//            Expect.equal (testInfFunc Nal5_conversion_contrapostion_negation t1 t2)  expected "Nal1_5_conversion_contrapostion_negation: negation16 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m ==> p>." + truth tv1
//            let t2 = parseEvent <|  "<s ==> m>." + truth tv1
//            let expected = [("<s ==> p>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 1 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =|> p>." + truth tv1
//            let t2 = parseEvent <|  "<s =|> m>." + truth tv1
//            let expected = [("<s =|> p>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 2 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t2 = parseEvent <|  "<s =-> m>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t1 = parseEvent <|  "<m =-> p>." + truth tv1
//            let expected = [("<s =-> p>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 3 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =+> p>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<s =+> m>." + truth tv1
//            let expected = [("<s =+> p>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 4 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 5" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p ==> m>." + truth tv1
//            let t2 = parseEvent <|  "<s ==> m>." + truth tv1
//            let expected = [("<s ==> p>", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 5 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 6" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =|> m>." + truth tv1
//            let t2 = parseEvent <|  "<s =|> m>." + truth tv1
//            let expected = [("<s =|> p>", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 6 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 7" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =+> m>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<s =+> m>." + truth tv1
//            let expected = [("<s =|> p>", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 7 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 8" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t2 = parseEvent <|  "<s =-> m>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t1 = parseEvent <|  "<p =-> m>." + truth tv1
//            let expected = [("<s =|> p>", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 8 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 9" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m ==> p>." + truth tv1
//            let t2 = parseEvent <|  "<m ==> s>." + truth tv1
//            let expected = [("<s ==> p>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 9 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 10" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =+> p>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<m =+> s>." + truth tv1
//            let expected = [("<s =|> p>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 10 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 11" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =|> p>." + truth tv1
//            let t2 = parseEvent <|  "<m =|> s>." + truth tv1
//            let expected = [("<s =|> p>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 11 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 12" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t2 = parseEvent <|  "<m =-> s>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t1 = parseEvent <|  "<m =-> p>." + truth tv1
//            let expected = [("<s =|> p>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 12 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 13" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p ==> m>." + truth tv1
//            let t2 = parseEvent <|  "<m ==> s>." + truth tv1
//            let expected = [("<s ==> p>", Some <| exe(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 13 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 14" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =+> m>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<m =+> s>." + truth tv1
//            let expected = [("<s =-> p>", Some <| exe(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 14 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 15" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t2 = parseEvent <|  "<m =-> s>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t1 = parseEvent <|  "<p =-> m>." + truth tv1
//            let expected = [("<s =+> p>", Some <| exe(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 15 failed"

//        testCase "nal_5_implication_based_syllogism_Imp: 16" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =|> m>." + truth tv1
//            let t2 = parseEvent <|  "<m =|> s>." + truth tv1
//            let expected = [("<s =|> p>", Some <| exe(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Imp t1 t2)  expected "nal_5_implication_based_syllogism_Imp: 16 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s ==> p>." + truth tv1
//            let t2 = parseEvent <|  "<p ==> s>." + truth tv1
//            let expected = [("<s <=> p>", Some <| int(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 1 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s =|> p>." + truth tv1
//            let t2 = parseEvent <|  "<p =|> s>." + truth tv1
//            let expected = [("<s <|> p>", Some <| int(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 2 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s =+> p>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<p =-> s>." + truth tv1
//            let expected = [("<s <+> p>", Some <| int(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 3 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s =-> p>." + truth tv1
//            let t2 = parseEvent <|  "<p =+> s>." + truth tv1
//            let expected = [("<p <+> s>", Some <| int(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 4 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 5" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p ==> m>." + truth tv1
//            let t2 = parseEvent <|  "<s ==> m>." + truth tv1
//            let expected = [("<s <=> p>", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 5 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 6" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =+> m>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<s =+> m>." + truth tv1
//            let expected = [("<s <|> p>", Some <| ind(tv1, tv2))
//                            ("<s <+> p>", Some <| ind(tv1, tv2))
//                            ("<p <+> s>", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 6 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 7" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =|> m>." + truth tv1
//            let t2 = parseEvent <|  "<s =|> m>." + truth tv1
//            let expected = [("<s <|> p>", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 7 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 8" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t2 = parseEvent <|  "<s =-> m>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t1 = parseEvent <|  "<p =-> m>." + truth tv1
//            let expected = [("<s <|> p>", Some <| com(tv1, tv2))
//                            ("<s <+> p>", Some <| com(tv1, tv2))
//                            ("<p <+> s>", Some <| com(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 8 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 9" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m ==> p>." + truth tv1
//            let t2 = parseEvent <|  "<m ==> s>." + truth tv1
//            let expected = [("<s <=> p>", Some <| com(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 9 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 10" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =+> p>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<m =+> s>." + truth tv1
//            let expected = [("<s <|> p>", Some <| com(tv1, tv2))
//                            ("<s <+> p>", Some <| com(tv1, tv2))
//                            ("<p <+> s>", Some <| com(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 10 failed"

//        testCase "nal_5_implication_based_syllogism_Equ1: 11" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =|> p>." + truth tv1
//            let t2 = parseEvent <|  "<m =|> s>." + truth tv1
//            let expected = [("<s <|> p>", Some <| com(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ1 t1 t2)  expected "nal_5_implication_based_syllogism_Equ1: 11 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m ==> p>." + truth tv1
//            let t2 = parseEvent <|  "<s <=> m>." + truth tv1
//            let expected = [("<s ==> p>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 1 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =+> p>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<s <+> m>." + truth tv1
//            let expected = [("<s =+> p>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 2 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =+> p>." + truth tv1
//            let t2 = parseEvent <|  "<s <|> m>." + truth tv1
//            let expected = [("<s =+> p>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 3 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =|> p>." + truth tv1
//            let t2 = parseEvent <|  "<s <|> m>." + truth tv1
//            let expected = [("<s =|> p>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 4 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 5" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =-> p>." + truth tv1
//            let t2 = parseEvent <|  "<s <+> m>." + truth tv1
//            let expected = [("<s =-> p>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 5 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 6" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =-> p>." + truth tv1
//            let t2 = parseEvent <|  "<s <|> m>." + truth tv1
//            let expected = [("<s =-> p>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 6 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 7" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p ==> m>." + truth tv1
//            let t2 = parseEvent <|  "<s <=> m>." + truth tv1
//            let expected = [("<s ==> p>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 7 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 8" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =+> m>." + truth tv1
//            let t2 = parseEvent <|  "<s <|> m>." + truth tv1
//            let expected = [("<p =-> s>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 8 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 9" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =|> m>." + truth tv1
//            let t2 = parseEvent <|  "<s <|> m>." + truth tv1
//            let expected = [("<p =|> s>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 9 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 10" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =-> m>." + truth tv1
//            let t2 = parseEvent <|  "<s <+> m>." + truth tv1
//            let expected = [("<p =-> s>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 10 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 11" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =-> m>." + truth tv1
//            let t2 = parseEvent <|  "<s <|> m>." + truth tv1
//            let expected = [("<p =-> s>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 11 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 12" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m <+> p>." + truth tv1
//            let t2 = parseEvent <|  "<s <|> m>." + truth tv1
//            let expected = [("<s <+> p>", Some <| res(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 12 failed"

//        testCase "nal_5_implication_based_syllogism_Equ2: 13" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m <|> p>." + truth tv1
//            let t2 = parseEvent <|  "<s <+> m>." + truth tv1
//            let expected = [("<s <+> p>", Some <| res(tv1, tv2))]
//            Expect.equal (testInfFunc nal_5_implication_based_syllogism_Equ2 t1 t2)  expected "nal_5_implication_based_syllogism_Equ2: 13 failed"

//        testCase "nal5_implication_based_composition: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p ==> m>." + truth tv1
//            let t2 = parseEvent <|  "<s ==> m>." + truth tv1
//            let expected = [("<(p || s) ==> m>", Some <| int(tv1, tv2))
//                            ("<(p && s) ==> m>", Some <| uni(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_implication_based_composition t1 t2)  expected "nal5_implication_based_composition: 1 failed"

//        testCase "nal5_implication_based_composition: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =|> m>." + truth tv1
//            let t2 = parseEvent <|  "<s =|> m>." + truth tv1
//            let expected = [("<(p || s) =|> m>", Some <| int(tv1, tv2))
//                            ("<(p ; s) =|> m>", Some <| uni(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_implication_based_composition t1 t2)  expected "nal5_implication_based_composition: 2 failed"

//        testCase "nal5_implication_based_composition: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<p =+> m>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<s =+> m>." + truth tv1
//            let expected = [("<(p || s) =+> m>", Some <| int(tv1, tv2))
//                            ("<(p ; s) =+> m>", Some <| uni(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_implication_based_composition t1 t2)  expected "nal5_implication_based_composition: 3 failed"

//        testCase "nal5_implication_based_composition: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t2 = parseEvent <|  "<s =-> m>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t1 = parseEvent <|  "<p =-> m>." + truth tv1
//            let expected = [("<(p || s) =-> m>", Some <| int(tv1, tv2))
//                            ("<(p ; s) =-> m>", Some <| uni(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_implication_based_composition t1 t2)  expected "nal5_implication_based_composition: 4 failed"

//        testCase "nal5_implication_based_composition: 5" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m ==> p>." + truth tv1
//            let t2 = parseEvent <|  "<m ==> s>." + truth tv1
//            let expected = [("<m ==> (p && s)>", Some <| int(tv1, tv2))
//                            ("<m ==> (p || s)>", Some <| uni(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_implication_based_composition t1 t2)  expected "nal5_implication_based_composition: 5 failed"

//        testCase "nal5_implication_based_composition: 6" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =+> p>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t2 = parseEvent <|  "<m =+> s>." + truth tv1
//            let expected = [("<m =+> (p ; s)>", Some <| int(tv1, tv2))
//                            ("<m =+> (p || s)>", Some <| uni(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_implication_based_composition t1 t2)  expected "nal5_implication_based_composition: 6 failed"

//        testCase "nal5_implication_based_composition: 7" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =|> p>." + truth tv1
//            let t2 = parseEvent <|  "<m =|> s>." + truth tv1
//            let expected = [("<m =|> (p ; s)>", Some <| int(tv1, tv2))
//                            ("<m =|> (p || s)>", Some <| uni(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_implication_based_composition t1 t2)  expected "nal5_implication_based_composition: 7 failed"

//        testCase "nal5_implication_based_composition: 8" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t2 = parseEvent <|  "<m =-> s>." + truth tv1
//            Thread.Sleep(TemporalDistance)
//            let t1 = parseEvent <|  "<m =-> p>." + truth tv1
//            let expected = [("<m =-> (p ; s)>", Some <| int(tv1, tv2))
//                            ("<m =-> (p || s)>", Some <| uni(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_implication_based_composition t1 t2)  expected "nal5_implication_based_composition: 8 failed"

//        testCase "nal5_implication_based_composition: 9" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<d =+> r>." + truth tv1
//            let t2 = parseEvent <|  "<d =-> k>." + truth tv1
//            let expected = [("<k =+> r>", Some <| abd(tv1, tv2))
//                            ("<k =-> r>", Some <| ind(tv1, tv2))
//                            ("<k <+> r>", Some <| com(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_implication_based_composition t1 t2)  expected "nal5_implication_based_composition: 9 failed"

//        testCase "nal5_nal8_implication_based_decomposition1: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s ==> m>." + truth tv1
//            let t2 = parseEvent <|  "<(s || (r || t)) ==> m>." + truth tv1
//            let expected = [("<(r || t) ==> m>", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition1 t1 t2)  expected "nal5_nal8_implication_based_decomposition1: 1 failed"

//        testCase "nal5_nal8_implication_based_decomposition1: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s =+> m>." + truth tv1
//            let t2 = parseEvent <|  "<(s || (r || t)) ==> m>." + truth tv1
//            let expected = [("<(r || t) =+> m>", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition1 t1 t2)  expected "nal5_nal8_implication_based_decomposition1: 2 failed"

//        testCase "nal5_nal8_implication_based_decomposition1: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s =|> m>." + truth tv1
//            let t2 = parseEvent <|  "<(s || (r || t)) ==> m>." + truth tv1
//            let expected = [("<(r || t) =|> m>", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition1 t1 t2)  expected "nal5_nal8_implication_based_decomposition1: 3 failed"

//        testCase "nal5_nal8_implication_based_decomposition1: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s =-> m>." + truth tv1
//            let t2 = parseEvent <|  "<(s || (r || t)) ==> m>." + truth tv1
//            let expected = [("<(r || t) =-> m>", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition1 t1 t2)  expected "nal5_nal8_implication_based_decomposition1: 4 failed"


//        testCase "nal5_nal8_implication_based_decomposition2: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s ==> m>." + truth tv1
//            let t2 = parseEvent <|  "<(s && (r && t)) ==> m>." + truth tv1
//            let expected = [("<(r && t) ==> m>", Some <| npp(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition2 t1 t2)  expected "nal5_nal8_implication_based_decomposition2: 1 failed"

//        testCase "nal5_nal8_implication_based_decomposition2: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s =+> m>." + truth tv1
//            let t2 = parseEvent <|  "<(s && (r && t)) =+> m>." + truth tv1
//            let expected = [("<(r && t) =+> m>", Some <| npp(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition2 t1 t2)  expected "nal5_nal8_implication_based_decomposition2: 2 failed"

//        testCase "nal5_nal8_implication_based_decomposition2: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s =|> m>." + truth tv1
//            let t2 = parseEvent <|  "<(s && (r && t)) =|> m>." + truth tv1
//            let expected = [("<(r && t) =|> m>", Some <| npp(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition2 t1 t2)  expected "nal5_nal8_implication_based_decomposition2: 3 failed"

//        testCase "nal5_nal8_implication_based_decomposition2: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s =-> m>." + truth tv1
//            let t2 = parseEvent <|  "<(s && (r && t)) =-> m>." + truth tv1
//            let expected = [("<(r && t) =-> m>", Some <| npp(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition2 t1 t2)  expected "nal5_nal8_implication_based_decomposition2: 4 failed"

//        testCase "nal5_nal8_implication_based_decomposition3: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m ==> s>." + truth tv1
//            let t2 = parseEvent <|  "<m ==> (s && (r && t))>." + truth tv1
//            let expected = [("<m ==> (r && t)>", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition3 t1 t2)  expected "nal5_nal8_implication_based_decomposition3: 1 failed"

//        testCase "nal5_nal8_implication_based_decomposition3: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =+> s>." + truth tv1
//            let t2 = parseEvent <|  "<m =+> (s && (r && t))>." + truth tv1
//            let expected = [("<m =+> (r && t)>", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition3 t1 t2)  expected "nal5_nal8_implication_based_decomposition3: 2 failed"

//        testCase "nal5_nal8_implication_based_decomposition3: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =|> s>." + truth tv1
//            let t2 = parseEvent <|  "<m =|> (s && (r && t))>." + truth tv1
//            let expected = [("<m =|> (r && t)>", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition3 t1 t2)  expected "nal5_nal8_implication_based_decomposition3: 3 failed"

//        testCase "nal5_nal8_implication_based_decomposition3: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =-> s>." + truth tv1
//            let t2 = parseEvent <|  "<m =-> (s && (r && t))>." + truth tv1
//            let expected = [("<m =-> (r && t)>", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition3 t1 t2)  expected "nal5_nal8_implication_based_decomposition3: 4 failed"

//        testCase "nal5_nal8_implication_based_decomposition4: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m ==> s>." + truth tv1
//            let t2 = parseEvent <|  "<m ==> (s || (r || t))>." + truth tv1
//            let expected = [("<m ==> (r || t)>", Some <| npp(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition4 t1 t2)  expected "nal5_nal8_implication_based_decomposition4: 1 failed"

//        testCase "nal5_nal8_implication_based_decomposition4: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =+> s>." + truth tv1
//            let t2 = parseEvent <|  "<m =+> (s || (r || t))>." + truth tv1
//            let expected = [("<m =+> (r || t)>", Some <| npp(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition4 t1 t2)  expected "nal5_nal8_implication_based_decomposition4: 2 failed"

//        testCase "nal5_nal8_implication_based_decomposition4: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m =|> s>." + truth tv1
//            let t2 = parseEvent <|  "<m =|> (s || (r || t))>." + truth tv1
//            let expected = [("<m =|> (r || t)>", Some <| npp(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition4 t1 t2)  expected "nal5_nal8_implication_based_decomposition4: 3 failed"

           
//        testCase "nal5_nal8_implication_based_decomposition5: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<m ==> p>." + truth tv1
//            let expected = [("p", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 1 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<m =+> p>." + truth tv1
//            let expected = [("p", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 2 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<m =|> p>." + truth tv1
//            let expected = [("p", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 3 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<m =-> p>." + truth tv1
//            let expected = [("p", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 4 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 5" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<p ==> m>." + truth tv1
//            let expected = [("p", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 5 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 6" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<p =+> m>." + truth tv1
//            let expected = [("p", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 6 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 7" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<p =|> m>." + truth tv1
//            let expected = [("p", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 7 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 8" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<p =-> m>." + truth tv1
//            let expected = [("p", Some <| ind(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 8 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 9" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<s <=> m>." + truth tv1
//            let expected = [("s", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 9 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 10" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<s <|> m>." + truth tv1
//            let expected = [("s", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 10 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 11" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<s <+> m>." + truth tv1
//            let expected = [("s", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 11 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 12" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<m <=> s>." + truth tv1
//            let expected = [("s", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 12 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 13" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<m <|> s>." + truth tv1
//            let expected = [("s", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 13 failed"

//        testCase "nal5_nal8_implication_based_decomposition5: 14" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "m ." + truth tv1
//            let t2 = parseEvent <|  "<m <+> s>." + truth tv1
//            let expected = [("s", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition5 t1 t2)  expected "nal5_nal8_implication_based_decomposition5: 14 failed"

//        testCase "nal5_nal8_implication_based_decomposition6: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "(s && t)." + truth tv1
//            let t2 = parseEvent <|  "s ." + truth tv1
//            let expected = [("t", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition6 t1 t2)  expected "nal5_nal8_implication_based_decomposition6: 1 failed"

//        testCase "nal5_nal8_implication_based_decomposition6: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "(s , t)." + truth tv1
//            let t2 = parseEvent <|  "s ." + truth tv1
//            let expected = [("t", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition6 t1 t2)  expected "nal5_nal8_implication_based_decomposition6: 2 failed"

//        testCase "nal5_nal8_implication_based_decomposition6: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "(s ; t)." + truth tv1
//            let t2 = parseEvent <|  "s ." + truth tv1
//            let expected = [("t", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition6 t1 t2)  expected "nal5_nal8_implication_based_decomposition6: 3 failed"

//        testCase "nal5_nal8_implication_based_decomposition7: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "s ." + truth tv1
//            let t2 = parseEvent <|  "(s , (r , t))." + truth tv1
//            let expected = [("(r , t)", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition7 t1 t2)  expected "nal5_nal8_implication_based_decomposition7: 1 failed"

//        testCase "nal5_nal8_implication_based_decomposition7: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "s ." + truth tv1
//            let t2 = parseEvent <|  "(s ; (r ; t))." + truth tv1
//            let expected = [("(r ; t)", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition7 t1 t2)  expected "nal5_nal8_implication_based_decomposition7: 2 failed"

//        testCase "nal5_nal8_implication_based_decomposition7: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "s ." + truth tv1
//            let t2 = parseEvent <|  "(s && (r && t))." + truth tv1
//            let expected = [("(r && t)", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition7 t1 t2)  expected "nal5_nal8_implication_based_decomposition7: 3 failed"

//        testCase "nal5_nal8_implication_based_decomposition7: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "s ." + truth tv1
//            let t2 = parseEvent <|  "(s || (r || t))." + truth tv1
//            let expected = [("(r || t)", Some <| pnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition7 t1 t2)  expected "nal5_nal8_implication_based_decomposition7: 4 failed"

//        testCase "nal5_nal8_implication_based_decomposition8: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "s ." + truth tv1
//            let t2 = parseEvent <|  "(--s , (r , t))." + truth tv1
//            let expected = [("(r , t)", Some <| nnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition8 t1 t2)  expected "nal5_nal8_implication_based_decomposition8: 1 failed"

//        testCase "nal5_nal8_implication_based_decomposition8: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "s ." + truth tv1
//            let t2 = parseEvent <|  "(--s ; (r ; t))." + truth tv2
//            let expected = [("(r ; t)", Some <| nnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition8 t1 t2)  expected "nal5_nal8_implication_based_decomposition8: 2 failed"

//        testCase "nal5_nal8_implication_based_decomposition8: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "s ." + truth tv1
//            let t2 = parseEvent <|  "(--s && (r && t))." + truth tv2
//            let expected = [("(r && t)", Some <| nnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition8 t1 t2)  expected "nal5_nal8_implication_based_decomposition8: 3 failed"

//        testCase "nal5_nal8_implication_based_decomposition8: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "s ." + truth tv1
//            let t2 = parseEvent <|  "(--s || (r || t))." + truth tv2
//            let expected = [("(r || t)", Some <| nnn(tv1, tv2))]
//            Expect.equal (testInfFunc nal5_nal8_implication_based_decomposition8 t1 t2)  expected "nal5_nal8_implication_based_decomposition8: 4 failed"

//        testCase "nal6_variable_introduction: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<s --> m>." + truth tv1
//            let t2 = parseEvent <|  "<p --> m>." + truth tv2
//            let expected = [("<<p --> $X> ==> <s --> $X>>", Some <| abd(tv1, tv2))
//                            ("<<s --> $X> ==> <p --> $X>>", Some <| ind(tv1, tv2))
//                            ("<<p --> $X> <=> <s --> $X>>", Some <| com(tv1, tv2))
//                            ("(<p --> #Y> && <s --> #Y>)", Some <| int(tv1, tv2))

//                            ("<<p --> $X> =|> <s --> $X>>", Some <| abd(tv1, tv2))
//                            ("<<s --> $X> =|> <p --> $X>>", Some <| ind(tv1, tv2))
//                            ("<<p --> $X> =|> <s --> $X>>", Some <| ind(tv1, tv2))
//                            ("(<p --> #Y> ; <s --> #Y>)", Some <| int(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_introduction t1 t2)  expected "nal6_variable_introduction: 1 failed"

//        testCase "nal6_variable_introduction: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<m --> s>." + truth tv1
//            let t2 = parseEvent <|  "<m --> p>." + truth tv2
//            let expected = [("<<$X --> s> ==> <$X --> p>>", Some <| ind(tv1, tv2))
//                            ("<<$X --> p> ==> <$X --> s>>", Some <| abd(tv1, tv2))
//                            ("<<$X --> s> <=> <$X --> p>>", Some <| com(tv1, tv2))
//                            ("(<#Y --> s> && <#Y --> p>)", Some <| int(tv1, tv2))

//                            ("<<$X --> s> =|> <$X --> p>>", Some <| ind(tv1, tv2))
//                            ("<<$X --> p> =|> <$X --> s>>", Some <| abd(tv1, tv2))
//                            ("<<$X --> s> =|> <$X --> p>>", Some <| ind(tv1, tv2))
//                            ("<<$X --> s> <|> <$X --> p>>", Some <| com(tv1, tv2))
//                            ("(<#Y --> s> ; <#Y --> p>)", Some <| int(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_introduction t1 t2)  expected "nal6_variable_introduction: 2 failed"

//        testCase "nal6_variable_syllogisms: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<<a --> r> ==> z>." + truth tv1
//            let t2 = parseEvent <|  "<(<#Y --> b> && <#Y --> r>) ==> z>." + truth tv2
//            let expected = [("<a --> b>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_syllogisms t1 t2)  expected "nal6_variable_syllogisms: 1 failed"

//        testCase "nal6_variable_syllogisms: 1a" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<<a --> r> ==> z>." + truth tv1
//            let t2 = parseEvent <|  "<(<#Y --> r> && <#Y --> b>) ==> z>." + truth tv2
//            let expected = [("<a --> b>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_syllogisms t1 t2)  expected "nal6_variable_syllogisms: 1a failed"

//        testCase "nal6_variable_syllogisms: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<u --> l>." + truth tv1
//            let t2 = parseEvent <|  "<(<#Y --> l> && <#Y --> r>) ==> z>." + truth tv2
//            let expected = [("<<u --> r> ==> z>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_syllogisms t1 t2)  expected "nal6_variable_syllogisms: 2 failed"

//        testCase "nal6_variable_syllogisms: 2a" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<u --> l>." + truth tv1
//            let t2 = parseEvent <|  "<(<#Y --> r> && <#Y --> l>) ==> z>." + truth tv2
//            let expected = [("<<u --> r> ==> z>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_syllogisms t1 t2)  expected "nal6_variable_syllogisms: 2a failed"

//        testCase "nal6_variable_elimination: 1" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "(<($x * $y) --> larger> && <($y * $x) --> smaller>)." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| anon_ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 1 failed"

//        testCase "nal6_variable_elimination: 2" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "(<($x * $y) --> smaller> && <($y * $x) --> larger>)." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| anon_ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 2 failed"

//        testCase "nal6_variable_elimination: 3" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "(<($x * $y) --> smaller> && <($y * $x) --> larger>)." + truth tv2
//            let t2 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1            
//            let expected = [("<(apple * earth) --> smaller>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 3 failed"

//        testCase "nal6_variable_elimination: 4" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "(<($x * $y) --> smaller> && <($y * $x) --> larger>)." + truth tv2
//            let t2 = parseEvent <|  "<(earth * apple) --> smaller>." + truth tv1            
//            let expected = [("<(apple * earth) --> larger>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 4 failed"

//        testCase "nal6_variable_elimination: 5" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> larger> ==> <($y * $x) --> smaller>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 1 failed"

//        testCase "nal6_variable_elimination: 6" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> larger> =+> <($y * $x) --> smaller>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 2 failed"

//        testCase "nal6_variable_elimination: 7" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> larger> =|> <($y * $x) --> smaller>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 7 failed"
        
//        testCase "nal6_variable_elimination: 8" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> larger> =-> <($y * $x) --> smaller>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ded(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 8 failed"

//        testCase "nal6_variable_elimination: 9" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> smaller> ==> <($y * $x) --> larger>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 9 failed"

//        testCase "nal6_variable_elimination: 10" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> smaller> =+> <($y * $x) --> larger>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 10 failed"

//        testCase "nal6_variable_elimination: 11" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> smaller> =|> <($y * $x) --> larger>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 11 failed"

//        testCase "nal6_variable_elimination: 12" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> smaller> =-> <($y * $x) --> larger>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| abd(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 12 failed"

//        testCase "nal6_variable_elimination: 13" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> larger> <=> <($y * $x) --> smaller>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 13 failed"
        
//        testCase "nal6_variable_elimination: 14" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> larger> <|> <($y * $x) --> smaller>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 14 failed"

//        testCase "nal6_variable_elimination: 14a" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "(<rabbit --> animal> ; <PETER --> word>)." + truth tv1
//            let t2 = parseEvent <|  "<($1 ; <$2 --> word>) <|> <(REPRESENT / $2 _) <-> $1>>." + truth tv2
//            let expected = [("<(REPRESENT / PETER _) <-> <rabbit --> animal>>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 14a failed"


//        testCase "nal6_variable_elimination: 15" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> larger> <+> <($y * $x) --> smaller>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 15 failed"

//        testCase "nal6_variable_elimination: 16" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> smaller> <=> <($y * $x) --> larger>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 16 failed"

//        testCase "nal6_variable_elimination: 17" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> smaller> <|> <($y * $x) --> larger>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 17 failed"

//        testCase "nal6_variable_elimination: 18" <| fun () ->   
//            let tv1 = {F = 1.0f; C = 0.9f}
//            let tv2 = {F = 1.0f; C = 0.9f}
//            let t1 = parseEvent <|  "<(earth * apple) --> larger>." + truth tv1
//            let t2 = parseEvent <|  "<<($x * $y) --> smaller> <+> <($y * $x) --> larger>>." + truth tv2
//            let expected = [("<(apple * earth) --> smaller>", Some <| ana(tv1, tv2))]
//            Expect.equal (testInfFunc nal6_variable_elimination t1 t2)  expected "nal6_variable_elimination: 18 failed"       
//]

