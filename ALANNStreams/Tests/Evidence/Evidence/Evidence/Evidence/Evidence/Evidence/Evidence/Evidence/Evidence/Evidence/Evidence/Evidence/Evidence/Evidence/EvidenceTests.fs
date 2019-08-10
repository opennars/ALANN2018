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

module EvidenceTests

open Expecto
open Evidence

let makeEvidence(n) = [for i in 0..n - 1 -> i]
let makeEvidenceFrom(n, l) = [for i in n..(l + n) -> i]
let makeEvenEvidence(n) = [for i in 0..2..(n * 2) - 1 -> i]
let makeOddEvidence(n) = [for i in 1..2..(n * 2) -> i]
let makeExpected evidence = List.truncate Params.TRAIL_LENGTH evidence
let makeInterleavedEvidence a b = List.zip a b |> List.collect (fun (a,b)-> [a;b]) |> List.truncate Params.TRAIL_LENGTH

[<Tests>]
let test1 =
    testList "EvidenceTests" [
        testCase "EvidenceTest1" <| fun () ->
            let e1 = []
            let e2 = []
            let expected = []
            Expect.equal (merge e1 e2) expected "Incorrect evidence"

        testCase "EvidenceTest2" <| fun () ->
            let e1 = makeEvidence(20) //[0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19]
            let e2 = []
            let expected = e1 |> makeExpected //[0;1;2;3;4;5;6;7;8;9;10;11;12;13;14]
            Expect.equal (merge e1 e2) expected "Incorrect evidence"

        testCase "EvidenceTest3" <| fun () ->
            let e1 = []
            let e2 = makeEvidence(20) //[0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19]
            let expected = e2 |> makeExpected // [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14]
            Expect.equal (merge e1 e2) expected "Incorrect evidence"

        testCase "EvidenceTest4" <| fun () ->
            let e1 = makeEvenEvidence(10) //[0;2;4;6;8;10;12;14;16;18]
            let e2 = makeOddEvidence(10) //[1;3;5;7;9;11;13;15;17;19]
            let expected = makeInterleavedEvidence e1 e2 // [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14]
            Expect.equal (merge e1 e2) expected "Incorrect evidence"

        testCase "EvidenceTest5" <| fun () ->
            let e1 = makeEvenEvidence(10) //[0;2;4;6;8;10;12;14;16;18]
            let e2 = makeOddEvidence(2) //[1;3]
            let expected = [0;1;2;3;4;6;8;10;12;14;16;18]
            Expect.equal (merge e1 e2) expected "Incorrect evidence"

        testCase "EvidenceTest6" <| fun () ->
            let e1 = [1;3]
            let e2 = [0;2;4;6;8;10;12;14;16;18]
            let expected = [1;0;3;2;4;6;8;10;12;14;16;18]
            Expect.equal (merge e1 e2) expected "Incorrect evidence"

        testCase "EvidenceTest7" <| fun () ->
            let e1 = makeEvidenceFrom(1, 20) //[1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20]
            let e2 = makeEvidenceFrom(21, 20) //[21;22;23;24;25;26;27;28;28;30;31;32;33;34;35;36;37]
            let expected = makeInterleavedEvidence e1 e2 //[1;21;2;22;3;23;4;24;5;25;6;26;7;27;8]
            Expect.equal (merge e1 e2) expected "Incorrect evidence"

        testCase "EvidenceTest8" <| fun () ->
            let e1 = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20]
            let e2 = [1;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37]
            let expected = false
            Expect.equal (nonOverlap e1 e2) expected "Incorrect overlap"

        testCase "EvidenceTest9" <| fun () ->
            let e1 = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20]
            let e2 = [21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37]
            let expected = true
            Expect.equal (nonOverlap e1 e2) expected "Incorrect overlap"
    ]

