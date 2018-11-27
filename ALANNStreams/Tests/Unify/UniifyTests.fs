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

module UnifyTests

open Unify
//open ParserCombinators
//open Parser2
open Parser
open Expecto

let parseTerm str = 
    match testp pterm str with
    | Some x -> x
    | None -> failwith "error"

[<Tests>]
let test1 =
    testList "FormatTests" [
        testCase "FormatTest1" <| fun () ->   
            let t1 = parseTerm "<{a} --> b>"
            let q1 = parseTerm "<?what --> b>"         
            let expected = true
            Expect.equal  (unifies t1 q1) expected "Incorrect format"

        testCase "FormatTest2" <| fun () ->   
            let t1 = parseTerm "(cat && mouse)"
            let q1 = parseTerm "(#1 && #2)"         
            let expected = true
            Expect.equal  (unifies t1 q1) expected "Incorrect format"

        testCase "FormatTest3" <| fun () ->   
            let t1 = parseTerm "<<a --> x> ==> <a --> y>>"
            let q1 = parseTerm "<<$1 --> x> ==> <$1 --> y>>"       
            let expected = true
            Expect.equal  (unifies t1 q1) expected "Incorrect format"

        testCase "FormatTest4" <| fun () ->   
            let t1 = parseTerm "<<a --> x> ==> <b --> x>>"
            let q1 = parseTerm "<<a --> $1> ==> <b --> $1>>"       
            let expected = true
            Expect.equal  (unifies t1 q1) expected "Incorrect format"

        testCase "FormatTest5" <| fun () ->   
            let t1 = parseTerm "<<(a * b) --> c> ==> <d --> e>>"
            let q1 = parseTerm "<<?what --> c> ==> <d --> e>>"
            let expected = true
            Expect.equal  (unifies t1 q1) expected "Incorrect format"

        testCase "FormatTest6" <| fun () ->   
            let t1 = parseTerm "<<(a * b) --> c> ==> <d --> e>>"
            let q1 = parseTerm "<<(a * ?what) --> c> ==> <d --> e>>"
            let expected = true
            Expect.equal  (unifies t1 q1) expected "Incorrect format"

        testCase "FormatTest7" <| fun () ->   
            let t1 = parseTerm "<<(a * b) --> c> ==> <d --> e>>"
            let q1 = parseTerm "<<(a * b) --> c> ==> ?what>"
            let expected = true
            Expect.equal  (unifies t1 q1) expected "Incorrect format"

        testCase "FormatTest8" <| fun () ->   
            let t1 = parseTerm "<<(a * b) --> c> ==> <d --> e>>"
            let q1 = parseTerm "<<(a * b) --> c> ==> <d --> ?what>>"
            let expected = true
            Expect.equal  (unifies t1 q1) expected "Incorrect format"

        testCase "FormatTest9" <| fun () ->   
            let t1 = parseTerm "<<(a * b) --> c> ==> <d --> e>>"
            let q1 = parseTerm "<?what ==> <d --> e>>"
            let expected = true
            Expect.equal  (unifies t1 q1) expected "Incorrect format"
    ]
