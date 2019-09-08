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

module TermFormattersTests

open TermFormatters
//open ParserCombinators
//open Parser2
open Parser
open FParsec
open Expecto 

let parseTerm str = 
    match testp (ws >>. pterm) str with
    | Some x -> x
    | None -> failwith "error"

[<Tests>]
let test1 =
    testList "TermFormatTests" [
        testCase "TermFormatTest1" <| fun () ->            
            let expected = "cat"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest2" <| fun () ->            
            let expected = "<a --> b>"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest3" <| fun () ->            
            let expected = "<a <-> b>"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest4" <| fun () ->            
            let expected = "<<a --> b> ==> <b --> c>>"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest5" <| fun () ->            
            let expected = "<<a --> b> <=> <b --> c>>"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest6" <| fun () ->            
            let expected = "<<a --> b> =+> <b --> c>>"
            let term = parseTerm expected
            Expect.containsAll (ft term) expected "Incorrect format"

        testCase "TermFormatTest7" <| fun () ->            
            let expected = "<<a --> b> =-> <b --> c>>"
            let term = parseTerm expected
            Expect.containsAll (ft term) expected "Incorrect format"

        testCase "TermFormatTest8" <| fun () ->            
            let expected = "<<a --> b> =|> <b --> c>>"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest9" <| fun () ->            
            let expected = "<<a --> b> <|> <b --> c>>"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest10" <| fun () ->            
            let expected = "<<a --> b> <+> <b --> c>>"
            let term = parseTerm expected
            Expect.containsAll (ft term) expected "Incorrect format"

        testCase "TermFormatTest11" <| fun () ->            
            let expected = "(a && b)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest12" <| fun () ->            
            let expected = "(a || b)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest13" <| fun () ->            
            let expected = "(a & b)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest14" <| fun () ->            
            let expected = "(a | b)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest15" <| fun () ->            
            let expected = "(a - b)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest16" <| fun () ->            
            let expected = "(a ~ b)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest17" <| fun () ->            
            let expected = "(a * b)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest18" <| fun () ->            
            let expected = "(a / b _)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest19" <| fun () ->            
            let expected = "(a \\ b _)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest20" <| fun () ->            
            let expected = "{a b c d}"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest21" <| fun () ->            
            let expected = "[a b c d]"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest22" <| fun () ->            
            let expected = "(a; b)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"

        testCase "TermFormatTest23" <| fun () ->            
            let expected = "(a, b)"
            let term = parseTerm expected
            Expect.equal (ft term) expected "Incorrect format"
    ]
