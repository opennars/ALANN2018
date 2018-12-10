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

module TermUtilsTests

open Types
open TermUtils
open Expecto

[<Tests>]
let test1 =
    testList "SubtermTests" [
        testCase "SubtermTest1" <| fun () ->
            let t1 = Term(PreImp, [Term(Inh, [Term(Prod, [Word "not_included"; Var(IVar, "2")]); Word "smaller"]); Term(Inh, [Term(Prod, [Var(IVar, "2"); Var(IVar, "1")]); Word "larger"])])
            let expected = [Term(PreImp, [Term(Inh, [Term(Prod, [Word "not_included"; Var(IVar, "2")]); Word "smaller"]); Term(Inh, [Term(Prod, [Var(IVar, "2"); Var(IVar, "1")]); Word "larger"])]);
                            Term(Inh, [Term(Prod, [Word "not_included"; Var(IVar, "2")]); Word "smaller"]);
                            Term(Prod, [Word "not_included"; Var(IVar, "2")]);
                            Word "smaller";
                            Term(Inh, [Term(Prod, [Var(IVar, "2"); Var(IVar, "1")]); Word "larger"]);
                            Term(Prod, [Var(IVar, "2"); Var(IVar, "1")]);
                            Word "larger"]
            Expect.equal expected (terms t1) "Incorrect subterms"

        testCase "SubtermTest2" <| fun () ->
            let t1 = Term(Inh, [Term(ExtSet, [Word "a"; Word "b"; Word "c"]); Word "letters"])
            let expected = [Term(Inh, [Term(ExtSet, [Word "a"; Word "b"; Word "c"]); Word "letters"]);
                            Term(ExtSet, [Word "a"; Word "b"; Word "c"]);
                            Word "a";
                            Word "b";
                            Word "c";
                            Word "letters"]
            Expect.equal expected (terms t1) "Incorrect subterms"

        testCase "SubtermTest3" <| fun () ->
            let t1 = Term(Inh, [Word "a"; Word "b"])
            let expected = [Term(Inh, [Word "a"; Word "b"]);
                            Word "a";
                            Word "b"]
            Expect.equal expected (terms t1) "Incorrect subterms"

        testCase "SubtermTest4" <| fun () ->
            let t1 = Word "a"
            let expected = [Word "a"]
            Expect.equal expected (terms t1) "Incorrect subterms"

        testCase "SubtermTest5" <| fun () ->
            let t1 = Var(QVar, "what")
            let expected = []
            Expect.equal expected (terms t1) "Incorrect subterms"
    ]
