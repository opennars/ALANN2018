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

module ParserTests

open Types
open Parser
open Expecto

[<Tests>]
let tests =
    testList "BudgetTests" [
        testCase "BudgetTest1" <| fun () ->
            let expected = Some {STI = 1.0f; LTI = 0.99f}
            Expect.equal (testp pav_ws "[1.0 0.99]") expected "Incorrect budget"

        testCase "BudgetTest2" <| fun () ->
            let expected = None
            Expect.notEqual (testp pav_ws "[0.0 0.99]") expected "Incorrect budget"

        testCase "BudgetTest3" <| fun () ->
            let expected = Some {STI = 1.0f; LTI = 0.9999f}
            Expect.equal (testp pav_ws "[1.000000  0.9999] ") expected "Incorrect budget"
    ]

[<Tests>]
let test1 =
    testList "TVTests" [
        testCase "TVTest1" <| fun () ->
            let expected = Some {F = 1.0f; C = 0.99f}
            Expect.equal (testp ptruth_ws "{1.0 0.99}") expected "Incorrect TV"

        testCase "TVTest2" <| fun () ->
            let expected = Some {F = 0.0f; C = 0.99f}
            Expect.notEqual (testp ptruth_ws "{1.0 0.99}") expected "Incorrect TV"

        testCase "TVTest3" <| fun () ->
            let expected = Some {F = 1.0f; C = 0.9999f}
            Expect.equal (testp ptruth_ws "{1.000000  0.9999} ") expected "Incorrect TV"
    ]

[<Tests>]
let test1a =
    testList "WordTests" [
        testCase "WordTest1" <| fun () ->
            let expected = Some (Word "a")
            Expect.equal (testp pword_ws " a ") expected "Incorrect Word"

        testCase "WordTest2" <| fun () ->
            let expected = Some (Word "1")
            Expect.equal (testp pword_ws " 1 ") expected "Incorrect Word"

        testCase "WordTest3" <| fun () ->
            let expected = Some (Word "_")
            Expect.equal (testp pword_ws " _ ") expected "Incorrect Word"

        testCase "WordTest4" <| fun () ->
            let expected = Some (Word "1_a")
            Expect.equal (testp pword_ws " 1_a ") expected "Incorrect Word"

        testCase "WordTest5" <| fun () ->
            let expected = Some (Word "abcdefghijklmnopqrstuvwxyz")
            Expect.equal (testp pword_ws " abcdefghijklmnopqrstuvwxyz ") expected "Incorrect long Word"
    ]

[<Tests>]
let tests4 =
    // test in sequqnce to avoid single threading in parser with var renaming
    testSequenced  <| 
        testList "VariableTests" [
            testCase "VariableTest1" <| fun () ->
                let expected = Some (Var(QVar, "1"))
                Expect.equal (testp pvariable_ws " ?what ") expected "Incorrect variable"

            testCase "VariableTest2" <| fun () ->
                let expected = Some (Var(QVar, "1"))
                Expect.equal (testp pvariable_ws " ?1 ") expected "Incorrect variable"

            testCase "VariableTest3" <| fun () ->
                let expected = Some (Var(DVar, "1"))
                Expect.equal (testp pvariable_ws " #1 ") expected "Incorrect variable"

            testCase "VariableTest4" <| fun () ->
                let expected = Some (Var(IVar, "1"))
                Expect.equal (testp pvariable_ws " $cat ") expected "Incorrect variable"

            testCase "VariableTest5" <| fun () ->
                let expected = Some (Var(IVar, "1"))
                Expect.equal (testp pvariable_ws " $1 ") expected "Incorrect variable"
        ]

[<Tests>]
let tests5a =
    testList "SetTests" [
        testCase "SetTest1" <| fun () ->
            let expected = Some (Term(ExtSet, [Word "1"; Word "2"]))
            Expect.equal (testp pset_ws "{ 2  1 } ") expected "Incorrect extSet"

        testCase "SetTest2" <| fun () ->
            let expected = Some (Term(IntSet, [Word "1"; Word "2"]))
            Expect.equal (testp pset_ws "[ 2 1 ] ") expected "Incorrect intSet"
    ]

[<Tests>]
let tests5 =
    testList "CompoundTermTests" [
        testCase "CompoundTermTest3" <| fun () ->
            let expected = Some (Term(And, [Word "1"; Word"2"]))
            Expect.equal (testp pcompound_term_ws " ( 1 && 2 ) ") expected "Incorrect conjunction"

        testCase "CompoundTermTest4" <| fun () ->
            let expected = Some (Term(Or, [Word "1"; Word"2"]))
            Expect.equal (testp pcompound_term_ws " ( 1 || 2 ) ") expected "Incorrect disjunction"

        testCase "CompoundTermTest5" <| fun () ->
            let expected = Some (Term(Prod, [Word "1"; Word "2"]))
            Expect.equal (testp pcompound_term_ws " ( 1 * 2 ) ") expected "Incorrect product"

        testCase "CompoundTermTest5a" <| fun () ->
            let expected = Some (Term(Prod, [Word "1"; Term(Prod, [Word "2"; Word "3"])]))
            Expect.equal (testp pcompound_term_ws " ( 1 * ( 2 * 3 ) ) ") expected "Incorrect nested product"

        testCase "CompoundTermTest6" <| fun () ->
            let expected = Some (Term(Par, [Word "a"; Word "b"]))
            Expect.equal (testp pcompound_term_ws " ( a ; b ) ") expected "Incorrect par"

        testCase "CompoundTermTest6a" <| fun () ->
            let expected = Some (Term(Par, [Word "b"; Term(Par, [Word "a"; Word "c"])]))
            Expect.equal (testp pcompound_term_ws " ( b ; ( a ; c ) ) ") expected "Incorrect nested par"

        testCase "CompoundTermTest7" <| fun () ->
            let expected = Some (Term(Seq, [Word "a"; Word "b"]))
            Expect.equal (testp pcompound_term_ws " ( a , b ) ") expected "Incorrect seq"

        testCase "CompoundTermTest7a" <| fun () ->
            let expected = Some (Term(Seq, [Word "a"; Word "b"]))
            Expect.equal (testp pcompound_term_ws "(a, b)") expected "Incorrect seq"

        testCase "CompoundTermTest7b" <| fun () ->
            let expected = Some (Term(Seq, [Word "a"; Term(Seq, [Word "b"; Word "c"])]))
            Expect.equal (testp pcompound_term_ws " ( a , ( b , c) ) ") expected "Incorrect nested seq"

        testCase "CompoundTermTest7c" <| fun () ->
            let expected = Some (Term(Seq, [Word "a"; Term(Seq, [Word "b"; Word "c"])]))
            Expect.equal (testp pcompound_term_ws "(a,(b,c))") expected "Incorrect nested seq"

        testCase "CompoundTermTest7d" <| fun () ->
            let expected = Some (Term(Seq, [Term(Seq, [Word "a"; Word "b"]); Word "c"]))
            Expect.equal (testp pcompound_term_ws "((a,b),c)") expected "Incorrect nested seq"

        testCase "CompoundTermTest8" <| fun () ->
            let expected = Some (Term(IntInt, [Word "a"; Word "b"]))
            Expect.equal (testp pcompound_term_ws " ( a | b ) ") expected "Incorrect intInt"

        testCase "CompoundTermTest9" <| fun () ->
            let expected = Some (Term(ExtInt, [Word "a"; Word "b"]))
            Expect.equal (testp pcompound_term_ws " ( a & b ) ") expected "Incorrect extInt"

        testCase "CompoundTermTest10" <| fun () ->
            let expected = Some (Term(ExtImg, [Word "a"; Word "b"; Word "_"]))
            Expect.equal (testp pcompound_term_ws " ( a / b _ ) ") expected "Incorrect extImg"

        testCase "CompoundTermTest11" <| fun () ->
            let expected = Some (Term(IntImg, [Word "a"; Word "b"; Word "_"]))
            Expect.equal (testp pcompound_term_ws " ( a \  b _ ) ") expected "Incorrect intImg"

        testCase "CompoundTermTest12" <| fun () ->
            let expected = Some (Term(ExtDif, [Word "a"; Word "b"]))
            Expect.equal (testp pcompound_term_ws " ( a - b ) ") expected "Incorrect extInt"

        testCase "CompoundTermTest13" <| fun () ->
            let expected = Some (Term(IntDif, [Word "a"; Word "b"]))
            Expect.equal (testp pcompound_term_ws " ( a ~ b ) ") expected "Incorrect extInt"

        testCase "CompoundTermTest14" <| fun () ->
            let expected = Some (Term(Par, [Term(Inh, [Word "a"; Word "b"]); Term(Inh, [Word "b"; Word "c"])]))
            Expect.equal (testp pcompound_term_ws " (<a --> b> ; <b --> c>) ") expected "Incorrect par term"

        testCase "CompoundTermTest15" <| fun () ->
            let t1 = Term (Seq, [Term(Inh, [Word "a"; Word "b"]); Term(Inh, [Word "b"; Word "c"])] )
            let t2 = Term (Seq, [Term(Inh, [Word "a"; Word "b"]); Term(Inh, [Word "b"; Word "c"])] )
            let expected = Some (Term(Seq, [t1; t2]))
            Expect.equal (testp pcompound_term_ws " ((<a --> b> , <b --> c>) , (<a --> b> , <b --> c>)) ") expected "Incorrect par term"

        testCase "CompoundTermTest16" <| fun () ->
            let expected = Some (Term(Seq, [Term(Inh, [Word "a"; Word "b"]); Term(Inh, [Word "b"; Word "c"])]))
            Expect.equal (testp pcompound_term_ws " (<a --> b> , <b --> c>) ") expected "Incorrect seq term"
    ]

[<Tests>]
let tests6 =
    testList "StatementExprTests" [
        testCase "StatementExprTest1" <| fun () ->
            let expected = Some (Term(Inh, [Word "a"; Word "b"]))
            Expect.equal (testp statement " < a --> b > ") expected "Incorrect inh"

        testCase "StatementExprTest2" <| fun () ->
            let expected = Some (Term(Sim, [Word "a"; Word "b"]))
            Expect.equal (testp statement " < a <-> b > ") expected "Incorrect sim"

        testCase "StatementExprTest3" <| fun () ->
            let expected = Some (Term(Inh, [Term(ExtSet, [Word "a"]); Word "b"]))
            Expect.equal (testp statement " < a {-- b > ") expected "Incorrect instance"

        testCase "StatementExprTest4" <| fun () ->
            let expected = Some (Term(Inh, [Word "a"; Term(IntSet, [Word "b"])]))
            Expect.equal (testp statement " < a --] b > ") expected "Incorrect property"

        testCase "StatementExprTest5" <| fun () ->
            let expected = Some (Term(Inh, [Term(ExtSet, [Word "a"]); Term(IntSet, [Word "b"])]))
            Expect.equal (testp statement " < a {-] b > ") expected "Incorrect instProp"

        testCase "StatementExprTest6" <| fun () ->
            let expected = Some (Term(Imp, [Word "a"; Word "b"]))
            Expect.equal (testp statement " < a ==> b > ") expected "Incorrect imp"

        testCase "StatementExprTest7" <| fun () ->
            let expected = Some (Term(Equ, [Word "a"; Word "b"]))
            Expect.equal (testp statement " < a <=> b > ") expected "Incorrect equ"

        testCase "StatementExprTest8" <| fun () ->
            let expected = Some (Term(ConEqu, [Word "a"; Word "b"]))
            Expect.equal (testp statement " < a <|> b > ") expected "Incorrect conEqu"

        testCase "StatementExprTest9" <| fun () ->
            let expected = Some (Term(PreEqu, [Word "a"; Word "b"]))
            Expect.equal (testp statement " < a <+> b > ") expected "Incorrect preEqu"

        testCase "StatementExprTest10" <| fun () ->
            let expected = Some (Term(PreImp, [Word "a"; Word "b"]))
            Expect.equal (testp statement " < a =+> b > ") expected "Incorrect preImp"

        testCase "StatementExprTest11" <| fun () ->
            let expected = Some (Term(ConImp, [Word "a"; Word "b"]))
            Expect.equal (testp statement " < a =|> b > ") expected "Incorrect conImp"

        testCase "StatementExprTest12" <| fun () ->
            let expected = Some (Term(RetImp, [Word "a"; Word "b"]))
            Expect.equal (testp statement " < a =-> b > ") expected "Incorrect retImp"
    ]

[<Tests>]
let tests7 =
    testList "TermTests" [
        testCase "TermTest1" <| fun () ->
            let expected = Some (Word "cat")
            Expect.equal (testp pterm " cat ") expected "Incorrect Word"

        testCase "TermTest2" <| fun () ->
            let expected = Some (Var(QVar, "1"))
            Expect.equal (testp pterm " ?what ") expected "Incorrect variable"

        testCase "TermTest3" <| fun () ->
            let expected = Some (Term(Not, [Word "cat"]))
            Expect.equal (testp pterm " -- cat ") expected "Incorrect compoundTerm"

        testCase "TermTest4" <| fun () ->
            let expected = Some (Term(Not, [Word "cat"]))
            Expect.equal (testp pterm "--cat ") expected "Incorrect compoundTerm"

        testCase "TermTest5" <| fun () ->
            let expected = Some (Term(Not, [Word "cat"]))
            Expect.equal (testp pterm " -- cat ") expected "Incorrect compoundTerm"

        testCase "TermTest6" <| fun () ->
            let expected = Some (Term(Inh, [Word "cat"; Word "animal"]))
            Expect.equal (testp pterm " < cat --> animal > ") expected "Incorrect statementExpr"
    ]

[<Tests>]
let tests8 =
    testList "StatementTests" [
        testCase "StatementTest1" <| fun () ->
            let expected = Some (Term(Imp, [Term(Inh, [Word "a"; Word "b"]); Term(Sim, [Word "b"; Word "c"])]))
            Expect.equal (testp statement " <<a --> b> ==> <b <-> c>> ") expected "Incorrect statementExpr"
    ]

[<Tests>]
let tests9 =
    testList "SentenceTests" [
        testCase "SentenceTest1" <| fun () ->
            let expected = Some {EventType = Belief; Term = Term(Inh, [Word "a"; Word "b"]); TV = Some {F = 1.0f; C = 0.9f}}
            Expect.equal (testp psentence_ws " <a --> b>.") expected "Incorrect belief no options"

        testCase "SentenceTest2" <| fun () ->
            let expected = Some {EventType = Belief; Term = Term(Inh, [Word "a"; Word "b"]); TV = Some {F = 1.0f; C = 0.9f}}
            Expect.equal (testp psentence_ws " <a --> b>. ") expected "Incorrect belief with present tense"

        testCase "SentenceTest3" <| fun () ->
            let expected = Some {EventType = Belief; Term = Term(Inh, [Word "a"; Word "b"]); TV = Some {F = 0.5f; C = 0.75f}}
            Expect.equal (testp psentence_ws " <a --> b>. {0.5 0.75}") expected "Incorrect belief with truth"

        testCase "SentenceTest4" <| fun () ->
            let expected = Some {EventType = Belief; Term = Term(Inh, [Word "a"; Word "b"]); TV = Some {F = 0.5f; C = 0.75f}}
            Expect.equal (testp psentence_ws " <a --> b>. {0.5 0.75}") expected "Incorrect belief with truth and interval tense"

        testCase "SentenceTest5" <| fun () ->
            let expected = Some {EventType = Question; Term = Term(Inh, [Word "a"; Word "b"]); TV = None}
            Expect.equal (testp psentence_ws " <a --> b>?") expected "Incorrect questions no options"

        testCase "SentenceTest6" <| fun () ->
            let expected = Some {EventType = Question; Term = Term(Inh, [Word "a"; Word "b"]); TV = None}
            Expect.equal (testp psentence_ws " <a --> b>? ") expected "Incorrect questions with tense"

        testCase "SentenceTest7" <| fun () ->
            let expected = Some {EventType = Question; Term = Term(Inh, [Var(QVar, "1"); Word "b"]); TV = None}
            Expect.equal (testp psentence_ws " <?what --> b>? ") expected "Incorrect questions with tense"

        testCase "SentenceTest8" <| fun () ->
            let expected = Some {EventType = Goal; Term = Term(Inh, [Word "a"; Word "b"]); TV = Some {F =1.0f; C = 0.9f}}
            Expect.equal (testp psentence_ws " <a --> b>!") expected "Incorrect goal"

        testCase "SentenceTest9" <| fun () ->
            let expected = Some {EventType = Goal; Term = Word "a"; TV = Some {F =1.0f; C = 0.9f}}
            Expect.equal (testp psentence_ws " a!") expected "Incorrect goal"

        testCase "SentenceTest10" <| fun () ->
            let expected = Some {EventType = Quest; Term = Term(Inh, [Word "a"; Word "b"]); TV = None}
            Expect.equal (testp psentence_ws " <a --> b>@") expected "Incorrect quest"

        testCase "SentenceTest11" <| fun () ->
            let expected = Some {EventType = Quest; Term = Word "a"; TV = None}
            Expect.equal (testp psentence_ws " a@") expected "Incorrect quest"
    ]
