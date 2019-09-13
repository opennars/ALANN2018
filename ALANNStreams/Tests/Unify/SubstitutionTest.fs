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

module SubstitutionTest

open Unify
open Parser
open Expecto

let parseStatement str = 
    vNum <- 0; vMap.Clear(); 
    match testp pstatement str with
    | Some x -> x
    | None -> failwith "error"

[<Tests>]
let test1 =
    testSequenced <| 
        testList "SubstitutionTests" [
            testCase "SubstitutionTest1" <| fun () ->   
                let t1 = parseStatement "<{Tweety} --> [with_wings]>"
                let t2 = parseStatement "<$Y --> [with_wings]>"         
                let t3 = parseStatement "<(<$x --> [chirping]> && <$x --> [with_wings]>) ==> <$x --> bird>>"
                let expected = parseStatement "<(<{Tweety} --> [chirping]> && <{Tweety} --> [with_wings]>) ==> <{Tweety} --> bird>>"
                Expect.equal  (substUnify t3 t1 t2) expected "Incorrect format"  

            testCase "SubstitutionTest2" <| fun () ->   
                let t1 = parseStatement "<{Tweety} --> [with_wings]>"
                let t2 = parseStatement "<#Y --> [with_wings]>"         
                let t3 = parseStatement "<(<#Y --> [chirping]> && <#Y --> [with_wings]>) ==> <{Tweety} --> bird>>"
                let expected = parseStatement "<(<{Tweety} --> [chirping]> && <{Tweety} --> [with_wings]>) ==> <{Tweety} --> bird>>"
                Expect.equal  (substUnify t3 t1 t2) expected "Incorrect format"  

            testCase "SubstitutionTest3" <| fun () ->   
                let t1 = parseStatement "(<{($a * 0)} --> tokenAtEnum> && <{($b * 3)} --> tokenAtEnum>)"
                let t2 = parseStatement "(<{((frogo & ta) * 0)} --> #1> && <{((to & te) * 3)} --> #1>)"         
                let t3 = parseStatement "(<{($a * 0)} --> tokenAtEnum> && <{($b * 3)} --> tokenAtEnum>)"
                let expected = parseStatement "(<{((frogo & ta) * 0)} --> tokenAtEnum> && <{((to & te) * 3)} --> tokenAtEnum>)"
                Expect.equal  (substUnify t3 t1 t2) expected "Incorrect format"  
        ]
