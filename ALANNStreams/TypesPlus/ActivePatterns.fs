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

module ActivePatterns

open Types

let (|Inh|_|) = function | Term(Inh, [a; b]) -> Some(Inh(a, b)) | _ -> None 
let (|Sim|_|) = function | Term(Sim, [a; b]) -> Some(Sim(a, b)) | _ -> None
let (|ExtSet|_|) = function | Term(ExtSet, lst) -> Some(ExtSet(lst)) | _ -> None
let (|IntSet|_|) = function | Term(IntSet, lst) -> Some(IntSet(lst)) | _ -> None
let (|ExtInt|_|) = function | Term(ExtInt, lst) -> Some(ExtInt(lst)) | _ -> None
let (|IntInt|_|) = function | Term(IntInt, lst) -> Some(IntInt(lst)) | _ -> None
let (|ExtDif|_|) = function | Term(ExtDif, [a; b]) -> Some(ExtDif(a, b)) | _ -> None
let (|IntDif|_|) = function | Term(IntDif, [a; b]) -> Some(IntDif(a, b)) | _ -> None
let (|Prod|_|) = function | Term(Prod, [a; b]) -> Some(Prod(a, b)) | _ -> None
let (|ExtImg|_|) = function | Term(ExtImg, [a; b; c]) -> Some(ExtImg(a, b, c)) | _ -> None
let (|IntImg|_|) = function | Term(IntImg, [a; b; c]) -> Some(IntImg(a, b, c)) | _ -> None
let (|Not|_|) = function | Term(Not, [a]) -> Some(Not(a)) | _ -> None
let (|Imp|_|) = function | Term(Imp, [a; b]) -> Some(Imp(a, b)) | _ -> None
let (|ConImp|_|) = function | TemporalTerm(ConImp, [a; b], _) -> Some(ConImp(a, b)) | _ -> None 
let (|Equ|_|) = function | Term(Equ, [a; b]) -> Some(Equ(a, b)) | _ -> None
let (|RetImp|_|) = function | TemporalTerm(RetImp, [a; b], i) -> Some(RetImp(a, b, i)) | _ -> None 
let (|PreImp|_|) = function | TemporalTerm(PreImp, [a; b], i) -> Some(PreImp(a, b, i)) | _ -> None 
let (|PreEqu|_|) = function | TemporalTerm(PreEqu, [a; b], i) -> Some(PreEqu(a, b, i)) | _ -> None 
let (|ConEqu|_|) = function | TemporalTerm(ConEqu, [a; b], i) -> Some(ConEqu(a, b, 0s)) | _ -> None 
let (|And|_|) = function | Term(And, lst) -> Some(And(lst)) | _ -> None
let (|Or|_|) = function | Term(Or, lst) -> Some(Or(lst)) | _ -> None
let (|Par|_|) = function | TemporalTerm(Par, [a; c], _) -> Some(Par(a, c, 0s)) | _ -> None
let (|Seq|_|) = function | TemporalTerm(Seq, [a; c], i) -> Some(Seq(a, c, i)) | _ -> None
let (|Oper|_|) = function | Term(Oper, lst) -> Some(Oper(lst)) | _ -> None
let (|And2|_|) t1 t2 = function | Term(OpCode.And, lst) when List.contains t1 lst && List.contains t2 lst -> Some(And2(lst)) | _ -> None
let (|And1|_|) = function | Term(OpCode.And, t1::lst) when List.contains t1 lst -> Some(And1(lst)) | _ -> None
