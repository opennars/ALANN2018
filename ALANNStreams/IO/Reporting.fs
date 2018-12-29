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

module Reporting

open System.Collections.Concurrent
open System.Threading
open ALANNSystem
open Types
open Akkling
open TermUtils
open TermFormatters

let answerDict = ConcurrentDictionary<string, string>()

let displayAnswer answer =    
    match answerDict.ContainsKey answer with
    | false ->
        match answerDict.TryAdd(answer, answer) with
        | true -> printActor <! PrintMessage (sprintf "?%s" answer)
        | _ -> ()
    | _ -> ()

let updateStatus() =
    myprintfn (sprintf ":Cycle [%d]" !cycle)
    myprintfn ":Status: ALANN Server Running"
    myprintfn (sprintf ":Events %d/s" (Interlocked.Exchange(eventsPerSecond, 0L)))            


let showTrace state e =
    let activationType (e : Event) = if isTemporal e.Term then "TEMPORAL" else "GENERAL"
    let msg1 = sprintf "TRACING NODE '%s': ACTIVATED WITH %s ATTENTION %.2f" (ft state.Term) (activationType e) state.Attention
    let msg2 = formatEvent e
    printActor <! PrintMessage msg1
    printActor <! PrintMessage msg2
