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

module Loggers

open Akka.Streams.Dsl
open Akkling.Streams
open Types
open System.Diagnostics
open TermFormatters
open System
open PrintUtils
open System.Collections.Concurrent

let logTimer1 = Stopwatch()
logTimer1.Start()
let mutable k = 0
let mutable log1_time = 0L

let mutable i = 0L
let mutable log2_time = 0L
let log2 e = 
    i <- i + 1L
    if i % Params.INFERENCES_PROCESSED_MOD = 0L then
        let now = logTimer1.ElapsedMilliseconds
        let duration = now - log2_time
        printfn "****Derivations  %d/s" ((i / duration) * 1000L)
        log2_time <- now
        i <- 0L
    e

let numConcepts () =
    let rec loop acc n =
        match n with
        | 0 -> acc + stores.[n].Count
        | _ -> loop (acc + stores.[n].Count) (n - 1)

    loop 0 (stores.GetLength(0) - 1)


let showSelectedConcepts() =
    for store in stores do
        for kvp in store do
            let node = kvp.Value
            if node.Beliefs.Count < 3 then
                [for b in node.Beliefs.Beliefs() -> b]
                |> List.iter (fun b -> printfn "Node %s [%f] %s {%s} %s" (ft node.Term) node.AV.STI (ft b.Term) (truth b.TV) (Trail b.Stamp.Evidence))

let mutable l = 0L
let mutable log3_time = 0L
let log3 (e : Event) = 
    l <- l + 1L
    if l % Params.EVENTS_PROCESSED_MOD = 0L then

        printfn "Number of concepts = %d" (numConcepts())
        let now = logTimer1.ElapsedMilliseconds
        let duration = now - log3_time
        printfn "Cycled events %d/s" ((l / duration) * 1000L)
        cprintf ConsoleColor.Red "[%f] " e.AV.STI
        cprintf ConsoleColor.Yellow "%s " (match e.TV with | Some tv -> truth tv | _ -> "None")
        cprintf ConsoleColor.Green "%s " (ft e.Term)
        cprintf ConsoleColor.Gray "%s \n" (Trail e.Stamp.Evidence)
        log3_time <- now
        l <- 0L
        //let term = Term(Inh, [Term(Prod, [Word "cat"; Term(IntSet, [Word "blue"])]); Word "likes"])
        //let term = Term(Inh, [Term(ExtSet, [Word "tom"]); Term(IntSet, [Word "living"])])
        //let term = Term(Inh, [Word "animal"; Term(IntSet, [Word "living"])])
        //let term = Term(ExtSet, [Word "tom"])
        //let term = Word "cod"
        //let i =  abs(term.GetHashCode() % Params.NUM_TERM_STREAMS )
        //for node in stores.[i].Values do
        //    if node.Term = term then
        //        printfn "Node = %s [%d]" (ft node.Term) node.Beliefs.Count
        //        printfn "Event = %s %A [%A]" (ft e.Term) e.EventType e.Stamp.Evidence
        //        for belief in node.Beliefs.Beliefs() do
        //            printfn "%s {%f %f} %A" (ft belief.Term) belief.TV.F belief.TV.C belief.Stamp.Evidence
        //showSelectedConcepts()
    e

let logger2 = Flow.Create<Event>() |> Flow.map log2            
let eventLogger = Flow.Create<Event>() |> Flow.map log3

