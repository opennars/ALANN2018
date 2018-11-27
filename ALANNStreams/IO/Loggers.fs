module Loggers

open Akka.Streams.Dsl
open Akkling.Streams
open Types
open System.Diagnostics
open TermFormatters
open System
open PrintUtils
open Params

let logTimer1 = Stopwatch()
logTimer1.Start()
let mutable k = 0
let mutable log1_time = 0L

let log1 (eb : EventBelief) = 
    k <- k + 1
    if k % Params.EVENTS_PROCESSED_MOD = 0 then
        let now = logTimer1.ElapsedMilliseconds
        printfn "FROM TERMSTREAMS: %d took %dms " k (now - log1_time)
        cprintf ConsoleColor.Red "[%f] " eb.AV.STI
        cprintf ConsoleColor.Yellow "%s " (match eb.Event.TV with | Some tv -> truth tv | _ -> "None")
        cprintf ConsoleColor.Green "%s " (ft eb.Event.Term)
        cprintf ConsoleColor.Gray "%s \n" (Trail eb.Event.Stamp.Evidence)
        //timer1.Restart()
        log1_time <- now
    eb

//let timer2 = Stopwatch()
//timer2.Start()
let mutable i = 0
let mutable log2_time = 0L
let log2 e = 
    i <- i + 1
    if i % Params.INFERENCES_PROCESSED_MOD = 0 then
        let now = logTimer1.ElapsedMilliseconds
        //printfn "****DERIVER: %d took %dms " i (now - log2_time)
        //timer2.Restart()
        log2_time <- now

    e

//let timer3 = Stopwatch()
//timer3.Start()
let mutable l = 0
let mutable log3_time = 0L
let log3 (e : Event) = 
    l <- l + 1
    if l % Params.EVENTS_PROCESSED_MOD = 0 then
        let now = logTimer1.ElapsedMilliseconds
        printfn "FROM SEQUENCER: %d took %dms " l (now - log3_time)
        cprintf ConsoleColor.Red "[%f] " e.AV.STI
        cprintf ConsoleColor.Yellow "%s " (match e.TV with | Some tv -> truth tv | _ -> "None")
        cprintf ConsoleColor.Green "%s " (ft e.Term)
        cprintf ConsoleColor.Gray "%s \n" (Trail e.Stamp.Evidence)
        //timer3.Restart()
        log3_time <- now
    e

let eventBeliefLogger = Flow.Create<EventBelief>() |> Flow.map log1
let logger2 = Flow.Create<Event>() |> Flow.map log2            
let eventLogger = Flow.Create<Event>() |> Flow.map log3
