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

module TemporalInference

open Types
open TruthFunctions
open TermUtils
open Factories
open EventStore
open Evidence
open TermFormatters
open SystemState

let store = EventStore(20) :> IEventStore

let makeEvent2 e1 e2 (term, tv) =
    let stamp1 = e1.Stamp
    let stamp2 = e2.Stamp
    let now = SystemTime()
    let stamp = {Created = now
                 SC = syntacticComplexity term 
                 Evidence = merge stamp1.Evidence stamp2.Evidence
                 LastUsed = now
                 UseCount = 0L
                 Source = Derived}

    {EventType = EventType.Belief; Term = term; TV = Some tv; AV = e1.AV; Stamp = stamp; Solution = None}

let temporalInf e2 e1 =
    let pstamp = e1.Stamp
    let sstamp = e2.Stamp

    let nal7_temporal_inference = function
        | p, s when isConcurrent pstamp sstamp && isNotImpOrEqu p && isNotImpOrEqu s  -> [(Term(ConImp, [s; p]), temporal_ind)
                                                                                          (Term(ConImp, [p; s]), temporal_abd)
                                                                                          (Term(ConEqu, [s; p]), temporal_com)
                                                                                          (Term(Par, [s; p]), temporal_int)]

        | p, s when isAfter pstamp sstamp && isNotImpOrEqu p && isNotImpOrEqu s -> [(Term(PreImp, [s; p]), temporal_ind)
                                                                                    (Term(RetImp, [p; s]), temporal_abd)
                                                                                    (Term(PreEqu, [s; p]), temporal_com)
                                                                                    (Term(Seq, [s; p]), temporal_int)]

        | p, s when isBefore pstamp sstamp && isNotImpOrEqu p && isNotImpOrEqu s -> [(Term(PreImp, [p; s]), temporal_ind)
                                                                                     (Term(RetImp, [s; p]), temporal_abd)
                                                                                     (Term(PreEqu, [p; s]), temporal_com)
                                                                                     (Term(Seq, [p; s]), temporal_int)]

        | _ -> []
    
    let processEvent e1 e2 = 
        match e1.EventType, e2.EventType with
        | Belief, Belief -> 
            nal7_temporal_inference(e1.Term, e2.Term) 
            |> List.map (fun (term, tf1) -> makeEvent2 e1 e2 (term, (tf1(e1.TV.Value, e2.TV.Value))))
        | _ -> []

    processEvent e1 e2

let sequenced = ref 0

let sequencer (e1 : Event) =
    let f e =
        store.Push(e)
         //do inf
        if nonOverlap e1.Stamp.Evidence e.Stamp.Evidence then
            e1::temporalInf e1 e
        else
            [e1]
    
    let events = store.GetAll()
    store.Reset()

    let results =
        Array.map f events
        |> Seq.concat
        
    store.Push(e1)            

    //Seq.iter (fun e -> printfn "%s" (formatEvent e)) results
    results