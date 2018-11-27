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

module Choice

open Types
open TermUtils
open Unify
open TruthFunctions
open TermFormatters
open Akkling
open ALANNSystem

let maxTV (b1 : Belief) (b2 : Belief) = 
    let cond1 = b1.TV.C > b2.TV.C 
    let cond2 = (b1.TV.C = b2.TV.C) && (b1.TV.F > b2.TV.F)
    match cond1 || cond2 with
    | true -> b1
    | false -> b2

let choice state (event : Event) =
    let matcher t = match isSelective t with | true -> unifies | _ -> (=)

    let matches = 
        [for belief in state.Beliefs -> belief]
        |> List.filter (fun b -> (matcher event.Term) b.Term event.Term)

    match matches with
    | [] -> None
    | lst when isSelective event.Term -> Some(List.maxBy (fun (b : Belief) -> expectation b.TV) lst)
    | lst -> Some <| List.reduce maxTV lst

let doChoice state event =
    match event.EventType with
    | Question -> choice state event
    | _ -> None

let tryPrintAnswer e (b : Belief) =
    if e.Stamp.Source = User then
        let msg =  sprintf "Unifies %s %s %s %f %s " (ft e.Term) (ft b.Term) (truth b.TV) e.AV.STI (Trail b.Stamp.Evidence)
        printActor <! PrintMessage msg
    
let chosen state event =
    match doChoice state event with
    | Some b -> 
        tryPrintAnswer event b
        Some b
    | None -> None
