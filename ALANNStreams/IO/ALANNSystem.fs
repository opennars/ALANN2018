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

module ALANNSystem

open Akkling
open Akkling.Streams
open Types
open System.Net
open Network

let system = System.create "ALANNLobe" <| Configuration.defaultConfig()    
let mat = system.Materializer()

let behavior3 (m:Actor<_>) =
    let rec loop () = actor {
        let! msg = m.Receive ()
        match msg with
        | PrintMessage str ->
            System.Console.WriteLine str
        | _ -> failwith "Unexpected message in print actor"   // This rule is never matched
        return! loop ()
    }
    loop ()
    
let printActor  =
    outSocketUI.Connect(clientEndPoint)
    spawn system "Printer" <| props behavior3 

let myprintfn msg = 
    printActor <! PrintMessage msg


