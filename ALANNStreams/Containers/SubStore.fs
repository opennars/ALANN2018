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

module SubStore

open C5
open Types
open TermUtils

type ISubStore =
    abstract Contains : Key -> bool
    abstract Insert : Key * Belief -> unit
    abstract Update : Key * Belief -> unit
    abstract TryGetValue : Key -> Belief option
    abstract Clear : unit -> unit
    abstract Count : int
    abstract GetBeliefs : unit -> seq<Belief>

type SubStore(n : int, ranking : Belief -> float32) =
    let q = IntervalHeap<Belief>(n) :> IPriorityQueue<Belief>
    let d = HashDictionary<Key, IPriorityQueueHandle<Belief>>() :>IDictionary<Key, IPriorityQueueHandle<Belief>>

    let maxSize = n

    let addBelief key (belief : Belief) h =
        q.Add(h, belief) |> ignore
        d.Add(key, !h)        

    let deleteMinBelief() =
        let (deleted, h) = q.DeleteMin()
        let h = ref h
        match d.Remove(deleted.Term, h ) with
        | true -> ()
        | false -> failwith "ConceptStore.Insert() : failed to remove on maxSize"    
        !h

    interface ISubStore with
        member x.Contains(key) = d.Contains key

        member x.Insert(key, belief) =
            if d.Count >= maxSize then
                if ranking(belief) >= ranking(q.FindMin()) then                     
                    ref <| deleteMinBelief()
                    |> addBelief key belief
            else
                let h = ref null
                addBelief key belief (ref null)

        member x.Update(key, belief) =

            let h = d.[key]
            q.[h] <- belief

        member x.TryGetValue key =    
        
            if d.Contains(key) then Some(q.[d.[key]])
            else None

        member x.Clear() =        
            d.Clear()
            while not(q.IsEmpty) do
                let (_,_) = q.DeleteMax()
                ()

        member x.Count = d.Count
        member x.GetBeliefs() = q :> seq<Belief>
