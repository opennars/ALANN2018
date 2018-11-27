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

module AnswerQueue

open System.Diagnostics
open C5
open Types

//type AnswerQueue(n : int) =
//    let q = CircularQueue<Event> 
//    let q = IntervalHeap<Belief>(n) :> IPriorityQueue<Belief>
//    let d = HashDictionary<Term, IPriorityQueueHandle<Belief>>() :>IDictionary<Term, IPriorityQueueHandle<Belief>>

//    let maxSize = n

//    interface IStore with
//        member x.Contains(term) = d.Contains term

//        member x.Insert(event) =
//            //let cond = maxSize >= d.Count
//            //Debug.Assert(cond)
            
//            if d.Count >= maxSize  then 
//                if event.TV.C >= q.FindMin().TV.C then
//                    let (deleted, h) = q.DeleteMin()
//                    let h = ref h
//                    match d.Remove(deleted.Term, h ) with
//                    | true -> ()
//                    | false -> failwith "ConceptStore.Insert() : failed to remove on maxSize"

//                    q.Add(h, event) |> ignore
//                    d.Add(event.Term, !h)
//            else
//                let h = ref null
//                q.Add(h, event) |> ignore
//                d.Add(event.Term, !h)

//        member x.Update(event) =

//            //if not(d.Contains(event.Term) ) then
//            //    failwith "ConceptStore.Update() : conceptRef does not exist"

//            let h = d.[event.Term]
//            q.[h] <- event

//        member x.TryGetValue term = 
            
//            if d.Contains(term) then Some(q.[d.[term]])
//            else None

//        member x.Clear() =
        
//            d.Clear()
//            while not(q.IsEmpty) do
//                let (_,_) = q.DeleteMax()
//                ()

//        member x.GetEnumerator() = q.GetEnumerator()

//        member x.Count = d.Count

//        //member x.DeleteMin() = 
//        //    let (deleted, h) = q.DeleteMin()
//        //    let h = ref h
//        //    match d.Remove(deleted, h ) with
//        //    | true -> ()
//        //    | false -> failwith "ConceptStore.DeleteMin() : failed to remove from dictionary"
//        //    deleted

//        member x.Beliefs() = q :> seq<Belief>