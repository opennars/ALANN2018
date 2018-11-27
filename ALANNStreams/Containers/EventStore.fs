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

module EventStore

open C5
open Types
open System.Collections.Generic

//type IEventStore =
//    abstract GetAll : unit -> IList<IPriorityQueueHandle<Event>>
//    abstract Get : IPriorityQueueHandle<Event> -> Event
//    abstract Update : IPriorityQueueHandle<Event> * Event -> unit
//    abstract Count : int
//    abstract Push : Event -> unit
//    abstract Pop : unit -> IPriorityQueueHandle<Event> * Event


type IEventStore =
    abstract GetAll : unit -> Event array
    abstract Count : int
    abstract Push : Event -> unit
    abstract Reset : unit -> unit

type Comparer() =
    interface IComparer<Event> with

        member this.Compare(x, y) =
            let now = SystemTime()
            let age e = 
                let age = float32((now - e.Stamp.Created))
                age / (age + 10.0f)
            let a = x.AV.STI * age x * match x.TV with | Some {F = f; C = c} -> c | None -> 1.0f
            let b = y.AV.STI * age y * match y.TV with | Some {F = f; C = c} -> c | None -> 1.0f
            //let a = x.AV.STI
            //let b = y.AV.STI

            a.CompareTo(b)

let eventComparer = Comparer()

//type EventEqualityComparer() =
//    interface IEqualityComparer<Event> with

//        member this.Equals(x, y) =
//            x.Term = y.Term

//        member this.GetHashCode(x) = x.GetHashCode()

//let eventEqualityComparer = EventEqualityComparer()

type EventStore(n : int) =
    let q = new TreeSet<Event>(eventComparer) :> IIndexedSorted<Event>

    let maxSize = n

    interface IEventStore with
        member x.GetAll() = q.ToArray()

        member x.Reset() = q.Clear()

        member x.Push(event) =            
            if q.Count >= maxSize then
                if event.AV.STI > q.FindMin().AV.STI then
                    q.DeleteMin() |> ignore
                    q.Add(event) |> ignore
            else
                q.Add(event) |> ignore               

        member x.Count = q.Count

//type EventStore(n : int) =
//    let q = IntervalHeap<Event>(n, eventComparer) :> IPriorityQueue<Event>
//    let handles = new ArrayList<IPriorityQueueHandle<Event>>(n) :> IList<IPriorityQueueHandle<Event>>

//    let maxSize = n

//    interface IEventStore with
//        member x.GetAll() = handles

//        member x.Get(h) = q.[h]

//        member x.Update(h, e) =
//            q.[h] <- e

//        member x.Count = q.Count

//        member x.Push(event) =
//            if q.Count >= maxSize then
//                let (_, h) = q.DeleteMin()
//                let h = ref h
//                q.Add(h, event)|> ignore
//            else
//                let h = ref null
//                q.Add(h, event) |> ignore
//                handles.Add(!h) |> ignore

//        member x.Pop() =
//            let h = ref null
//            let e = q.FindMax(h)
//            (!h, e)

