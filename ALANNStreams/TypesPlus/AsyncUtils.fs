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

module AsyncUtils

    open System.Threading
    open System.Threading.Tasks

    type SynchronizationContext with 
        /// A standard helper extension method to raise an event on the GUI thread
        member syncContext.RaiseEvent (event: Event<_>) args = 
            syncContext.Post((fun _ -> event.Trigger args),state=null)

        /// A standard helper extension method to capture the current synchronization context.
        /// If none is present, use a context that executes work in the thread pool.
        static member CaptureCurrent () = 
            match SynchronizationContext.Current with 
            | null -> new SynchronizationContext()
            | ctxt -> ctxt

    let AwaitTaskVoid : (Task -> Async<unit>) = Async.AwaitIAsyncResult >> Async.Ignore

    let DoPeriodicWork (f: unit -> Async<unit>) (interval: int) (token: CancellationToken) =
        async {
            while not token.IsCancellationRequested do
                do! f()
                do! Task.Delay(interval, token) |> AwaitTaskVoid
        }