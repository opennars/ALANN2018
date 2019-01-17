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

module SystemState

open System.Collections.Concurrent
open System.Threading
open System.Diagnostics
open Types

let mutable systemState = 
    {
        Id = ref 0L
        StartTime = 0L
        SC_Term_ID = ref 0L
        EventsPerSecond = ref 0
        Activations = ref 0
        stores = [|for i in 0..(Params.NUM_TERM_STREAMS - 1) -> new ConcurrentDictionary<Term, Node>(Params.NUM_TERM_STREAMS, Params.GROUP_BLOCK_SIZE)|]
    }

let ID() = Interlocked.Increment(systemState.Id)

let timer = Stopwatch.StartNew()
let SystemTime() = timer.ElapsedMilliseconds + systemState.StartTime 
let ResetTime() = timer.Restart()

let mutable resetSwitch = false
