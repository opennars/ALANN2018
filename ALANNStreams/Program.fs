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

open System
open Controller
open Reporting

[<EntryPoint>]
let main argv = 

    // runTestsInAssembly defaultConfig argv |> ignore

    printfn ""
    printfn "\t********************************"
    printfn "\t***                          ***"
    printfn "\t*** Starting ALANN system    ***"
    printfn "\t***                          ***"
    printfn "\t********************************"
    printfn ""
    printfn "\tConfiguring..."
    printfn ""

    let controller = new Controller()

    controller.Initialise()
    controller.ParseError.Add (fun e -> printfn "%s" e.Error)
    controller.DisplayAnswer.Add (fun e -> displayAnswer e.Answer)

    printfn "\tConfiguration complete\n"
    printfn "\tRunning system in [%s] mode\n" (if Params.WORKSTATION then "WORKSTATION" else "SERVER")
    printfn "\tReady to accept commands\n"


    Console.ReadLine() |> ignore

    0 // return ok here
