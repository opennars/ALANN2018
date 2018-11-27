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

module Unify

open Types
open Akka.Actor

//
// Unify - a recursive unifier that unifies two terms
//

let unify x y =
    let rec unifyRec x y map =
        let rec unifyList argsx argsy map =
            let map' =
                match argsx, argsy with 
                | x::restx, y::resty -> 
                    let map' = unifyRec x y map
                    match map with | Some map ->  unifyList restx resty map' | _ -> None
                | [], [] -> map
                | _ -> None
            map'

        let rec unifyVar var x map =
            match map with
            | Some map ->
                if Map.containsKey var map then unifyRec (Map.find var map) x (Some map)
                elif Map.containsKey x map then unifyRec var (Map.find x map) (Some map)
                else Some (Map.add var x map)
            | None -> None

        let map' = 
            match x, y with
            | x, y when x = y -> map
            | Term(termType1, argsx), Term(termType2, argsy) when termType1 = termType2 && List.length argsx = List.length argsy -> unifyList argsx argsy map
            | Var(_, _), y -> unifyVar x y map
            | x, Var(_, _) -> unifyVar y x map
            | _ -> None
        map'

    // Main function body here

    match unifyRec x y (Some Map.empty) with | Some map -> map |> Map.toList | _ -> []

let unifies x y = unify x y <> []

//
// makeSubstitutions - recursively apply substitutions from subs list to r e.g. R{S/T}
//

let rec makeSubstitutions r subs =           
    let rec makeSubstitution r x y =
        let rec substituteFromList lst x y =
            match lst with | r::rest ->  (makeSubstitution r x y) :: (substituteFromList rest x y) | [] -> []

        match r with
        | Term(termType, lst) -> Term(termType, substituteFromList lst x y)
        | r when r = x -> y
        | _ -> r

    if subs |> List.isEmpty then
        r
    else
        match subs with
        | (x, y)::rest -> makeSubstitutions (makeSubstitution r x y) rest
        | [] -> r    

let substUnify c b a =
    unify b a
    |> makeSubstitutions c
