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
open TermUtils

//
// Unify - a recursive unifier that unifies two terms
//

let unify x y =
    let rec unifyRec selective x y map =
        let rec unifyList argsx argsy map =
            match argsx, argsy with 
            | x::restx, y::resty ->                 
                match map with 
                | Some _ -> 
                    let map' = unifyRec selective x y map
                    unifyList restx resty map' 
                | _ -> None
            | [], [] -> map
            | _ -> None

        let rec occurCheck var x map =
            match (var, x) with
            | v, x when v = x -> true
            | v, x when Map.containsKey x map ->
                occurCheck v (Map.find x map) map
            | v, Term(_, args) ->
                List.exists (fun t -> occurCheck v t map) args
            | _ -> false            

        let rec unifyVar var x map =
            match map with
            | Some map ->
                if Map.containsKey var map then unifyRec selective (Map.find var map) x (Some map)
                elif Map.containsKey x map then unifyRec selective var (Map.find x map) (Some map)
                elif occurCheck var x map then None 
                else Some (Map.add var x map)
            | None -> None

        if map = None then None
        else
            match x, y with
            | x, y when x = y -> map
            | Term(op1, argsx), Term(op2, argsy) when op1 = op2 && List.length argsx = List.length argsy -> 
                              unifyList argsx argsy map
            | Var(QVar, _), y -> unifyVar x y map
            | x, Var(QVar, _) -> unifyVar y x map
            | Var(IVar, _), y when not(selective) -> unifyVar x y map
            | x, Var(IVar, _) when not(selective) -> unifyVar y x map
            | Var(DVar, _), y when not(selective) -> unifyVar x y map
            | x, Var(DVar, _) when not(selective) -> unifyVar y x map

            | _ -> None

    // Main function body here

    match unifyRec (isSelective x) x y (Some Map.empty) with | Some map -> map |> Map.toList | _ -> []

let unifies x y = if x = y then true else unify x y <> []

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
