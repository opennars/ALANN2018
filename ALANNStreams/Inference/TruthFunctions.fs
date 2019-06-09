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

module TruthFunctions

    open Types

    //personality factor 
    let k = Params.HORIZON

    // Extended boolean operators
    let inline _and lst = lst |> List.fold (fun acc x -> acc * x) 1.0f
    let inline _not a = 1.0f - a
    //let inline _or lst = 1.0f - (lst |> List.fold (fun acc x -> (1.0f - x) * acc) 1.0f)
    let inline _or(x, y) = 1.0f - (1.0f - x) * (1.0f - y)
    
    // Evidence conversion functions
    let inline w2c (wplus, w) = (wplus / w, w /(w + k))
    let inline c2w (f, c) = (k * f * c / (1.0f - c), k * c / (1.0f - c))   
    
    // Local inference
    let inline rev ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let a = c2w(f1, c1)
        let b = c2w(f2, c2)
        let wplus = (fst a + fst b)
        let w = (snd a + snd b)
        let f, c = w2c(wplus, w) 
        {F = f; C = min Params.CONF_MAX_CLAMP c}

    let inline exp ({F = f; C = c}) = c * (f - 0.5f) + 0.5f

    let inline dec (p, d) = ( p * (d - 0.5f))

    // Immediate inference
    let inline neg ({F=f; C=c}, _) = {F = 1.0f - f; C = c}
    let inline cnv ({F=f; C=c}, _) = {F = 1.0f; C = f * c / (f * c + k)} // w2c(_and [f; c], _and [f; c]) // wminus = 0 so w = wplus
    let inline cnt ({F=f; C=c}, _) = 
        let w = _not f * c
        {F = 0.0f; C = w / (w + k)}
    
    // Strong syllogism
    let inline ded ({F=f1; C=c1}, {F=f2; C=c2}) = {F = f1 * f2; C = f1 * f2 * c1 * c2}
    let inline ana ({F=f1; C=c1}, {F=f2; C=c2}) = {F = f1 * f2; C = f2 * c1 * c2}
    let inline res  ({F=f1; C=c1}, {F=f2; C=c2}) = {F =f1 * f2; C = (_or(f1, f2)) * c1 * c2}

    let inline anon_ana ({F=f1; C=c1}, tv2) = ana(tv2, {F = f1; C = c1 / (c1 + k)})
    
    // Weak syllogism    
    let inline abd ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f, c = w2c(f1 * f2 * c1 * c2, f1 * c1 * c2)
        {F = f; C = c}
    let inline ind ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f,c = w2c(f1 * f2 * c1 * c2, f2 * c1 * c2)
        {F = f; C = c}
    let inline exe ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f, c = w2c(f1 * f2 * c1 * c2, f1 * f2 * c1 * c2)
        {F = f; C = c}
    let inline com ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f, c = w2c(f1 * f2 * c1 * c2, (_or(f1, f2)) * c1 * c2)
        {F = f; C = c}
    
    // Term composition
    let inline int ({F=f1; C=c1}, {F=f2; C=c2}) = {F = f1 * f2; C = c1 * c2}
    let inline uni ({F=f1; C=c1}, {F=f2; C=c2}) = {F = _or(f1, f2); C = c1 * c2}
    let inline dif ({F=f1; C=c1}, {F=f2; C=c2}) = {F = f1 * _not f2; C = c1 * c2}
    
    // Term decomposition
    let inline pnn ({F=f1; C=c1}, {F=f2; C=c2}) =  
        let f2n = _not f2
        let fn = f1 * f2n 
        let f = _not fn
        let c = fn * c1 * c2
        {F = f; C = c}

    let inline npp ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f1n = _not f1
        let f = f1n * f2
        let c = f * c1 * c2
        {F = f; C = c}

    let inline pnp ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f2n = _not f2
        let f = f1 * f2n
        let c = f * c1 * c2
        {F = f; C = c}

    let inline nnn ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f1n = _not f1
        let f2n = _not f2
        let fn = f1n * f2n
        let f = _not fn
        let c = fn * c1 * c2
        {F = f; C = c}

        // Analytic truths
    let inline identity (tv, _) = tv
    
    let inline structuralDed (tv, _) = 
        ded(tv, {F = 1.0f; C = 1.0f})
    
    let inline structuralAbd (tv, _) = 
        abd({F = 1.0f; C = 1.0f}, tv)
    
    let inline structuralInt (tv, _) = 
        int(tv, {F = 1.0f; C = 1.0f})
    
    let inline beliefStructuralDed (tv, _) = 
        ded(tv, {F = 1.0f; C = 0.9f})
    
    let inline beliefNeg (tv, _) = 
        neg (tv)
    
    let inline beliefId (tv, _) = (tv)
    
    let beliefStructuralDif ({F = f1; C = c1}, _) = 
         {F = 1.0f - f1; C = f1 * 1.0f * c1 * 0.9f}

    // Desire truth functions
    let inline desire_structural_strong (tv, _) =
        ana(tv, {F = 1.0f; C = 1.0f})

    let inline desire_weak({F = f1; C = c1}, {F = f2; C = c2}) =
        {F = f1 * f2; C = c1 * c2 * f2}              // *** TODO need to add w2c(1.0f)

    let strong = ana
    let d_ded = int
    let weak = desire_weak
    let d_ind = ind
    let d_id = identity
    let d_neg = neg
    let d_structural_strong = desire_structural_strong

    //Temporal truth functions
    let inline temporal_abd ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f, c = w2c(f1 * f2 * c1 * c2, f1 * c1 * c2)
        {F = f; C = c}

    let inline temporal_ind ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f,c = w2c(f1 * f2 * c1 * c2, f2 * c1 * c2)
        {F = f; C = c}

    let inline temporal_com ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f, c = w2c(f1 * f2 * c1 * c2, _or(f1, f2) * c1 * c2)
        {F = f; C = c}

    let inline temporal_int ({F=f1; C=c1}, {F=f2; C=c2}) = {F = f1 * f2; C = c1 * c2}
