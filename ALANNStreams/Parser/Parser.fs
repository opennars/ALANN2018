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
 
module Parser

open FParsec
open Types
open Events
open TermUtils
open SystemState

// utilities
let sort lst = lst |> List.sort
let order a b = if a < b then (a,b) else (b, a)
let optor x y = match x with | Some x -> x | None -> y

// White space parsers
let str s = pstring s
let pcomment = str "'" >>. skipRestOfLine true
let ws = skipSepBy spaces pcomment
let str_ws s = pstring s .>> ws

// Forward refs
let pterm, ptermRef = createParserForwardedToRef<Term, unit>() 
let pstatement, pstatementRef = createParserForwardedToRef<Term, unit>()

// float parser
let pfloat_ws = pfloat .>> ws

// Word Parser
let pstringliteral =
    let isIdentifierFirstChar c = isLetter c || c = '_' || c = '"' || isDigit c || c = '''
    let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = ''' || c = '"' || c = '.'
    many1Satisfy2 isIdentifierFirstChar isIdentifierChar 
    |>> fun i -> Word(i)

let pword_ws = pstringliteral .>> ws 

// TV, DV and AV parsers
let ptruth  = between (str_ws "{") (str_ws "}") (tuple2 pfloat_ws pfloat_ws) |>> fun (f, c) -> {F = float32(f); C = float32(c)}
//let pdesire = between (str_ws "{") (str_ws "}") (tuple2 pfloat_ws pfloat_ws) |>> fun (p, d) -> {P = float32(p); D = float32(d)}
let pav     = between (str_ws "[") (str_ws "]") (tuple2 pfloat_ws pfloat_ws) |>> fun (s, l) -> {STI = float32(s); LTI = float32(l)}

let ptruth_ws  = ptruth .>> ws
//let pdesire_ws = pdesire .>> ws
let pav_ws     = pav .>> ws

//Variable parsers
let varname = 
    let isVarChar c = isLetter c || isDigit c
    many1SatisfyL isVarChar "variable name"

let varname_ws = varname .>> ws

let pivar = str_ws "$" >>. varname_ws |>> fun a -> Var(IVar, a.ToUpper()) 
let pdvar = str_ws "#" >>. varname_ws |>> fun a -> Var(DVar , a.ToUpper())
let pqvar = str_ws "?" >>. varname_ws |>> fun a -> Var(QVar , a.ToUpper())
let pvariable_ws = pivar <|> pdvar <|> pqvar .>> ws

// Set parser
let setBetweenStrings sOpen sClose pElement f =
    between (str_ws sOpen) (str_ws sClose)
        (many1 pElement |>> f)

// Intensional and Extensional set parsers
let pextset_ws = setBetweenStrings "{" "}" pterm (fun a -> Term(ExtSet, sort a))
let pintset_ws = setBetweenStrings "[" "]" pterm (fun a -> Term(IntSet, sort a))
let pset_ws = pextset_ws <|> pintset_ws

// Negation parser
let pneg_ws = (str_ws "--") >>. pterm |>> (fun a -> Term(Not, [a]))

//Binary operators
let higher_order_op =  str_ws "&&" <|> str_ws "||" <|> str_ws ";" <|> str_ws ","
let first_order_op = str_ws "\\" <|> str_ws "/" <|> str_ws "|" <|> str_ws "&" <|> str_ws "-" <|> str_ws "~" <|> str_ws "*"
//let first_order_op = many1Chars (anyOf "|&-~*,;")
let binary_op_ws = higher_order_op <|> first_order_op

// Compound Term parsers

// matches parsed term * op * term
let matchOpToTerm a b c =
  let hd = List.head c
  match b with
  | "|" -> Term(IntInt, sort [a; hd])
  | "&" -> Term(ExtInt, sort [a; hd])
  | "-" -> Term(ExtDif, [a; hd])
  | "~" -> Term(IntDif, [a; hd])
  | "," -> Term(Seq, [a; hd])
  | ";" -> Term(Par, [a; hd])
  | "*" -> Term(Prod, [a; hd])
  | "&&" -> Term(And, sort [a; hd])
  | "||" -> Term(Or, sort [a; hd])
  | "\\" -> Term(IntImg, a::c)
  | "/" -> Term(ExtImg, a::c)
  | _ -> failwith "Unexpected Operator in Parser"

let pcompound_term_ws = 
    between (str_ws "(") (str_ws ")")
      (pipe3 pterm binary_op_ws (many1 pterm) matchOpToTerm)

// Copula parsers       
let first_order_copula = str_ws "-->" <|> str_ws "<->" <|> str_ws "{--" <|> str_ws "--]" <|> str_ws "{-]" <|> str_ws "->>" <|> str_ws "<<-"
let higher_order_copula = str_ws "==>" <|> str_ws "=+>" <|> str_ws "=|>" <|> str_ws "=->" <|> str_ws "<=>" <|> str_ws "<+>" <|> str_ws "<|>"

let copula_ws = first_order_copula <|> higher_order_copula

// Statement parsers
let statement = 
    between (str_ws "<") (str_ws ">")
      (pipe3 pterm copula_ws pterm 
        (fun a b c -> 
          match b with
          | "-->" -> Term(Inh, [a; c])
          | "<->" -> Term(Sim, [a; c])
          | "{--" -> Term(Inh, [Term(ExtSet, [a]); c])
          | "--]" -> Term(Inh, [a; Term(IntSet, [c])])
          | "{-]" -> Term(Inh, [Term(ExtSet, [a]); Term(IntSet, [c])])
          | "==>" -> Term(Imp, [a; c])
          | "=+>" -> Term(PreImp, [a; c])
          | "=|>" -> Term(ConImp, [a; c])
          | "=->" -> Term(RetImp, [a; c])
          | "<=>" -> Term(Equ, [a; c])  
          | "<+>" -> Term(PreEqu, [a; c]) 
          | "<|>" -> Term(ConEqu, [a; c])  
          | _ -> failwith "Unexpected copula type in Parser"))

do ptermRef      := choice [pword_ws; pvariable_ws; pset_ws; pneg_ws; pcompound_term_ws; statement]
do pstatementRef := pterm <|> statement
 
// EventType parsers
let pbeliefType   = (str_ws ".") |>> (fun _ -> Belief)
let pgoalType     = (str_ws "!") |>> (fun _ -> Goal)
let pquestionType = (str_ws "?") |>> (fun _ -> Question)
let pquestType    = (str_ws "@") |>> (fun _ -> Quest)

let peventType = pbeliefType <|> pgoalType <|> pquestionType <|> pquestType

// Sentence parsers
let psentence    = pipe3 pstatement peventType (opt ptruth) (fun a b c -> {EventType = b; Term = a; TV = if b = Belief || b = Goal then Some(optor c {F = Params.FREQUENCY; C = Params.CONFIDENCE}) else None})
let psentence_ws = psentence .>> ws

let makeStamp eType term = 
    match eType with
    | Belief | Goal -> {Created = SystemTime(); SC = syntacticComplexity term; Evidence = [ID()]; LastUsed = SystemTime(); UseCount = 0; Source = User}
    | Question | Quest -> {Created = SystemTime(); SC = syntacticComplexity term; Evidence = [ID()]; LastUsed = SystemTime(); UseCount = 0; Source = User}

// Event parser
let pevent          = pipe2 (opt (attempt pav)) psentence (fun a b -> {Term = b.Term; AV = optor a {STI = Params.USER_STI; LTI = Params.USER_LTI}; EventType = b.EventType;  TV = b.TV; Stamp = makeStamp b.EventType b.Term; Solution = None})
let pevent_ws = ws >>. pevent

// General Parser entry point
let Parser(program:string) =
    match program.Trim() with
    | "" -> []
    | _ ->
        match run pevent_ws program with
        | Success(result, _, _)   -> 
            [result]
        | Failure(errorMsg, e, s) -> 
            raiseParseErrorEvent errorMsg
            []

// Parser tester
let testp p str =
    match run (ws >>. p) str with    
    | Success(result, _, _) -> Some result
    | Failure(errorMsg, _, _) -> None

