module Parsers

open ParserTypes
open ParserMonad
open ParserUtils

//let private parser = new ParserBuilder()

let pchar charToMatch = 
    let innerFn = function
        | "" 
        | null -> 
            Failure "No more input"
        | Prefix charToMatch remaining -> 
            Success (charToMatch,remaining)
        | str ->    
            sprintf "Expecting '%c'. Got '%c'" charToMatch str.[0]
            |> Failure    
    Parser innerFn        

let andThen = concatenateP

let private justBind selector p1 p2 =
    (p1 .>>. p2) >>= (selector >> returnP)

let private justMap selector p1 p2 =
    selector <!> (p1 .>>. p2)

let ( .>> ) p1 p2 = justMap fst p1 p2

let ( >>. ) p1 p2 = justBind snd p1 p2

let orElse p1 p2 =
    let innerFn input =
        let res1 = run p1 input
        match res1 with
        | Success _ -> res1
        | Failure _ -> run p2 input
    Parser innerFn

let ( <|> ) = orElse

let choice = List.reduce (<|>)

let anyOf chars =
    chars 
    |> List.map pchar
    |> choice

let private consP h t = lift2P cons h t

let rec sequence parserList = 
    match parserList with
    | [] -> returnP []
    | head::tail -> consP head (sequence tail)

let pstring str =
    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |> mapP charListToString

let private parseZeroOrMore parser =
    let rec innerFn input = 
        let res = run parser input
        match res with
        | Failure _ -> Success ([], input)
        | Success (firstValue, intermediateInput) -> 
            let other = innerFn intermediateInput
            match other with
            | Failure _ -> Success([], input)
            | Success (otherValues, finalInput) -> Success (firstValue::otherValues, finalInput)
    Parser innerFn

let many parser = parseZeroOrMore parser

let many1 parser =
    let parseRemaining = parseZeroOrMore parser
    parser >>= ( fun firstValue ->
    parseRemaining >>= (fun otherValues ->
        returnP (firstValue::otherValues)))

let parseDigit = anyOf ['0'..'9']

let optional parser =
    let some = parser |>> Some
    let none = returnP None
    some <|> none

let parseInt =
    let resultToInt (sign, digits) =
        let number = digits |> charListToInt
        match sign with
        | Some _ -> -number
        | None -> number
        
    let digits = many1 parseDigit
    
    //resultToInt <!> (opt (pchar '-') .>>. digits)
    optional (pchar '-') .>>. digits
    |>> resultToInt

let between before parser after =
    before >>. parser .>> after

let private sepByHelper parser sep composer =
    parser .>> (optional sep)
    |> composer

let sepBy1 parser separatorP =
    sepByHelper parser separatorP many1

let sepBy parser separatorP =
    sepByHelper parser separatorP many

(* 
#load "parserTypes.fs"; #load "parserbuilder.fs"; #load "parserutils.fs"; #load "parsermonad.fs";  #load "parsers.fs"; open Parser;;
let parseA = pchar 'a'
let parseB = pchar 'b'
 
run (parseA .>>. parseB) "abc"

*)