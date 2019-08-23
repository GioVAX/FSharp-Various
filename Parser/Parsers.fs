module Parsers

open ParserTypes
open ParserMonad
open ParserUtils

let internal satisfy predicate =
    let innerFn input = 
        if input = null || input = "" then
            Failure "No more input"
        else
            let first = input.[0]
            if predicate first then
                Success (first,input.[1..])
            else
                sprintf "Unexpected '%c'" first
                |> Failure
    Parser innerFn        

let pchar charToMatch = satisfy ((=) charToMatch)

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

let choice = Seq.reduce (<|>)

let anyOf chars =
    chars 
    |> Seq.map pchar
    |> choice

let private consP h t = lift2P cons h t

let sequence parsers = 
    let parsersList = parsers |> List.ofSeq
    let rec inner = function
        | [] -> returnP []
        | head::tail -> consP head (inner tail)
    inner parsersList

let pstring (str:string) =
    str
    |> Seq.map pchar
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

//let many1 p =
//    let parseFollowing = parseZeroOrMore p
//    let innerFn input = 
//        ParserBuilder.parser {
//            let! Success (r1, midInput) = run p input
//            let! Success (r2, remainingInput) = run parseFollowing midInput
//            return r1::r2, remainingInput
//        }
//    Parser innerFn

let many1 parser =
    let parseAdditional = parseZeroOrMore parser

    parser          >>= (fun r1 ->
    parseAdditional >>= (fun rs ->
    r1::rs          |> returnP ))

let pdigit = anyOf ['0'..'9']

let optional parser =
    let some = parser |>> Some
    let none = returnP None
    some <|> none

let pint =
    let resultToInt = function
        | (Some _, digits) -> -(digits |> charListToInt)
        | (None, digits) -> digits |> charListToInt
        
    let pDigits = many1 pdigit
    let pSign = '-' |> pchar |> optional
    
    //resultToInt <!> (sign .>>. digits)
    pSign .>>. pDigits
    |>> resultToInt

let between before parser after =
    before >>. parser .>> after

let private sepByHelper parser sep composer =
    parser .>> (optional sep)
    |> composer

let separatedBy1 parser separatorP =
    sepByHelper parser separatorP many1

let separatedBy parser separatorP =
    sepByHelper parser separatorP many

(* 
#load "parserTypes.fs"; #load "parserbuilder.fs"; #load "parserutils.fs"; #load "parsermonad.fs";  #load "parsers.fs"; open Parser;;
let parseA = pchar 'a'
let parseB = pchar 'b'
 
run (parseA .>>. parseB) "abc"

*)