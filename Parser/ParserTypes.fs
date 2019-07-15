module ParserTypes

type Result<'a> =
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string -> Result<'T * string>)

let run (Parser innerFn) input = 
    innerFn input