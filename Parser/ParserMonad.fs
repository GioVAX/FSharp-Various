module ParserMonad

open ParserTypes

let bindP f p =
    let innerFn input = 
        match run p input with
        | Failure err -> Failure err
        | Success (v1, remainingInput) 
            -> run (f v1) remainingInput
    Parser innerFn

let ( >>= ) p f = bindP f p




let returnP x =
    let innerFn input = Success (x,input)
    Parser innerFn  




let mapP f = bindP (f >> returnP)
//let mapP f parser =
//    let innerFn input =
//        let res = run parser input
//        match res with
//        | Failure err -> Failure err
//        | Success (value, remaining) -> Success( f value, remaining)
//    Parser innerFn

let ( <!> ) = mapP
let ( |>> ) x f = mapP f x




let internal _concatenate combiner p1 p2 =
    let innerFn input =
        ParserBuilder.parser {
            let! Success (r1, midInput) = run p1 input
            let! Success (r2, remainingInput) = run p2 midInput
            return (combiner r1 r2, remainingInput)
        }
    Parser innerFn
//let internal _concatenate combiner (p1: Parser<'a>) (p2:Parser<'b>) =
//    p1 >>= (fun r1 -> 
//    p2 >>= (fun r2 -> 
//        combiner r1 r2
//        |> returnP))

let internal concatenateToTuple p1 p2 = 
    _concatenate (fun r1 r2 -> (r1, r2)) p1 p2

let internal concatenateToList p1 p2 = 
    _concatenate (fun r1 r2 -> [r1;r2]) p1 p2

let concatenateP = concatenateToTuple
let ( .>>. ) = concatenateP




let applyP f p =
    concatenateP f p
    |>> (fun (f, x) -> f x)

let ( <*> ) = applyP




let lift2P f p1 p2 =
    returnP f <*> p1 <*> p2
