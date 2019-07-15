module ParserTools

open ParserTypes

let bindP fn p1 =
    let innerFn input = 
        match run p1 input with
        | Failure err -> Failure err
        | Success (v1, remainingInput) 
            -> run (fn v1) remainingInput
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

let concatenateP p1 p2 =
    p1 >>= (fun r1 -> 
    p2 >>= (fun r2 -> 
        returnP (r1, r2)))

let applyP fP xP =
    concatenateP fP xP
    |> mapP (fun (f, x) -> f x)

let applyMapP fP xP =
    (fun (f, x) -> f x) <!> concatenateP fP xP

let ( <*> ) = applyP

let lift2P f xP yP =
    returnP f <*> xP <*> yP