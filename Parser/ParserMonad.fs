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

let concatenateP p1 p2  =
    p1 >>= (fun r1 -> 
    p2 >>= (fun r2 -> 
        returnP (r1, r2)))

//let concatenateP = 
//    concatenate (fun r1 r2 -> (r1, r2))

let applyP f p =
    concatenateP f p
    |> mapP (fun (f, x) -> f x)

let applyMapP f p =
    (fun (f, x) -> f x) <!> concatenateP f p

let ( <*> ) = applyP

let lift2P f p1 p2 =
    returnP f <*> p1 <*> p2
