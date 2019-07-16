module ParserBuilder

open ParserTypes

type ParserBuilder() =
    member this.Bind(x, f) =
        match x with
        | Failure err -> Failure err
        | _ -> f x

    member this.Return(x) =
        x

let parser = new ParserBuilder()