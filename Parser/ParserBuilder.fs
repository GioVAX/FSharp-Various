module ParserBuilder

type ParserBuilder() =
    member this.Bind(x, f) =
        f x
    member this.Return(x) =
        x

//let private parser = new ParserBuilder()


