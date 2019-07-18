module returnP

open ParserTypes
open ParserMonad

open ParserTestsUtils

type ``-> returnP`` () = 

    member x.``returnP SHOULD wrap anything in Success`` anything (input:string) =
        let parser = returnP anything

        let result = run parser input

        result
        |> checkMatched anything

    member x.``returnP SHOULD not change the input`` anything (input:string) =
        let parser = returnP anything

        let result = run parser input

        result
        |> checkRemaining input