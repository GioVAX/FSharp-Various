module mapP

open FsCheck
open FsCheck.Xunit

open ParserTypes
open ParserMonad
open Parsers

open ParserTestsUtils

type ``-> mapP`` () =
    let doTest num str parserBuilder checkFn =
        let nums = num |> string
        let input =  nums + str

        let parser = parserBuilder nums

        let result = run parser input

        result
        |> checkFn num

    let failingINput = "Failure"
    let chkSuccess = checkMatched
    let chkFail = (fun n -> checkFailure "Expecting")

    [<Property>]
    member x.``SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        doTest num s.Get 
            (fun s -> mapP (int) (pstring s)) 
            chkSuccess
        
    [<Property>]
    member x.``infix SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        doTest num s.Get 
            (fun s -> int <!> (pstring s))  
            chkSuccess

    [<Property>]
    member x.``infix inverted SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        doTest num s.Get 
            (fun s -> (pstring s) |>> int) 
            chkSuccess


    [<Property>]
    member x.``SHOULD fail if no matches`` (num: int32) =
        doTest num failingINput 
            (fun s -> mapP (int) (pstring failingINput))  
            chkFail

    [<Property>]
    member x.``infix SHOULD fail if no matches`` (num: int32) =
        doTest num failingINput 
            (fun s -> int <!> (pstring failingINput)) 
            chkFail

    [<Property>]
    member x.``infix inverted SHOULD fail if no matches`` (num: int32) (s:NonEmptyString) =
        doTest num failingINput 
            (fun s -> (pstring failingINput) |>> int) 
            chkFail