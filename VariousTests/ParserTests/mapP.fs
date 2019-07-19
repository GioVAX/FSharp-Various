module mapP

open FsCheck
open FsCheck.Xunit

open ParserTypes
open ParserMonad
open Parsers

open ParserTestsUtils

let private doTest num str parserBuilder checkFn =
    let nums = num |> string
    let input =  nums + str

    let parser = parserBuilder nums

    let result = run parser input

    result
    |> checkFn num

let private failingINput = "Failure"

let private chkSuccess = checkMatched
let private chkFail = (fun n -> checkFailure "Expecting")

type ``-> prefix`` () =

    [<Property>]
    member x.``SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        doTest num s.Get 
            (fun s -> mapP (int) (pstring s)) 
            chkSuccess

    [<Property>]
    member x.``SHOULD fail if no matches`` (num: int32) =
        doTest num failingINput 
            (fun s -> mapP (int) (pstring failingINput))  
            chkFail
        
type ``-> infix`` () =
    [<Property>]
    member x.``SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        doTest num s.Get 
            (fun s -> int <!> (pstring s))  
            chkSuccess

    [<Property>]
    member x.``SHOULD fail if no matches`` (num: int32) =
        doTest num failingINput 
            (fun s -> int <!> (pstring failingINput)) 
            chkFail

type ``-> infix inverted params`` () =
    [<Property>]
    member x.``SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        doTest num s.Get 
            (fun s -> (pstring s) |>> int) 
            chkSuccess

    [<Property>]
    member x.``SHOULD fail if no matches`` (num: int32) (s:NonEmptyString) =
        doTest num failingINput 
            (fun s -> (pstring failingINput) |>> int) 
            chkFail