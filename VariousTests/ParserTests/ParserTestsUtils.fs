module ParserTestsUtils

open FsUnit.Xunit

open ParserTypes

let checkFailure errMsg = function
    | Failure msg -> msg |> should startWith errMsg
    | _ -> failwith "This should have failed!"

let checkSuccess fcheck = function
    | Success(matched, remainingInput) -> fcheck matched remainingInput
    | _ -> failwith "This should have succeeded!"

let checkMatched expected = 
    checkSuccess (fun res _ -> res |> should equal expected)

let checkRemaining expected = 
    checkSuccess (fun _ input -> input |> should equal expected)
