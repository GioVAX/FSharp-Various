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

let buildInput c1 c2 str =
    (Seq.append [c1;c2] str)
    |> Seq.toArray
    |> System.String

let injectRnd char string =
    let rand = new System.Random()
    let idx = rand.Next(string |> List.length)

    let rec inner c i l = 
        match i, l with
        | 0, _ -> c :: l
        | _, h::t -> h::inner c (i-1) t
        | _, [] -> failwith "Index out of range"

    inner char idx string            
