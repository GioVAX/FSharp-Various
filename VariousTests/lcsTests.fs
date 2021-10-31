module LcsTests

open FsUnit.Xunit
open Xunit

open Lcs

[<Fact(Skip="x")>]
let ``first test`` () =
    let res = lcs "BMOAL" "BLOA"
    res |> should equal "BOA"

[<Fact(Skip="x")>]
let ``second test`` () =
    let res = lcs "The movie on Saturday was great!" "The movie on Friday was great!"
    res |> should equal "The movie on rday was great!"