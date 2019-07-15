module ParserUtils

open System

let (|Prefix|_|) c (s:string) =
    if s.[0] = c then
        Some(s.Substring(1))
    else
        None

let cons head tail = head::tail

let charListToString = List.toArray >> String
let charListToInt = charListToString >> int

