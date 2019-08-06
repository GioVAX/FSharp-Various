module CompositeUtils

open ParserTypes
open Parsers

let parse composer c1 c2 input = 
    let pc1 = pchar c1
    let pc2 = pchar c2
    let parser = composer pc1 pc2

    run parser input