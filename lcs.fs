module Lcs

let lcs (s1:string) (s2:string) =
    let rec loop (s1:string) (s2:string) =
        match (s1,s2) with
        | ("",_) | (_,"") -> []
        | (_,_) ->
            match (s1.[0], s2.[0]) with
            | (c1,c2) when c1 = c2 ->
                c1::(loop s1.[1..] s2.[1..])
            | (_,_) ->
                let m1 = (loop s1 s2.[1..])
                let m2 = (loop s1.[1..] s2)
                if m1.Length > m2.Length then
                    m1
                else m2
    loop s1 s2
    |> List.toArray
    |> System.String