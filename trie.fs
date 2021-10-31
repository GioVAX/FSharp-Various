module Trie

type Trie = {
    Branches: Map<char,Trie>
    EndOfWord : bool
}

let empty = { 
    Branches = Map.empty
    EndOfWord = false
}

let add s root =
    let rec loop root = function
    | [c] -> 
        let newNode = 
            match root.Branches.TryFind c with
            | Some node -> node
            | None -> empty
        {root with Branches = root.Branches.Add (c, {newNode with EndOfWord = true})}
    | c::tail ->
        let newNode = 
            match root.Branches.TryFind c with
            | Some node -> 
                loop node tail
            | None -> 
                loop empty tail
        {root with Branches = root.Branches.Add (c, newNode)}
    | _ -> root

    loop root (s |> Seq.toList)