module TrieTests

open FsUnit.Xunit
open Xunit

open Trie

[<Fact>]
let ``empty trie`` () =
    let root = Trie.empty

    root.Branches |> should haveCount 0
    root.EndOfWord |> should be False

[<Fact>]
let ``add a 1-char string to an empty trie`` () =
    let root = Trie.empty
    let tree = root |> Trie.add "s"

    tree.Branches 
        |> should haveCount 1

    let sNode = tree.Branches |> Map.tryFind 's'
    sNode.IsSome |> should be True

    let node = sNode |> Option.get
    node.Branches |> should haveCount 0
    node.EndOfWord |> should be True