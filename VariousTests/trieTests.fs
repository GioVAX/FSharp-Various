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
    let root = Trie.empty |> Trie.add "s"

    root.Branches 
        |> should haveCount 1

    let sNode = root.Branches |> Map.tryFind 's'
    sNode.IsSome |> should be True

    let node = sNode |> Option.get
    node.Branches |> should haveCount 0
    node.EndOfWord |> should be True


[<Fact>]
let ``add a 2-char string to an empty trie`` () =
    let root = Trie.empty |> Trie.add "si"

    root.Branches |> should haveCount 1

    let sNode = root.Branches |> Map.tryFind 's'
    sNode.IsSome |> should be True

    let node = sNode |> Option.get
    node.Branches |> should haveCount 1
    node.EndOfWord |> should be False

    let iNode = node.Branches |> Map.tryFind 'i'
    iNode.IsSome |> should be True

    let node = iNode |> Option.get
    node.Branches |> should haveCount 0
    node.EndOfWord |> should be True

[<Fact>]
let ``add 2 strings one prefix of the other to an empty trie`` () =
    let root = Trie.empty |> Trie.add "s" |> Trie.add "si"

    root.Branches |> should haveCount 1

    let sNode = root.Branches |> Map.tryFind 's'
    sNode.IsSome |> should be True

    let node = sNode |> Option.get
    node.Branches |> should haveCount 1
    node.EndOfWord |> should be True

    let iNode = node.Branches |> Map.tryFind 'i'
    iNode.IsSome |> should be True

    let node = iNode |> Option.get
    node.Branches |> should haveCount 0
    node.EndOfWord |> should be True

[<Fact>]
let ``add 2 strings one prefix of the other to an empty trie 2`` () =
    let root = Trie.empty |> Trie.add "s" |> Trie.add "sin"

    root.Branches |> should haveCount 1

    let sNode = root.Branches |> Map.tryFind 's'
    sNode.IsSome |> should be True

    let node = sNode |> Option.get
    node.Branches |> should haveCount 1
    node.EndOfWord |> should be True

    let iNode = node.Branches |> Map.tryFind 'i'
    iNode.IsSome |> should be True

    let node = iNode |> Option.get
    node.Branches |> should haveCount 1
    node.EndOfWord |> should be False

    let nNode = node.Branches |> Map.tryFind 'n'
    nNode.IsSome |> should be True

    let node = nNode |> Option.get
    node.Branches |> should haveCount 0
    node.EndOfWord |> should be True