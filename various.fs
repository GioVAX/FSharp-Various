module Various

let rec doubleFactorial =
    function
    | x when x < 2 -> 1
    | y -> y * doubleFactorial (y - 2)

//
//
//
//
let rec repl n list =
    match n with
    | 0 -> []
    | num -> list :: repl (num - 1) list

let rec repl1 list =
    function
    | 0 -> []
    | num -> list :: repl (num - 1) list

//
//
//
//
let rec itemAt arr index =
    match (arr, index) with
    | ([], _) -> failwith "error"
    | (head :: _, 0) -> head
    | (_ :: tail, n) -> itemAt tail (n - 1)

//
//
//
//
let rec myzip arr1 arr2 =
    match (arr1, arr2) with
    | ([], _) -> []
    | (_, []) -> []
    | (h1 :: t1, h2 :: t2) -> (h1, h2) :: (myzip t1 t2)

// -------------------------------------------------
type Tree<'node> =
    | Leaf of 'node
    | Branch of (Tree<'node> * Tree<'node>)

let rec treeMap f =
    function
    | Leaf l -> Leaf(f l)
    | Branch(r, l) -> Branch(treeMap f r, treeMap f l)

//
// let rec treeMap f t =
//     match t with
//     | Leaf l -> Leaf(f l)
//     | Branch(r, l) -> Branch(treeMap f r, treeMap f l)
//
// let treeMap f =
//     let rec g = function
//         | Leaf l -> Leaf(f l)
//         | Branch(r, l) -> Branch(g r, g l)
//     g
//
//
//
//
let rec treeFold fBranch fLeaf =
    function
    | Leaf l -> fLeaf l
    | Branch(r, l) ->
        fBranch (treeFold fBranch fLeaf r) (treeFold fBranch fLeaf l)

// let rec treeFold fBranch fLeaf tree =
//     map tree with
//     | Leaf l -> fLeaf l
//     | Branch(r, l) ->
//         fBranch (treeFold fBranch fLeaf r) (treeFold fBranch fLeaf l)
//
// let treeFold fBranch fLeaf =
//     let rec g = function
//         | Leaf l -> fLeaf l
//         | Branch(r, l) -> fBranch (g r) (g l)
//     g
//
//
let tree1 : Tree<int> =
    Branch
        (Branch
             (Branch(Leaf 1, Branch(Leaf 2, Leaf 3)),
              Branch(Leaf 4, Branch(Leaf 5, Leaf 6))),
         Branch(Branch(Leaf 7, Leaf 8), Leaf 9))

// ----------------------------------------------------------
type Weird<'a, 'b> =
    | First of 'a
    | Second of 'b
    | Third of ('a * 'b) list
    | Fourth of Weird<'a, 'b>

let rec weirdMap f1 f2 =
    function
    | First v1 -> First(f1 v1)
    | Second v2 -> Second(f2 v2)
    | Third v3 -> Third(v3 |> List.map (fun (a, b) -> (f1 a, f2 b)))
    | Fourth v4 -> Fourth(weirdMap f1 f2 v4)
// let rec weirdMap2 f1 f2 w =
//     match w with
//     | First v1 -> First(f1 v1)
//     | Second v2 -> Second(f2 v2)
//     | Third v3 -> Third( v3 |> List.map (fun (a, b) -> (f1 a, f2 b)))
//     | Fourth v4 -> Fourth(weirdMap2 f1 f2 v4)
//
// let weirdMap3 f1 f2 =
//     let rec g =
//         function
//         | First f -> First(f1 f)
//         | Second s -> Second(f2 s)
//         | Third t -> Third( t |> List.map (fun (a, b) -> (f1 a, f2 b)))
//         | Fourth w -> Fourth (g w)
//     g
