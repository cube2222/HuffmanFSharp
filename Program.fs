// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharpx.Collections

[<CustomEquality; CustomComparison>]
type HuffmanTree = 
    | Node of int * HuffmanTree * HuffmanTree
    | Leaf of int * char

    member self.Value = match self with
    | Node (v, _, _) -> v
    | Leaf (v, _) -> v

    override x.Equals other = match other with
    | :? HuffmanTree as otherTree -> x.Value.Equals otherTree.Value

    interface System.IComparable with
        member x.CompareTo other = match other with
            | :? HuffmanTree as otherTree -> x.Value.CompareTo otherTree.Value

let rec getHuffmanTree pq =
    let first = PriorityQueue.pop pq
    match first with
    | (x: HuffmanTree, xs) ->
        if PriorityQueue.isEmpty xs
        then x
        else
            let second = PriorityQueue.pop xs
            match second with
            | (y: HuffmanTree, ys) ->
                getHuffmanTree (PriorityQueue.insert (Node(x.Value + y.Value, x, y)) ys)

let rec getHuffmanCodes prefix ht = match ht with
    | Leaf(_, ch) ->
        let myMap = Map.empty
        Map.add ch prefix myMap
    | Node(_, l, r) ->
        let leftMap = getHuffmanCodes (String.concat "" [prefix; "0"]) l
        let rightMap = getHuffmanCodes (String.concat "" [prefix; "1"]) r
        Map(Seq.concat [Map.toSeq leftMap; Map.toSeq rightMap])

[<EntryPoint>]
let main argv = 
    let phrase = "Hello my lovely world."
    printfn "%A" phrase

    let huffmanMap = 
        phrase
        |> Seq.groupBy(fun e -> e)
        |> Seq.map(fun e ->
        match e with
        | (k, v) -> (k, v |> Seq.length))
        |> Map.ofSeq
        |> Map.fold(fun q k v -> PriorityQueue.insert (Leaf(v, k)) q) (PriorityQueue.empty false)
        |> getHuffmanTree
        |> getHuffmanCodes ""

    printfn "%A" huffmanMap
    printfn "%A" huffmanMap
    0 // return an integer exit code
