namespace AdventOfCode2020.Solvers
module Day10 =
    let exampleInput = "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"

    let Part1 (input: string) =
        let parsedInput = exampleInput.Split '\n' |> Seq.map int |> Seq.sort |> Seq.pairwise |> Seq.toList
        let withFirst = List.append [0, fst(parsedInput.[0])] parsedInput
        let withLast = List.append withFirst [snd(parsedInput.[parsedInput.Length - 1]), snd(parsedInput.[parsedInput.Length - 1]) + 3]
        
        let joltCounts =
            withLast
            |> Seq.map (fun (p, c) -> (c, c - p))
            |> Seq.takeWhile (fun(_, d) -> d < 4)
            |> Seq.countBy (fun(_, d) -> d)
            |> Seq.toList
        
        let (_, oneJoltCount) = joltCounts.[0]
        let (_, threeJoltCount) = joltCounts.[1]
        oneJoltCount * threeJoltCount
    
    let rec calculateArrangements (largestArrangement: int list) (count: int64) =
        match largestArrangement with
        | [] -> count
        | first::rest ->
            let within3 = rest |> Seq.filter (fun(i) -> i <= first + 3) |> Seq.length
            match within3 with
            | 3 | 2 -> calculateArrangements rest count * 2L
            | _ -> calculateArrangements rest count
            
    let Part2 (input: string) =
        let parsedInput = exampleInput.Split '\n' |> Seq.map int |> Seq.sort |> Seq.pairwise |> Seq.toList
        let withFirst = List.append [0, fst(parsedInput.[0])] parsedInput
        let withLast = List.append withFirst [snd(parsedInput.[parsedInput.Length - 1]), snd(parsedInput.[parsedInput.Length - 1]) + 3]
        let largestJoltArrangement =
            withLast
            |> Seq.map (fun (p, c) -> (c, c - p))
            |> Seq.takeWhile (fun(_, d) -> d < 4)
            |> Seq.map (fun (j, _) -> j)
            |> Seq.toList
            |> List.indexed
        
        printfn "largestJoltArrangement %O" largestJoltArrangement
        
        largestJoltArrangement
        |> Seq.scan (fun acc (i, j) ->
            let possibilities =
                (largestJoltArrangement
                |> Seq.filter (fun (si, sj) -> si > i && sj - j < 3) |> Seq.length)
            
            acc * int64 (max possibilities 1)) 1L
        |> Seq.toList