open AdventOfCode2020.Solvers

let runSolver solver input =
    printfn "Running Solver - %s" solver
    match Array.ofSeq solver with
    | [|'1'; 'a'|] -> Day1.Part1 input |> string
    | [|'1'; 'b'|] -> Day1.Part2 input |> string
    | [|'2'; 'a'|] -> Day2.Part1 input |> string
    | [|'2'; 'b'|] -> Day2.Part2 input |> string
    | [|'3'; 'a'|] -> Day3.Part1 input |> string
    | [|'3'; 'b'|] -> Day3.Part2 input |> string
    | [|'4'; 'a'|] -> Day4.Part1 input |> string
    | [|'4'; 'b'|] -> Day4.Part2 input |> string
    | [|'5'; 'a'|] -> Day5.Part1 input |> string
    | [|'5'; 'b'|] -> Day5.Part2 input |> string
    | [|'6'; 'a'|] -> Day6.Part1 input |> string
    | [|'6'; 'b'|] -> Day6.Part2 input |> string
    | [|'7'; 'a'|] -> Day7.Part1 input |> string
    | [|'7'; 'b'|] -> Day7.Part2 input |> string
    | [|'8'; 'a'|] -> Day8.Part1 input |> string
    | [|'8'; 'b'|] -> Day8.Part2 input |> string
    | [|'9'; 'a'|] -> Day9.Part1 input |> string
    | [|'9'; 'b'|] -> Day9.Part2 input |> string
    | _ -> failwith "Could not find a matching solver"

[<EntryPoint>]
let Main argv =
    let result =
        match argv with
        | [|a; b|] -> runSolver a (System.IO.File.ReadAllText b)
        | _ -> failwith "Invalid Arguments"
    
    printfn "Result: %s" result
    0