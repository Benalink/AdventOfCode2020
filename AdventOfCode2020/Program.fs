open AdventOfCode2020.Solvers

let runSolver solver input =
    printfn "Running Solver - %s" solver
    match solver with
    | "1a" -> Day1.Part1 input |> string
    | "1b" -> Day1.Part2 input |> string
    | "2a" -> Day2.Part1 input |> string
    | "2b" -> Day2.Part2 input |> string
    | "3a" -> Day3.Part1 input |> string
    | "3b" -> Day3.Part2 input |> string
    | "4a" -> Day4.Part1 input |> string
    | "4b" -> Day4.Part2 input |> string
    | "5a" -> Day5.Part1 input |> string
    | "5b" -> Day5.Part2 input |> string
    | "6a" -> Day6.Part1 input |> string
    | "6b" -> Day6.Part2 input |> string
    | "7a" -> Day7.Part1 input |> string
    | "7b" -> Day7.Part2 input |> string
    | "8a" -> Day8.Part1 input |> string
    | "8b" -> Day8.Part2 input |> string
    | "9a" -> Day9.Part1 input |> string
    | "9b" -> Day9.Part2 input |> string
    | "10a" -> Day10.Part1 input |> string
    | "10b" -> Day10.Part2 input |> string
    | "11a" -> Day11.Part1 input |> string
    | "11b" -> Day11.Part2 input |> string
    | "12a" -> Day12.Part1 input |> string
    | "12b" -> Day12.Part2 input |> string
    | _ -> failwith "Could not find a matching solver"

[<EntryPoint>]
let Main argv =
    let result =
        match argv with
        | [|a; b|] -> runSolver a (System.IO.File.ReadAllText b)
        | _ -> failwith "Invalid Arguments"
    
    printfn "Result: %s" result
    0