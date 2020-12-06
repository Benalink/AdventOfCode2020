namespace AdventOfCode2020.Solvers
module Day2 =
    let private parseInput (input: string) =
        let parseLine (inputLine: string) =
            let policyAndPassword = inputLine.Split ':' |> Seq.toList
            match policyAndPassword with
            | [policy; password] -> (policy.Split [|'-'; ' '|], password)
            | _ -> failwith "Unexpected Input"

        input.Split '\n' |> Seq.map parseLine

    let Part1 (input: string) =
        let enforcePolicy (policy: string[]) (password: string) =
            let min = int policy.[0]
            let max = int policy.[1]
            let char = char policy.[2]
            let count = password |> Seq.filter (fun c -> c = char) |> Seq.length
            min <= count && count <= max

        parseInput input
        |> Seq.filter (fun (policy, password) -> enforcePolicy policy password)
        |> Seq.length

    let Part2 (input: string) =        
        let enforcePolicy (policy: string[]) (password: string) =
            let pos1 = int policy.[0]
            let pos2 = int policy.[1]
            let char = char policy.[2]
            (password.[pos1] = char) <> (password.[pos2] = char)

        parseInput input
        |> Seq.filter (fun (policy, password) -> enforcePolicy policy password)
        |> Seq.length