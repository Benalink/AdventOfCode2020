namespace AdventOfCode2020.Solvers
module Day9 =
    let rec private findPairWithSum list sum =
            match list with
            | [] -> None
            | first::rest ->
                let target = sum - first
                let matchingPair = rest |> List.tryFind(fun e -> e = target)  
                match matchingPair with
                | None -> findPairWithSum rest sum
                | Some x -> Some (first, x)

    let rec private findSetWithSum (list: int64 list) (sum: int64) (i: int) =
        match list with
        | [] -> None
        | first::rest ->
            let matchingSet =
                rest
                |> List.scan (+) first
                |> List.takeWhile (fun x -> x <= sum)
                
            let last = matchingSet |> List.tryLast
            match last with
            | Some(s) when s = sum -> Some(i, i + matchingSet.Length - 1)
            | _ -> findSetWithSum rest sum (i+1)

    let Part1 (input: string) =
        input.Split '\n' |> Array.map int64
        |> Seq.windowed 26
        |> Seq.find
            (fun window ->
                let target = window |> Seq.last
                let searchWindow = window |> Seq.take 25 |> Seq.toList
                (findPairWithSum searchWindow target) = None
            )
        |> Seq.last

    let Part2 (input: string) =
        let parsedInput = input.Split '\n' |> Array.map int64 |> List.ofArray
        let result = findSetWithSum parsedInput (Part1 input) 0
        match result with
        | None -> failwith "Could not find a set that sums to target"
        | Some(x, y) ->
            let matchingSet = parsedInput.[x..y]
            (matchingSet |> Seq.min) + (matchingSet |> Seq.max)