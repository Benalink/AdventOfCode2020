namespace AdventOfCode2020.Solvers
module Day1 =
    let private parseInput (input: string) =
        input.Split '\n' |> Seq.map System.Int32.Parse |> Seq.toList

    let Part1 (input: string) =
        let rec findPairWithSum list sum =
            match list with
            | [] -> None
            | first::rest ->
                let target = sum - first
                let matchingPair = rest |> List.tryFind(fun e -> e = target)  
                match matchingPair with
                | None -> findPairWithSum rest sum
                | Some x -> Some (first, x)

        match findPairWithSum (parseInput input) 2020 with
        | Some(i1, i2) -> i1 * i2
        | _ -> failwith "An unexpected issue occurred"

    let Part2 (input: string) =       
        let indexedInput = parseInput input |> Seq.toArray |> Array.indexed
        let find3WithSum array sum =
            array
            |> Array.pick
                (fun(i, a) ->
                    array.[i+1..]
                    |> Array.tryPick
                        (fun(i, b) ->
                            array.[i+1..]
                            |> Array.tryFind (fun(_, c) -> a + b + c = sum)
                            |> Option.map(fun (_, c) -> (a, b, c))
                        )
                )
 
        let (l1, l2, l3) = find3WithSum indexedInput 2020
        l1 * l2 * l3