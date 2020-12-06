namespace AdventOfCode2020.Solvers
module Day3 =
    let private generatePath xLength yLength vec =
        let (x, y) = vec
        let reps = (yLength - 1) / y
        [1..reps] |> Seq.map (fun i -> ((x * i) % xLength, y * i))
    
    let private calculateHits (array: string[]) (path: seq<int * int>) =
        path |> Seq.filter (fun (x, y) -> array.[y].[x] = '#') |> Seq.length

    let Part1 (input: string) =        
        let parsedInput = input.Split '\n' |> Seq.toArray
        let generatePathWithInput = generatePath parsedInput.[0].Length parsedInput.Length
        let path = generatePathWithInput (3, 1)
        calculateHits parsedInput path

    let Part2 (input: string) =  
        let pathVectors = [|(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)|]
        let parsedInput = input.Split '\n' |> Seq.toArray
        let generatePathWithInput = generatePath parsedInput.[0].Length parsedInput.Length
        
        pathVectors |> Seq.map generatePathWithInput
        |> Seq.map (fun p -> int64 (calculateHits parsedInput p))
        |> Seq.reduce (*)