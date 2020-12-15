namespace AdventOfCode2020.Solvers
module Day11 =        
    let cartesian xs ys = 
        xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))
        
    let addTuple a b =
        let (a1, a2) = a
        let (b1, b2) = b
        (a1 + b1, a2 + b2)
        
    let private neighbourVectors =
        cartesian [-1; 0; 1] [-1; 0; 1]
        |> Seq.filter (fun v -> v <> (0, 0))
        |> Seq.toList
    
    let applyRule seat neighbours tolerance =
        match seat with
        | '.' -> '.'
        | 'L' ->
            let filled = neighbours |> Seq.exists (fun s -> s = '#')
            match filled with
            | true -> 'L'
            | false -> '#'
        | '#' ->
            let filled = neighbours |> Seq.filter (fun s -> s = '#') |> Seq.length
            if filled < tolerance then '#' else 'L'
        | _ -> failwith "Invalid Seat Value"
    
    let rec private findStablePlan map getNeighbours tolerance =
        let nextMap =
            map |> Map.map(fun k v ->  applyRule v (getNeighbours map k) tolerance) 
        
        match nextMap with
        | m when m = map -> map
        | x -> findStablePlan x getNeighbours tolerance

    let Part1 (input: string) =            
        let getNeighbours (map: Map<int*int, char>) (position: int * int) =        
                neighbourVectors
                |> Seq.map (fun nv -> map.TryFind((addTuple position nv)))
                |> Seq.choose id
        
        let parsedInput =
            input.Split '\n'
            |> Array.map (fun l -> l.ToCharArray())
            |> Array.mapi (fun y r -> r |> Array.mapi (fun x c -> ((x, y), c) ))
            |> Array.collect id
            |> Map.ofArray
                   
        findStablePlan parsedInput getNeighbours 4
        |> Seq.filter (fun s -> s.Value = '#')
        |> Seq.length
    
    let Part2 (input: string) =
         let rec findVisibleSeat (map: Map<int*int, char>) (position: int * int) (vector: int*int) =
             let newPos = addTuple position vector
             let next = map.TryFind(newPos)
             
             match next with
             | None -> None
             | Some('.') -> findVisibleSeat map newPos vector
             | Some(x) -> Some(x)
         
         let getNeighbours (map: Map<int*int, char>) (position: int * int) =        
                neighbourVectors
                |> Seq.map (findVisibleSeat map position)
                |> Seq.choose id
         
         let parsedInput =
            input.Split '\n'
            |> Array.map (fun l -> l.ToCharArray())
            |> Array.mapi (fun y r -> r |> Array.mapi (fun x c -> ((x, y), c) ))
            |> Array.collect id
            |> Map.ofArray
                   
         findStablePlan parsedInput getNeighbours 5
         |> Seq.filter (fun s -> s.Value = '#')
         |> Seq.length
        