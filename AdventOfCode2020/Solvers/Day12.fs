namespace AdventOfCode2020.Solvers
module Day12 =
    let inline private modulo n m =
        let mod' = n % m
        if sign mod' >= 0 then mod'
        else abs m + mod'

    let private parseN (n: char list) =
            n |> List.toArray |> System.String |> int       

    let Part1 (input: string) =
        let directions = ['N'; 'E'; 'S'; 'W']        
        let rec runInstructions (instructions: string list) (position: int*int) (direction: int) =
            let (x, y) = position;
            match instructions with
            | [] -> position
            | first :: rest ->
                let fList = first |> Seq.toList
                let forwardHandledList =
                    match fList with
                    | 'F'::n -> [directions.[direction]] @ n
                    | _ -> fList
                    
                match (forwardHandledList) with
                | 'N'::n -> runInstructions rest (x, y + parseN(n)) direction
                | 'S'::n -> runInstructions rest (x, y - parseN(n)) direction
                | 'E'::n -> runInstructions rest (x + parseN(n), y) direction
                | 'W'::n -> runInstructions rest (x - parseN(n), y) direction
                | 'L'::n -> runInstructions rest position (modulo (direction - parseN(n) / 90) 4)
                | 'R'::n -> runInstructions rest position (modulo (direction + parseN(n) / 90) 4)
                | _ -> failwith "Unsupported Instruction"

        let parsedInput = input.Split '\n' |> List.ofArray
        let (x, y) = runInstructions parsedInput (0, 0) 1
        abs x + abs y

    let rec private rotate90 (v: int*int) (n: int) =
        let (x, y) = v
        match n with
        | 0 -> v
        | i when i < 0 -> rotate90 (-y, x) (i + 1)
        | i when i > 0 -> rotate90 (y, -x) (i - 1)
        | _ -> failwith "wat"

    let Part2 (input: string) =
        let rec runInstructions (instructions: string list) (position: int*int) (waypoint: int*int) =
            let (x, y) = position
            let (wX, wY) = waypoint           
            match instructions with
            | [] -> position
            | first :: rest ->
                let fList = first |> Seq.toList                    
                match (fList) with
                | 'N'::n -> runInstructions rest position (wX, wY + parseN(n))
                | 'S'::n -> runInstructions rest position (wX, wY - parseN(n))
                | 'E'::n -> runInstructions rest position (wX + parseN(n), wY)
                | 'W'::n -> runInstructions rest position (wX - parseN(n), wY)
                | 'L'::n -> runInstructions rest position (rotate90 waypoint (-parseN(n) / 90))
                | 'R'::n -> runInstructions rest position (rotate90 waypoint (parseN(n) / 90))
                | 'F'::n -> runInstructions rest (x + wX * parseN n, y + wY * parseN n) waypoint
                | _ -> failwith "Unsupported Instruction"
        
        let parsedInput = input.Split '\n' |> List.ofArray
        let (x, y) = runInstructions parsedInput (0, 0) (10, 1)
        abs x + abs y