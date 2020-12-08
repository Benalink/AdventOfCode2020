namespace AdventOfCode2020.Solvers
module Day8 =
    
    let private parseInstruction (instruction: string) =
        let instParts = instruction.Split ' '
        (instParts.[0], int instParts.[1])

    let runner (jumpTable: string[]) startPoint =
        let rec runInstruction instruction (visited: Set<int>) acc =
            if visited.Contains(instruction)
            then (visited |> Seq.rev, acc)
            else if instruction >= jumpTable.Length
            then (Seq.empty, acc)
            else
                let newVisited = visited.Add(instruction)            
                let (instName, instValue) = parseInstruction jumpTable.[instruction]
                
                match instName with
                | "nop" -> runInstruction (instruction + 1) newVisited acc
                | "jmp" -> runInstruction (instruction + instValue) newVisited acc
                | "acc" -> runInstruction (instruction + 1) newVisited (acc + instValue)
                | _ -> failwith "Instruction not supported."
        runInstruction startPoint Set.empty 0
        
    let Part1 (input: string) =
        let jumpTable = input.Split '\n'
        let (_, acc) = runner jumpTable 0
        acc
    
    let private fixProgram (jumpTable: string[]) (stackTrace: seq<int>) =
        let swapInstruction (instruction: string) =
            let op =
                match instruction.Substring(0, 3) with
                | "nop" -> "jmp"
                | "jmp" -> "nop"
                | _ -> failwith "Unsupported Instruction"
            op + instruction.Substring 3

        stackTrace
        |> Seq.filter (fun i -> jumpTable.[i].Contains("acc") |> not)
        |> Seq.map (fun i ->
            let testInstruction = swapInstruction jumpTable.[i]
            let testTable = jumpTable |> Array.copy
            testTable.[i] <- testInstruction
            runner testTable 0)
        |> Seq.find (fun (st, _) -> Seq.isEmpty st)        
           
    let Part2 (input: string) =
        let jumpTable = input.Split '\n'
        let (st, _) = runner jumpTable 0
        let (_, acc) = fixProgram jumpTable st
        acc