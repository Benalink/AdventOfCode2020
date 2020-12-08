namespace AdventOfCode2020.Solvers
module Day8 =
    
    let private parseInstruction (instruction: string) =
        let instParts = instruction.Split ' '
        (instParts.[0], int instParts.[1])

    let runner (jumpTable: (string * int)[]) startPoint =
        let rec runInstruction instruction (visited: Set<int>) acc =
            if visited.Contains(instruction)
            then (visited |> Seq.rev, acc)
            else if instruction >= jumpTable.Length
            then (Seq.empty, acc)
            else
                let newVisited = visited.Add(instruction)            
                let (instName, instValue) = jumpTable.[instruction]
                
                match instName with
                | "nop" -> runInstruction (instruction + 1) newVisited acc
                | "jmp" -> runInstruction (instruction + instValue) newVisited acc
                | "acc" -> runInstruction (instruction + 1) newVisited (acc + instValue)
                | _ -> failwith "Instruction not supported."
        runInstruction startPoint Set.empty 0
        
    let Part1 (input: string) =
        let jumpTable = input.Split '\n' |> Array.map parseInstruction
        let (_, acc) = runner jumpTable 0
        acc
    
    let private fixProgram (jumpTable: (string * int)[]) (stackTrace: seq<int>) =
        let swapInstruction (instruction: string * int) =
            match instruction with
            | ("nop", i) -> ("jmp", i)
            | ("jmp", i) -> ("nop", i)
            | _ -> failwith "Unsupported Instruction"


        stackTrace
        |> Seq.filter (fun i -> (fst (jumpTable.[i])) <> "acc")
        |> Seq.map (fun i ->
            let instruction = jumpTable.[i]
            let testInstruction = swapInstruction instruction
            jumpTable.[i] <- testInstruction
            let r = runner jumpTable 0
            jumpTable.[i] <- instruction
            r)
        |> Seq.find (fun (st, _) -> Seq.isEmpty st)        
           
    let Part2 (input: string) =
        let jumpTable = input.Split '\n' |> Array.map parseInstruction
        let (st, _) = runner jumpTable 0
        let (_, acc) = fixProgram jumpTable st
        acc