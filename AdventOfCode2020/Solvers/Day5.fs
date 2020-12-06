namespace AdventOfCode2020.Solvers
module Day5 =
    type private Search = { row: char list; column: char list }
    
    let rec private findInRange (range: int * int) (queryChars: char * char) (query: char list): int  =
        let (lowerBound, upperBound) = range
        let intRange = upperBound - lowerBound
        let oddMod = intRange % 2;
        let (lowerChar, upperChar) = queryChars
        let updateBound c =
            match c = lowerChar with
            | true -> (lowerBound, upperBound - (oddMod + (intRange / 2)))
            | false -> (lowerBound + (oddMod + (intRange / 2)), upperBound)
        
        match query with
        | [] when intRange = 0 -> lowerBound
        | first::rest -> findInRange (updateBound first) queryChars rest
        | _ -> failwith "Unexpected Input - query returns a range or is empty"
        
    let private getBoardingIds (input: string) =
        let inputs = Seq.toList (input.Split '\n')
        let parsedInputs = inputs |> List.map (fun s -> { row = Seq.toList (s.Substring(0, 7)); column = Seq.toList (s.Substring(7))})
        let searchResults = parsedInputs |> List.map (fun s -> (findInRange (0, 127) ('F', 'B') s.row, findInRange (0, 7) ('L', 'R') s.column))        
        searchResults |> Seq.map (fun (r, c) -> r * 8 + c)
    
    let Part1 (input: string) =
        getBoardingIds input |> Seq.max
    
    let Part2 (input: string) =
        let (left, _ ) =
            getBoardingIds input
            |> Seq.sort
            |> Seq.pairwise
            |> Seq.filter (fun (p,c) -> p + 1 <> c)
            |> Seq.exactlyOne
        left + 1;