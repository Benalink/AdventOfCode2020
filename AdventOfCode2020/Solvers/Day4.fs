namespace AdventOfCode2020.Solvers
module Day4 =
    open System.Text.RegularExpressions

    let private getValidSizedPassports (input: string) =
        let validatePassportSize (passport: string) =
            let length = passport.Split ' ' |> Array.length
            match length with
            | 8 -> true
            | 7 -> not (passport.Contains "cid:")
            | _ -> false
            
        input.Split "\n\n"
        |> Array.map (fun p -> p.Replace('\n', ' '))
        |> Array.filter validatePassportSize

    let Part1 (input: string) =
        getValidSizedPassports input
        |> Array.length

    let Part2 (input: string) =
        let (|Regex|_|) pattern input =
            let m = Regex.Match(input, pattern)
            if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
            else None
            
        let validYr (minYear: int) (maxYear: int) (input: string) =
            let parsedInput = System.Int32.TryParse input
            match parsedInput with
            | (false, _) -> false
            | (true, x)  -> minYear <= x && x <= maxYear

        let validByr = validYr 1920 2002
        let validIyr = validYr 2010 2020
        let validEyr = validYr 2020 2030

        let (|IntStrSuffix|_|) (s:string) (test:string) =
            if test.EndsWith(s) then
                let splitPoint = test.Length - s.Length
                Some((int (test.Substring(0, splitPoint)), test.Substring(splitPoint)))
            else
                None    

        let validHgt (input: string) =
            match input with
            | IntStrSuffix "cm" (v, _) -> v >= 150 && v <= 193
            | IntStrSuffix "in" (v, _) -> v >= 59 && v <= 76
            | _ -> false

        let validHcl (input: string) =
            match input with
            | Regex @"#[0-9a-f]{6}" _ -> true
            | _ -> false

        let validEcl (input: string) =
            match input with
            | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
            | _ -> false

        let validPid (input: string) =
            match input with
            | Regex @"^[0-9]{9}$" _ -> true
            | _ -> false

        let validateDetail (detailId: string) (value: string) =
            match detailId with
            | "byr" -> validByr value
            | "iyr" -> validIyr value
            | "eyr" -> validEyr value
            | "hgt" -> validHgt value
            | "hcl" -> validHcl value
            | "ecl" -> validEcl value
            | "pid" -> validPid value
            | "cid" -> true
            | _ -> false

        let validatePassportDetails (parts: List<string>) =
            parts
            |> List.chunkBySize 2
            |> List.forall (fun c -> validateDetail c.[0] c.[1])

        getValidSizedPassports input
            |> Array.filter (fun p ->
                let details = p.Split [|':'; ' '|] |> Seq.toList
                validatePassportDetails details)
            |> Array.length