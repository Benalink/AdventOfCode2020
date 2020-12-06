namespace AdventOfCode2020.Solvers
module Day6 =
    let private parseGroups (input: string) = input.Split "\n\n"

    let Part1 (input: string) =
        parseGroups input 
        |> Seq.map (fun g -> g.Replace("\n", "") |> Seq.distinct)
        |> Seq.sumBy (fun dg -> dg |> Seq.length)

    let Part2 (input: string) =
        parseGroups input
        |> Seq.map (fun g -> g.Split('\n') |> Seq.map Set.ofSeq)
        |> Seq.sumBy(fun sets -> sets |> Seq.reduce Set.intersect |> Seq.length)