namespace AdventOfCode2020.Solvers
module Day6 =
    let Part1 (input: string) =
        let groupInputs = input.Split "\n\n"
        let distinctGroupInputs = groupInputs |> Seq.map (fun g -> g.Replace("\n", "") |> Seq.distinct)
        distinctGroupInputs |> Seq.sumBy (fun dg -> dg |> Seq.length)

    let Part2 (input: string) =
        let groupInputs = input.Split "\n\n"
        let groupedSets = groupInputs |> Seq.map (fun g -> g.Split('\n') |> Seq.map Set.ofSeq)
        groupedSets
        |> Seq.map (fun sets -> sets |> Seq.reduce Set.intersect)
        |> Seq.sumBy(fun sets -> sets |> Seq.length)