namespace AdventOfCode2020.Solvers
module Day7 =
    type private Bag = { Colour: string; Contents: (string * int) list }
    
    let removeBagSuffix (input: string) =
        let a = input.Replace("bags.", "")
        let b = a.Replace("bag.", "")
        let c = b.Replace("bags", "")
        c.Replace("bag", "")
        
    let private parseBag (bagDescription: string) =
        let contentSplitter = "contain"
        let contentSplit = bagDescription.Split contentSplitter
        let bagColour = (removeBagSuffix contentSplit.[0]).Trim()
        let contents = contentSplit.[1].Split ","
        if contents.[0].Contains("no other bags") then
            { Colour = bagColour; Contents = [] }
        else
        let parsedContents =
            contents
            |> Seq.map (fun c ->
                let trimmed = c.Trim()
                let spacerIndex = trimmed.IndexOf " "
                ((removeBagSuffix (trimmed.Substring (spacerIndex + 1))).Trim(), int (trimmed.Substring(0, spacerIndex))))
            |> Seq.toList

        { Colour = bagColour; Contents = parsedContents }
    
    let Part1 (input: string) =
        let rec coloursThatCanContainColour (bags: seq<Bag>) (colours: string list) =
            let newColours =
                bags
                |> Seq.filter (fun b ->
                               Set.intersect (Set.ofSeq (b.Contents |> Seq.map (fun (c, _) -> c))) (Set.ofList colours)
                               |> Set.isEmpty
                               |> not)
                |> Seq.filter (fun b -> colours |> Seq.exists (fun c -> b.Colour = c) |> not)
                |> Seq.map (fun b -> b.Colour)
                |> Seq.toList
                
            match newColours with
            | [] -> colours
            | l -> coloursThatCanContainColour bags (colours @ l)

        let parsedBags =
            input.Split '\n'
            |> Seq.map parseBag
        
        (coloursThatCanContainColour parsedBags ["shiny gold"] |> Seq.length) - 1
        
    let Part2 (input: string) =
        let rec countBagsInBag (bag: Bag) (bagsMap: Map<string, Bag>) =
            match bag.Contents with
            | [] -> 1
            | (c, i)::rest -> i * countBagsInBag (bagsMap.[c]) bagsMap + countBagsInBag { bag with Contents = rest } bagsMap
        
        let parsedBags =
            input.Split '\n'
            |> Seq.map parseBag
        
        let bagMap =
            parsedBags
            |> Seq.map (fun b -> b.Colour, b) |> Map
        
        let shinyGoldBag = parsedBags |> Seq.find (fun b -> b.Colour = "shiny gold")
        (countBagsInBag shinyGoldBag bagMap) - 1
