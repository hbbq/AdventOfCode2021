module Day7


let Problem1 (input : string) =
    
    let crabs = input.Split(',') |> Array.map int

    let checks = [crabs |> Array.min .. crabs |> Array.max]

    let totalMoves c (d : int) =
        c |> Array.sumBy (fun e -> (e - d) |> abs)

    checks |> List.map (fun e -> (e, totalMoves crabs e)) |> List.sortBy snd |> List.head |> snd

    
let Problem2 (input : string) =
        
    let crabs = input.Split(',') |> Array.map int

    let costs = [|1 .. crabs |> Array.max|] |> Array.scan (+) 0
    
    let checks = [crabs |> Array.min .. crabs |> Array.max]
    
    let totalMoves c (d : int) =
        c |> Array.sumBy (fun e -> costs[(e - d) |> abs])
    
    checks |> List.map (fun e -> (e, totalMoves crabs e)) |> List.sortBy snd |> List.head |> snd