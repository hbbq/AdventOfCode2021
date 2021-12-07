module Day7


let Problem1 (input : string) =
    
    let crabs = input.Split(',') |> Array.map int

    let checks = [crabs |> Array.min .. crabs |> Array.max]

    let totalMoves c (d : int) =
        c |> Array.map (fun e -> System.Math.Abs(e - d)) |> Array.sum

    checks |> List.map (fun e -> (e, totalMoves crabs e)) |> List.sortBy snd |> List.head |> snd

    
let Problem2 (input : string) =
        
    let crabs = input.Split(',') |> Array.map int

    let costs = [1 .. crabs |> Array.max] |> List.scan (fun e f -> e + f) 0
    
    let checks = [crabs |> Array.min .. crabs |> Array.max]
    
    let totalMoves c (d : int) =
        c |> Array.map (fun e -> costs[System.Math.Abs(e - d)]) |> Array.sum
    
    checks |> List.map (fun e -> (e, totalMoves crabs e)) |> List.sortBy snd |> List.head |> snd