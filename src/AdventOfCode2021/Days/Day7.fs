module Day7


let Problem1 (input : string) =
    
    let crabs = input.Split(',') |> Array.map int

    let checks = [|crabs |> Array.min .. crabs |> Array.max|]

    let totalMoves (d : int) =
        crabs |> Array.sumBy (fun e -> (e - d) |> abs)
        
    checks |> Array.map totalMoves |> Array.min

    
let Problem2 (input : string) =
    
    let crabs = input.Split(',') |> Array.map int

    let costs = [|1 .. (crabs |> Array.max) - (crabs |> Array.min)|] |> Array.scan (+) 0
    
    let checks = [|crabs |> Array.min .. crabs |> Array.max|]
    
    let totalMoves (d : int) =
        crabs |> Array.sumBy (fun e -> costs[(e - d) |> abs])
    
    checks |> Array.map totalMoves |> Array.min