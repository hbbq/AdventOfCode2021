module Day7


let Problem1 (input : string) =
    
    let crabs = input.Split(',') |> Array.map int
    
    let min = crabs |> Array.min
    let max = crabs |> Array.max

    let totalMoves (d : int) =
        crabs |> Array.sumBy (fun e -> (e - d) |> abs)
        
    [|min .. max|] |> Array.minBy totalMoves

    
let Problem2 (input : string) =
    
    let crabs = input.Split(',') |> Array.map int

    let min = crabs |> Array.min
    let max = crabs |> Array.max

    let costs = [|1 .. max - min|] |> Array.scan (+) 0
            
    let totalFuel d =
        crabs |> Array.sumBy (fun e -> costs[(e - d) |> abs])
    
    [|min .. max|] |> Array.minBy totalFuel