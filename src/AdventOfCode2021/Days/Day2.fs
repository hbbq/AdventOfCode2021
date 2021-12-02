module Day2


let Problem1 input =
    
    let Commands =
        input |> Common.Lines |> Array.map Common.Words
        
    let Forward =
        Commands |> Array.where (fun e -> e[0] = "forward") |> Array.map (fun e -> e[1]) |> Array.map int |> Array.sum

    let Up =
        Commands |> Array.where (fun e -> e[0] = "up") |> Array.map (fun e -> e[1]) |> Array.map int |> Array.sum

    let Down =
        Commands |> Array.where (fun e -> e[0] = "down") |> Array.map (fun e -> e[1]) |> Array.map int |> Array.sum

    let Total = Forward * (Down - Up)

    Total


let Problem2 input =
    
    let Commands =
        input |> Common.Lines |> Array.map Common.Words

    let AimChange (c : string[]) =
        match c[0] with
        | "up" -> int c[1] * -1
        | "down" -> int c[1]
        | _ -> 0

    let TotalAims =
        Commands |> Array.map AimChange |> (Array.scan (+) 0 >> Array.tail)

    let Forward = 
        Commands |> Array.where (fun e -> e[0] = "forward") |> Array.map (fun e -> e[1]) |> Array.map int |> Array.sum

    let DepthMovement (a : string[]) b =
        match a[0] with
        | "forward" -> int a[1] * b
        | _ -> 0

    let Down =
        Array.map2 (fun a b -> DepthMovement a b) Commands TotalAims |> Array.sum

    let Total = Forward * Down

    Total