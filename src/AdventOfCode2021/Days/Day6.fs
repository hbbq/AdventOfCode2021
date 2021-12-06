module Day6


let Problem1 (input : string) = 
    
    let fish = input.Split([|','|]) |> Array.map int

    let nextDay f = 
        let newFish = f |> Array.where (fun e -> e = 0) |> Array.map (fun e -> 8)
        let newAges = f |> Array.map (fun e -> match e with | 0 -> 6 | _ -> e - 1)
        Array.append newAges newFish

    let rec simulate f d =
        if d = 0 then
            f
        else
            let n = nextDay f
            simulate n (d - 1)

    let total = simulate fish 80 |> Array.length

    total


let Problem2 (input : string) = 
    
    let fish = input.Split([|','|]) |> Array.map int

    let nums = 
        [0..8]
        |> List.map (fun e -> fish |> Array.where (fun f -> f = e) |> Array.length) 
        |> List.map int64

    let nextDay f _ = 
        f
        |> List.mapi (fun i e ->
            match i with
            | 8 -> f[0]
            | 6 -> f[7] + f[0]
            | _ -> f[i + 1]
        )

    [1..256] |> List.fold nextDay nums |> List.sum
