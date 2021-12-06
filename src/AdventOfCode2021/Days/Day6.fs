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
        [|0..8|]
        |> Array.map (fun e -> fish |> Array.where (fun f -> f = e) |> Array.length)
        |> Array.map int64

    let nextDay (f : int64[]) = 
        [| f[1]; f[2]; f[3]; f[4]; f[5]; f[6]; f[7] + f[0]; f[8]; f[0] |]

    let rec simulate f d =
        if d = 0 then
            f
        else
            let n = nextDay f
            simulate n (d - 1)

    let total = simulate nums 256 |> Array.sum

    total