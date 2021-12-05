module Day5


let Problem1 input =

    let rows = input |> Common.Lines

    let lines = 
        rows 
        |> Array.map (fun e -> e |> Common.Words) 
        |> Array.map (fun e -> 
            (
                e[0].Split([|','|]) |> (fun x -> (int x[0], int x[1]))
                ,
                e[2].Split([|','|]) |> (fun x -> (int x[0], int x[1]))
            )
        )

    let calculateLine (line : (int * int) * (int * int))  =
        let first = fst line
        let last = snd line
        if fst first <> fst last && snd first <> snd last then
            [||]
        else
            if fst first <> fst last then 
                let s = if fst first < fst last then first else last
                let e = if fst first < fst last then last else first
                [|fst s..fst e|] |> Array.map (fun e -> (e, snd first))
            else
                let s = if snd first < snd last then first else last
                let e = if snd first < snd last then last else first
                [|snd s..snd e|] |> Array.map (fun e -> (fst first, e))

    let positions = lines |> Array.collect calculateLine

    positions |> Array.groupBy (fun e -> e) |> Array.where (fun (k, s) -> s.Length > 1) |> Array.length


let Problem2 input =

    let rows = input |> Common.Lines

    let lines = 
        rows 
        |> Array.map (fun e -> e |> Common.Words) 
        |> Array.map (fun e -> 
            (
                e[0].Split([|','|]) |> (fun x -> (int x[0], int x[1]))
                ,
                e[2].Split([|','|]) |> (fun x -> (int x[0], int x[1]))
            )
        )

    let calculateLine (line : (int * int) * (int * int))  =
        let first = fst line
        let last = snd line
        if fst first <> fst last && snd first <> snd last then
            let xi = if fst first < fst last then 1 else -1
            let yi = if snd first < snd last then 1 else -1
            let xs = [|fst first..xi..fst last|]
            let ys = [|snd first..yi..snd last|]
            Array.map2 (fun a b -> (a, b)) xs ys
        else
            if fst first <> fst last then 
                let s = if fst first < fst last then first else last
                let e = if fst first < fst last then last else first
                [|fst s..fst e|] |> Array.map (fun e -> (e, snd first))
            else
                let s = if snd first < snd last then first else last
                let e = if snd first < snd last then last else first
                [|snd s..snd e|] |> Array.map (fun e -> (fst first, e))

    let positions = lines |> Array.collect calculateLine

    positions |> Array.groupBy (fun e -> e) |> Array.where (fun (k, s) -> s.Length > 1) |> Array.length