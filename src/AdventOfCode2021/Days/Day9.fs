module Day9


let Problem1 (input : string) =

    let map = input |> Common.Lines |> Array.map (fun e -> e |> Common.Digits |> Seq.toArray)

    let w = map |> Array.length
    let h = map[0].Length

    let isLow x y =
        let points =

            if x > 0 then [map[x-1][y]] else []
            @
            if x < w - 1 then [map[x+1][y]] else []
            @
            if y > 0 then [map[x][y-1]] else []
            @
            if y < h - 1 then [map[x][y+1]] else []
            |> List.toArray
        let v = map[x][y]
        points |> Array.where (fun e -> v >= e) |> Array.length = 0

    [|0..w-1|] |> Array.collect (fun x ->
        [|0..h-1|] |> Array.map (fun y ->
            if (isLow x y) then (map[x][y]) + 1 else 0
        )
    ) |> Array.sum


let Problem2 (input : string) =

    let map = input |> Common.Lines |> Array.map (fun e -> e.ToCharArray() |> Array.map (fun e -> e = '9'))

    let w = map |> Array.length
    let h = map[0].Length

    let getBasin x y =
        let rec points px py =
            match map[px][py] with
            | true -> []
            | _ ->
                map[px][py] <- true
                let these =
                    [(px-1,py);(px+1,py);(px,py-1);(px,py+1)]
                    |> List.where (fun (ax,ay) -> ax >= 0 && ax < w && ay >= 0 && ay < h && map[ax][ay] = false)
                    |> List.collect (fun (ax, ay) -> points ax ay)
                (px,py) :: these
        points x y |> List.length

    let basins =
        [
            for x in [|0..w-1|] do
                for y in [|0..h-1|] do
                    if not(map[x][y]) then
                        getBasin x y                    
        ]

    basins |> List.sortDescending |> List.take 3 |> List.reduce (*)