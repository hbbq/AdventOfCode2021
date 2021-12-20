module Day20


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let map = lines[0].ToCharArray() |> Array.map (fun e -> match e with | '#' -> 1 | _ -> 0)

    let positions =
        let t = lines |> Array.tail
        [|
            for y in [0..(t.Length - 1)] do
                let l = t[y].ToCharArray()
                for x in [0..(l.Length-1)] do
                    if l[x] = '#' then (x, y)
        |]

    let xs = positions |> Array.map fst
    let ys = positions |> Array.map snd
    let minx = (xs |> Array.min) - 10
    let maxx = (xs |> Array.max) + 10
    let miny = (ys |> Array.min) - 10
    let maxy = (ys |> Array.max) + 10

    let bitsToNum (bits : int[]) =
        bits[0] * 256 +
        bits[1] * 128 +
        bits[2] * 64 +
        bits[3] * 32 +
        bits[4] * 16 +
        bits[5] * 8 +
        bits[6] * 4 +
        bits[7] * 2 +
        bits[8]

    let enhance (m : (int*int)[]) =
        [|
            for x in [minx..maxx] do
                for y in [miny..maxy] do
                    let num =
                        [| 
                            (x-1,y-1) ; (x,y-1) ; (x+1,y-1)
                            (x-1,y) ; (x,y) ; (x+1,y)
                            (x-1,y+1) ; (x,y+1) ; (x+1,y+1)
                        |]
                        |> Array.map (fun (a,b) ->
                            if (m |> Array.tryFind (fun (c, d) -> c = a && d = b)).IsSome then 1 else 0
                        )
                        |> bitsToNum
                    if map[num] = 1 then (x, y)

        |]

    let rec lop (m : (int*int)[]) n =
        if n = 0 then
            m
        else 
            let nm = enhance m
            lop nm (n - 1)

    lop positions 2 
    |> Array.where (fun (a,b) -> a > minx+1 && a < maxx-1 && b > miny+1 && b < maxy-1)
    |> Array.length

    
let Problem2 (input : string) =
    
    let lines = input |> Common.Lines

    let iter = 50
    
    let map = lines[0].ToCharArray() |> Array.map (fun e -> match e with | '#' -> 1 | _ -> 0)
    
    let positions =
        let t = lines |> Array.tail
        [|
            for y in [0..(t.Length - 1)] do
                let l = t[y].ToCharArray()
                for x in [0..(l.Length-1)] do
                    if l[x] = '#' then (x, y)
        |]
    
    let xs = positions |> Array.map fst
    let ys = positions |> Array.map snd
    let minx = (xs |> Array.min)
    let maxx = (xs |> Array.max)
    let miny = (ys |> Array.min)
    let maxy = (ys |> Array.max)
    let rmaxx = 6*iter + maxx - minx
    let rmaxy = 6*iter + maxy - miny
    let xd = 3*iter - minx
    let yd = 3*iter - miny

    let rm =
        [|
            for x in [0..rmaxx] do
                [|
                    for y in [0..rmaxy] do
                        if (positions |> Array.tryFind (fun (a,b) -> a + xd = x && b + yd = y)).IsSome then 1 else 0
                |]
        |]

    
    let bitsToNum (bits : int[]) =
        bits[0] * 256 +
        bits[1] * 128 +
        bits[2] * 64 +
        bits[3] * 32 +
        bits[4] * 16 +
        bits[5] * 8 +
        bits[6] * 4 +
        bits[7] * 2 +
        bits[8]
    
    let enhance (m : int[][]) =        
        [|
            for x in [0..rmaxx] do
                [|
                    for y in [0..rmaxy] do
                        if x = 0 || y = 0 || x = rmaxx || y =rmaxy then
                            map[if m[x][y] = 0 then 0 else 511]
                        else
                            let num =
                                [| 
                                    (x-1,y-1) ; (x,y-1) ; (x+1,y-1)
                                    (x-1,y) ; (x,y) ; (x+1,y)
                                    (x-1,y+1) ; (x,y+1) ; (x+1,y+1)
                                |]
                                |> Array.map (fun (a,b) -> m[a][b]                            
                                )
                                |> bitsToNum
                            if map[num] = 1 then 1 else 0
                |]
    
        |]
    
    let rec lop (m : int[][]) n =
        if n = 0 then
            m
        else 
            let nm = enhance m
            lop nm (n - 1)
    
    lop rm iter
    |> Array.sumBy Array.sum