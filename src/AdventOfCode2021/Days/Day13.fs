module Day13


let Problem1 (input : string) =
    
    let lines = input |> Common.Lines

    let dots = 
        lines 
        |> Array.where (fun e -> not(e.StartsWith("fold"))) 
        |> Array.map (fun e -> 
            let p = e.Split(',') |> Array.map int
            (p[0], p[1])
        )

    let fold (d : (int * int)[]) h p =
        let xc x = if h then x else min x (2 * p - x)
        let yc y = if h then min y (2 * p - y) else y
        d
        |> Array.map (fun (x, y) -> (xc x, yc y))
        |> Array.distinct

    let folds =
        lines
        |> Array.where (fun e -> e.StartsWith("fold"))

    
    let fi = (folds[0] |> Common.Words |> Array.last).Split('=')

    let folded = fold dots (fi[0] = "y") (int fi[1])

    folded |> Array.length


let Problem2 (input : string) =
    
    let lines = input |> Common.Lines

    let dots = 
        lines 
        |> Array.where (fun e -> not(e.StartsWith("fold"))) 
        |> Array.map (fun e -> 
            let p = e.Split(',') |> Array.map int
            (p[0], p[1])
        )

    let fold (d : (int * int)[]) h p =
        let xc x = if h then x else min x (2 * p - x)
        let yc y = if h then min y (2 * p - y) else y
        d
        |> Array.map (fun (x, y) -> (xc x, yc y))
        |> Array.distinct

    let folds =
        lines
        |> Array.where (fun e -> e.StartsWith("fold"))

    let foldAll =
        folds
        |> Array.fold (fun r f ->
            let fi = (f |> Common.Words |> Array.last).Split('=')
            fold r (fi[0] = "y") (int fi[1])
        ) dots

    let maxX = foldAll |> Array.map fst |> Array.max
    let maxY = foldAll |> Array.map snd |> Array.max

    for y in [0..maxY] do
        for x in [0..maxX] do
            let dot = foldAll |> Array.where (fun (xd, yd) -> xd = x && yd = y) |> Array.length > 0
            if dot then
                System.Console.Write("#")
            else
                System.Console.Write(" ")
        System.Console.WriteLine()

    0