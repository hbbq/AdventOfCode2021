module Day17


let Problem1 (input : string) =

    [1 .. -1 - (input.Split('=','.')[4] |> int)] |> List.reduce (+)
    

let Problem2 (input : string) =

    let [|xmin;xmax;ymin;ymax|] = input.Split('=','.',',') |> Array.where Common.IsNumeric |> Array.map int

    let minxv = 
        {1..xmin}
        |> Seq.scan (+) 0
        |> Seq.mapi (fun i e -> (i, e))
        |> Seq.find (fun (_, e) -> e >= xmin)
        |> fst

    let valids =
        [|
            for xv in [minxv .. xmax] do 
                for yv in [ymin .. -1-ymin] do
                    let mutable px = 0
                    let mutable py = 0
                    let mutable txv = xv
                    let mutable tyv = yv
                    while px < xmax && py > ymin && not(px >= xmin && px <= xmax && py >= ymin && py <= ymax) do
                        px <- px + txv
                        py <- py + tyv
                        if txv > 0 then txv <- txv - 1
                        tyv <- tyv - 1
                    if px >= xmin && px <= xmax && py >= ymin && py <= ymax then (xv, yv)

        |]

    valids |> Array.length