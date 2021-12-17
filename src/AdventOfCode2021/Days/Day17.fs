module Day17


let Problem1 (input : string) =

    Seq.sum {1 .. -1 - (input.Split('=','.')[4] |> int)}
    

let Problem2 (input : string) =

    let [|xmin;xmax;ymin;ymax|] = input.Split('=','.',',') |> Array.where Common.IsNumeric |> Array.map int

    let minxv = xmin * 2 |> float |> sqrt |> floor |> int

    let rec step x y (xv, yv) =

                let nx = x + xv
                let ny = y + yv
                let hit = nx >= xmin && nx <= xmax && ny >= ymin && ny <= ymax
                let miss = nx > xmax || ny < ymin

                if not(hit || miss) then
                    step nx ny ((max (xv - 1) 0), (yv - 1))
                else
                    hit

    List.allPairs [minxv .. xmax] [ymin .. -1-ymin] |> List.where (step 0 0) |> List.length