module Day17


let Problem1 (input : string) =

    Seq.sum {1 .. -1 - (input.Split('=','.')[4] |> int)}
    

let Problem2 (input : string) =

    let [|xmin;xmax;ymin;ymax|] = input.Split('=','.',',') |> Array.where Common.IsNumeric |> Array.map int

    let minxv = xmin * 2 |> float |> sqrt |> floor |> int

    let valids =
        [|
            for xv in [minxv .. xmax] do 
                for yv in [ymin .. -1-ymin] do
                    let rec step x y xv yv =
                        let nx = x + xv
                        let ny = y + yv
                        if nx >= xmin && nx <= xmax && ny >= ymin && ny <= ymax then
                            true
                        elif nx > xmax || ny < ymin then
                            false
                        else
                            let nxv = if xv = 0 then 0 else xv - 1
                            let nyv = yv - 1
                            step nx ny nxv nyv
                    if (step 0 0 xv yv) then (xv, yv)
        |]

    valids |> Array.length