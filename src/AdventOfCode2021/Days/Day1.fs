module Day1


let Problem1 input =
        
    let Nums = input |> Common.Lines |> Array.map int

    let Count = Nums |> Array.windowed 2 |> Array.where (fun e -> e[0] < e[1]) |> Array.length

    Count


let Problem2 input =

    let Nums = input |> Common.Lines |> Array.map int

    let Windowed = Nums |> Array.windowed 3 |> Array.map (fun e -> e |> Array.sum)

    let Count = Windowed |> Array.windowed 2 |> Array.where (fun e -> e[0] < e[1]) |> Array.length

    Count