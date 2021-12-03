module Day3


let Problem1 input =
    
    let Lines = input |> Common.Lines

    let BitsLines = Lines |> Array.map (fun e -> e |> Common.Digits |> Seq.toArray)

    let TotalCount = Lines |> Array.length

    let MostCommon pos =
        let Sum = BitsLines |> Array.map (fun e -> e[pos-1]) |> Array.sum
        if Sum > TotalCount / 2 then
            1
        else
            0

    let BitCount = BitsLines[0] |> Array.length

    let Powers = [1..BitCount] |> Seq.map (fun e -> int (2.0 ** float (BitCount - e))) |> Seq.toArray

    let Gamma =
        [1..BitCount] |> Seq.map (fun e -> (MostCommon e) * Powers[e-1]) |> Seq.sum

    let Epsilon =
        [1..BitCount] |> Seq.map (fun e -> (1 - (MostCommon e)) * Powers[e-1]) |> Seq.sum    

    Gamma * Epsilon


let Problem2 input =

    
    let Lines = input |> Common.Lines

    let BitsLines = Lines |> Array.map (fun e -> e |> Common.Digits |> Seq.toArray)

    let TotalCount = Lines |> Array.length

    let MostCommon (list : int[][]) tie pos =
        let Sum = list |> Array.map (fun e -> e[pos-1]) |> Array.sum
        if Sum*2 = (list |> Array.length) then
            tie
        else if Sum*2 > (list |> Array.length) then
            1
        else
            0

    let BitCount = BitsLines[0] |> Array.length

    let Powers = [1..BitCount] |> Seq.map (fun e -> int (2.0 ** float (BitCount - e))) |> Seq.toArray
    
    let Oxygen =
        let rec Filter (list : int[][]) pos =
            let Common = MostCommon list 1 pos
            let Filtered = list |> Array.where (fun e -> e[pos - 1] = Common)
            if (Filtered |> Array.length) = 1 then
                Filtered[0]
            else
                Filter Filtered (pos + 1)
        let Found = Filter BitsLines 1
        [1..BitCount] |> Seq.map (fun e -> Found[e-1] * Powers[e-1]) |> Seq.sum

    let CO2 =
        let rec Filter (list : int[][]) pos =
            let Common = MostCommon list 1 pos
            let Filtered = list |> Array.where (fun e -> e[pos - 1] <> Common)
            if (Filtered |> Array.length) = 1 then
                Filtered[0]
            else
                Filter Filtered (pos + 1)
        let Found = Filter BitsLines 1
        [1..BitCount] |> Seq.map (fun e -> Found[e-1] * Powers[e-1]) |> Seq.sum

    Oxygen * CO2