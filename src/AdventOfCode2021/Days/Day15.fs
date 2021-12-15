module Day15

let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let map = lines |> Array.map (fun e -> e |> Common.Digits |> Seq.toArray)

    let w = map[0].Length
    let h = map.Length

    let cmap = lines |> Array.map (fun e -> e |> Common.Digits |> Seq.toArray |> Array.map (fun _ -> false))

    let toCheck = new System.Collections.Generic.List<(int*int*int)>()

    toCheck.Add((0,0,0))
    cmap[0][0] <- true

    let mutable found = 0

    while found = 0 do
           
        let (x,y,v) = toCheck[0]

        toCheck.RemoveAt(0)

        let l =
            [|(x-1,y);(x+1,y);(x,y-1);(x,y+1)|]
            |> Array.where (fun (ix,iy) -> ix >= 0 && ix < w && iy >= 0 && iy < h && not(cmap[iy][ix]))
            |> Array.map (fun (ix,iy) ->
                (ix,iy,v + (map[iy][ix]))
            )

        for (ix,iy,iv) in l do            
            if ix = w - 1 && iy = h - 1 then found <- iv
            cmap[iy][ix] <- true
            toCheck.Add((ix,iy,iv))        

        toCheck.Sort((fun (_,_,a) (_,_,b) -> a.CompareTo(b)))

    found


let Problem2 (input : string) = 

    let lines = input |> Common.Lines

    let addWrap x y =
        let r = (x + y) % 9
        if r = 0 then 9 else r

    let buildLine line add =
        let digits = line |> Common.Digits
        [|
        for a in [|0..4|] do
            for digit in digits do
                addWrap digit (a + add)
        |]
    
    let map = 
        [|
        for a in [|0..4|] do
            for line in lines do
                buildLine line  a
        |]
    
    let w = map[0].Length
    let h = map.Length
    
    let cmap = [|1..h|] |> Array.map (fun _ -> [|1..w|] |> Array.map (fun _ -> false))
    
    let toCheck = new System.Collections.Generic.List<(int*int*int)>()
    
    toCheck.Add((0,0,0))
    cmap[0][0] <- true
    
    let mutable found = 0
    
    while found = 0 do
               
        let (x,y,v) = toCheck[0]
    
        toCheck.RemoveAt(0)
    
        let l =
            [|(x-1,y);(x+1,y);(x,y-1);(x,y+1)|]
            |> Array.where (fun (ix,iy) -> ix >= 0 && ix < w && iy >= 0 && iy < h && not(cmap[iy][ix]))
            |> Array.map (fun (ix,iy) ->
                (ix,iy,v + (map[iy][ix]))
            )
    
        for (ix,iy,iv) in l do            
            if ix = w - 1 && iy = h - 1 then found <- iv
            cmap[iy][ix] <- true
            toCheck.Add((ix,iy,iv))        
    
        toCheck.Sort((fun (_,_,a) (_,_,b) -> a.CompareTo(b)))
    
    found