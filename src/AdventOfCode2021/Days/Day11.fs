module Day11


let Problem1 (input : string) =

    let lines = input |> Common.Lines
    
    let w = lines[0].Length
    let h = lines.Length

    let map = lines |> Array.collect (fun e -> e.ToCharArray() |> Array.map (string >> int))

    let c2i x y =
        x + y * w

    let i2c i =
        (i % w, (i - (i % w)) / h)

    let nextStep (p, (m : int[])) _ =
        
        let t = m |> Array.mapi (fun i e -> (i, e + 1))
        let flashes = 0
        
        let l = new System.Collections.Generic.List<int>()
        let q = new System.Collections.Generic.Queue<int>()
        
        for i in t |> Array.where (fun (_, e) -> e > 9) |> Array.map (fun (i, _) -> i) do
            l.Add i
            q.Enqueue i

        while q.Count > 0 do
            let i = q.Dequeue()
            let (x, y) = i2c i
            for xn in [x-1..x+1] do
                for yn in [y-1..y+1] do
                    if not(xn = x & yn = y) then
                        if (xn >= 0 && xn < w && yn >= 0 && yn < h) then
                            let ix = c2i xn yn
                            let v = snd t[ix]
                            if v = 9 then 
                                q.Enqueue ix
                                l.Add ix
                            t[ix] <- (ix, snd t[ix] + 1)

        (p + l.Count, t |> Array.map (fun (i, e) -> if e > 9 then 0 else e))

    let r = 
        [|1..100|] |> Array.fold nextStep (0,map)

    fst r

    
let Problem2 (input : string) =
    
    let lines = input |> Common.Lines
        
    let w = lines[0].Length
    let h = lines.Length
    
    let map = lines |> Array.collect (fun e -> e.ToCharArray() |> Array.map (string >> int))
    
    let c2i x y =
        x + y * w
    
    let i2c i =
        (i % w, (i - (i % w)) / h)
    
    let nextStep (m : int[]) =
            
        let t = m |> Array.mapi (fun i e -> (i, e + 1))
        let flashes = 0
            
        let l = new System.Collections.Generic.List<int>()
        let q = new System.Collections.Generic.Queue<int>()
            
        for i in t |> Array.where (fun (_, e) -> e > 9) |> Array.map (fun (i, _) -> i) do
            l.Add i
            q.Enqueue i
    
        while q.Count > 0 do
            let i = q.Dequeue()
            let (x, y) = i2c i
            for xn in [x-1..x+1] do
                for yn in [y-1..y+1] do
                    if not(xn = x & yn = y) then
                        if (xn >= 0 && xn < w && yn >= 0 && yn < h) then
                            let ix = c2i xn yn
                            let v = snd t[ix]
                            if v = 9 then 
                                q.Enqueue ix
                                l.Add ix
                            t[ix] <- (ix, snd t[ix] + 1)
    
        (l.Count, t |> Array.map (fun (i, e) -> if e > 9 then 0 else e))
    
    let rec t l i = 
        let r = nextStep l
        if (fst r = map.Length) then
            i
        else 
            t (snd r) (i + 1)

    let r = t map 1

    r