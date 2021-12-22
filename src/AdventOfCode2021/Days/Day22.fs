module Day22


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let lineToStep (l : string) =
        let p = l.Split(' ','=','.',',')
        let onOff = p[0] = "on"
        let [|xs;xe;ys;ye;zs;ze|] = p |> Array.where Common.IsNumeric |> Array.map int
        (onOff,xs,xe,ys,ye,zs,ze)

    let steps = lines |> Array.map lineToStep |> Array.where (fun (_,a,b,c,d,e,f) -> a <= 50 && b >= -50 && c <= 50 && d >= -50 && e <= 50 && f >= -50)

    let calc =

        let getAll (a : int[]) =
            a |> Array.distinct |> Array.append [| (a |> Array.max) + 1 |] |> Array.sort 
            |> Array.pairwise
            |> Array.collect (fun (a,b) -> 
                [|
                    (a,a)
                    if b > a + 1 then
                        (a+1,b-1)
                |]
            )
            |> Array.map (fun (a,b) -> (max a -50, min b 50))
            |> Array.distinct
            |> Array.map (fun (a,b) -> (a,b,1+b-a))
                    
        let xps = steps |> Array.collect (fun (_,a,b,_,_,_,_) -> [|a;b|]) |> getAll
        let yps = steps |> Array.collect (fun (_,_,_,a,b,_,_) -> [|a;b|]) |> getAll
        let zps = steps |> Array.collect (fun (_,_,_,_,_,a,b) -> [|a;b|]) |> getAll
                
        let collect = 
            [|

                for (xs,xe,xc) in xps do
            
                    for (ys,ye,yc) in yps do
                
                        for(zs,ze,zc) in zps do

                            let onOrOff = 
                                match (steps |> Array.rev |> Array.tryFind (fun (_,a,b,c,d,e,f) -> xs >= a && xe <= b && ys >= c && ye <= d && zs >= e && ze <= f)) with
                                | Some (a,_,_,_,_,_,_) -> a
                                | _ -> false

                            if onOrOff then
                                xc*yc*zc
            |]

        collect |> Array.sum

    calc


let Problem2 (input : string) =

    let lines = input |> Common.Lines

    let lineToStep (l : string) =
        let p = l.Split(' ','=','.',',')
        let onOff = p[0] = "on"
        let [|xs;xe;ys;ye;zs;ze|] = p |> Array.where Common.IsNumeric |> Array.map int
        (onOff,xs,xe,ys,ye,zs,ze)

    let steps = lines |> Array.map lineToStep

    let calc =

        let getAll (arr : int[]) =
            arr |> Array.distinct |> Array.append [| (arr |> Array.max) + 1 |] |> Array.sort 
            |> Array.pairwise
            |> Array.collect (fun (a,b) -> 
                [|
                    (a,a)
                    if b > a + 1 then
                        (a+1,b-1)
                |]
            )
            |> Array.distinct
            |> Array.map (fun (a,b) -> (a,b,int64 (1+b-a)))
                                
        let xps = steps |> Array.collect (fun (_,a,b,_,_,_,_) -> [|a;b|]) |> getAll
        let yps = steps |> Array.collect (fun (_,_,_,a,b,_,_) -> [|a;b|]) |> getAll
        let zps = steps |> Array.collect (fun (_,_,_,_,_,a,b) -> [|a;b|]) |> getAll

        let cs = steps |> Array.rev
                
        let collect = 
            [|

                for (xs,xe,xc) in xps do

                    let xsteps = cs |> Array.where (fun (_,sxs,sxe,_,_,_,_) -> sxs <= xe && sxe >= xs)

                    let miny = xsteps |> Array.map (fun (_,_,_,a,_,_,_) -> a) |> Array.min
                    let maxy = xsteps |> Array.map (fun (_,_,_,_,a,_,_) -> a) |> Array.max
            
                    for (ys,ye,yc) in yps do

                        if ys <= maxy && ye >= miny then

                            let ysteps = xsteps |> Array.where (fun (_,_,_,sys,sye,_,_) -> sys <= ye && sye >= ys)
                            
                            if ysteps.Length > 0 then
                            
                                let minz = ysteps |> Array.map (fun (_,_,_,_,_,a,_) -> a) |> Array.min
                                let maxz = ysteps |> Array.map (fun (_,_,_,_,_,_,a) -> a) |> Array.max
                
                                for(zs,ze,zc) in zps do

                                    if zs <= maxz && ze >= minz then

                                        match (ysteps |> Array.tryFind (fun (_,a,b,c,d,e,f) -> xs >= a && xe <= b && ys >= c && ye <= d && zs >= e && ze <= f)) with
                                        | Some (a,_,_,_,_,_,_) -> if a then xc*yc*zc
                                        | _ -> 0 |> ignore
    
            |]

        collect |> Array.sum

    calc
