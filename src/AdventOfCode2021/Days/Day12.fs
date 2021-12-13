module Day12


let Problem1 (input : string) =
    
    let lines = input |> Common.Lines

    let segs = 
        lines 
        |> Array.collect (fun e -> 
            let p = e.Split('-')
            [|(p[0], p[1]); (p[1],p[0])|]
        )

    let rec next where (path : string list) =
        let tp = where :: path
        if where = "end" then
            [tp]
        else
            let valids = 
                segs 
                |> Array.where (fun (a, b) -> a = where && not(b.ToLower() = b && path |> List.contains b) && b <> "start")
            valids |> Array.toList |> List.collect (fun (_, b) -> next b tp)
    
    let paths = next "start" []

    paths |> List.length
    
    
let Problem2 (input : string) =

    let lines = input |> Common.Lines
    
    let segs = 
        lines 
        |> Array.collect (fun e -> 
            let p = e.Split('-')
            [|(p[0], p[1]); (p[1],p[0])|]
        )
    
    let rec next where (path : string list) =
        let tp = where :: path
        if where = "end" then
            [tp]
        else            
            let allowRevisit =
                tp |> List.where (fun e -> e.ToLower() = e) |> List.groupBy (fun e -> e) |> List.where (fun (_, v) -> v |> List.length > 1) |> List.length = 0
            let valids = 
                segs 
                |> Array.where (fun (a, b) -> a = where && not(not(allowRevisit) && b.ToLower() = b && path |> List.contains b) && b <> "start")
            valids |> Array.toList |> List.collect (fun (_, b) -> next b tp)
        
    let paths = next "start" []
    
    paths |> List.length