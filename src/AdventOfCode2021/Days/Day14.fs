module Day14


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let template = lines[0].ToCharArray() |> Array.toList

    let pairs = 
        lines 
        |> Array.tail
        |> Array.map (fun e ->
            let [|a; _; b|] = e.Split(' ')
            (a.ToCharArray(), (b |> char))
        )

    let insert (t : char list) _ =
        t.Head :: (
            t |> List.pairwise |> List.collect (fun (a ,b) ->
                let m = pairs |> Array.tryFind (fun (c, _) -> c[0] = a && c[1] = b)
                match m with
                | None -> [b]
                | Some (_, n) -> [n; b]
            )
        )

    let lop = [1..10] |> List.fold insert template

    let grouped = lop |> List.groupBy (fun e -> e) |> List.map (fun (_, l) -> l.Length)

    (grouped |> List.max) - (grouped |> List.min)

    
    
let Problem2 (input : string) =
    
    let lines = input |> Common.Lines
    
    let template = lines[0].ToCharArray() |> Array.toList
    
    let pairs = 
        lines 
        |> Array.tail
        |> Array.map (fun e ->
            let [|a; _; b|] = e.Split(' ')
            (a.ToCharArray(), (b |> char))
        )
    
    let allPairs =
        let chars = (template @ (pairs |> Array.map snd |> Array.toList)) |> List.distinct
        List.allPairs chars chars

    let counts = 
        (' ', template[0], 1L) :: (
            allPairs |> List.map (fun (e, f) -> (e, f, template |> List.pairwise |> List.where (fun (a, b) -> a = e && b = f) |> List.length |> int64))
        )

    let insert (l : (char * char * int64) list) _ =
        let collect =
            l |> List.collect (fun (a, b, l) ->
                let m = pairs |> Array.tryFind (fun (e, _) -> e[0] = a && e[1] = b)
                match m with
                | None -> [(a, b, l)]
                | Some (_, n) -> [(a, n, l); (n, b, l)]
            )
        let nc = l |> List.map (fun (a, b, _) -> (a, b, collect |> List.where (fun (c, d, _) -> c = a && d = b) |> List.sumBy (fun (_, _, l) -> l)))
        nc

    let lop = [1..40] |> List.fold insert counts

    let chars = lop |> List.map (fun (_, c, l) -> (c, l))

    let charCounts = chars |> List.map (fst) |> List.distinct |> List.map (fun e -> 
        chars |> List.where (fun (f, l) -> f = e) |> List.sumBy snd
    )
        
    (charCounts |> List.max) - (charCounts |> List.min)
