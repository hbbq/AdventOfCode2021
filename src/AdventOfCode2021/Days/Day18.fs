module Day18


type thing = 
    | PIn
    | POut
    | PDelim
    | Regular of int
    | PIgnore

let Problem1 (input : string) = 

    let lines = input |> Common.Lines
    
    let strToList (a : string) =
        a.ToCharArray() |> Array.map (fun e ->
            match e with
            | '[' -> PIn
            | ']' -> POut
            | ',' -> PDelim
            | x -> Regular (x |> string |> int)
        )
        |> Array.toList

    let listToStr (a : thing list) =
        a
        |> List.map (fun e ->
            match e with
            | PIn -> "["
            | POut -> "]"
            | PDelim -> ","
            | PIgnore -> ""
            | Regular x -> x |> string
        )
        |> List.reduce (+)

    let reduceOrSplit (a : thing list) =

        //System.Diagnostics.Debug.WriteLine (a |> listToStr)
        
        let mutable Depth = 0
        let mutable arr = a |> List.toArray
        let mutable hit = false

        for lp in [0..1] do
            for i in [0 .. (arr |> Array.length) - 1] do
                if not(hit) then
                    let v = arr[i]
                    match v with
                    | PIn -> Depth <- Depth + 1
                    | POut -> Depth <- Depth - 1
                    | Regular x ->
                        if x > 9 && lp = 1 then
                            hit <- true
                            let l = (x / 2)
                            let r = (x / 2) + (x % 2)
                            arr[i] <- PIn                
                            arr <- Array.insertAt (i + 1) (Regular l) arr
                            arr <- Array.insertAt (i + 2) (PDelim) arr
                            arr <- Array.insertAt (i + 3) (Regular r) arr
                            arr <- Array.insertAt (i + 4) (POut) arr
                        elif Depth = 5 && lp = 0 then
                            if (match arr[i-1] with | PIn -> true | _ -> false) then
                                hit <- true
                                let l = x
                                let r = match arr[i + 2] with | Regular y -> y | _ -> 0
                                let fl = 
                                    match (
                                        [(i-1) .. -1 .. 0] 
                                        |> List.tryFind (fun e -> 
                                            match arr[e] with | Regular _ -> true | _ -> false
                                        ) 
                                    ) with
                                    | Some i -> i
                                    | None -> -1
                                let fr = 
                                    match (
                                        [(i+3) .. 1 .. (arr |> Array.length) - 1] 
                                        |> List.tryFind (fun e -> 
                                            match arr[e] with | Regular _ -> true | _ -> false
                                        ) 
                                    ) with
                                    | Some i -> i
                                    | None -> -1
                                if fl > -1 then
                                    let tl = match arr[fl] with | Regular y -> y | _ -> -1
                                    arr[fl] <- Regular (tl + l)
                                if fr > -1 then
                                    let tr = match arr[fr] with | Regular y -> y | _ -> -1
                                    arr[fr] <- Regular (tr + r)
                                arr[i-1] <- PIgnore
                                arr[i] <- Regular 0
                                arr[i+1] <- PIgnore
                                arr[i+2] <- PIgnore
                                arr[i+3] <- PIgnore
                                //arr[i+4] <- PIgnore
                    | _ -> 0 |> ignore

        let l = arr |> Array.where (fun e -> match e with | PIgnore -> false | _ -> true) |> Array.toList

        (l, hit)

    let rec reduceLoop (l : thing list) =

        let (x, h) = reduceOrSplit l
        if h then 
            reduceLoop x
        else 
            x

            
    let add (a : thing list) (b : thing list) =
        let added =
            List.append (
                List.append (
                    List.append (
                        List.append [PIn] a
                    ) [PDelim]
                ) b
            ) [POut]
                
        let reduced = reduceLoop added

        reduced

    let rec addAll (s : thing list) (p : int) =
        let next = lines[p] |> strToList

        let added = add s next
        if p = (lines.Length - 1) then 
            added
        else
            addAll added (p+1)

    let rec magnitude (l : thing list) =
        if l |> List.length = 1 then
            match l[0] with | Regular x -> x | _ -> 0
        else
            let hits =
                l
                |> List.windowed 3
                |> List.mapi (fun i e ->
                    let f = match e[0] with | Regular x -> x | _ -> -1
                    let s = match e[2] with | Regular x -> x | _ -> -1
                    let d = match e[1] with | PDelim -> true | _ -> false
                    (i, if d && f > -1 && s > -1 then 3*f + 2*s else -1)
                )
                |> List.where (fun (_,e) -> e > -1)
            let arr = l |> List.toArray
            for (i,s) in hits do
                arr[i-1] <- PIgnore
                arr[i] <- Regular s
                arr[i+1] <- PIgnore
                arr[i+2] <- PIgnore
                arr[i+3] <- PIgnore
            let nl = arr |> Array.where (fun e -> match e with | PIgnore -> false | _ -> true) |> Array.toList
            magnitude nl

    let added = addAll (lines[0] |> strToList) 1

    added |> magnitude
            

let Problem2 (input : string) = 

    let lines = input |> Common.Lines
    
    let strToList (a : string) =
        a.ToCharArray() |> Array.map (fun e ->
            match e with
            | '[' -> PIn
            | ']' -> POut
            | ',' -> PDelim
            | x -> Regular (x |> string |> int)
        )
        |> Array.toList

    let listToStr (a : thing list) =
        a
        |> List.map (fun e ->
            match e with
            | PIn -> "["
            | POut -> "]"
            | PDelim -> ","
            | PIgnore -> ""
            | Regular x -> x |> string
        )
        |> List.reduce (+)

    let reduceOrSplit (a : thing list) =

        //System.Diagnostics.Debug.WriteLine (a |> listToStr)
        
        let mutable Depth = 0
        let mutable arr = a |> List.toArray
        let mutable hit = false

        for lp in [0..1] do
            for i in [0 .. (arr |> Array.length) - 1] do
                if not(hit) then
                    let v = arr[i]
                    match v with
                    | PIn -> Depth <- Depth + 1
                    | POut -> Depth <- Depth - 1
                    | Regular x ->
                        if x > 9 && lp = 1 then
                            hit <- true
                            let l = (x / 2)
                            let r = (x / 2) + (x % 2)
                            arr[i] <- PIn                
                            arr <- Array.insertAt (i + 1) (Regular l) arr
                            arr <- Array.insertAt (i + 2) (PDelim) arr
                            arr <- Array.insertAt (i + 3) (Regular r) arr
                            arr <- Array.insertAt (i + 4) (POut) arr
                        elif Depth = 5 && lp = 0 then
                            if (match arr[i-1] with | PIn -> true | _ -> false) then
                                hit <- true
                                let l = x
                                let r = match arr[i + 2] with | Regular y -> y | _ -> 0
                                let fl = 
                                    match (
                                        [(i-1) .. -1 .. 0] 
                                        |> List.tryFind (fun e -> 
                                            match arr[e] with | Regular _ -> true | _ -> false
                                        ) 
                                    ) with
                                    | Some i -> i
                                    | None -> -1
                                let fr = 
                                    match (
                                        [(i+3) .. 1 .. (arr |> Array.length) - 1] 
                                        |> List.tryFind (fun e -> 
                                            match arr[e] with | Regular _ -> true | _ -> false
                                        ) 
                                    ) with
                                    | Some i -> i
                                    | None -> -1
                                if fl > -1 then
                                    let tl = match arr[fl] with | Regular y -> y | _ -> -1
                                    arr[fl] <- Regular (tl + l)
                                if fr > -1 then
                                    let tr = match arr[fr] with | Regular y -> y | _ -> -1
                                    arr[fr] <- Regular (tr + r)
                                arr[i-1] <- PIgnore
                                arr[i] <- Regular 0
                                arr[i+1] <- PIgnore
                                arr[i+2] <- PIgnore
                                arr[i+3] <- PIgnore
                                //arr[i+4] <- PIgnore
                    | _ -> 0 |> ignore

        let l = arr |> Array.where (fun e -> match e with | PIgnore -> false | _ -> true) |> Array.toList

        (l, hit)

    let rec reduceLoop (l : thing list) =

        let (x, h) = reduceOrSplit l
        if h then 
            reduceLoop x
        else 
            x

            
    let add (a : thing list) (b : thing list) =
        let added =
            List.append (
                List.append (
                    List.append (
                        List.append [PIn] a
                    ) [PDelim]
                ) b
            ) [POut]
                
        let reduced = reduceLoop added

        reduced

    let rec addAll (s : thing list) (p : int) =
        let next = lines[p] |> strToList

        let added = add s next
        if p = (lines.Length - 1) then 
            added
        else
            addAll added (p+1)

    let rec magnitude (l : thing list) =
        if l |> List.length = 1 then
            match l[0] with | Regular x -> x | _ -> 0
        else
            let hits =
                l
                |> List.windowed 3
                |> List.mapi (fun i e ->
                    let f = match e[0] with | Regular x -> x | _ -> -1
                    let s = match e[2] with | Regular x -> x | _ -> -1
                    let d = match e[1] with | PDelim -> true | _ -> false
                    (i, if d && f > -1 && s > -1 then 3*f + 2*s else -1)
                )
                |> List.where (fun (_,e) -> e > -1)
            let arr = l |> List.toArray
            for (i,s) in hits do
                arr[i-1] <- PIgnore
                arr[i] <- Regular s
                arr[i+1] <- PIgnore
                arr[i+2] <- PIgnore
                arr[i+3] <- PIgnore
            let nl = arr |> Array.where (fun e -> match e with | PIgnore -> false | _ -> true) |> Array.toList
            magnitude nl

    let nums = lines |> Array.map strToList

    let tests =
        [|
            for a in [0..(nums |> Array.length) - 1] do
                for b in [0..(nums |> Array.length) - 1] do
                    if a <> b then
                        add nums[a] nums[b] |> magnitude
        |]

    tests |> Array.max
            
        