module Day10


let Problem1 (input : string) =

    let lines = input |> Common.Lines
        
    let checkIllegal (line : string) = 
        let rec remove (l : string) =
            let nl = l.Replace("()", "").Replace("<>", "").Replace("{}", "").Replace("[]", "")
            if nl.Length = l.Length then 
                nl.Replace("(","").Replace("[","").Replace("{","").Replace("<","") 
            else 
                remove nl
        let ckd = remove line
        if ckd.Length = 0 then 
            0
        else
            match ckd[0] with
            | ')' -> 3
            | ']' -> 57
            | '}' -> 1197
            | '>' -> 25137
            | _ -> 0

    lines |> Array.map checkIllegal |> Array.reduce (+)
                

let Problem2 (input : string) =

    let lines = input |> Common.Lines

    let remaining (line : string) = 
        let rec remove (l : string) =
            let nl = l.Replace("()", "").Replace("<>", "").Replace("{}", "").Replace("[]", "")
            if nl.Length = l.Length then 
                nl
            else 
                remove nl
        let ckd = remove line
        if ckd.Contains("]") || ckd.Contains("}") || ckd.Contains(")") || ckd.Contains(">") then
            [||]
        else
            ckd.ToCharArray()

    let lineScore (r : char[]) =
        r 
        |> Array.rev
        |> Array.map (fun e -> 
            match e with
            | '(' -> 1L
            | '[' -> 2L
            | '{' -> 3L
            | '<' -> 4L
            | _ -> 0l
        )
        |> Array.reduce (fun a b -> a * 5L + b)

    let scores = lines |> Array.map remaining |> Array.where (fun e -> e.Length > 0) |> Array.map lineScore |> Array.sort

    scores[(scores.Length-1)/2]
               