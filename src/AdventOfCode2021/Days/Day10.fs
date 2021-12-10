module Day10


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let pairs = 
        [|
            ('(', ')')
            ('[', ']')
            ('{', '}')
            ('<', '>')
        |]

    let checkIllegal (line : string) = 
        let stack = new System.Collections.Generic.Stack<char>()
        let check =
            [|
                for c in line.ToCharArray() do
                    let pair = pairs |> Array.tryFind (fun e -> fst e = c)
                    match pair with
                    | Some p -> stack.Push(snd p)
                    | None -> 
                        let t = if stack.Count > 0 then stack.Pop() else ' '
                        if c <> t then c
            |]
        if check.Length = 0 then
            0
        else
            match check[0] with
            | ')' -> 3
            | ']' -> 57
            | '}' -> 1197
            | '>' -> 25137
            | _ -> 0

    lines |> Array.map checkIllegal |> Array.reduce (+)
                

let Problem2 (input : string) =

    let lines = input |> Common.Lines

    let pairs = 
        [|
            ('(', ')')
            ('[', ']')
            ('{', '}')
            ('<', '>')
        |]

    let remaining (line : string) = 
        let stack = new System.Collections.Generic.Stack<char>()
        let check =
            [|
                for c in line.ToCharArray() do
                    let pair = pairs |> Array.tryFind (fun e -> fst e = c)
                    match pair with
                    | Some p -> stack.Push(snd p)
                    | None -> 
                        let t = if stack.Count > 0 then stack.Pop() else ' '
                        if c <> t then c
            |]
        if check.Length > 0 then
            [||]
        else
            stack.ToArray()

    let lineScore (r : char[]) =
        r 
        |> Array.map (fun e -> 
            match e with 
            | ')' -> 1L
            | ']' -> 2L
            | '}' -> 3L
            | '>' -> 4L
            | _ -> 0L
        )
        |> Array.reduce (fun a b -> a * 5L + b)


    let scores = lines |> Array.map remaining |> Array.where (fun e -> e.Length > 0) |> Array.map lineScore |> Array.sort

    scores[(scores.Length-1)/2]
                

