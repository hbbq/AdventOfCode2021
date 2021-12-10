module Day10


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let pairs = 
        [|
            ('(', ')', 3)
            ('[', ']', 57)
            ('{', '}', 1197)
            ('<', '>', 25137)
        |]

    let checkIllegal (line : string) = 
        let stack = new System.Collections.Generic.Stack<char>()
        let check =
            [|
                for c in line do
                    let pair = pairs |> Array.tryFind (fun (a, _, _) -> a = c)
                    match pair with
                    | Some (_, a, _) -> stack.Push(a)
                    | None -> 
                        if stack.Count > 0 then
                            let t = stack.Pop()
                            if c <> t then c
            |]
        if check.Length = 0 then
            0
        else
           pairs |> Array.find (fun (_, a, _) -> a = check[0]) |> (fun (_, _, a) -> a)

    lines |> Array.map checkIllegal |> Array.reduce (+)
                

let Problem2 (input : string) =

    let lines = input |> Common.Lines

    let pairs = 
        [|
            ('(', ')', 1L)
            ('[', ']', 2L)
            ('{', '}', 3L)
            ('<', '>', 4L)
        |]

    let remaining (line : string) = 
        let stack = new System.Collections.Generic.Stack<char>()
        let check =
            [|
                for c in line do
                    let pair = pairs |> Array.tryFind (fun (a, _, _) -> a = c)
                    match pair with
                    | Some (_, a, _) -> stack.Push(a)
                    | None -> 
                        if stack.Count > 0 then
                            let t = stack.Pop()
                            if c <> t then c
            |]
        if check.Length > 0 then
            [||]
        else
            stack.ToArray()

    let lineScore (r : char[]) =
        r 
        |> Array.map (fun e -> pairs |> Array.find (fun (_, a, _) -> a = e) |> (fun (_, _, a) -> a))
        |> Array.reduce (fun a b -> a * 5L + b)


    let scores = lines |> Array.map remaining |> Array.where (fun e -> e.Length > 0) |> Array.map lineScore |> Array.sort

    scores[(scores.Length-1)/2]
                

