module Day4


let Problem1 input =

    let lines = input |> Common.Lines

    let numbers = (lines |> Array.head).Split([|','|]) |> Array.map int

    let boardData = lines |> Array.tail |> Array.chunkBySize(5)

    let buildBoard (l : string[]) =
        l |> Array.collect Common.Words |> Array.map int |> Array.chunkBySize(5)

    let boards = 
        boardData |> Array.map buildBoard

    let markBoard board (numbers : int[]) =
        let checkNumber n =
            if numbers |> Array.contains n then
                -1
            else
                n
        board |> Array.map (Array.map checkNumber)

    let checkMarkedBoard (b : int[][]) =
        let checkRow r = 
            if r |> Array.sum = -5 then 1 else 0
        let ccmpletedRow = 
            if b |> Array.where (fun e -> (checkRow e) > 0) |> Array.length > 0 then 1 else 0
        let checkColumn c =
            if b |> Array.where (fun e -> e[c] = -1) |> Array.length = 5 then 1 else 0
        let completedColumn =
            if [|0..4|] |> Array.map checkColumn |> Array.sum > 0 then 1 else 0
        completedColumn + ccmpletedRow > 0
        
    let checkBoard b n =
        let mb = markBoard b n
        if checkMarkedBoard mb then
            let sum = mb |> Array.map (fun e -> e |> Array.map (fun e -> if e = -1 then 0 else e) |> Array.sum) |> Array.sum
            sum * (n |> Array.last)
        else
            0

    let checkBoards n =
        boards |> Array.map (fun e -> checkBoard e n) |> Array.sum

    let r =
        let s = {5..numbers.Length} |> Seq.map (fun e -> checkBoards (numbers |> Array.take(e)))
        s |> Seq.find (fun e -> e > 0)

    r


let Problem2 input =

    let lines = input |> Common.Lines
    
    let numbers = (lines |> Array.head).Split([|','|]) |> Array.map int
    
    let boardData = lines |> Array.tail |> Array.chunkBySize(5)
    
    let buildBoard (l : string[]) =
        l |> Array.collect Common.Words |> Array.map int |> Array.chunkBySize(5)
    
    let boards = 
        boardData |> Array.map buildBoard
    
    let markBoard board (numbers : int[]) =
        let checkNumber n =
            if numbers |> Array.contains n then
                -1
            else
                n
        board |> Array.map (Array.map checkNumber)
    
    let checkMarkedBoard (b : int[][]) =
        let checkRow r = 
            if r |> Array.sum = -5 then 1 else 0
        let ccmpletedRow = 
            if b |> Array.where (fun e -> (checkRow e) > 0) |> Array.length > 0 then 1 else 0
        let checkColumn c =
            if b |> Array.where (fun e -> e[c] = -1) |> Array.length = 5 then 1 else 0
        let completedColumn =
            if [|0..4|] |> Array.map checkColumn |> Array.sum > 0 then 1 else 0
        completedColumn + ccmpletedRow > 0
            
    let checkBoard b n =
        let mb = markBoard b n
        if checkMarkedBoard mb then
            let sum = mb |> Array.map (fun e -> e |> Array.map (fun e -> if e = -1 then 0 else e) |> Array.sum) |> Array.sum
            sum * (n |> Array.last)
        else
            0
    
    let checkBoards n =
        let w = boards |> Array.map (fun e -> checkBoard e n) |> Array.where (fun e -> e > 0)
        (w |> Array.length, w |> Array.sum)
    
    let r =
        let bc =
            boards
            |> Array.map (fun e -> 
                {5..numbers.Length} |> Seq.map (fun f -> (f, checkBoard e (numbers |> Array.take(f)))) |> Seq.tryFind (fun f -> snd f > 0)
            )
            |> Array.where (fun e -> match e with | None -> false | _ -> true)
            |> Array.map (fun e -> e.Value)
        let max = bc |> Array.map (fst) |> Array.max
        let lb = bc |> Array.where (fun e -> fst e = max) |> Array.head
        snd lb
        
        
    
    r