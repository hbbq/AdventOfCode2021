[<EntryPoint>]
let main argv = 
    
    let p1 = Day2.Problem1
    let p2 = Day2.Problem2
        
    printfn "1: %A" p1
    printfn "2: %A" p2

    System.Diagnostics.Trace.WriteLine(p1.ToString())
    System.Diagnostics.Trace.WriteLine(p2.ToString())

    System.Console.ReadKey() |> ignore

    0