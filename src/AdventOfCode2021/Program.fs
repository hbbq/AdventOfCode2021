open System


[<EntryPoint>]
let main argv = 


    let types = System.Reflection.Assembly.GetExecutingAssembly().GetTypes()

    let maxDay = 
        types 
        |> Array.filter (fun e -> e.Name.StartsWith("Day")) 
        |> Array.map (fun e -> int (e.Name.Replace("Day", ""))) 
        |> Array.max

    Console.Write $"Day ({maxDay}): "

    let inpDay = Console.ReadLine()

    let day = match inpDay with | "" -> maxDay |> string | _ -> inpDay

    let dayType = types |> Array.filter (fun e -> e.Name = "Day" + day) |> Array.exactlyOne


    let dayMethods = dayType.GetMethods()

    let maxTask =
        dayMethods
        |> Array.filter (fun e -> e.Name.StartsWith("Problem"))
        |> Array.map (fun e -> int (e.Name.Replace("Problem", "")))
        |> Array.max

    let task =

        if maxTask = 1 then
            Console.WriteLine $"Only task 1 present, choosing it..."
            "1"
        else
            Console.Write $"Task ({maxTask}): " 
            let inpTask = Console.ReadLine()
            match inpTask with | "" -> maxTask |> string | _ -> inpTask


    let taskMethod = dayMethods |> Array.filter (fun e -> e.Name = "Problem" + task) |> Array.exactlyOne


    let sampleInputType = System.Reflection.Assembly.GetExecutingAssembly().GetType("SampleInputs").GetMethods() |> Array.tryFind (fun e -> e.Name = "get_Day" + day)

    let puzzleInputType = System.Reflection.Assembly.GetExecutingAssembly().GetType("PuzzleInputs").GetMethods() |> Array.tryFind (fun e -> e.Name = "get_Day" + day)

    let data =
        match puzzleInputType with
        | None ->             
            Console.WriteLine $"Only sample data present, choosing it..."
            "0"
        | _ ->
            Console.Write "Data (0=sample, 1=puzzle): "
            Console.ReadLine()

    let dataMethod = if data = "1" then puzzleInputType else sampleInputType

    match dataMethod with
    | None -> 
        Console.WriteLine()
        Console.WriteLine "No data found"
    | Some x ->
        let watch = System.Diagnostics.Stopwatch.StartNew()
        let Input = x.Invoke(null, null)
        let Result = taskMethod.Invoke(null, [|Input|]) 
        let Elapsed = watch.Elapsed
        Console.WriteLine()
        printfn "Result: %A" Result
        printfn "Time:  %A" Elapsed

    Console.ReadKey() |> ignore

    0