open System


[<EntryPoint>]
let main argv = 

    Console.Write "Day: "

    let Day = Console.ReadLine()
        
    let Type = System.Reflection.Assembly.GetExecutingAssembly().GetType("Day" + Day)

    Console.Write "Task: "

    let Task = Console.ReadLine()

    let Methods = Type.GetMethods()

    let Method = Type.GetMethod("Problem" + Task)

    Console.Write "Data (0=sample, 1=puzzle): "

    let Data = Console.ReadLine()

    let TypeName = 
        match Data with
        | "1" -> "PuzzleInputs"
        | _ -> "SampleInputs"

    let DataType = System.Reflection.Assembly.GetExecutingAssembly().GetType(TypeName)

    let DataMethod = DataType.GetMethod("get_Day" + Day)

    let Input = DataMethod.Invoke(null, null)

    let Result = Method.Invoke(null, [|Input|])

    Console.WriteLine()

    printfn "Result: %A" Result

    Console.ReadKey() |> ignore

    0