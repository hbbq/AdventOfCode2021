module Day16


let Problem1 (input : string) =

    let hexToBits (hex : char) =
        match hex with
        | '0' -> "0000"
        | '1' -> "0001"
        | '2' -> "0010"
        | '3' -> "0011"
        | '4' -> "0100"
        | '5' -> "0101"
        | '6' -> "0110"
        | '7' -> "0111"
        | '8' -> "1000"
        | '9' -> "1001"
        | 'A' -> "1010"
        | 'B' -> "1011"
        | 'C' -> "1100"
        | 'D' -> "1101"
        | 'E' -> "1110"
        | 'F' -> "1111"
        |> (fun f -> f.ToCharArray())
        |> Array.map int

    let bits = 
        input.ToCharArray()
        |> Array.collect hexToBits


    let bitsToNum (bits : int[]) =
        bits |> Array.rev |> Array.mapi (fun i e -> e * int (2.0 ** float i)) |> Array.reduce (+)

    let rec collect (b : int[]) = 

        let version = b |> Array.take 3 |> bitsToNum

        let tpe = b |> Array.skip 3 |> Array.take 3 |> bitsToNum

        if tpe = 4 then

            version

        else

            version //!!

        // !!!!!

    0 //!


