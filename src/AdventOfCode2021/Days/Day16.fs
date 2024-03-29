﻿module Day16


type SubLengt = | Count of int | Length of int

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
        |> Array.map (string >> int)

    let bits = 
        input.ToCharArray()
        |> Array.collect hexToBits

    let queue = new System.Collections.Generic.Queue<int>(bits)

    let mutable total = 0

    let bitsToNum (bits : int[]) =
        bits |> Array.rev |> Array.mapi (fun i e -> e * int (2.0 ** float i)) |> Array.reduce (+)

    let getFromQueue num =
        [| for i in [1..num] do queue.Dequeue() |]

    let rec collect (sl : SubLengt) = 

        let ct = match sl with | Count x -> x | _ -> 0
        let qt = match sl with | Length x -> queue.Count - x | _ -> 0

        let mutable tc = 0

        while queue.Count > qt && (tc < ct || ct = 0) do

            tc <- tc + 1

            let version = getFromQueue 3 |> bitsToNum

            let tpe = getFromQueue 3 |> bitsToNum

            total <- total + version

            if tpe = 4 then

                let mutable read = true

                while read do
                    let test = getFromQueue 1 |> Array.head
                    getFromQueue 4 |> ignore
                    if test = 0 then read <- false

            else

                let lt = getFromQueue 1 |> Array.head
                
                let length = 
                    match lt with
                    | 0 -> (Length (getFromQueue 15 |> bitsToNum))
                    | 1 -> (Count (getFromQueue 11 |> bitsToNum))

                collect length

    collect (Count 1) |> ignore

    total

let Problem2 (input : string) = 

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
        |> Array.map (string >> int)

    let bits = 
        input.ToCharArray()
        |> Array.collect hexToBits

    let queue = new System.Collections.Generic.Queue<int>(bits)

    let bitsToNum (bits : int[]) =
        bits |> Array.rev |> Array.mapi (fun i e -> int64 e * int64 (2.0 ** float i)) |> Array.reduce (+)

    let getFromQueue num =
        [| for i in [1..num] do queue.Dequeue() |]

    let rec collect (sl : SubLengt) = 

        let ct = match sl with | Count x -> x | _ -> 0
        let qt = match sl with | Length x -> queue.Count - x | _ -> 0

        let mutable tc = 0

        [|

            while queue.Count > qt && (tc < ct || ct = 0) do

                tc <- tc + 1

                let version = getFromQueue 3 |> bitsToNum

                let tpe = getFromQueue 3 |> bitsToNum

                if tpe = 4L then

                    let mutable read = true

                    let bits = 
                        [|
                            while read do
                                let test = getFromQueue 1 |> Array.head
                                for b in getFromQueue 4 do
                                    b
                                if test = 0 then read <- false
                        |]

                    bits |> bitsToNum

                else

                    let lt = getFromQueue 1 |> Array.head
            
                    let length = 
                        match lt with
                        | 0 -> (Length (getFromQueue 15 |> bitsToNum |> int))
                        | 1 -> (Count (getFromQueue 11 |> bitsToNum |> int))

                    let nums = collect length

                    match tpe with
                    | 0L -> nums |> Array.sum
                    | 1L -> nums |> Array.reduce (*)
                    | 2L -> nums |> Array.min
                    | 3L -> nums |> Array.max
                    | 5L -> if nums[0] > nums[1] then 1L else 0L
                    | 6L -> if nums[0] < nums[1] then 1L else 0L
                    | 7L -> if nums[0] = nums[1] then 1L else 0L

        |]

    collect (Count 1) |> Array.head



