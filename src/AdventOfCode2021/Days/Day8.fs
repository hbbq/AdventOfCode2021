﻿module Day8


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let outputs = lines |> Array.map (fun e -> e.Split('|')[1])

    let words = outputs |> Array.collect (fun e -> e |> Common.Words)

    words |> Array.filter (fun e ->
        match e.Length with
        | 2 -> true
        | 4 -> true
        | 3 -> true
        | 7 -> true
        | _ -> false
    ) |> Array.length

let Problem2 (input : string) =

    let lines = input |> Common.Lines

    let chars = [|'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'|]

    let calculateLine (line : string) = 
        let allDigits = line.Replace("|", "") |> Common.Words
        let digitToGroups (digit : string) =
            digit.ToCharArray() |> Array.map (fun e -> (digit.Length, e))
        let collect = allDigits |> Array.collect digitToGroups

        let InValids = [|
            collect |> Array.where (fun (c, d) -> match c with | 2 -> true | 4 -> true | _ -> false) |> Array.map snd |> Array.distinct ;
            collect |> Array.where (fun (c, d) -> match c with | 2 -> true | 3 -> true | _ -> false) |> Array.map snd |> Array.distinct ;
            [||] ;
            collect |> Array.where (fun (c, d) -> match c with | 2 -> true | 3 -> true | _ -> false) |> Array.map snd |> Array.distinct ;
            collect |> Array.where (fun (c, d) -> match c with | 2 -> true | 3 -> true | 4 -> true | _ -> false) |> Array.map snd |> Array.distinct ;
            [||] ;
            collect |> Array.where (fun (c, d) -> match c with | 2 -> true | 3 -> true | 4 -> true | _ -> false) |> Array.map snd |> Array.distinct ;
        |]

        let valids =
            InValids |> Array.map (fun i ->
                (chars |> Array.where (fun c -> not (i |> Array.contains(c))))
            )
       
        let combinations = 
            valids[0] |> Array.collect (fun a ->
                valids[1] |> Array.collect ( fun b ->
                    valids[2] |> Array.collect ( fun c ->
                        valids[3] |> Array.collect ( fun d ->
                            valids[4] |> Array.collect ( fun e ->
                                valids[5] |> Array.collect ( fun f ->
                                    valids[6] |> Array.map ( fun g ->
                                        [|a;b;c;d;e;f;g|]
                                    )
                                )
                            )
                        )
                    )
                )
            )

        let validCombinations = 
            combinations |> Array.where (fun e -> e |> Array.distinct |> Array.length = 7)

        let digits = 
            seq {              
                for comb in validCombinations do
                    let a = comb[0]
                    let b = comb[1]
                    let c = comb[2]
                    let d = comb[3]
                    let e = comb[4]
                    let f = comb[5]
                    let g = comb[6]
                    if ([|a; b; c; d; e; f; g|] |> Array.distinct |> Array.length = 7) then
                        let output = line.Split('|')[1] |> Common.Words
                        let fix (digit : string) =
                            let translated = 
                                digit.ToCharArray() |> Array.map (fun x ->
                                    if   x = a then 'a'
                                    elif x = b then 'b'
                                    elif x = c then 'c'
                                    elif x = d then 'd'
                                    elif x = e then 'e'
                                    elif x = f then 'f'
                                    elif x = g then 'g'
                                    else ' '
                                )
                            let sorted =
                                translated |> Array.sort |> (fun e -> System.String.Join(' ', e).Replace(" ", ""))
                            let toNum =
                                match sorted with
                                | "abcefg" -> 0
                                | "cf" -> 1
                                | "acdeg" -> 2
                                | "acdfg" -> 3
                                | "bcdf" -> 4
                                | "abdfg" -> 5
                                | "abdefg" -> 6
                                | "acf" -> 7
                                | "abcdefg" -> 8
                                | "abcdfg" -> 9
                                | _ -> -1
                            toNum
                        output |> Array.map fix
            }
            

        let s = digits |> Seq.toArray
        
        s |> Array.where (fun e -> not(e |> Array.contains -1)) |> Array.head |> Array.map string |> (fun e -> System.String.Join(' ', e).Replace(" ", "") |> int)

    lines |> Array.map calculateLine |> Array.sum
                                                
                   