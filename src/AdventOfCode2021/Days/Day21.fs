module Day21


    let Problem1 (input : string) =

        let starts = input |> Common.Lines |> Array.map (fun e -> e.Split(": ")[1] |> int)

        let dice = 
            let rec s n = seq {
                let nn = (n % 100) + 1
                yield nn
                yield! s nn
            }
            s 0

        let rolls = 
            dice |> Seq.chunkBySize 3 |> Seq.map Array.sum

        let points =
            rolls 
            |> Seq.chunkBySize 2
            |> Seq.scan (fun a b -> 
                Array.map2 (fun e f -> ((e+f-1) % 10) + 1) a b
            ) starts
            |> Seq.skip 1
            |> Seq.scan (fun a b -> [|a[0]+b[0];a[1]+b[1]|]) [|0;0|]
            |> Seq.skip 1
            |> Seq.mapi (fun i a -> (i, a))

        let hit =
            points
            |> Seq.find (fun (i, a) -> a |> Array.max >= 1000)

        let rolls = 
            fst hit * 6 + if (snd hit)[0] >= 1000 then 3 else 6

        let lowest =
            if (snd hit)[0] >= 1000 then 
                (points |> Seq.skip (fst hit - 1) |> Seq.head |> snd)[1]
            else
                (snd hit)[0]

        rolls * lowest


    let Problem2 (input : string) =
            
        let starts = input |> Common.Lines |> Array.map (fun e -> e.Split(": ")[1] |> int)

        let winat = 21

        let rolls = [|(3,1L);(4,3L);(5,6L);(6,7L);(7,6L);(8,3L);(9,1L)|]

        let rec split tp ts op os cnt =
            let scores =
               rolls 
               |> Array.map (fun (r,c) -> 
                    let np = ((tp+r-1) % 10) + 1
                    (np,ts+np,c*cnt)
                )
            let wins =
                scores
                |> Array.where (fun (_,s,_) -> s >= winat)
                |> Array.sumBy (fun (_,_,c) -> c)
            let collect =
                scores 
                |> Array.where (fun (_,s,_) -> s < winat)
                |> Array.groupBy (fun (a,b,_) -> (a,b)) 
                |> Array.map (fun ((a,b),c) -> 
                    (a,b,c |> Array.sumBy (fun (_,_,c) -> c))
                )
            let recurse =
                let inn =
                    collect
                    |> Array.map (fun (a,b,c) -> 
                        split op os a b c
                    )
                (inn |> Array.sumBy fst, inn |> Array.sumBy snd)
            (snd recurse + wins, fst recurse)

        let r = split starts[0] 0 starts[1] 0 1
        
        max (fst r) (snd r)
                


            
            