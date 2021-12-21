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

        let t1 (a,_,_) = a
        let t2 (_,a,_) = a
        let t3 (_,_,a) = a

        let rec split (p : (int*int)[]) player cnt =
            let (pos,score) = p[player]
            let rolls = [|(3,1L);(4,3L);(5,6L);(6,7L);(7,6L);(8,3L);(9,1L)|]
            let scores =
               rolls 
               |> Array.map (fun (r,c) -> 
                    let np = ((pos+r-1) % 10) + 1
                    let ns = score + np
                    (np,ns,c*cnt)
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
                        let pairs =
                            if player = 0 then
                                [|(a,b);p[1]|]
                            else
                                [|p[0];(a,b)|]
                        split pairs (1-player) c
                    )
                (inn |> Array.sumBy fst, inn |> Array.sumBy snd)
            (((fst recurse) + if player = 0 then wins else 0),((snd recurse) + if player = 1 then wins else 0))

        let r = split [|(starts[0],0);(starts[1],0)|] 0 1
        
        max (fst r) (snd r)
                


            
            