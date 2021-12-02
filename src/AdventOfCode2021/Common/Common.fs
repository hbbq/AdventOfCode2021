module Common

let Words (s : string) =
    s.Replace("\t"," ").Split(' ')
    |> Array.filter (fun s -> s <> "")

let Lines (s : string) =
    s.Replace("\n", "\r").Split('\r')
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")

let Chars =
    string >> (fun e -> seq [for c in e -> c])

let IsNumeric v = 
    fst (System.Int32.TryParse(string(v)))

let Digits = 
    string >> Chars >> Seq.where IsNumeric >> Seq.map (string >> int)

let DigitSum<'a> =
    string >> Digits >> Seq.sum

let rec InfSeq start inc = seq {
    yield start
    yield! InfSeq (start + inc) inc
}

let IsPrime n =
    let f = float n
    f = 2.0 
    || (
        f % 2.0 > 0.0
        &&
        seq {3.0..2.0..sqrt f} |> Seq.tryFind (fun e -> f % e = 0.0) |> (=) None
    )

let Primes =
    seq {
        yield! [2; 3; 5] |> List.toSeq
        yield! InfSeq 7 6 
            |> Seq.collect (fun e -> seq {yield e; yield e + 4})
            |> Seq.filter (fun e -> e < 6 || IsPrime e)
    } 

let rec Factorize (n:int) = seq {
    if n > 1 then
        let f = 
            Primes 
            |> Seq.find (fun e -> n % e = 0) 
            |> int
        yield f
        yield! Factorize (n / f)
}