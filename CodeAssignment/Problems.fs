namespace Caddi
module Problem01 = 
    type Command = 
        | Open of string 
        | Close of int // close the most recently opened ones 
        | Clear


    let inputToCommand (input: string) = 
        let inputSegements = 
            input.Split(" ")
            |> List.ofArray

        match inputSegements with 
        | x::y::_ when x.ToLower().Contains("close") ->
            Close (int y)
        | x::y::_ when x.ToLower().Contains("open") ->
            Open y 
        | x::_ when x.ToLower().Contains("clear") -> 
            Clear 
        | _ -> 
            failwith "invalid command"
            

    let getOpenApplications inputs = 
        let rec helper acc commands =
            match commands with 
            | x::rest -> 
                match x with 
                | Open s -> 
                    helper (s::acc) rest 
                | Clear -> 
                    helper [] rest 
                | Close k -> 
                    helper acc[k..] rest 
            | _ -> 
                acc 
                |> List.rev 

        helper [] (inputs |> List.map inputToCommand)



module Problem02 = 
    let isAnagram (word01:string) (word02: string) = 
        let w1 = 
            word01
            |> List.ofSeq
            |> List.sort
            |> System.String.Concat

        let w2 = 
            word02
            |> List.ofSeq
            |> List.sort
            |> System.String.Concat

        w1 = w2
        
    let getSearchResults (words: string list) (queries: string list) = 
        let mutable result: string list list = []
        queries
        |> List.iter (fun q -> 
            let matched = 
                words
                |> List.filter (fun x -> 
                    isAnagram x q 
                )
            if matched.Length <> 0 then 
                result <- matched :: result
        )
        result






