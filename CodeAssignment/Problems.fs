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






