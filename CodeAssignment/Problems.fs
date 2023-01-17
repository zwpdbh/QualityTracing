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



module Problem03 = 
    let sampleInput01 = 
        """
        xxx.com - - [01/Jul/1995:00:12 -0400] "GET /path01/path02/some.GIF HTTP/1.0" 200 0
        """
    let sampleInput02 = 
        """
        xxx.com - - [01/Jul/1995:00:12 -0400] "GET /path01/path02/some.GIF HTTP/1.0" 304 0
        """


    let (|GetValidForCode200|_|) (input: string) = 
        let inputSegements = 
            input.Trim().Split(" ") 
            |> List.ofArray
            |> List.rev 

        match inputSegements with 
        | _::code::content as line ->
            if (int code) = 200 then 
                Some content 
            else 
                None 
        | _ -> 
            None

    let (|GetValidForMethod|_|) (input: string list) = 
        match input with 
        | _::filePath::method::_ -> 
            if method.ToLower().Contains("get") then 
                Some filePath
            else 
                None 
        | _ -> 
            None 

    let (|GetGif|_|) (filePath: string) = 
        let file = 
            filePath.Trim().Split("/")
            |> List.ofArray
            |> List.rev
            |> List.tryHead
        
        match file with 
        | Some fileName when fileName.ToLower().Contains("gif") -> 
            Some fileName
        | _ -> None 


    let getGifFileName (input: string) = 
        match input with 
        | GetValidForCode200 txt -> 
            match txt with 
            | GetValidForMethod filePath ->
                match filePath with 
                | GetGif file -> 
                    Some file 
                | _ -> 
                    None
            | _ ->
                None
        | _ -> 
            None


    let getGifFiles (inputs: string list) = 
        inputs
        |> List.choose (fun x -> 
            getGifFileName x
        )
        |> List.distinct
        |> List.rev

    let workingDirectory = System.Environment.CurrentDirectory

    let saveResultToFile (fileName: string) (results: string list) = 
        let resultFilePath = System.IO.Path.Combine(workingDirectory, fileName)

        System.IO.File.WriteAllLines(resultFilePath, results)
        results
        
    let getInputsFromFile filePath = 
        let currFilePath = System.IO.Path.Combine(workingDirectory, filePath)
        
        System.IO.File.ReadAllLines currFilePath 
        |> List.ofArray
        
    let demo () = 
        // read the string filename
        let filename = System.Console.ReadLine()
       
        getInputsFromFile filename
        |> getGifFiles
        |> saveResultToFile filename
        |> List.iter (fun x -> 
            printfn "%A" x
        )

    let test () = 
        match sampleInput01 with 
        | GetValidForCode200 txt -> 
            printfn "%A" txt
            match txt with 
            | GetValidForMethod filePath ->
                printfn "%A" filePath
                match filePath with 
                | GetGif file -> 
                    printfn "%A" file 
                | _ -> 
                    printfn "No gif file"
            | _ ->
                printfn "No filePath"
        | _ -> 
            printfn "No 200 response"