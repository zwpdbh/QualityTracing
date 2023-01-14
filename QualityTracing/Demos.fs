namespace QualityTracing
open System

module Demos = 
    open PhotoAgent
    open Person
    open Photo
    open PhotoManagement

    let demoConcurrentlyUpdatePhoto () = 

        let photo = DB.pickRandomOne DB.photos
        let photoAgent = new PhotoAgent(photo)

        let printMessages messages = 
            messages 
            |> List.iter (fun x -> 
                printfn $"{x}"
            )

        printfn "Show A photo agent can process many messages:\n"

        DB.messages
        |> Seq.map photoAgent.ProcessMessage 
        |> Async.Parallel 
        |> Async.RunSynchronously
        |> Array.choose (fun eachResonse -> 
            match eachResonse with 
            | Result.Ok response -> 
                printfn $"\nAdded Message result:\n {printMessages [response]}"
                Some response
            | _ ->
                None 
        )
        |> ignore 

 
        printfn "\nView the message associated with a photo"
        let response = 
            async {
                return! photoAgent.ListMessages ()
            } 
            |> Async.RunSynchronously
        match response with 
        | Result.Ok messagesList -> 
            match messagesList with 
            | Some messages -> 
                printMessages messages
            | None -> 
                failwith "No messages"
        | err ->
            failwith $"err: {err}"


    let demoManagementAgent () = 
        printfn "simulate 20 persons, working on 200 photos with 50000 messages"

        DB.messages
        |> Seq.map PhotoManagement.processMessage 
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.choose (fun eachResponse -> 
            match eachResponse with 
            | Result.Ok response -> 
                Some response
            | _ -> 
                None 
        )
        |> ignore



        
            


        
