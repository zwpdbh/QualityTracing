namespace QualityTracing
open System

module Demos = 
    open PhotoAgent
    open ManagementService
    open FSharp.Control

    let demoConcurrentlyUpdatePhoto () = 

        let photoId = DB.pickRandomOne DB.photoIds
        let photoAgent = new PhotoAgent(photoId, DB.getPhoto)

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
        
    let execution () = 
        DB.messages
        |> AsyncSeq.ofSeq
        |> AsyncSeq.mapAsync ManagementService.processMessage
        |> AsyncSeq.choose (fun x -> 
                match x with 
                | Result.Ok response -> 
                    Some response
                | _ -> 
                    None 
        ) 
        |> AsyncSeq.toListSynchronously
        |> fun messages -> 
            printfn $"Total processed messages: {messages.Length}"
            messages
        |> List.groupBy (fun x -> 
            match x with 
            | Photo.Text message -> message.WhichPhoto
            | Photo.Annotate message -> message.WhichPhoto 
        )
        |> List.iter (fun (whichPhoto, messages) -> 
            printfn $"PhotoAgent{whichPhoto} has {messages.Length} messages"
        )


    let demoManagementAgent () = 
        // Test if ManagementService and photoAgents works
        let duration f =
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()
            f () |> ignore
            timer.ElapsedMilliseconds

        printfn $"simulate {DB.nPersons} persons, working on {DB.nPhotos} photos with {DB.nMessages} messages"
        
        let result = duration execution
        printfn $"===Took {result} ms===\n\n\n"
