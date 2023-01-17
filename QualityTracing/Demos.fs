namespace QualityTracing
open System

module Demos = 
    open PhotoAgent
    open ManagementService
    open FSharp.Control

        
    // Process many message to see if prototype works
    let processMessages messages = 
        
        messages
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
        
        let result = duration (fun _ -> processMessages DB.messages)
        printfn $"===Took {result} ms===\n\n\n"
