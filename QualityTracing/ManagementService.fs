namespace QualityTracing

module ManagementService = 
    open PhotoManagement
    open Photo
    // A single instance PhotoManagement
    let service: PhotoManagementAgent = new PhotoManagementAgent (DB.getPhoto)

    let (|GetPhotoAgent|_|) imgId = 
        async {
            let! photoAgentResponse = service.GetPhotoAgent imgId
            match photoAgentResponse with 
            | Result.Ok photoAgent -> 
                return Some photoAgent
            | Result.Error err -> 
                printfn $"PhotoManagementAgent failed to get photoAgent: {err}"
                return None
        } |> Async.RunSynchronously


    let processMessage message = 
            let imgId = 
                match message with 
                | Text msg -> msg.WhichPhoto
                | Annotate msg -> msg.WhichPhoto

            async {
                match imgId with 
                | GetPhotoAgent photoAgent -> 
                    let! processMessageResponse = photoAgent.ProcessMessage message
                    match processMessageResponse with 
                    | Result.Ok message -> 
                        return Result.Ok message
                    | Result.Error err -> 
                        return Result.Error $"{photoAgent} processMessage failed with err: {err}"
                | _ -> 
                    return Result.Error $"PhotoManagementAgent failed to get photoAgent"
            }


    let getMessages imgId = 
        async {
            match imgId with 
            | GetPhotoAgent photoAgent -> 
                let! processMessageResponse = photoAgent.ListMessages ()
                match processMessageResponse with 
                | Result.Ok messages ->
                    return Result.Ok messages
                | Result.Error err -> 
                    return Result.Error $"{photoAgent} processMessage failed with err: {err}"       
            | _ -> 
                return Result.Error $"PhotoManagementAgent failed to get photoAgent"
        }

