namespace QualityTracing

module Person = 
    type Company = 
        {
            Id: string 
            Name: string 
        }

    type Department = 
        {
            Id: string 
            Name: string 
            Company: Company
        }

    type Person =
        {
            Id: string  
            Name: string 
            DepartmentKey: string 
        }


module Photo = 
    type Point = {x: int; y: int}

    type Area = 
        | Rectangle of point: Point* width: int * height: int 
        | Circle of point: Point * radius: int 

    // Represent a scratch area with message associated with them
    type Annotation = 
        {
            Id: string 
            title: string option
            WhichPhoto: string
            Where: Area
            Who: Person.Person
            CreatedAt: System.DateTime
        }

    type AnnotationId = string 
    type TextMessage = 
        {
            Id: string 
            Text: string
            WhichPhoto: string
            Who: Person.Person
            CreatedAt: System.DateTime
            ForAnnotation: AnnotationId option 
        }

    type Message = 
        | Text of TextMessage 
        | Annotate of Annotation

    type PicFormat = 
        | PNG
        | JPEG

    type ProductId = string

    type Photo = 
        {
            Id: string 
            Title: string option 
            ImgPath: string // We don't store the binary data here, only the path to read actual image
            ImgSHA1: string
            Format: PicFormat
            Messages: Message list option  // We will only update this during communication
            CreatAt: System.DateTime
            Product: ProductId
        }

// ThreadAgent represent a place people do communication with a context of a photo
module PhotoAgent = 

    type PhotoMsg = 
        | AddPhotoMessage of AsyncReplyChannel<Result<Photo.Message, string>> * Photo.Message
        | ListPhotoMessages of AsyncReplyChannel<Result<Photo.Message list option, string>>
        // TBD:: we could add more messages to support other operations

    type PhotoAgent (photo: Photo.Photo) = 
        let photo = photo 
        let agent = 
            MailboxProcessor.Start(fun inbox -> 
                let rec loop (messages: Photo.Message list option) = 
                    async {
                        let! msg = inbox.Receive()
                        match msg with 
                        | AddPhotoMessage (chnl, message) -> 
                            let messages' = 
                                chnl.Reply (Result.Ok message)
                                match messages with 
                                | None ->                                    
                                    Some [message]
                                | Some existingMessages -> 
                                    Some (message::existingMessages)                                    
                            return! loop(messages')
                        | ListPhotoMessages chnl -> 
                            chnl.Reply (Result.Ok messages)
                            return! loop(messages)
                    }
                loop(photo.Messages)        
            )

        member x.ProcessMessage (message: Photo.Message) = 
            async {
                return! agent.PostAndAsyncReply(fun chnl -> AddPhotoMessage(chnl, message))
            }

        member x.ListMessages () = 
            async {
                return! agent.PostAndAsyncReply(fun chnl -> ListPhotoMessages(chnl))
            }


// This module is used in Web Service such that use call its pure functions to send messages associated with a photo id 
module PhotoManagement = 
    open PhotoAgent 
    open Photo

    type ManagePhotoMsg = 
        | GetPhotoAgent of AsyncReplyChannel<Result<PhotoAgent, string>> * string

    
    let simplePhotoApi id = 
        // Could be bottleneck 
        async {
            let photo = {
                Id = id 
                Title = None 
                ImgPath = "driverX/" + id
                ImgSHA1 = "Image SHA1"
                Format = PicFormat.PNG
                Messages = None
                CreatAt = System.DateTime.Now
                Product = "SomeProductNo."
            }
            return photo
        }

    type PhotoManagementAgent () = 
        // Works as memory cache
        let lookupMap: Map<string, PhotoAgent> = Map.empty
        let agent = 
            MailboxProcessor.Start(fun inbox ->
                let rec loop (lookupMap: Map<string, PhotoAgent>) =
                    async {
                        let! msg = inbox.Receive()
                        match msg with 
                        | GetPhotoAgent (chnl, photoId) -> 
                            match lookupMap.TryGetValue photoId with 
                            | true, photoAgent -> 
                                chnl.Reply (Result.Ok photoAgent)
                                return! loop lookupMap
                            | false, _ -> 
                                let! photo = simplePhotoApi photoId 
                                // TBD:: change photo to Photo ID to dely the fetch of photo here
                                let photoAgent = new PhotoAgent(photo) 
                                chnl.Reply (Result.Ok photoAgent)
                                return! loop (lookupMap.Add(photoId, photoAgent))
                    }
                loop lookupMap
            )

        // get the corresponding photoAgent using a photo's id
        member x.GetPhotoAgent id = 
            async {
                return! agent.PostAndAsyncReply(fun chnl -> GetPhotoAgent(chnl, id))
            }

    // A single instance PhotoManagement
    let service: PhotoManagementAgent = new PhotoManagementAgent()

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

    // Helper functions
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
                        printfn $"process message: {message} successfully"
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