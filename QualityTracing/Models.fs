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

// PhotoAgent represent a place people do communication with a context of a photo
module PhotoAgent = 

    type PhotoMsg = 
        | SendPhotoMessage of AsyncReplyChannel<Result<Photo.Message, string>> * Photo.Message
        | ListPhotoMessages of AsyncReplyChannel<Result<Photo.Message list option, string>>
        // TBD:: we could add more messages to support other operations

    type PhotoAgent (photoId: string, f: string -> Async<Photo.Photo option>) = 
        let photoResult = 
            async {
                return! f photoId
            }|> Async.RunSynchronously
        let photo = 
            match photoResult with 
            | Some p -> p 
            | _ -> failwith ""

        let agent = 
            MailboxProcessor.Start(fun inbox -> 
                let rec loop (messages: Photo.Message list option) = 
                    async {
                        let! msg = inbox.Receive()
                        
                        match msg with 
                        | SendPhotoMessage (chnl, message) -> 
                            let messages' =                                 
                                chnl.Reply (Result.Ok message)
                                match messages with 
                                | None ->                                    
                                    Some [message]
                                | Some existingMessages -> 
                                    Some (message::existingMessages)     
                            //Represent some computation cost
                            //do! Async.Sleep (1 * 100) 
                            //printfn "updated messages for message: %A" msg
                            return! loop(messages')
                        | ListPhotoMessages chnl -> 
                            chnl.Reply (Result.Ok messages)
                            return! loop(messages)
                    }
                loop(photo.Messages)        
            )

        member x.ProcessMessage (message: Photo.Message) = 
            async {
                return! agent.PostAndAsyncReply(fun chnl -> SendPhotoMessage(chnl, message))
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

    let initPhotoAgent (photoId:string, getPhotoFun: string -> Async<Photo.Photo option>)= 
        async {
            return new PhotoAgent(photoId, getPhotoFun)
        }


    type PhotoManagementAgent (getPhotoFun: string -> Async<Photo.Photo option>) = 
        let getPhoto = getPhotoFun
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
                                //printfn $"Use existing photoAgent for photo: {photoId} "
                                chnl.Reply (Result.Ok photoAgent)
                                return! loop lookupMap
                            | false, _ -> 
                                //printfn $"Create new photoAgent for photo: {photoId}"
                                // TBD: return Result<Async<PhotoAgent>, string>? 
                                let! photoAgent = initPhotoAgent(photoId, getPhoto) 
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