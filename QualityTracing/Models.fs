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
            Where: Area
            Who: Person.Person
            CreatedAt: System.DateTime
        }

    type AnnotationId = string 
    type TextMessage = 
        {
            Id: string 
            Content: string
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


    type Photo = 
        {
            Id: string 
            Title: string option 
            Content: byte array 
            Format: PicFormat
            Messages: Message list option  
            CreatAt: System.DateTime
        }

// ThreadAgent represent a place people do communication with a context of a photo
module ThreadAgent = 

    type ThreadMsg = 
        | AddPhotoMessage of AsyncReplyChannel<Result<Photo.Message list, string>> * Photo.Message
        | ListPhotoMessages of AsyncReplyChannel<Result<Photo.Message list option, string>>
        // TBD:: we could add more messages to support other operations

    type ThreadAgent (photo: Photo.Photo) = 
        let photo = photo 
        let agent = 
            MailboxProcessor.Start(fun inbox -> 
                let rec loop (messages: Photo.Message list option) = 
                    async {
                        let! msg = inbox.Receive()
                        match msg with 
                        | AddPhotoMessage (chnl, message) -> 
                            let messages' = 
                                match messages with 
                                | None -> 
                                    chnl.Reply (Result.Ok [message])
                                    Some [message]
                                | Some existingMessages -> 
                                    chnl.Reply (Result.Ok (message::existingMessages))
                                    Some (message::existingMessages)
                                    
                            return! loop(messages')
                        | ListPhotoMessages chnl -> 
                            chnl.Reply (Result.Ok messages)
                            return! loop(messages)
                    }
                loop(photo.Messages)        
            )

        member x.AddMessage (message: Photo.Message) = 
            async {
                return! agent.PostAndAsyncReply(fun chnl -> AddPhotoMessage(chnl, message))
            }

        member x.ListMessages () = 
            async {
                return! agent.PostAndAsyncReply(fun chnl -> ListPhotoMessages(chnl))
            }


