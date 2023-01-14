namespace QualityTracing
open System

module Demos = 
    open ThreadAgent
    open Person
    open Photo
    
    let demoConcurrentlyUpdatePhoto () = 
        let persons = 
            seq {
                for x in [1..3] do 
                    yield {
                        Id = "person" + string x 
                        Name = "zw" + string x 
                        DepartmentKey = "Dep" + string x 
                    }
            }
            |> Array.ofSeq


        let photo =
            {
                Id = "1" 
                Title = None 
                Content = System.Text.Encoding.ASCII.GetBytes("hello world!")
                Format = PicFormat.PNG
                Messages = None
                CreatAt = DateTime.Now
            }

        let threadAgent = new ThreadAgent(photo)

        let messages = [
            Text {
                Id = "1"
                Content = "text message 01"
                Who = persons[0]
                CreatedAt = DateTime.Now
                ForAnnotation = None 
            }
            Annotate {
                Id = "2"
                title = Some "This part01 has problem"
                Where = Rectangle({x=100; y = 200}, 50, 75)
                Who = persons[1]
                CreatedAt = DateTime.Now
            }
            Annotate {
                Id = "3"
                title = Some "This part02 has problem"
                Where = Circle({x=200; y = 300}, 10)
                Who = persons[2]
                CreatedAt = DateTime.Now            
            }
        ]

        let printMessage messages = 
            messages 
            |> List.iter (fun x -> 
                printfn $"{x}"
            )

        printfn "Concurrently send messages to photo agent"
        messages
        |> List.map threadAgent.AddMessage 
        |> Async.Parallel 
        |> Async.RunSynchronously
        |> Array.choose (fun eachResonse -> 
            match eachResonse with 
            | Result.Ok response -> 
                printfn $"\nAdded Message result:\n {printMessage response}"
                Some response
            | _ ->
                None 
        )
        |> ignore 

 
        printfn "\nView the message associated with a photo"
        let response = 
            async {
                return! threadAgent.ListMessages ()
            } 
            |> Async.RunSynchronously
        match response with 
        | Result.Ok messagesList -> 
            match messagesList with 
            | Some messages -> 
                printMessage messages
            | None -> 
                failwith "No messages"
        | err ->
            failwith $"err: {err}"



        
