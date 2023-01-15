namespace QualityTracing
module DB = 
    open PhotoAgent
    open Person
    open Photo
    open PhotoManagement
    open System



    // Simuate multiple messages associated with different photos
    let nPhotos = 5
    let nPersons = 10
    let nMessages = 1000

    let photoIds = 
        [1..nPhotos]
        |> List.map string 

    let personIds = [1..nPersons] |> List.map string 
    let messageIds = [1..nMessages] |> List.map string 

    //let rnd = new Random()
    //let randomPhotoId () = 
    //    rnd.Next(1, Seq.length photos + 1)

    let pickRandomOne x = 
        let shuffleR (r : Random) xs = 
            xs |> Seq.sortBy (fun _ -> r.Next())
        x |> shuffleR (Random ()) |> Seq.head

    let persons = 
        seq {
            for x in personIds do 
                yield {
                    Id = "person" + x 
                    Name = "zw" + x 
                    DepartmentKey = "Dep" + x 
                }
        }
        
    let photos = 
        seq {
            for x in photoIds do 
                yield {
                    Id = x 
                    Title = None 
                    ImgPath = "Path to the image. Locally or remotelly"
                    ImgSHA1 = $"Image SHA1 {x}"
                    Format = PicFormat.JPEG
                    Messages = None
                    CreatAt = DateTime.Now
                    Product = $"SomeProductNo.{x}"
                }
        }


    let getPhoto id = 
        async {
            // bottleneck for initializing a photo to create a photo agent
            do! Async.Sleep (1 * 1000)
            return 
                photos
                |> Seq.filter (fun x -> x.Id =  id)
                |> Seq.tryHead
        }

    let messages = 
        seq {
            for x in messageIds do 
                match (int x) % 2 = 0 with 
                | true -> 
                    yield Text {
                        Id = x 
                        Text = $"text message {x}"
                        Who = pickRandomOne persons
                        WhichPhoto =                             
                            let photo = pickRandomOne photos
                            photo.Id
                        CreatedAt = DateTime.Now
                        ForAnnotation = None 
                    } 
                | false -> 
                    yield Annotate {
                        Id = "2"
                        title = Some "This part01 has problem"
                        Where = Rectangle({x=100; y = 200}, 50, 75)
                        Who = pickRandomOne persons
                        WhichPhoto = 
                            let photo = pickRandomOne photos
                            photo.Id
                        CreatedAt = DateTime.Now           

                    }
        }


