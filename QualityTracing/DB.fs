namespace QualityTracing
module DB = 
    open PhotoAgent
    open Person
    open Photo
    open PhotoManagement
    open System


    // Simuate multiple messages associated with different photos
    let persons = 
        seq {
            for x in [1..4] do 
                yield {
                    Id = "person" + string x 
                    Name = "zw" + string x 
                    DepartmentKey = "Dep" + string x 
                }
        }


    let photos = 
        seq {
            for x in [1..2] do 
                yield {
                    Id = string x 
                    Title = None 
                    ImgPath = "Path to the image. Locally or remotelly"
                    ImgSHA1 = $"Image SHA1 {x}"
                    Format = PicFormat.JPEG
                    Messages = None
                    CreatAt = DateTime.Now
                    Product = $"SomeProductNo.{x}"
                }
        }
        

    //let rnd = new Random()
    //let randomPhotoId () = 
    //    rnd.Next(1, Seq.length photos + 1)

    let pickRandomOne x = 
        let shuffleR (r : Random) xs = 
            xs |> Seq.sortBy (fun _ -> r.Next())
        x |> shuffleR (Random ()) |> Seq.head
    

    let messages = 
        seq {
            for x in [1..12] do 
                match x % 2 = 0 with 
                | true -> 
                    yield Text {
                        Id = string x 
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


