open System
open System.IO
open EdgeDetect.E

[<EntryPoint>]
let main argv = 

    let InputDir = argv.[0]
    let OutputDir = argv.[1]

    Async.Parallel [
        for input_file in Directory.GetFiles(InputDir) ->
            async {
                let filename = Path.GetFileName(input_file)
                let output_file = Path.Combine(OutputDir, filename)

                printf "Processing input file '%s'\n" input_file
                
                let start = DateTime.UtcNow
                
                process_file input_file output_file MarginSize

                let delta = DateTime.UtcNow - start

                printf "Done in %d(ms) \n" <| int delta.TotalMilliseconds
            } ]
    |> Async.RunSynchronously |> ignore
    0