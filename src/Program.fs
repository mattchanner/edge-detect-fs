open System
open System.IO
open System.Linq
open EdgeDetect.E

[<EntryPoint>]
let main argv = 
    
    let files() = 
        if argv.Length = 0 then
            ("..\..\..\data\source", "..\..\..\data\dest")
        else
            (argv.[0], argv.[1])

    let InputDir, OutputDir = files()

    let startTime = System.DateTime.UtcNow

    #if PARALLEL_FILE_LOOP
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
            }
    ] |> Async.RunSynchronously |> ignore
    #else
    Directory.GetFiles(InputDir) |> Array.iter (fun input_file ->
            let filename = Path.GetFileName(input_file)
            let output_file = Path.Combine(OutputDir, filename)

            printf "Processing input file '%s'\n" input_file
                
            let start = DateTime.UtcNow
                
            process_file input_file output_file MarginSize

            let delta = DateTime.UtcNow - start

            printf "Done in %d(ms) \n" <| int delta.TotalMilliseconds)
    #endif
    
    let elapsed = DateTime.UtcNow - startTime
    printf "Total time %d(ms) \n" <| int elapsed.TotalMilliseconds

    0