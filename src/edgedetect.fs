namespace EdgeDetect

open System
open System.Drawing
open System.Drawing.Imaging
open System.IO
open System.Linq
open System.Threading.Tasks

module E =
    [<Literal>]
    let MarginSize = 10

    [<Literal>]
    let AdaptiveThreshold = 0.6

    /// ensures v is not less than l (lower) and greater than u (upper)
    let clamp l v u = if v < l then l elif v > u then u else v

    /// Given a width and height, compute a 1d array index from a 2d coordinate
    let xy_to_i w h x y = clamp w (w * y + x) h

    /// Calls fn with values x, y and i based on a given array width and height, returning the result as a sequence
    let seqxy2 w h =
        seq {
            let pos = ref 0
            for x in 0 .. (w - 1) do
                for y in 0 .. (h - 1) do
                    yield (x, y, !pos)
                    pos := !pos + 1
        }

    /// Calls fn with values x, y and i based on a given array width and height
    let iterxy w h fn =
        let pos = ref 0
        for x in 0 .. (w - 1) do
            for y in 0 .. (h - 1) do
                fn x y !pos
                pos := !pos + 1

    /// Reads a file into a tuple containing (image width, image height, color[])
    let read_image file =
        let img = Bitmap.FromFile(file) :?> Bitmap
        let w, h = img.Width, img.Height
        let pixels = Array.create<Color> (w * h) Color.Yellow
        iterxy w h (fun x y i -> Array.set pixels i (img.GetPixel(x, y)))
        (w, h, pixels)

    /// Writes an image out to disk with the given width and height
    let write_image file (w:int) (h:int) (pixels: array<Color>) =
        let img = new Bitmap(w, h)
        iterxy w h (fun x y i -> img.SetPixel(x, y, pixels.[i]))
        img.Save(file)

    /// Writes an image out to disk with the given width and height
    let write_image_with_bounds file (w:int) (h:int) (pixels: array<Color>) (rx:int) (ry:int) (rw:int) (rh:int) =
        let img = new Bitmap(w, h)
        iterxy w h (fun x y i -> img.SetPixel(x, y, pixels.[i]))
        Graphics.FromImage(img).DrawRectangle(Pens.Red, rx, ry, rw, rh)
        img.Save(file)

    /// Determines the absolute distance between two points
    let dist x1 y1 x2 y2 = abs (sqrt(float ((pown (x1 - x2) 2) + (pown (y1 - y2) 2))))

    let edge_detect2 w h (pixels:array<Color>) =

        let mutable minX = w
        let mutable maxX = 0
        let mutable minY = h
        let mutable maxY = 0
        
        iterxy w h (fun x y index ->
            let pixel = pixels.[index]

            // if this point is not a background pixel, determine how far away from each corner it is
            if (int pixel.R) = 0 then
                if x < minX then minX <- x
                if x > maxX then maxX <- x
                if y < minY then minY <- y
                if y > maxY then maxY <- y
        )

        ((minX, minY), (maxX, maxY))
    
    /// Crops an image based on a top left and bottom right coordinates
    let crop srcW srcH x1 y1 x2 y2 (pixels:array<Color>) =
        let destW, destH = (x2 - x1), (y2 - y1)
        let output = Array.create<Color> (destW * destH) Color.Red
        let index = ref 0

        iterxy srcW srcH (fun x y i ->
            if x > x1 && x <= x2 && y > y1 && y <= y2 then
                Array.set output !index pixels.[i]
                index := !index + 1)
        
        output

    /// Applies adaptive thresholding to an input array, returning the result as a binary image
    /// where pixels that are greater than 90% different from its surrounding pixel average is treated
    /// as a white pixel, and pixels below that are treated as black
    let adaptive_threshold threshold w h (pixels:array<Color>) =

        // curry function as we know w and h up front
        let idx = xy_to_i 0 ((w * h) - 1)

        // return a list of neighbouring points
        let neighbours x y = [
            pixels.[idx (x - 1) (y - 1)]; // top left
            pixels.[idx x (y - 1)];       // top middle
            pixels.[idx (x + 1) (y - 1)]; // top right
            pixels.[idx (x - 1) y];       // middle left
            pixels.[idx (x + 1) y];       // middle right
            pixels.[idx (x - 1) (y + 1)]; // bottom left
            pixels.[idx x (y + 1)];       // bottom middle
            pixels.[idx (x + 1) (y + 1)]] // bottom right

        let pixel_value (c:Color) = ((float (c.R + c.G + c.B)) / 3.0) + 0.1

        // Returns the average grey scale colour from a list of colours
        let average pixels =
            let sum =
                pixels
                |> List.map pixel_value
                |> List.sum
            sum / (float pixels.Length)

        let output = Array.create<Color> (pixels.Length) Color.White

        #if PARALLEL_FOREACH
        let lazy_seq = seqxy2 w h
        Parallel.ForEach(lazy_seq, fun (x, y, i) -> 
            let pixel = pixel_value pixels.[i]
            let avg = average <| neighbours x y
            let diff = ((abs (avg - pixel)) / avg)
            if diff > threshold then
                Array.set output i Color.Black) |> ignore
        #else
        iterxy w h (fun x y i -> 
            let pixel = pixel_value pixels.[i]
            let avg = average <| neighbours x y
            let diff = ((abs (avg - pixel)) / avg)
            if diff > threshold then
                Array.set output i Color.Black)
        #endif
        output

    let process_file inputFile outputFile margins =
        // read the source image into an array of colours
        let (srcW, srcH, source_pixels) = (read_image inputFile)

        // convert the image to grey scale and apply adaptive thresholding to create a binary image
        let black_and_white = adaptive_threshold AdaptiveThreshold srcW srcH source_pixels

        // determine name of intermediate file to write out with debug details 
        let outputFile2 = Path.Combine(FileInfo(outputFile).Directory.FullName, Path.GetFileNameWithoutExtension(outputFile) + ".bw.png")

        // determine where the corners of the visible area are
        let ((x1, y1), (x2, y2)) = edge_detect2 srcW srcH (black_and_white |> Array.ofSeq)

        write_image_with_bounds outputFile2 srcW srcH black_and_white x1 y1 (x2 - x1) (y2 - y1)

        // add some margin around the corners to give the image some white space (assuming there is room to do so)
        let x1m = if x1 > margins then x1 - margins else x1
        let x2m = if (x2 + margins) < srcW then (x2 + margins) else x2
        let y1m = if y1 > margins then y1 - margins else y1
        let y2m = if (y2 + margins < srcH) then (y2 + margins) else y2

        // crop this section of the image
        let subset = crop srcW srcH x1m y1m x2m y2m source_pixels

        // save the cropped image out to a new file
        write_image outputFile (x2m - x1m) (y2m - y1m) subset
