namespace EdgeDetect

open System
open System.Drawing
open System.Drawing.Imaging
open System.IO

module E =
    [<Literal>]
    let MarginSize = 40

    [<Literal>]
    let AdaptiveThreshold = 0.9

    /// ensures v is not less than l (lower) and greater than u (upper)
    let clamp l v u = if v < l then l elif v > u then u else v

    /// Given a width and height, compute a 1d array index from a 2d coordinate
    let xy_to_i w h x y = clamp w (w * y + x) h

    /// Calls fn with values x, y and i based on a given array width and height, returning the result as a sequence
    let seqxy w h fn =
        seq {
            let pos = ref 0
            for x in 0 .. (w - 1) do
                for y in 0 .. (h - 1) do
                    yield fn x y !pos
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

    /// Determines the absolute distance between two points
    let dist x1 y1 x2 y2 = abs (sqrt(float ((pown (x1 - x2) 2) + (pown (y1 - y2) 2))))

    /// Given a binary image, determine the location of the cropped rectangle, returning the
    /// result as a pair of points representing the top left * bottom right coordinate positions
    let edge_detect w h (pixels:array<Color>) =

        let TopLeft, TopRight, BottomLeft, BottomRight = 0, 1, 2, 3

        // the edges of the used area, defaults to the full page
        let edges = [| (0, 0); (w, 0); (0, h); (w, h) |]

        // keeps track of the distance each point is from the edges of the page.
        let current = [| for x in 0 .. 3 do yield System.Double.MaxValue |]

        // list of functions to calculate the distance from each corner
        let distance_funcs = [
            dist 0 0 (* top left *);
            dist w 0 (* top right *);
            dist 0 h (* bottom left *);
            dist w h (* bottom right *)]

        iterxy w h (fun x y index ->

            let pixel = pixels.[index]

            // if this point is not a background pixel, determine how far away from each corner it is
            if (int pixel.R) > 0 then
                // apply x and y to each distance function to determine whether this is the closest point to an edge
                distance_funcs |> List.iteri (fun i fn ->
                    let dist = fn x y
                    if dist < current.[i] then
                        Array.set edges i (x, y)
                        Array.set current i dist)
                ()
            ())

        let top_left =
            let x = edges |> Array.map (fun e -> fst e) |> Array.min
            let y = edges |> Array.map (fun e -> snd e) |> Array.min
            (x, y)

        let bottom_right =
            let x = edges |> Array.map (fun e -> fst e) |> Array.max
            let y = edges |> Array.map (fun e -> snd e) |> Array.max
            (x, y)

        (top_left, bottom_right)

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

        // Returns the average colour from a list of colours
        let average pixels =
            let sum =
                pixels
                |> List.map (fun (c:Color) -> c.ToArgb())
                |> List.sum
            (float sum) / (float pixels.Length)

        let output = Array.create<Color> (pixels.Length) Color.White
        iterxy w h (fun x y i ->
            let pixel = pixels.[i].ToArgb()
            let avg = average <| neighbours x y
            let diff = float avg / float pixel
            if diff > threshold then
                Array.set output i Color.Black)

        (w, h, output)

    let process_file inputFile outputFile margins =
        // read the source image into an array of colours
        let (srcW, srcH, source_pixels) = (read_image inputFile)

        // convert the image to grey scale and apply adaptive thresholding to create a binary image
        let (_, _, black_and_white) = adaptive_threshold AdaptiveThreshold srcW srcH source_pixels

        // determine where the corners of the visible area are
        let ((x1, y1), (x2, y2)) = edge_detect srcW srcH (black_and_white |> Array.ofSeq)

        // add some margin around the corners to give the image some white space (assuming there is room to do so)
        let x1m = if x1 > margins then x1 - margins else x1
        let x2m = if (x2 + margins) < srcW then (x2 + margins) else x2
        let y1m = if y1 > margins then y1 - margins else y1
        let y2m = if (y2 + margins < srcH) then (y2 + margins) else y2

        // crop this section of the image
        let subset = crop srcW srcH x1m y1m x2m y2m source_pixels

        // save the cropped image out to a new file
        write_image outputFile (x2m - x1m) (y2m - y1m) subset
