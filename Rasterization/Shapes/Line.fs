namespace Rasterization.Drawing

open System
open Avalonia
open Rasterization
open System.Collections.Generic

module Line =
    let lerp (a) (b) (t: float) = (1.0 - t) * (float a) + t * (float b)

    let cov (d: float) r =
        if d <= r then
            (acos (d / r) - d * sqrt (r * r - d * d)) / Math.PI
        else
            0

    let coverage (w: float) (D: float) (r: float) =
        match w >= r with
        | true when w <= D -> cov (D - w) r
        | true when 0.0 <= D && D <= w -> 1.0 - cov (w - D) r
        | false when 0.0 <= D && D <= w -> 1.0 - (cov (w - D) r) - (cov (w + D) r)
        | false when w <= D && D <= (r - w) -> (cov (D - w) r) - (cov (D + w) r)
        | false when (r - w) <= D && D <= (r + w) -> cov (D - w) r
        | _ -> 0

    let intensifyPixel (pixels: List<Pixel>) (shapeColor: SystemColor) x y thickness distance =
        let r = 0.5
        let cov = coverage thickness distance r

        if cov > 0 then
            let R = (lerp 255 shapeColor.R cov) |> int
            let G = (lerp 255 shapeColor.G cov) |> int
            let B = (lerp 255 shapeColor.B cov) |> int
            pixels.Add({ X = x; Y = y; color = color R G B })

        cov

    let copyThickPixels (pixels: ResizeArray<Pixel>) (shapeColor: SystemColor) x y thickness moreVerticalLine =
        let thickness = thickness / 2

        let mapThickness thicc =
            if moreVerticalLine then
                (x + thicc, x - thicc), (y, y)
            else
                (x, x), (y + thicc, y - thicc)

        [ 0..thickness ]
        |> List.map mapThickness
        |> List.iter (fun ((x1, x2), (y1, y2)) ->
            pixels.Add({ X = x1; Y = y1; color = shapeColor })
            pixels.Add({ X = x2; Y = y2; color = shapeColor }))


    let copyPixelsWithAntiAliasing
        (pixels: ResizeArray<Pixel>)
        (shapeColor: SystemColor)
        x
        y
        thickness
        deltaInvertedDenominator
        vD
        dx
        dy
        =
        let thickness =
            if thickness = 1 then
                1
            else
                thickness / 2

        intensifyPixel pixels shapeColor x y thickness deltaInvertedDenominator
        |> ignore

        let mutable i = 1.0
        let mutable coverage = 1.0 // just some postitive value

        while coverage > 0 do
            coverage <-
                intensifyPixel pixels shapeColor (x + (int i) * dx) (y + (int i) * dy) thickness (i * deltaInvertedDenominator - vD)
            i <- i + 1.0
            
        coverage <- 1.0 // just some postitive value

        let mutable i = 1.0
        while coverage > 0 do
            coverage <-
                intensifyPixel pixels shapeColor (x - (int i) * dx) (y - (int i) * dy) thickness (i * deltaInvertedDenominator + vD)
            i <- i + 1.0

    let getHorizontalLinePixels x1 y1 x2 y2 (shapeColor: SystemColor) thickness (aliasing: AntiaAlisaingMode) =
        let line = ResizeArray<Pixel>()
        let dx = x2 - x1
        let dy = abs (y2 - y1)
        let mutable d = 2 * dy - dx
        let dE = 2 * dy
        let dNE = 2 * (dy - dx)
        let mutable xStart = x1
        let mutable yStart = y1
        let mutable xEnd = x2
        let mutable yEnd = y2

        let mutable twoVdX = 0.0 //numerator, v = 0 for the first pixel 
        let invertedDenominator = 1.0 / (2.0 * sqrt (float (dx * dx + dy * dy)))
        let twoDyInvertedDenominator = 2.0 * (float dx) * invertedDenominator

        let mutable delta = 1
        if y2 - y1 < 0 then delta <- -1

        match aliasing with
        | GuptaSproull ->
            copyPixelsWithAntiAliasing
                line
                shapeColor
                xStart
                yStart
                thickness
                twoDyInvertedDenominator
                (twoVdX * invertedDenominator)
                0
                delta

            copyPixelsWithAntiAliasing
                line
                shapeColor
                xEnd
                yEnd
                thickness
                twoDyInvertedDenominator
                (twoVdX * invertedDenominator)
                0
                -delta
        | NoAntiAliasing ->
            copyThickPixels line shapeColor xStart yStart (int thickness) false
            copyThickPixels line shapeColor xEnd yEnd (int thickness) false

        while xStart < xEnd do
            xStart <- xStart + 1
            xEnd <- xEnd - 1

            if d < 0 then
                twoVdX <- float (d + dx)
                d <- d + dE
            else
                twoVdX <- float (d - dx)
                d <- d + dNE
                yStart <- yStart + delta
                yEnd <- yEnd - delta

            match aliasing with
            | GuptaSproull ->
                copyPixelsWithAntiAliasing
                    line
                    shapeColor
                    xStart
                    yStart
                    thickness
                    twoDyInvertedDenominator
                    (twoVdX * invertedDenominator)
                    0
                    delta

                copyPixelsWithAntiAliasing
                    line
                    shapeColor
                    xEnd
                    yEnd
                    thickness
                    twoDyInvertedDenominator
                    (twoVdX * invertedDenominator)
                    0
                    -delta
            | NoAntiAliasing ->
                copyThickPixels line shapeColor xStart yStart (int thickness) false
                copyThickPixels line shapeColor xEnd yEnd (int thickness) false

        line

    let getVerticalLinePixels x1 y1 x2 y2 (shapeColor: SystemColor) thickness (aliasing: AntiaAlisaingMode) =
        let line = ResizeArray<Pixel>()
        let dx = x2 - x1
        let dy = abs (y2 - y1)
        let mutable d = 2 * dx - dy
        let dN = 2 * dx
        let dNE = 2 * (dx - dy)
        let mutable xStart = x1
        let mutable yStart = y1
        let mutable xEnd = x2
        let mutable yEnd = y2

        let mutable twoVdY = 0.0
        let invertedDenominator = 1.0 / (2.0 * sqrt (float (dx * dx + dy * dy)))
        let twoDyInvertedDenominator = 2.0 * (float dy) * invertedDenominator

        let mutable delta = 1
        if y2 - y1 < 0 then delta <- -1

        match aliasing with
        | GuptaSproull ->
            copyPixelsWithAntiAliasing
                line
                shapeColor
                xStart
                yStart
                thickness
                twoDyInvertedDenominator
                (twoVdY * invertedDenominator)
                1
                0

            copyPixelsWithAntiAliasing
                line
                shapeColor
                xEnd
                yEnd
                thickness
                twoDyInvertedDenominator
                (twoVdY * invertedDenominator)
                -1
                0
        | NoAntiAliasing ->
            copyThickPixels line shapeColor xStart yStart (int thickness) true
            copyThickPixels line shapeColor xEnd yEnd (int thickness) true

        while (delta * (yStart - yEnd)) < 0 do
            yStart <- yStart + delta
            yEnd <- yEnd - delta

            if d < 0 then
                twoVdY <- float (d + dy)
                d <- d + dN
            else
                twoVdY <- float (d - dy)
                d <- d + dNE
                xStart <- xStart + 1
                xEnd <- xEnd - 1

            match aliasing with
            | GuptaSproull ->
                copyPixelsWithAntiAliasing
                    line
                    shapeColor
                    xStart
                    yStart
                    thickness
                    twoDyInvertedDenominator
                    (twoVdY * invertedDenominator)
                    1
                    0

                copyPixelsWithAntiAliasing
                    line
                    shapeColor
                    xEnd
                    yEnd
                    thickness
                    twoDyInvertedDenominator
                    (twoVdY * invertedDenominator)
                    -1
                    0
            | NoAntiAliasing ->
                copyThickPixels line shapeColor xStart yStart (int thickness) true
                copyThickPixels line shapeColor xEnd yEnd (int thickness) true

        line

    let getLinePixels (shape: Shape) (antialiasing: AntiaAlisaingMode) =
        let points = ResizeArray<Pixel>()

        let x1 = int shape.Points.[0].X
        let y1 = int shape.Points.[0].Y
        let x2 = int shape.Points.[1].X
        let y2 = int shape.Points.[1].Y

        let mutable X1 = x1
        let mutable Y1 = y1
        let mutable X2 = x2
        let mutable Y2 = y2

        if x2 < x1 then
            X1 <- x2
            Y1 <- y2
            X2 <- x1
            Y2 <- y1

        let dy = Y2 - Y1
        let dx = X2 - X1

        if abs dx > abs dy then
            getHorizontalLinePixels X1 Y1 X2 Y2 shape.Color shape.Thickness antialiasing
            |> points.AddRange
        else
            getVerticalLinePixels X1 Y1 X2 Y2 shape.Color shape.Thickness antialiasing
            |> points.AddRange

        points


    let drawLine (shape: Shape) (bitmap: SystemBitmap) (antialiasing: AntiaAlisaingMode) =
        let tmp = new SystemBitmap(bitmap)

        shape
        |> (fun s -> getLinePixels s antialiasing)
        |> filterOutsidePixels
        |> Seq.iter (fun p -> tmp.SetPixel(p.X, p.Y, p.color))

        shape, tmp

    let moveLine (shape: Shape) (newCenter: Point) (bitmap: SystemBitmap) (antialiasing: AntiaAlisaingMode) =
        let tmp = new SystemBitmap(bitmap)
        
        let lineStart = shape.Points.[0]
        let lineEnd = shape.Points.[1]
        let lineXDiffBy2 = abs(lineStart.X - lineEnd.X) / 2.0
        let lineYDiffBy2 = abs(lineStart.Y - lineEnd.Y) / 2.0
        let lineCenterX = if lineStart.X < lineEnd.X then lineStart.X + lineXDiffBy2 else lineStart.X - lineXDiffBy2
        let lineCenterY = if lineStart.Y < lineEnd.Y then lineStart.Y + lineYDiffBy2 else lineStart.Y - lineYDiffBy2
        
        let xDiff = newCenter.X - lineCenterX 
        let yDiff = newCenter.Y - lineCenterY
        
        let newLineEnds = [
            Point(lineStart.X + xDiff, lineStart.Y + yDiff)
            Point(lineEnd.X + xDiff, lineEnd.Y + yDiff)
        ]
        
        let newLine = {shape with Points = newLineEnds}
        getLinePixels newLine antialiasing
        |> filterOutsidePixels
        |> Seq.iter (fun p -> tmp.SetPixel(p.X, p.Y, p.color))
        
        (newLine, tmp)