namespace Rasterization.Drawing

open Avalonia



module Circle =

    open System
    open Rasterization
    
    let getCirclePixels (shape: Shape) =
        let pixels = ResizeArray<Pixel>()
        
        let centerX = shape.Points.[0].X |> Convert.ToInt32
        let centerY = shape.Points.[0].Y |> Convert.ToInt32

        let R =
            Math.Sqrt(
                (shape.Points.[0].X - shape.Points.[1].X) ** 2
                + (shape.Points.[0].Y - shape.Points.[1].Y) ** 2
            )
            |> Convert.ToInt32

        let mutable dE = 3
        let mutable dSE = 5 - 2 * R
        let mutable d = 1 - R
        let mutable x = 0
        let mutable y = R

        pixels.Add({ x = centerX; y = y + R ; color = shape.Color })
        pixels.Add({ x = centerX; y = y - R; color = shape.Color })
        pixels.Add({ x = centerX - R; y = y; color = shape.Color })
        pixels.Add({ x = centerX + R; y = y; color = shape.Color })

        while y > x do
            if d < 0 then
                d <- d + dE
                dE <- dE + 2
                dSE <- dSE + 2
            else
                d <- d + dSE
                dE <- dE + 2
                dSE <- dSE + 4
                y <- y - 1

            x <- x + 1
            // TODO: handle case between quaters
            pixels.Add(
                { x = centerX + x
                  y = centerY + y
                  color = shape.Color }
            )

            pixels.Add(
                { x = centerX + x
                  y = centerY - y
                  color = shape.Color }
            )

            pixels.Add(
                { x = centerX - x
                  y = centerY + y
                  color = shape.Color }
            )

            pixels.Add(
                { x = centerX - x
                  y = centerY - y
                  color = shape.Color }
            )

            pixels.Add(
                { x = centerX + y
                  y = centerY + x
                  color = shape.Color }
            )

            pixels.Add(
                { x = centerX + y
                  y = centerY - x
                  color = shape.Color }
            )

            pixels.Add(
                { x = centerX - y
                  y = centerY + x
                  color = shape.Color }
            )

            pixels.Add(
                { x = centerX - y
                  y = centerY - x
                  color = shape.Color }
            )

        pixels

    let drawCircle (shape: Shape) (bitmap: SystemBitmap) (antiaAliasing: AntiaAlisaingMode) =
        let tmp = new SystemBitmap(bitmap)

        getCirclePixels shape
        |> Seq.filter (fun p ->
            p.x < CANVAS_WIDTH
            && p.x >= 0
            && p.y < CANVAS_HEIGHT
            && p.y >= 0)
        |> Seq.iter (fun p -> tmp.SetPixel(p.x, p.y, p.color))

        (shape, tmp)
        
    let moveCircle (shape: Shape) (newCenter: Point) (bitmap: SystemBitmap)  =
        let tmp = new SystemBitmap(bitmap)
        
        let center = shape.Points.[0]
        let circlePoint = shape.Points.[1]
        let xDiff = newCenter.X - center.X
        let yDiff = newCenter.Y - center.Y 
        let newShapePoints = [
            Point(center.X + xDiff, center.Y + yDiff)
            Point(circlePoint.X + xDiff, circlePoint.Y + yDiff)
        ]
        let newCircle = {shape with Points = newShapePoints}
        
        let newCirclePixels = getCirclePixels newCircle
        newCirclePixels
        |> filterOutsidePixels
        |> Seq.iter (fun p -> tmp.SetPixel(p.x, p.y, p.color))
        
        (newCircle, tmp)