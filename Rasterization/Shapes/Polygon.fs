namespace Rasterization.Drawing

open Avalonia
open Rasterization
open Line

module Polygon =
    let getPolygonPixels (shape: Shape) (antialiasing: AntiaAlisaingMode) =
        let pixels = ResizeArray<Pixel>()
        for i in 0..shape.Points.Length - 3 do
            let line = line shape.Color shape.Points.[i] shape.Points.[i + 1] shape.Thickness
            let linePixels = getLinePixels line antialiasing
            pixels.AddRange(linePixels)
        
        let mutable isComplete = false 
        if pointsAreInProximity shape.Points.[shape.Points.Length - 1] shape.Points.[0] then
            let line = line shape.Color shape.Points.[shape.Points.Length - 2] shape.Points.[0] shape.Thickness
            let linePixels = getLinePixels line antialiasing
            pixels.AddRange(linePixels)
            isComplete <- true
        else
            let line = line shape.Color shape.Points.[shape.Points.Length - 2] shape.Points.[shape.Points.Length - 1] shape.Thickness
            let linePixels = getLinePixels line antialiasing
            pixels.AddRange(linePixels)
        
        (pixels, isComplete)   
    
    let drawPolygon (shape: Shape) (bitmap: SystemBitmap) (antiaAliasing: AntiaAlisaingMode) =
        let tmp = new SystemBitmap(bitmap)
        let pixels, complete = getPolygonPixels shape antiaAliasing
        pixels |> Seq.iter (fun p -> tmp.SetPixel(p.x, p.y, p.color))
        
        {shape with IsComplete = complete}, tmp
        
    let movePolygon (shape: Shape) (newCenter: Point) (bitmap: SystemBitmap) (antiaAliasing: AntiaAlisaingMode) =
        let tmp = new SystemBitmap(bitmap)
        
        let maxPolygonX = shape.Points |> List.map (fun p -> p.X) |> List.max
        let minPolygonX = shape.Points |> List.map (fun p -> p.X) |> List.min
        let maxPolygonY = shape.Points |> List.map (fun p -> p.Y) |> List.max
        let minPolygonY = shape.Points |> List.map (fun p -> p.Y) |> List.min
        
        let line = line shape.Color (Point(maxPolygonX, maxPolygonY)) (Point(minPolygonX, minPolygonY)) 1
        let lineStart = line.Points.[0]
        let lineEnd = line.Points.[1]
        let lineXDiffBy2 = abs(lineStart.X - lineEnd.X) / 2.0
        let lineYDiffBy2 = abs(lineStart.Y - lineEnd.Y) / 2.0
        let lineCenterX = if lineStart.X < lineEnd.X then lineStart.X + lineXDiffBy2 else lineStart.X - lineXDiffBy2
        let lineCenterY = if lineStart.Y < lineEnd.Y then lineStart.Y + lineYDiffBy2 else lineStart.Y - lineYDiffBy2
        
        let xDiff = newCenter.X - lineCenterX 
        let yDiff = newCenter.Y - lineCenterY
        
        let newPolygonPoints = shape.Points |> List.map (fun p -> Point(p.X + xDiff, p.Y + yDiff))
        let newPolygon = {shape with Points = newPolygonPoints}
        let newPolygonPixels, _ = getPolygonPixels newPolygon antiaAliasing 
        newPolygonPixels
        |> filterOutsidePixels
        |> Seq.iter (fun p -> tmp.SetPixel(p.x, p.y, p.color))
        
        newPolygon, tmp