module Rasterization.Shapes.Rectangle

open Avalonia
open Rasterization
open Rasterization.Drawing.Polygon


//let getRectangleVertices (point: Point list) =
//    let p1 = point.[0]
//    let p2 = point.[1]
//    
//    if p1.X < p2.X && p1.Y < p2.Y then 
//        [p1; Point(p2.X, p1.Y); p2; Point(p1.X, p2.Y)]
//    else if p1.X > p2.X && p1.Y > p2.Y then
//        [p2; Point(p1.X, p2.Y); p1; Point(p2.X, p1.Y)]
//    else if p1.X < p2.X && p1.Y > p2.Y then
//        [p1; Point(p1.X, p2.Y); p2; (Point(p2.X, p1.Y))]
//    else
//        [p2; Point(p2.X, p1.Y); p1; (Point(p1.X, p2.Y))]
//        
//let getRectangleAsPolygon (shape: Shape) =
//    let p1 = shape.Points[0]
//    {shape with Points = shape.Points @ [p1]}
//
//let drawRectangle (shape: Shape) (bitmap: SystemBitmap) (antiaAliasing: AntiaAlisaingMode) =
//    let tmp = new SystemBitmap(bitmap)
//    let pixels, _ = getPolygonPixels (getRectangleAsPolygon shape) antiaAliasing
//
//    pixels
//    |> Seq.iter (fun p -> tmp.SetPixel(p.X, p.Y, p.color))
//
//    shape, tmp

//let movePolygon (shape: Shape) (newCenter: Point) (bitmap: SystemBitmap) (antiaAliasing: AntiaAlisaingMode) =
//    let tmp = new SystemBitmap(bitmap)
//
//    let tmpShape = if shape.IsRect then getRectangleAsPolygon shape else shape
//    
//    let maxPolygonX =
//        tmpShape.Points
//        |> List.map (fun p -> p.X)
//        |> List.max
//
//    let minPolygonX =
//        tmpShape.Points
//        |> List.map (fun p -> p.X)
//        |> List.min
//
//    let maxPolygonY =
//        tmpShape.Points
//        |> List.map (fun p -> p.Y)
//        |> List.max
//
//    let minPolygonY =
//        tmpShape.Points
//        |> List.map (fun p -> p.Y)
//        |> List.min
//
//    let line =
//        line tmpShape.Color (Point(maxPolygonX, maxPolygonY)) (Point(minPolygonX, minPolygonY)) 1
//
//    let lineStart = line.Points.[0]
//    let lineEnd = line.Points.[1]
//    let lineXDiffBy2 = abs (lineStart.X - lineEnd.X) / 2.0
//    let lineYDiffBy2 = abs (lineStart.Y - lineEnd.Y) / 2.0
//
//    let lineCenterX =
//        if lineStart.X < lineEnd.X then
//            lineStart.X + lineXDiffBy2
//        else
//            lineStart.X - lineXDiffBy2
//
//    let lineCenterY =
//        if lineStart.Y < lineEnd.Y then
//            lineStart.Y + lineYDiffBy2
//        else
//            lineStart.Y - lineYDiffBy2
//
//    let xDiff = newCenter.X - lineCenterX
//    let yDiff = newCenter.Y - lineCenterY
//
//    let newPolygonPoints =
//        tmpShape.Points
//        |> List.map (fun p -> Point(p.X + xDiff, p.Y + yDiff))
//
//    let newPolygon = { tmpShape with Points = newPolygonPoints }
//
//    let newPolygonPixels, _ =
//        getPolygonPixels newPolygon antiaAliasing
//
//    newPolygonPixels
//    |> filterOutsidePixels
//    |> Seq.iter (fun p -> tmp.SetPixel(p.X, p.Y, p.color))
//
//    
//    if shape.IsRect then {newPolygon with Points = List.distinct newPolygon.Points} else newPolygon
//    , tmp
