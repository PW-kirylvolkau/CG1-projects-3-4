module Rasterization.ShapeClipping

open Avalonia
open Microsoft.FSharp.Core
open Rasterization

let dot (p0: Pixel) (p1: Pixel) =
    p0.X * p1.X + p0.Y * p1.Y
    
let dotProduct (p0: Point) (p1: Point) =
    p0.X * p1.X + p0.Y * p1.Y

// https://www.geeksforgeeks.org/line-clipping-set-2-cyrus-beck-algorithm/
let clipLineCyrusBeck (vertices: Point list) (shape: Shape) =
    let n = vertices.Length
    let mutable normals = []
    
    for i in 0..n - 1 do
        normals <- normals @ [
            {Y = vertices[i].Y - vertices[(i + 1) % n].Y |> int
             X = vertices[(i + 1) % n].X - vertices[i].X |> int 
             color = SystemColor.Red}
        ]
    
    let PI_P0 = {
        X = shape.Points[1].X - shape.Points[0].X |> int
        Y = shape.Points[1].Y - shape.Points[0].Y |> int 
        color = SystemColor.Red}
    
    let mutable P0_PEi = []
    
    for i in 0..n-1 do
        P0_PEi <- P0_PEi @ [ {
            X = vertices[i].X - shape.Points[0].X |> int 
            Y = vertices[i].Y - shape.Points[0].Y |> int 
            color = SystemColor.Red
        }]
    
    let mutable numerator = []
    let mutable denominator = []
    
    for i in 0..n-1 do
        numerator <- numerator @ [dot normals[i] P0_PEi[i]]
        denominator <- denominator @ [dot normals[i] PI_P0]
    
    let mutable t = []
    let mutable tE = []
    let mutable tL = []
    
    // opt: finding min & max here (of tE - tL) 
    for i in 0..n-1 do
        let tI = float numerator[i] / float denominator[i]
        t <- t @ [tI]
        if denominator[i] > 0 then
            tE <- tE @ [tI]
        else 
            tL <- tL @ [tI]
            
    tE <- tE @ [0.0]
    tL <- tL @ [1.0]
    
    let mutable value: Shape option = None
    
    if Seq.min tL > Seq.max tE then
        // TODO: just not draw
        value <- Some (line SystemColor.Red (Point(0, 0)) (Point(0, 0)) 1)
    
    match value with
    | Some x -> x
    | None ->
        let p1 = Point(
            shape.Points[0].X + float PI_P0.X * (Seq.max tE),
            shape.Points[0].Y + float PI_P0.Y * (Seq.max tE))
        let p2 = Point(
            shape.Points[0].X + float PI_P0.X * (Seq.min tL),
            shape.Points[0].Y + float PI_P0.Y * (Seq.min tL))
        line SystemColor.Red p1 p2 shape.Thickness   