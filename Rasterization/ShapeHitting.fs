module Rasterization.ShapeHitting

open Avalonia
open Rasterization
open Rasterization.Drawing.Line
open Rasterization.Drawing.Circle
open Rasterization.Drawing.Polygon

let hitShape (point: Point) (antialiasing: AntiaAlisaingMode) (shape: Shape) =
    let getHit (point: Point) (pixels: ResizeArray<Pixel>)  =
        pixels
        |> Seq.map (fun p -> Point(p.x, p.y))
        |> Seq.exists (pointsAreInProximity point)
        |> function
            | true -> Some shape
            | false -> None

    match shape.Type with
    | Line -> getLinePixels shape antialiasing |> (getHit point)
    | Circle -> getCirclePixels shape |> (getHit point)
    | Polygon -> getPolygonPixels shape antialiasing |> fst |> (getHit point)

let tryFindHitShape (shapes: Shape list) (point: Point) (antialiasing: AntiaAlisaingMode) =
    let checkedShapes = shapes |> List.map (hitShape point antialiasing)

    checkedShapes
    |> List.choose id
    |> function
        | [] -> None
        | first :: _ -> Some first

type PolygonHit = Vertex of int | Edge of Point * int | NoHit

let getPolygonHit (shape: Shape) (point: Point) (aliasing: AntiaAlisaingMode) =
    let mutable vertexIndex = -1
    for i in 0..shape.Points.Length-1 do
        if pointsAreInProximity shape.Points.[i] point then
            vertexIndex <- i
            
    if vertexIndex > -1 then Vertex vertexIndex
    else
        let mutable edgeIndex = -1 
        for i in 0..shape.Points.Length-2 do
            let line = line shape.Color shape.Points[i] shape.Points[i + 1] shape.Thickness
            let linePixels = getLinePixels line aliasing |> Seq.map (fun p -> Point(p.x, p.y))
            if Seq.exists (fun p -> pointsAreInProximity p point) linePixels then
                edgeIndex <- i + 1 
            
        if edgeIndex > -1 then Edge (point, edgeIndex) else NoHit        