[<AutoOpen>]
module Rasterization.Utils

open System.Collections.Generic
open System.Drawing.Imaging
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media.Imaging

let toAvaloniaBitmap (bmp: System.Drawing.Bitmap) =
    let stream = new MemoryStream()
    bmp.Save(stream, ImageFormat.Bmp)
    stream.Position <- 0
    new Bitmap(stream)
    
// F# - like extensions methods 
type System.Drawing.Bitmap with
    member i.ToAvaloniaBitmap() =
        let stream = new MemoryStream()
        i.Save(stream, ImageFormat.Bmp)
        stream.Position <- 0
        new AvaloniaBitmap(stream)

let bmp = new SystemBitmap(5,5)
let avalonia = bmp.ToAvaloniaBitmap()
let position (args: PointerEventArgs) =
    args.GetPosition(args.Source :?> Control)
    |> fun p -> (Point(int p.X, int p.Y))


let color r g b = SystemColor.FromArgb(255, r, g, b)

let line (color: SystemColor) (point1: Point) (point2: Point) (thickness: int) =
    { Type = Line
      Color = color
      IsComplete = true
      Points = [ point1; point2 ]
      Thickness = thickness }

let circle (color: SystemColor) (center: Point) (circlePoint: Point) =
    { Type = Circle
      Color = color
      Points = [ center; circlePoint ]
      IsComplete = true
      Thickness = 1 }

let polygon (color: SystemColor) (points: Point list) (thickness: int) =
    {
        Type = Polygon
        Color = color
        Points = points
        IsComplete = false
        Thickness = thickness
    }
    
let arc (points: Point list) =
    {
        Type = Arc
        Color = SystemColor.Black
        Points = points
        IsComplete = true
        Thickness = 1
    }

let pointsAreInProximity (point1: Point) (point2: Point) =
    let hitMatrix = HashSet<int * int>()
    for i in 0..5 do
        for j in 0..5 do
            hitMatrix.Add((i,j)) |> ignore
            hitMatrix.Add((-i,j)) |> ignore
            hitMatrix.Add((i,-j)) |> ignore
            hitMatrix.Add((-i,-j)) |> ignore
    
    hitMatrix
    |> Seq.distinct
    |> Seq.exists (fun (i,j) -> (int point1.X) + i = (int point2.X) && (int point1.Y) + j = (int point2.Y))
    
let filterOutsidePixels (points: seq<Pixel>) =
      Seq.filter (fun p ->
             p.x < CANVAS_WIDTH
            && p.x >= 0
            && p.y < CANVAS_HEIGHT
            && p.y >= 0)
            points
 