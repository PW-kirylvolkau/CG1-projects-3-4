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
      IsRect = false
      Fill = None
      AllPixels = []
      Points = [ point1; point2 ]
      Thickness = thickness }

let circle (color: SystemColor) (center: Point) (circlePoint: Point) =
    { Type = Circle
      Color = color
      AllPixels = []
      IsRect = false
      Fill = None
      Points = [ center; circlePoint ]
      IsComplete = true
      Thickness = 1 }

let polygon (color: SystemColor) (points: Point list) (thickness: int) =
    {
        Type = Polygon
        Color = color
        Fill = None
        Points = points
        IsRect = false
        IsComplete = false
        AllPixels = []
        Thickness = thickness
    }
    
let arc (points: Point list) =
    {
        Type = Arc
        IsRect = false
        Fill = None
        Color = SystemColor.Black
        Points = points
        AllPixels = []
        IsComplete = true
        Thickness = 1
    }

let rectangle (color: SystemColor) (points: Point list) (thickness: int) =
    {
        Type = Polygon
        Fill = None
        Color = color
        IsRect = true
        Points = points
        AllPixels = []
        IsComplete = true
        Thickness = thickness
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
             p.X < CANVAS_WIDTH
            && p.X >= 0
            && p.Y < CANVAS_HEIGHT
            && p.Y >= 0)
            points
 