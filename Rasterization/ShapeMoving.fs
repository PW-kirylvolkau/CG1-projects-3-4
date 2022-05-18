module Rasterization.ShapeMoving

open Avalonia
open Rasterization
open Rasterization.Drawing.Circle
open Rasterization.Drawing.Line
open Rasterization.Drawing.Polygon

let moveShape (shape: Shape) (newCenter: Point) (bitmap: SystemBitmap) (antialiasingMode: AntiaAlisaingMode) =
    match shape.Type with
    | Circle -> moveCircle shape newCenter bitmap
    | Line -> moveLine shape newCenter bitmap antialiasingMode
    | Polygon -> movePolygon shape newCenter bitmap antialiasingMode