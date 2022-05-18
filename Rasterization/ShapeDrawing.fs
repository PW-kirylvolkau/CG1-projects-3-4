module Rasterization.ShapeDrawing

open Rasterization
open Rasterization.Drawing.Line
open Rasterization.Drawing.Circle
open Rasterization.Drawing.Polygon
open Rasterization.Shapes.Arc

let drawShape (bitmap: SystemBitmap) (antiaAliasing: AntiaAlisaingMode) (shape: Shape) =
    match shape.Type with
    | Circle -> drawCircle shape bitmap antiaAliasing
    | Polygon -> drawPolygon shape bitmap antiaAliasing
    | Line -> drawLine shape bitmap antiaAliasing
    | Arc -> drawArc shape bitmap
