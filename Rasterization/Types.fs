[<AutoOpen>]
module Rasterization.Types

open Avalonia

// constants here
let CANVAS_WIDTH = 600

let CANVAS_HEIGHT = 600

type AntiaAlisaingMode =
    | NoAntiAliasing
    | GuptaSproull

type ShapeType =
    | Polygon
    | Circle
    | Line
    | Arc 

type LineDrawingMode =
    | FirstPoint
    | SecondPoint

type CircleDrawingMode =
    | Center
    | CirclePoint

type PolygonDrawingMode =
    | StartPoint
    | NextPoint
    
type ArcDrawingMode =
    | A
    | B
    | C 

type ShapeModificationMode =
    | FirstLinePoint
    | SecondLinePoint
    | CircleRadius
    | PolygonVertex of int 
    | PolygonEdge of Point * int 

type DrawingMode =
    | Selection
    | LineMode of LineDrawingMode
    | CircleMode of CircleDrawingMode
    | PolygonMode of PolygonDrawingMode
    | ArcMode of ArcDrawingMode
    | ModificationMode of ShapeModificationMode 


type Shape =
    { Type: ShapeType
      Color: System.Drawing.Color
      Thickness: int
      IsComplete: bool
      Points: Point list }

type IInterface =
    abstract f: int -> int 

type AvaloniaBitmap = Avalonia.Media.Imaging.Bitmap

type SystemBitmap = System.Drawing.Bitmap

type SystemColor = System.Drawing.Color

type Pixel = { x: int; y: int; color: SystemColor }
