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

type RectangleDrawingMode =
    | FirstCorner 
    | SecondCorner

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

type FloodFillMode = Four | Eight 

type DrawingMode =
    | Selection
    | LineMode of LineDrawingMode
    | CircleMode of CircleDrawingMode
    | PolygonMode of PolygonDrawingMode
    | RectangleMode of RectangleDrawingMode
    | ArcMode of ArcDrawingMode
    | ModificationMode of ShapeModificationMode
    | PolygonClipMode of PolygonDrawingMode
    | FloodFill of FloodFillMode

type AvaloniaBitmap = Avalonia.Media.Imaging.Bitmap

type SystemBitmap = System.Drawing.Bitmap

type SystemColor = System.Drawing.Color

type Pixel = { X: int; Y: int; color: SystemColor }

type Shape =
    { Type: ShapeType
      Color: System.Drawing.Color
      Thickness: int
      Fill: SystemColor option
      mutable AllPixels: Pixel list
      IsComplete: bool
      IsRect: bool
      Points: Point list }

type DrawingState = {
          image: System.Drawing.Bitmap
          storedPoints: Point list
          figures: Shape list
          drawingMode: DrawingMode
          floodFill: SystemColor option
          antiaAlisaingMode: AntiaAlisaingMode
          selectedFigure: Shape option
          }