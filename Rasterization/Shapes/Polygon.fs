namespace Rasterization.Drawing

open System.Collections.Generic
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
    
    let getRectangleVertices (point: Point list) =
        let p1 = point.[0]
        let p2 = point.[1]
    
        if p1.X < p2.X && p1.Y < p2.Y then 
            [p1; Point(p2.X, p1.Y); p2; Point(p1.X, p2.Y)]
        else if p1.X > p2.X && p1.Y > p2.Y then
            [p2; Point(p1.X, p2.Y); p1; Point(p2.X, p1.Y)]
        else if p1.X < p2.X && p1.Y > p2.Y then
            [p1; Point(p1.X, p2.Y); p2; (Point(p2.X, p1.Y))]
        else
            [p2; Point(p2.X, p1.Y); p1; (Point(p1.X, p2.Y))]
    
    let getRectangleAsPolygon (shape: Shape) =
        let p1 = shape.Points[0]
        {shape with Points = shape.Points @ [p1]}
    
    let getRectanglePixels (shape: Shape) (antialiasing: AntiaAlisaingMode) =
        getPolygonPixels (getRectangleAsPolygon shape) antialiasing
        
    let clamp (value: int) =
            let mutable result = value
            if value.CompareTo(255) > 0 then
                result <- 255
            if value.CompareTo(0) < 0 then
                result <- 0
            byte result

    let lowerY (p1: Pixel) (p2: Pixel) =
        if p1.Y <= p2.Y then p1 else p2

    let upperY (p1: Pixel) (p2: Pixel) =
        if p1.Y >= p2.Y then p1 else p2
    
    let fillPolygon (vertices: Pixel list) (shape: Shape) (color: SystemColor) =
        let getSecondItem (_1, _2, _3) = _2
    
        let N = vertices.Length
        let mutable AET = List.empty<int * float * float>
        let P = vertices
        let P1 = List.sortBy (fun p -> p.Y) P
        // duplicate points: the lambda should be used here(input is not a point is an index) 
        let indices = [for i in 0..N-1 -> List.findIndex (fun x -> x = P1.[i]) P]
        let mutable k = 0
        let mutable i = indices.[k]
        let mutable y = P.[indices.[0]].Y
        let mutable ymax = P.[indices.[N - 1]].Y
        while y < ymax do
        while P.[i].Y = y do
            if i > 0 then
                if P.[i - 1].Y > P.[i].Y then
                    let l = lowerY P.[i-1] P.[i]
                    let u = upperY P.[i-1] P.[i]
                    AET <- AET @ [(int u.Y, l.X, float (P.[i - 1].X - P.[i].X) / float ( P[i - 1].Y - P[i].Y))] 
            else
                if P[N - 1].Y > P[i].Y then
                    let l = lowerY P.[N-1] P.[i]
                    let u = upperY P.[N-1] P.[i]
                    AET <- AET @ [(int u.Y, l.X, float (P.[N - 1].X - P.[i].X) / float (P[N - 1].Y - P[i].Y))]
            if i < N - 1 then
                if P[i + 1].Y > P[i].Y then
                    let l = lowerY P.[i+1] P.[i]
                    let u = upperY P.[i+1] P.[i]
                    AET <- AET @ [(int u.Y, l.X, float (P[i + 1].X - P[i].X) / float (P[i + 1].Y - P[i].Y))]
            else
                if P[0].Y > P[i].Y then
                    let l = lowerY P.[0] P.[i]
                    let u = upperY P.[0] P.[i]
                    AET <- AET @ [(int u.Y, l.X, float (P[0].X - P[i].X) / float (P[0].Y - P[i].Y))]
            k <- k + 1
            i <- indices[k]
        // sort AET by X value
        AET <- List.sortBy getSecondItem AET
        for j in 0..2..AET.Length-1 do
            if j + 1 < AET.Length then
                let start = getSecondItem AET[j] |> int 
                let endd = getSecondItem AET[j + 1] |> int 
                for x in start..endd do
                    shape.AllPixels <- shape.AllPixels @ [{X = int x; Y = int y; color = color}]
        y <- y + 1
        AET <- List.filter (fun (_1, _, _) -> _1 <> int y) AET
        for j in 0..AET.Length - 1 do
            let value (_1, _2, _3) = _1, _2 + _3, _3 
            AET <- List.updateAt j (value AET[j]) AET
    
    let floodFill (p: Point) (color: SystemColor) (bitmap: SystemBitmap) (mode: FloodFillMode) = 
        let tmp = new SystemBitmap(bitmap)
        // DFS implementation 
        let stack = Stack<Point>()
        stack.Push(p)
        let mutable oldColor = tmp.GetPixel(int p.X, int p.Y)
        
        while stack.Count <> 0 do
            let point = stack.Pop()
            if point.X >= 0 && point.X < CANVAS_WIDTH && point.Y >= 0 && point.Y < CANVAS_HEIGHT then
                let currentColor = tmp.GetPixel(int point.X, int point.Y)
                if currentColor.Equals(oldColor) then
                    tmp.SetPixel(int point.X, int point.Y, color)
                    match mode with
                    | Four -> 
                        stack.Push(point.WithX(point.X + 1.))
                        stack.Push(point.WithX(point.X - 1.))
                        stack.Push(point.WithY(point.Y + 1.))
                        stack.Push(point.WithY(point.Y - 1.))
                    | Eight -> 
                        stack.Push(point.WithX(point.X + 1.))
                        stack.Push(point.WithX(point.X + 2.))
                        stack.Push(point.WithX(point.X - 1.))
                        stack.Push(point.WithX(point.X - 2.))
                        stack.Push(point.WithY(point.Y + 1.))
                        stack.Push(point.WithY(point.Y + 2.))
                        stack.Push(point.WithY(point.Y - 1.))    
                        stack.Push(point.WithY(point.Y - 2.))    
        tmp         
    
    let drawFilledPolygon (shape: Shape) (color: SystemColor) (antialiasing: AntiaAlisaingMode) (bmp: SystemBitmap) =
        try
            let tmpShape = if shape.IsRect then getRectangleAsPolygon shape else shape
            fillPolygon (
                tmpShape.Points
                |> List.removeAt (tmpShape.Points.Length - 1 )
                |> List.map (fun p -> {X = int p.X; Y = int p.Y; color = shape.Color})) shape color
        with
        | error -> printfn $"{error}"
        
        shape.AllPixels <- shape.AllPixels
                       |> List.append ((if not shape.IsRect then getPolygonPixels shape antialiasing else getRectanglePixels shape antialiasing)  |> fst |> Seq.toList)
                       |> List.distinctBy (fun p -> p.X, p.Y)
    
        let tmp = new SystemBitmap(bmp)
    
        shape.AllPixels
        |> List.filter (fun p -> p.X < CANVAS_WIDTH && p.X >=0  && p.Y < CANVAS_HEIGHT && p.Y >= 0)
        |> List.iter (fun p -> tmp.SetPixel(p.X, p.Y, p.color))
    
        shape.AllPixels <- []
        
        tmp
        
    let drawPolygon (shape: Shape) (bitmap: SystemBitmap) (antiaAliasing: AntiaAlisaingMode) =
        match shape.Fill with
        | None -> 
            let tmp = new SystemBitmap(bitmap)
            let pixels, complete =
                if not shape.IsRect then getPolygonPixels shape antiaAliasing else getRectanglePixels shape antiaAliasing 
            pixels
            |> filterOutsidePixels
            |> Seq.iter (fun p -> tmp.SetPixel(p.X, p.Y, p.color))
        
            {shape with IsComplete = complete; AllPixels = []}, tmp
        | Some color ->
            {shape with AllPixels = []}, drawFilledPolygon shape color antiaAliasing bitmap
            
    let movePolygon (shape: Shape) (newCenter: Point) (bitmap: SystemBitmap) (antiaAliasing: AntiaAlisaingMode) =
        
        let shape = if shape.IsRect then getRectangleAsPolygon shape else shape
        
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
        let newPolygon = if not shape.IsRect then {shape with Points = newPolygonPoints} else {shape with Points = List.removeAt (newPolygonPoints.Length - 1) newPolygonPoints}
        
        drawPolygon newPolygon bitmap antiaAliasing
        
    
       