namespace Rasterization

open Avalonia
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI

open Rasterization
open ShapeHitting
open ShapeDrawing
open ShapeMoving
open Rasterization.Drawing.Polygon
open ShapeClipping

module DrawingCanvas =
    let private swapAntiaAliasing =
        function
        | GuptaSproull -> NoAntiAliasing
        | NoAntiAliasing -> GuptaSproull

    // I can't yet understand if this is related to the MacOS touchpad events
    // (they work in a different way; but one press is treated as two)
    // moreover, it could be related to two events send!
    let mutable ignoreNext = true

    let private initialImage =
        let image = new System.Drawing.Bitmap(CANVAS_WIDTH, CANVAS_HEIGHT)

        for i in 0 .. CANVAS_WIDTH - 1 do
            for j in 0 .. CANVAS_HEIGHT - 1 do
                image.SetPixel(i, j, System.Drawing.Color.White)

        image

    let initialState =
        { image = initialImage
          storedPoints = []
          figures = []
          floodFill = None
          drawingMode = Selection
          antiaAlisaingMode = NoAntiAliasing
          selectedFigure = None
          }
        
    let redrawCanvas (state: DrawingState) (figures: Shape list) swapAliasing =
        let aliasingMode =
            if swapAliasing then
                swapAntiaAliasing state.antiaAlisaingMode
            else
                state.antiaAlisaingMode

        figures
        |> Seq.filter (fun image -> image.IsComplete)
        |> Seq.fold (fun x image -> snd (drawShape x aliasingMode image)) initialState.image

    let handlePressedEvent (state: DrawingState) (args: PointerPressedEventArgs) : DrawingState =
        let pressPosition = position args
        if ignoreNext then
            ignoreNext <- false
            state
        else
            ignoreNext <- true

            match state.drawingMode with
            | CircleMode mode ->
                match mode with
                | Center ->
                    { state with
                        storedPoints = state.storedPoints @ [ position args ]
                        drawingMode = CircleMode CirclePoint }
                | CirclePoint ->
                    (circle SystemColor.Black state.storedPoints.[0] (position args))
                    |> drawShape state.image state.antiaAlisaingMode
                    |> fun drawn ->
                        { state with
                            image = snd drawn
                            figures = state.figures @ [ fst drawn ]
                            storedPoints = []
                            drawingMode = initialState.drawingMode }
            | LineMode mode ->
                match mode with
                | FirstPoint ->
                    { state with
                        storedPoints = state.storedPoints @ [ position args ]
                        drawingMode = LineMode SecondPoint }
                | SecondPoint ->
                    (line SystemColor.Black state.storedPoints.[0] (position args) 1)
                    |> drawShape state.image state.antiaAlisaingMode
                    |> fun drawn ->
                        { state with
                            image = snd drawn
                            figures = state.figures @ [ fst drawn ]
                            storedPoints = []
                            drawingMode = initialState.drawingMode }
            | PolygonMode mode ->
                match mode with
                | StartPoint ->  { state with
                                    storedPoints = state.storedPoints @ [ position args ]
                                    drawingMode = PolygonMode NextPoint }
                | NextPoint ->
                    let image = redrawCanvas state state.figures false 
                    let polygon = polygon SystemColor.Black (state.storedPoints @ [position args]) 1
                    let drawnPolygon, newImage = drawShape image state.antiaAlisaingMode polygon
                    if drawnPolygon.IsComplete then
                        {state with image = newImage; storedPoints = []; drawingMode = Selection; figures = state.figures @ [drawnPolygon]}
                    else
                        {state with image = newImage; storedPoints = state.storedPoints @ [position args]; drawingMode = PolygonMode NextPoint}
            | Selection ->
                let moveFigure figure =
                        let tmpFigures = List.except [figure] state.figures
                        let tmpImg = redrawCanvas state tmpFigures false 
                        let movedShape, image = moveShape figure (position args) tmpImg state.antiaAlisaingMode
                        {state with figures = tmpFigures @ [movedShape]; drawingMode = Selection; image = image; selectedFigure = None}
                
                match state.selectedFigure with
                | None ->  { state with selectedFigure = tryFindHitShape state.figures (position args) state.antiaAlisaingMode }
                | Some figure ->
                    match figure.Type with
                    | Line ->
                        if pointsAreInProximity figure.Points.[0] (position args) then
                            {state with drawingMode = ModificationMode FirstLinePoint}
                        else if pointsAreInProximity figure.Points.[1] (position args) then
                            {state with drawingMode = ModificationMode SecondLinePoint}
                        else
                            moveFigure figure
                    | Circle ->
                        match hitShape (position args) state.antiaAlisaingMode figure with
                        | None -> moveFigure figure
                        | Some _ -> {state with drawingMode = ModificationMode CircleRadius }
                    | Polygon ->
                        match getPolygonHit figure (position args) state.antiaAlisaingMode with
                        | NoHit -> moveFigure figure
                        | Edge(point, i) -> {state with drawingMode = (point, i) |> PolygonEdge |> ModificationMode }
                        | Vertex i -> {state with drawingMode = i |> PolygonVertex |> ModificationMode }
                    | Arc -> state
            | ModificationMode mode ->
                match mode with
                | FirstLinePoint ->
                     match state.selectedFigure with
                     | Some figure -> 
                        let tmpFigures = List.except [figure] state.figures
                        let points = [(position args); figure.Points.[1]]
                        let newLine = {figure with Points = points}
                        let newfigures = tmpFigures @ [ newLine ]
                        let newImg = redrawCanvas state newfigures false
                        {state with image = newImg; figures = newfigures; storedPoints = []; drawingMode = Selection; selectedFigure = None}
                     | None -> state
                | SecondLinePoint ->
                     match state.selectedFigure with
                     | Some figure -> 
                        let tmpFigures = List.except [figure] state.figures
                        let points = [figure.Points.[0]; (position args)]
                        let newLine = {figure with Points = points}
                        let newfigures = tmpFigures @ [ newLine ]
                        let newImg = redrawCanvas state newfigures false
                        {state with image = newImg; figures = newfigures; storedPoints = []; drawingMode = Selection; selectedFigure = None}
                     | None -> state
                | CircleRadius ->
                     match state.selectedFigure with
                     | Some figure ->
                        let tmpFigures = List.except [figure] state.figures
                        let points = [figure.Points.[0]; (position args)]
                        let newCircle = {figure with Points = points}
                        let newfigures = tmpFigures @ [ newCircle ]
                        let newImg = redrawCanvas state newfigures false
                        {state with image = newImg; figures = newfigures; storedPoints = []; drawingMode = Selection; selectedFigure = None}
                     | None -> state
                | PolygonVertex i ->
                     match state.selectedFigure with
                     | None -> state
                     | Some figure when not figure.IsRect ->
                        let tmpFigures = List.except [figure] state.figures
                        let mutable newPoints = List.updateAt i (position args) figure.Points
                        if i = 0 || i = figure.Points.Length - 1 then
                            newPoints <- List.updateAt 0 (position args) newPoints
                            newPoints <- List.updateAt (figure.Points.Length - 1) (position args) newPoints
                        let newPoly = {figure with Points = newPoints}
                        let newfigures = tmpFigures @ [ newPoly ]
                        let newImg = redrawCanvas state newfigures false
                        {state with image = newImg; figures = newfigures; storedPoints = []; drawingMode = Selection; selectedFigure = None}
                | PolygonEdge(point, i) ->
                    match state.selectedFigure with
                    | None -> state
                    | Some figure when not figure.IsRect ->
                        let tmpFigures = List.except [figure] state.figures
                        let newPoint = (position args)
                        let xDiff = newPoint.X - point.X
                        let yDiff = newPoint.Y - point.Y
                        let mutable newPoints = List.updateAt i (Point(figure.Points.[i].X + xDiff, figure.Points.[i].Y + yDiff)) figure.Points
                        newPoints <- List.updateAt (i-1) (Point(figure.Points.[i-1].X + xDiff, figure.Points.[i-1].Y + yDiff)) newPoints
                        if i = 1 || i  = figure.Points.Length - 1  then
                            newPoints <- List.updateAt 0 (Point(figure.Points.[0].X + xDiff, figure.Points.[0].Y + yDiff)) newPoints
                            newPoints <- List.updateAt (figure.Points.Length - 1) (Point(figure.Points.[figure.Points.Length - 1].X + xDiff, figure.Points.[figure.Points.Length - 1].Y + yDiff)) newPoints
                        let newPoly = {figure with Points = newPoints}
                        let newfigures = tmpFigures @ [ newPoly ]
                        let newImg = redrawCanvas state newfigures false
                        {state with image = newImg; figures = newfigures; storedPoints = []; drawingMode = Selection; selectedFigure = None}
            | ArcMode mode ->
                match mode with
                | A ->  { state with
                            storedPoints = state.storedPoints @ [ position args ]
                            drawingMode = ArcMode B }
                | B ->  { state with
                            storedPoints = state.storedPoints @ [ position args ]
                            drawingMode = ArcMode C }
                | C ->  // TODO 
                    let arc = arc (state.storedPoints @ [position args])
                    let shape, img = drawShape state.image state.antiaAlisaingMode arc 
                    { state with
                            image = img
                            figures = state.figures @ [ shape ]
                            storedPoints = []
                            drawingMode = initialState.drawingMode }
            | RectangleMode mode ->
                        match mode with
                        | FirstCorner -> { state with
                                            storedPoints = pressPosition :: state.storedPoints
                                            drawingMode = RectangleMode SecondCorner }
                        | SecondCorner ->
                            let image = redrawCanvas state state.figures false
                            let rectangleVertices = getRectangleVertices (state.storedPoints @ [position args])
                            let rectangle = rectangle SystemColor.Black rectangleVertices 1
                            let drawnRectangle, newImage = drawShape image state.antiaAlisaingMode rectangle
                            {state with image = newImage; storedPoints = []; drawingMode = Selection; figures = state.figures @ [drawnRectangle]}
            | FloodFill mode ->
                match state.floodFill with
                | None ->  state
                | Some color -> 
                    let newImg = floodFill pressPosition color state.image mode
                    {state with image = newImg}
                
    type Msg =
        | Pressed of PointerPressedEventArgs
        // TODO | Moved of PointerPressedEventArgs
        | SetLineMode
        | SetCircleMode
        | SetSelectionMode
        | SetArcMode
        | SetPolygonMode
        | SetRectangleMode
        | SetAntiAliasing
        | SetFloodFillColor of SystemColor * FloodFillMode
        | FillShape of SystemColor * Shape
        | SetClipShape of Shape
        | DeleteShape of Shape
        | UpdateColor of SystemColor * Shape
        | UpdateThickness of int * Shape
        | Clear

    let update (msg: Msg) (state: DrawingState) : DrawingState =
        match msg with
        | SetFloodFillColor (color, mode) ->
            {state with floodFill = Some color; drawingMode = FloodFill mode}
        | Pressed args -> handlePressedEvent state args
        // TODO | Moved args -> state
        | Clear -> initialState
        | FillShape (color, shape) ->
                if shape.Type = Polygon then
                    let newFigures = (List.except [shape] state.figures) @ [{shape with Fill = Some color}]
                    {state with image = redrawCanvas state newFigures false; drawingMode = Selection; selectedFigure = None; figures = newFigures}
                else state
        | SetClipShape shape ->
            let polyShapeRepr = if shape.IsRect then getRectangleAsPolygon shape else shape
            let justLines = state.figures
                               |> List.filter (fun f -> f.Type = Line)
                               
            let polygons =
                state.figures
                |> List.except [shape]
                |> List.filter (fun s -> s.Type = Polygon)
                |> List.map (fun s -> if s.IsRect then getRectangleAsPolygon s else s)
            
            let mutable polygonsAsLines = []
            
            for polygon in polygons do
                for i in 0..polygon.Points.Length-2 do
                    let p1 = polygon.Points[i]
                    let p2 = polygon.Points[i + 1]
                    
                    let newLine = line SystemColor.Red p1 p2 polygon.Thickness
                    polygonsAsLines <- polygonsAsLines @ [newLine]
                    
            
            let allLines = polygonsAsLines @ justLines
            try
                let clippedLines = allLines |> List.map (clipLineCyrusBeck (List.removeAt (polyShapeRepr.Points.Length - 1) polyShapeRepr.Points))
            
                let newImg = List.fold (fun img s -> snd (drawShape img state.antiaAlisaingMode s) ) state.image clippedLines
            
                {state with image = newImg; drawingMode = Selection; storedPoints = []; selectedFigure = None}
            with error -> printfn "%s \n %s" error.Message error.StackTrace; state
        | SetLineMode ->
            { state with
                storedPoints = []
                drawingMode = LineMode FirstPoint
                selectedFigure = None
                image = redrawCanvas state state.figures false }
        | SetCircleMode ->
            { state with
                storedPoints = []
                drawingMode = CircleMode Center
                selectedFigure = None
                image = redrawCanvas state state.figures false }
        | SetRectangleMode ->
            { state with
                storedPoints = []
                drawingMode = RectangleMode FirstCorner
                selectedFigure = None
                image = redrawCanvas state state.figures false }            
        | SetSelectionMode ->
            { state with
                storedPoints = []
                drawingMode = Selection
                selectedFigure = None
                image = redrawCanvas state state.figures false  }
        | SetAntiAliasing ->
            { state with
                storedPoints = []
                drawingMode = Selection
                antiaAlisaingMode = swapAntiaAliasing state.antiaAlisaingMode
                image = redrawCanvas state state.figures true }
        | DeleteShape shape ->
            let newFigures = state.figures |> List.except [ shape ]

            { state with
                figures = newFigures
                image = redrawCanvas state newFigures false
                selectedFigure = None }
        | UpdateColor (color, shape) ->
            let newFigures =
                (List.except [ shape ] state.figures)
                @ [ { shape with Color = color } ]

            { state with
                figures = newFigures
                image = redrawCanvas state newFigures false
                selectedFigure = None }
        | UpdateThickness (i, shape) ->
            let newFigures =
                (List.except [ shape ] state.figures)
                @ [ { shape with Thickness = i } ]

            { state with
                figures = newFigures
                image = redrawCanvas state newFigures false
                selectedFigure = None }
        | SetPolygonMode -> { state with
                                storedPoints = []
                                drawingMode = PolygonMode StartPoint
                                selectedFigure = None
                                image = redrawCanvas state state.figures false  }
        | SetArcMode -> { state with
                                storedPoints = []
                                drawingMode = ArcMode A
                                selectedFigure = None
                                image = redrawCanvas state state.figures false  }

            

    let view (state: DrawingState) (dispatch: Msg -> unit) : IView =
        Image.create [ Image.width CANVAS_WIDTH
                       Image.height CANVAS_HEIGHT
                       Image.source (toAvaloniaBitmap state.image)
                       Image.dock Dock.Right
                       Image.onPointerPressed (Pressed >> dispatch) ]
        |> generalize
