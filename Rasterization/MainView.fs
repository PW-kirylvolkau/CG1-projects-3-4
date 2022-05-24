namespace Rasterization

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.Input
open Avalonia.Media
open Newtonsoft.Json
open Rasterization

module MainView =
    let filePath = "/Users/kirylvolkau/Desktop/export.json"
    open Avalonia.FuncUI
    open Avalonia.Layout
    
    let getHitFigure (shape: Shape option) =
        match shape with
        | Some x -> sprintf "%A" {x with AllPixels = []}// hide all pixels - they mess up the screen.
        | None -> "no figure selected"
    
    type State = {
        drawingState: DrawingState
        red: double
        blue: double
        green: double
        thickness: int
    }
    
    let initialState() = { drawingState = DrawingCanvas.initialState; red = 0; blue = 0; green = 0; thickness = 1 }, Elmish.Cmd.none
    
    let export (state: State) =
         let serialized = JsonConvert.SerializeObject(state.drawingState.figures)
         System.IO.File.WriteAllText(filePath, serialized)
    
    let import () =
        let json = System.IO.File.ReadAllText(filePath)
        let serialized = JsonConvert.DeserializeObject<Shape list>(json)
        {
            DrawingCanvas.initialState with
                figures = serialized
                image = DrawingCanvas.redrawCanvas DrawingCanvas.initialState serialized false
        }
    
    let brokenExport window =
        let dialog = SaveFileDialog()
        let filter = ResizeArray<string>()
        filter.Add(".json")
        dialog.Filters.Add(FileDialogFilter(Name = "JSON", Extensions = filter))
        dialog.ShowAsync window |> Async.AwaitTask
        
    
    type Msg = 
         | DrawingMsg of DrawingCanvas.Msg
         | UpdateRed of double
         | UpdateGreen of double
         | UpdateBlue of double
         | UpdateDrawingColor of Shape
         | UpdateFillColor of Shape
         | UpdateThickness of int
         | UpdateShapeThickness of Shape
         | SetClipShape of Shape
         | FloodFill of FloodFillMode
         | BrokenExport
         | AfterExportSelected of string
         | Export 
         | Import
         
         
    let update (window: HostWindow) (msg: Msg) (state: State) =
        match msg with
        | DrawingMsg drawingMsg ->
            { state with drawingState = (DrawingCanvas.update drawingMsg state.drawingState) }, Elmish.Cmd.none
        | UpdateBlue blue -> {state with blue = blue}, Elmish.Cmd.none
        | UpdateGreen green -> {state with green = green}, Elmish.Cmd.none
        | UpdateRed red -> {state with red = red}, Elmish.Cmd.none
        | UpdateDrawingColor shape ->
            let newColor = color (int state.red) (int state.green) (int state.blue) 
            {
             state with drawingState = (DrawingCanvas.update (DrawingCanvas.Msg.UpdateColor(newColor, shape)) state.drawingState)
            }, Elmish.Cmd.none
        | FloodFill mode ->
            let newColor = color (int state.red) (int state.green) (int state.blue) 
            {
             state with drawingState = (DrawingCanvas.update (DrawingCanvas.Msg.SetFloodFillColor(newColor, mode)) state.drawingState)
            }, Elmish.Cmd.none
        | UpdateFillColor shape ->
             let newColor = color (int state.red) (int state.green) (int state.blue)
             {
             state with drawingState = (DrawingCanvas.update (DrawingCanvas.Msg.FillShape(newColor, shape)) state.drawingState)
             }, Elmish.Cmd.none
        | UpdateThickness i -> {state with thickness = i}, Elmish.Cmd.none
        | UpdateShapeThickness shape ->
            {state with drawingState =  (DrawingCanvas.update (DrawingCanvas.Msg.UpdateThickness(state.thickness, shape)) state.drawingState) },
            Elmish.Cmd.none
        | BrokenExport ->
            state, Elmish.Cmd.OfAsync.perform brokenExport window AfterExportSelected
        | AfterExportSelected path ->
            let serialized = System.Text.Json.JsonSerializer.Serialize(state.drawingState.figures)
            System.IO.File.WriteAllText(path, serialized)
            state, Elmish.Cmd.none
        | Export ->
            export state
            state, Elmish.Cmd.none
        | Import ->
            { state with drawingState = import () }, Elmish.Cmd.none
        | SetClipShape shape ->
            {state with drawingState = (DrawingCanvas.update (DrawingCanvas.Msg.SetClipShape(shape)) state.drawingState) }, Elmish.Cmd.none
        
    let editingPanel (shape: Shape) (state: State) (dispatch: Msg -> unit) =
        DockPanel.create [
             DockPanel.width 200
             DockPanel.dock Dock.Bottom
             DockPanel.verticalAlignment VerticalAlignment.Center
             DockPanel.horizontalAlignment HorizontalAlignment.Center
             DockPanel.children [
                 TextBlock.create [
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.dock Dock.Bottom
                                TextBlock.text (getHitFigure state.drawingState.selectedFigure)
                            ]
                 Button.create [
                     Button.dock Dock.Top
                     Button.content "Delete"
                     Button.onClick (fun _ -> shape |> DrawingCanvas.Msg.DeleteShape |> DrawingMsg |> dispatch )
                 ]
                 TextBlock.create [
                     TextBlock.dock Dock.Top
                     TextBlock.text "NEW COLOR"
                 ]
                 TextBlock.create [
                     TextBlock.dock Dock.Top
                     TextBlock.text $"RED: {state.red}"
                 ]
                 Slider.create [
                     Slider.dock Dock.Top
                     Slider.name "RED"
                     Slider.minimum 0
                     Slider.maximum 255
                     Slider.value 0
                     Slider.tickFrequency 1
                     Slider.tickPlacement TickPlacement.Outside
                     Slider.isSnapToTickEnabled true
                     Slider.onValueChanged (UpdateRed >> dispatch)
                 ]
                 TextBlock.create [
                     TextBlock.dock Dock.Top
                     TextBlock.text $"GREEN: {state.green}"
                 ]
                 Slider.create [
                     Slider.dock Dock.Top
                     Slider.name "GREEN"
                     Slider.minimum 0
                     Slider.maximum 255
                     Slider.tickFrequency 1
                     Slider.tickPlacement TickPlacement.Outside
                     Slider.isSnapToTickEnabled true
                     Slider.onValueChanged (UpdateGreen >> dispatch)
                 ]
                 TextBlock.create [
                     TextBlock.dock Dock.Top
                     TextBlock.text $"BLUE: {state.blue}"
                 ]
                 Slider.create [
                     Slider.dock Dock.Top
                     Slider.name "BLUE"
                     Slider.minimum 0
                     Slider.maximum 255
                     Slider.tickFrequency 1
                     Slider.tickPlacement TickPlacement.Outside
                     Slider.isSnapToTickEnabled true
                     Slider.value 0
                     Slider.onValueChanged (UpdateBlue >> dispatch)
                 ]
                 Canvas.create [
                     Canvas.dock Dock.Top
                     Canvas.width 20
                     Canvas.height 20
                     Canvas.background (SolidColorBrush(Color.FromRgb(byte state.red, byte state.green, byte state.blue)))
                 ]
                 Button.create [
                     Button.dock Dock.Top
                     Button.content "UPD COLOR"
                     Button.onClick (fun _ -> shape |> UpdateDrawingColor |> dispatch )
                 ]
                 Button.create [
                     Button.dock Dock.Top
                     Button.content "FILL CLR COLOR"
                     Button.onClick (fun _ -> shape |> UpdateFillColor |> dispatch )
                 ]
                 Button.create [
                     Button.dock Dock.Top
                     Button.content "SET FLOOD FILL COLOR (4 connected)"
                     Button.onClick (fun _ -> Four |> FloodFill |> dispatch )
                 ]
                 Button.create [
                     Button.dock Dock.Top
                     Button.content "SET FLOOD FILL COLOR (8 connected)"
                     Button.onClick (fun _ -> Eight |> FloodFill |> dispatch )
                 ]
                 TextBlock.create [
                     TextBlock.dock Dock.Top
                     TextBlock.text $"New thickness: {state.thickness}"
                 ]
                 Slider.create [
                     Slider.dock Dock.Top
                     Slider.tickFrequency 2
                     Slider.tickPlacement TickPlacement.Outside
                     Slider.minimum 1
                     Slider.maximum 9
                     Slider.isSnapToTickEnabled true
                     Slider.onValueChanged (int >> UpdateThickness >> dispatch)
                 ]
                 Button.create [
                     Button.dock Dock.Top
                     Button.content "Update thickness"
                     Button.onClick (fun _ -> shape |> UpdateShapeThickness |> dispatch )
                 ]
                 if shape.Type = Polygon then
                    Button.create [
                         Button.dock Dock.Top
                         Button.content "Set as a clip"
                         Button.onClick (fun _ -> shape |> SetClipShape |> dispatch )
                    ]
             ]
        ]
    
    let view (state: State) (dispatch: Msg -> unit) =
            DockPanel.create [
                DockPanel.verticalAlignment VerticalAlignment.Center
                DockPanel.horizontalAlignment HorizontalAlignment.Center
                DockPanel.children [
                    DrawingCanvas.view state.drawingState (DrawingMsg >> dispatch)
                    DockPanel.create [
                        DockPanel.dock Dock.Left
                        DockPanel.children [
                            TextBlock.create [
                                TextBlock.width 200
                                TextBlock.dock Dock.Bottom
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.text $"MODE:\n{state.drawingState.drawingMode}"
                            ]
                            Button.create [
                                Button.width 200
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content "Clear"
                                Button.onClick (fun _ -> DrawingCanvas.Msg.Clear |> DrawingMsg |> dispatch )
                                Button.dock Dock.Bottom
                            ]
                            Button.create [
                                Button.width 200
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content $"Antialiasing {match state.drawingState.antiaAlisaingMode with
                                                                | GuptaSproull -> true
                                                                | _ -> false }"
                                Button.onClick (fun _ -> DrawingCanvas.Msg.SetAntiAliasing |> DrawingMsg |> dispatch )
                                Button.dock Dock.Bottom
                            ]
                            Button.create [
                                Button.width 200
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content "MODE: line"
                                Button.onClick (fun _ -> DrawingCanvas.Msg.SetLineMode |> DrawingMsg |> dispatch )
                                Button.dock Dock.Bottom
                            ]
                            Button.create [
                                Button.width 200
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content "MODE: rect"
                                Button.onClick (fun _ -> DrawingCanvas.Msg.SetRectangleMode |> DrawingMsg |> dispatch )
                                Button.dock Dock.Bottom
                            ]
                            Button.create [
                                Button.width 200
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content "MODE: Polygon"
                                Button.onClick (fun _ -> DrawingCanvas.Msg.SetPolygonMode |> DrawingMsg |> dispatch )
                                Button.dock Dock.Bottom
                            ]
                            Button.create [
                                Button.width 200
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content "MODE: circle"
                                Button.onClick (fun _ -> DrawingCanvas.Msg.SetCircleMode |> DrawingMsg |> dispatch )
                                Button.dock Dock.Bottom
                            ]
                            Button.create [
                                Button.width 200
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content "MODE: arc"
                                Button.onClick (fun _ -> DrawingCanvas.Msg.SetArcMode |> DrawingMsg |> dispatch )
                                Button.dock Dock.Bottom
                            ]
                            Button.create [
                                Button.width 200
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content "MODE: selection"
                                Button.onClick (fun _ -> DrawingCanvas.Msg.SetSelectionMode |> DrawingMsg |> dispatch )
                                Button.dock Dock.Bottom
                            ]
                            Button.create [
                                Button.width 200
                                Button.dock Dock.Bottom
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content "EXPORT"
                                Button.onClick (fun _ -> dispatch Export)
                            ]
                            Button.create [
                                Button.width 200
                                Button.dock Dock.Bottom
                                Button.horizontalAlignment HorizontalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.content "IMPORT"
                                Button.onClick (fun _ -> dispatch Import)
                            ]
                            match state.drawingState.selectedFigure with
                            | Some x -> editingPanel x state dispatch
                            | None -> TextBlock.create [
                                TextBlock.dock Dock.Bottom
                                TextBlock.text "no figure selected"
                            ]
                        ]
                    ]
                ]
            ]
            |> generalize
