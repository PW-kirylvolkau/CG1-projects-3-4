namespace Rasterization

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Elmish
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Elmish
open Rasterization

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Rasterization"
        base.Width <- 1200.0
        base.Height <- 1200.0
        this.Content <- MainView.view

        //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        let updateWithWindow = MainView.update this

        Program.mkProgram MainView.initialState updateWithWindow MainView.view
        |> Program.withHost this
        |> Program.run


type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme(baseUri = null, Mode = FluentThemeMode.Dark))

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main (args: string []) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
