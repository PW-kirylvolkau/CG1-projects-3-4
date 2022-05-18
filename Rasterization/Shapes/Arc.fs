module Rasterization.Shapes.Arc

open Avalonia
open Rasterization
open Rasterization.Drawing.Circle

let determinant (a: Pixel) (b: Pixel) (c: Pixel) =
    a.x * b.y - a.x * c.y - a.y * b.x + a.y * c.x + b.x * c.y - b.y * c.x

let conditionalDraw (bmp: SystemBitmap) (color: SystemColor) (a: Pixel) (b: Pixel) (c: Pixel) (d: Pixel)  =
    if determinant a b c > 0 then
        if determinant a b d > 0 && determinant a c d < 0 then bmp.SetPixel(int d.x, int d.y, color)
    else
        if not (determinant a b d < 0 && determinant a c d > 0) then bmp.SetPixel(int d.x, int d.y, color)

let drawArc (shape: Shape) (bitmap: SystemBitmap) =
    let pixel (p: Point) = {x = int p.X; y = int p.Y; color = SystemColor.Black}
    let tmp = new SystemBitmap(bitmap)
    
    let A = shape.Points.[0] 
    let B = shape.Points.[1] 
    let C = shape.Points.[2] 
    
    let circle = circle SystemColor.Black A B
    let circlePixels = getCirclePixels circle
    
    let drawConditionally = conditionalDraw tmp circle.Color (pixel A) (pixel B) (pixel C)

    circlePixels
    |> Seq.iter drawConditionally
    
    (shape, tmp)