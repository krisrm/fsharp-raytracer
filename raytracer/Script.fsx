#load "Raytracer.fs"
open System.Windows.Forms


module Runner =
    open Raytracer.Main

    let form = new Form()

    form.StartPosition <- FormStartPosition.CenterScreen
    form.Text <- "Raytracer"
    form.Size <- System.Drawing.Size(int canvasSize.X, int canvasSize.Y)
    form.FormBorderStyle <- FormBorderStyle.FixedSingle
    form.MaximizeBox <- false

    form.MinimizeBox <- false

    let drawDialog (e:PaintEventArgs)=
        try
            let canvasBitmap = raytraceSceneToBitmap
            e.Graphics.DrawImage(canvasBitmap, 0, 0)
        with
            | ex -> printfn "%s" (ex.ToString())
       
    form.Paint.Add drawDialog

    form.ShowDialog()
    