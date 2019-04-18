namespace Raytracer
open System
open System.Drawing

module Constants = 
    [<Literal>]
    let Inf = 10000.0


type Vec3 = struct
    val X : float
    val Y : float
    val Z : float
 
    new (x,y,z) = { X = x; Y = y; Z = z }

    new (c:Color) = { X = float c.R; Y = float c.G; Z= float c.B}
 
    override v.ToString() = sprintf "[%f, %f, %f]" v.X v.Y v.Z

    static member (+)(o: Vec3, o2: Vec3) = 
        Vec3(o.X+o2.X, o.Y+o2.Y, o.Z+o2.Z)

    static member (-)(o: Vec3, o2: Vec3) = 
        Vec3(o.X-o2.X, o.Y-o2.Y, o.Z-o2.Z)

    static member (*)(o: float, o2: Vec3) = 
        Vec3(o*o2.X, o*o2.Y, o*o2.Z)

    static member (/)(o: Vec3, o2: float) = 
        Vec3(o.X/o2, o.Y/o2, o.Z/o2)

    member this.Dot (o: Vec3) = 
        (this.X*o.X) + (this.Y*o.Y) + (this.Z*o.Z)

    member this.Length =
        sqrt((this.X * this.X) + (this.Y*this.Y) + (this.Z*this.Z))

    member this.Sqrt =
        Vec3(sqrt this.X, sqrt this.Y, sqrt this.Z)
    
    member this.AsColor =
        Color.FromArgb(int this.X, int this.Y, int this.Z)
end

type Vec2 = struct
    val X: float
    val Y: float
    new(x: float, y: float) = { X = x; Y = y }
    override v.ToString() = sprintf "[%f, %f]" v.X v.Y
end

type Sphere = struct
    val C: Vec3
    val R: float
    val color: Color
    val Specular: float
    val Reflective: float

    new(
        x: float, y: float, z: float,
        r: float, 
        color: Color, 
        s: float,
        ref: float) = 
        { C = Vec3(x, y, z); R = r; color = color; Specular = s; Reflective = ref}

    member this.IntersectRaySphere(O: Vec3, D: Vec3) =
        let c = this.C
        let r = this.R

        let oc = O - c

        let k1 = D.Dot(D)
        let k2 = 2.0*oc.Dot(D)
        let k3 = (oc.Dot(oc)) - (r * r)

        let discriminant = (k2*k2) - (4.0*k1*k3)

        if discriminant < 0.0 then
            (Constants.Inf, Constants.Inf)
        else 
            let t1 = ((-1.0 * k2) + sqrt(discriminant)) / (2.0*k1)
            let t2 = ((-1.0 * k2) - sqrt(discriminant)) / (2.0*k1)

            (t1, t2)
end


type Light = struct
    val Intensity: float
    val Type: string
    val PositionDirection: Option<Vec3>
    
    new(i:float, ty: string, pd: Option<Vec3>) = 
        {Intensity = i; Type = ty; PositionDirection = pd}
end


module Util =

    let colorWeight(c: Color) =
        (float c.R / 255.0, float c.G / 255.0, float c.B / 255.0)

    let multColorWeight(w: float*float*float, c: Color) =
        let (x, y, z) = w
        let (r ,g, b) = 
            (float c.R * x, float c.G * y, float c.B * z)
        Color.FromArgb(int r,int g, int b)

    let averageColor(colors: List<Vec3>, xySize: int) = 
        let d = xySize*xySize
        
        let squareColor (c:Vec3) = Vec3(c.X*c.X,c.Y*c.Y,c.Z*c.Z)
        let divD (c:Vec3) = Vec3(c.X/float d,c.Y/ float d,c.Z/ float d)

        let listOfSquares = [for i in 1 .. d -> squareColor(colors.[i]) ]
        let sumOfSquares = List.reduce (fun acc e -> e+acc) listOfSquares
        divD(sumOfSquares).Sqrt.AsColor


    let multColor (i: float, c: Color) =
        let (r ,g, b) = 
            (float c.R * i, float c.G * i, float c.B * i)
        Color.FromArgb(int r,int g, int b)

    let addColor (c: Color, c2: Color) =
        let ceil255 x = if int x > 255 then 255 else int x
        
        let (r, g, b) = (c.R + c2.R, c.G+c2.G, c.B+ c2.B)
        
        Color.FromArgb(ceil255 r, ceil255 g, ceil255 b)


module Main =
    open Util
    open Constants

    let O : Vec3 = Vec3(0.0,0.0,1.0)

    let canvasSize : Vec2 = Vec2(800.0, 800.0)
    let viewportSize : Vec2 = Vec2(2.0, 2.0)

    let raysPerPixelXY : int = 4
    let fullCanvasSize = 
        Vec2(
            canvasSize.X * float raysPerPixelXY, 
            canvasSize.Y * float raysPerPixelXY
        )

    let projectionPlaneD = 1.0
    let rayRecursionDepth = 2

    let scene = [
        Sphere(0.0,0.5, 3.15, 1.0, Color.Red, 1000.0, 0.4);
        Sphere(2.0,0.0, 4.0, 1.0, Color.Blue, 500.0, 0.3);
        Sphere(-2.0,0.0, 4.0, 1.0, Color.Green, 10.0, 0.1);
        //Sphere(0.0,-3.0, 5.0, 2.0, Color.FromArgb(240,220,220), 1000.0, 0.65);
        Sphere(0.0,5002.0, 0.0, 5000.0, Color.Yellow, 5000.0, 0.8)
    ]

    let lighting = [
        Light(0.2,"ambient",None)
        Light(0.6,"point", Some(Vec3(2.0, 1.0, 0.0)) )
        Light(0.5,"directional", Some(Vec3(-2.0, -2.0, 0.0)) )
    ]

    let fullCanvasToViewport(canvasXF: float, canvasYF: float) =
        Vec3(
            (viewportSize.X / fullCanvasSize.X) * canvasXF,
            (viewportSize.Y / fullCanvasSize.Y) * canvasYF,
            projectionPlaneD
        )

    let closestIntersection(O:Vec3, D: Vec3, tMin: float, tMax: float) =
        let mutable closestT = Inf
        let mutable closestSphere: Option<Sphere> = None
        //TODO make this functional. Don't wanna

        for sphere in scene do
            let (t1, t2) = sphere.IntersectRaySphere(O, D)

            if t1 > tMin && t1 < tMax && t1 < closestT then
                closestT <- t1
                closestSphere <- Some sphere

            if t2 > tMin && t2 < tMax && t2 < closestT then
                closestT <- t2
                closestSphere <- Some sphere
        
        (closestSphere, closestT)

    let reflectRay(R: Vec3, N: Vec3) =
        2.0*(N.Dot(R))*N - R

    let computeLighting(P: Vec3, N: Vec3, V: Vec3, s: float) =
        let mutable i = 0.0
        let mutable tMax = 1.0

        //TODO functionalize
        
        for light in lighting do

            if light.Type = "ambient" then
                i <- i + light.Intensity
            else
                let L = 
                    if light.Type = "point" then
                        tMax <- 1.0
                        light.PositionDirection.Value - P
                    else
                        tMax <- Inf
                        light.PositionDirection.Value
                //Shadow check
                let (shadowSphere, shadowT) = closestIntersection(P, L, 0.00001, tMax)
                
                if shadowSphere.IsNone then

                    //Diffuse lighting
                    let nDotL = N.Dot(L)
                    if nDotL > 0.0 then
                         i <- i + (light.Intensity* nDotL)/(N.Length*L.Length)
                    
                    //Specular lighting
                    if s <> -1.0 then
                        let R = nDotL*(2.0*N) - L
                        let rDotV = R.Dot(V)
                        if rDotV > 0.0 then
                            let vScale = (rDotV/(R.Length*V.Length)) ** s
                            i <- i + (light.Intensity * vScale)
            
        if i > 1.0 then 1.0 else i

   

    let rec traceRay(O: Vec3, D: Vec3, tMin: float, tMax: float, depth: int) =
        
        let (closestSphere, closestT) =
            closestIntersection(O, D, tMin, tMax)
   
        if closestSphere.IsNone then
            Color.FromArgb(30,30,30)                //Background color
        else

        let sphere = closestSphere.Value
        let p : Vec3= O + (closestT * D)            //Ray/Sphere intersection
        let n : Vec3 = (p - closestSphere.Value.C)  //sphere normal @intersection
        let n1 : Vec3 = n / n.Length                //normalize the normal :D

        let localColor =         
            let lightingI = computeLighting(p,n1, -1.0*D, sphere.Specular)  //find lighting intensity
            multColor(lightingI, sphere.color)

        let reflection = sphere.Reflective

        if depth <=0 || reflection <= 0.0 then
            localColor
        else

        let r = reflectRay(-1.0*D, n)
        let reflectedColor = traceRay(p, r, 0.0001, Inf, depth - 1)

        addColor(
            multColor((1.0 - reflection), localColor),
            multColor(reflection, reflectedColor)
            )

    
    let canvasBitmap = new Bitmap(int canvasSize.X + 1, int canvasSize.Y + 1)
    //use for antialiasing
    let blueNoiseBMP = new Bitmap("raytracer\img\BlueNoise64Tiled.png")

    let renderSubpixel(x:int, y:int, xSub:int, ySub:int) =
        let (xA, yA) = (
            (x* raysPerPixelXY) + xSub, 
            (y*raysPerPixelXY) + ySub
        )

        let (randX, randY) = (
            float <| blueNoiseBMP.GetPixel(xA % blueNoiseBMP.Width, yA % blueNoiseBMP.Height).R,
            float <| blueNoiseBMP.GetPixel((xA+1) % blueNoiseBMP.Width, (yA+1) % blueNoiseBMP.Height).R
        )

        let (jitterX, jitterY) = (
            (randX/255.0) - 0.5,
            (randY/255.0) - 0.5
        )

        let (xT, yT) = (
            jitterX + float xA - (fullCanvasSize.X/2.0), 
            jitterY + float yA - (fullCanvasSize.Y/2.0)
        )

        let d = fullCanvasToViewport(xT, yT)
        
        Vec3(traceRay(O, d, projectionPlaneD , Inf, rayRecursionDepth))

    printfn "Starting Raytrace"    

    let raytraceSceneToBitmap =
        let canvasW = int canvasSize.X
        let canvasH = int canvasSize.Y
        for x in [0 .. canvasW] do
            for y in [0 .. canvasH] do
                let subpixels = 
                    [for xSub in [0 .. raysPerPixelXY] do
                        for ySub in [0 .. raysPerPixelXY] do
                            yield renderSubpixel(x, y, xSub, ySub)]
                if x % (canvasW/4) = 0 && y % (canvasH/4) = 0 then
                    printfn "%d %d" x y   
                canvasBitmap.SetPixel(x, y, averageColor(subpixels, raysPerPixelXY))
                
        canvasBitmap
