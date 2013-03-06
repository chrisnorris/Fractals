// a small chaos game framework. plays over various polygons chosen at random.
// wpf code taken from book 'F# for Scientists by Jon Harrop' see also http://fsharpnews.blogspot.co.uk/2010/07/lorenz-attractor.html 
#r "PresentationCore.dll";;
#r "PresentationFramework.dll";;
#r "WindowsBase.dll";;
#r "System.Xaml.dll";;
open System.Windows
open System.Windows.Media
open System

type chaosGameConfig = {windingNo : float; r : float; polygon : Point list}
let polygon n =
 let divider = (2.0 * Math.PI / n)
 Seq.unfold (fun s ->
   if s < ((n + 1.0) * divider) then Some (s, divider + s) else None) 0.0
   |> Seq.map(fun angle -> Point(400.0 + 400.0 * cos angle, 400.0 + 400.0 * sin angle))
   |> Seq.skip 1
   |> List.ofSeq

let sierpinskiChaosGames = 
    [ {windingNo = -2.0; r = (1.0 / 2.0 ); polygon = polygon 3.0 };
      {windingNo = -2.0; r = (3.0 / 4.0 ); polygon = polygon 3.0 };
      {windingNo = 3.0;  r = (3.0 / 4.0 ); polygon = polygon 4.0 };
      {windingNo = 4.0;  r = (3.0 / 4.0 ); polygon = polygon 5.0 };
      {windingNo = 5.0;  r = (1.0 / 3.0 ); polygon = polygon 6.0 };
      {windingNo = 7.0;  r = (3.0 / 4.0 ); polygon = polygon 8.0 };
      {windingNo = 15.0; r = (3.0 / 4.0 ); polygon = polygon 16.0}; ]
 
let random = System.Random(System.DateTime.Now.Millisecond)
let randListElem() input = 
   let i = random.Next(0, List.length input) in  input.[i]

let sierpinskiChaosGame = randListElem() sierpinskiChaosGames
let randMult x = x * (random.NextDouble() - 0.5)

let windingNo (polygon : Point list) (p : Point) =
  let getMetric p0 p1 p =
      match  [p0; p1; p] |> List.map (fun (p : Point) -> (p.X, p.Y)) with
      ((x0, y0) :: (x1, y1) :: (x, y) :: []) -> (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)
      | _                                    -> 0.0
  let rec getWinds (poly : Point list) (acc : float list) =
      match poly with (x :: y :: []) -> (getMetric x y p) :: acc
                     |(x :: y :: ys) ->  getWinds (y :: ys) ((getMetric x y p) :: acc)
                     | _             ->  acc
  List.fold (+) 0.0 ((getWinds polygon []) |> List.map (fun x -> if x >= 0.0 then 1.0 else -1.0))

let (|IsInsidePolygon|_|) p = if (windingNo sierpinskiChaosGame.polygon p) = sierpinskiChaosGame.windingNo
                              then Some p else None
let getPts() =
     let rec samplePoint() = 
         let point = Point(400.0 + randMult 800.0, 400.0 + randMult 800.0)
         match point with IsInsidePolygon p -> p | _ -> samplePoint()
  
     let rec chaosGamePoint (seed : Point) n acc =
         match n with
             0  -> acc
            | _ -> 
                let rVertex = randListElem() sierpinskiChaosGame.polygon
                let hOffset = (rVertex.X - seed.X) * sierpinskiChaosGame.r
                let vOffset = (rVertex.Y - seed.Y) * sierpinskiChaosGame.r
                let newPoint = Point(seed.X + hOffset, seed.Y + vOffset)
                chaosGamePoint newPoint (n-1) (newPoint :: acc)
     
     chaosGamePoint (samplePoint()) 50000 []
     |> List.map (fun p -> Media.RectangleGeometry(new Rect(p, new Size(0.02, 0.02)))) 
do
  let group = Media.GeometryGroup()
  getPts() |> List.iter (fun rectangleGeometry -> group.Children.Add rectangleGeometry |> ignore)
  let brush = Media.SolidColorBrush Media.Colors.Black
  let path = Shapes.Path(Data=group, Stroke=brush, StrokeThickness=0.7)
  let box = Controls.Viewbox(Child=path, Stretch=Media.Stretch.None)
  let window = Window(Content=box, Title="Chaos Game")
  (Application()).Run window
  |> ignore
