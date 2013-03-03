#r "PresentationCore.dll";;
#r "PresentationFramework.dll";;
#r "WindowsBase.dll";;
#r "System.Xaml.dll";;
open System.Windows
open System.Windows.Media
open System
//#time
type config = {transformConfig : ((double list * double) list); iterations : int}
let start = (0.0, 0.0)
let barnsleyfern = {
                     transformConfig = [ ([0.85 ; 0.04 ; -0.04; 0.85; 0.0; 1.6 ], 0.85);
                                         ([0.20 ; -0.26; 0.23 ; 0.22; 0.0; 1.6 ], 0.92);
                                         ([-0.15; 0.28 ; 0.26 ; 0.24; 0.0; 0.44], 0.99);
                                         ([0.0  ; 0.0  ; 0.0  ; 0.16; 0.0; 0.0 ], 1.00); ];
                     iterations = 150000
                   }
let random = System.Random()
let getPts() =
     let mmult (x, y) fn =
                     match fn with
                      [a; b; c; d; e; f] -> (a * x + b * y + e, c * x + d * y + f)
                     | _                 -> (0.0, 0.0)
     let rec pointGen  acc seed n =
         match n with
             0  -> acc
            | _ -> 
                let rnDbl = random.NextDouble()
                let pointGen' ac p = pointGen (p :: ac) p (n-1)
                barnsleyfern.transformConfig 
                |> List.pick( fun ipp -> if rnDbl < (snd ipp) then Some(fst ipp) else None)
                |> mmult seed
                |> pointGen' acc 

     pointGen [] start barnsleyfern.iterations
     |> Array.ofList
     |> Array.map (fun (x, y) -> Media.RectangleGeometry(new Rect(Point(y * 100.0, x * 100.0 + 300.0), new Size(0.08, 0.08)))) 
do
  let group = Media.GeometryGroup()
  getPts() |> Array.iter (fun rectangleGeometry -> group.Children.Add rectangleGeometry |> ignore)
  let brush = Media.SolidColorBrush Media.Colors.ForestGreen
  let path = Shapes.Path(Data=group, Stroke=brush, StrokeThickness=0.3)
  let box = Controls.Viewbox(Child=path, Stretch=Media.Stretch.None)
  let window = Window(Content=box, Title="Chaos Game")
  (Application()).Run window
  |> ignore
