// Implement a Penrose tiling using string substitution (Lindenmayer system)
// coupled with a turtle implementation to draw the curve
// challenge is to avoid using the 'mutable' keyword thus keeping a more pure functional style
// some wpf code taken from book 'F# for Scientists by Jon Harrop'.
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "WindowsBase.dll"
#r "System.Xaml.dll"
open System.Windows
open System.Windows.Media
open System
open System.Collections.Generic

type Config = { seed : char [];  rule : Map<Char, string>; angle : float; radius : float }
type Pen<'T> = Up of 'T | Down of 'T
type State  = { point : Pen<Point> ; theta : float }
let toChar (s : string) = [for i in s do yield i] |> Array.ofList
let getPoint (pen : Pen<'a>) = match pen with Up(p) -> p | Down(p) -> p
let (|Forward|Right|Left|Push|Pop|Other|) (c : char) = if (c = 'F') || (c = '1') then Forward
                                                       else match c with '+' -> Right
                                                                       | '-' -> Left
                                                                       | '[' -> Push
                                                                       | ']' -> Pop
                                                                       |  _  -> Other
let penrose = { seed   = toChar "[7]++[7]++[7]++[7]++[7]"
                rule   = [('6', "81++91----71[-81----61]++");
                          ('7', "+81--91[---61--71]+"      );
                          ('8', "-61++71[+++81++91]-"      );
                          ('9', "--81++++61[+91++++71]--71");
                          ('1', ""                         );]
                          |> Map.ofList
                angle  = 36.0
                radius = 15.0 }

let weed =    { seed   = toChar "F"
                rule   = [('F', "F[+F]F[-F][F]")]
                          |> Map.ofList
                angle  = -25.0
                radius = 5.0 }
let bush =    { seed   = toChar "F"
                rule   = [('F', "FF-[-F+F+F]+[+F-F-F]")]
                          |> Map.ofList
                angle = -22.5
                radius = 6.75}

let line_to (pen : Pen<Point>) = Media.LineSegment(getPoint pen, match pen with
                                                                   Up p   -> false
                                                                 | Down p -> true  ) :> Media.PathSegment

let getPath() (config : Config) =
    let rec run acc n =
        match n with
          0 -> acc
        | _ -> let acc' = acc
                       |> Array.fold (fun a elem ->
                                         if config.rule.ContainsKey elem then config.rule.Item elem |> toChar else [|elem|]
                                         |> Array.append a) [||]
               run acc' (n-1)
    let stack = new Stack<State>()
    let rec turtle (moves : char list) (path : (Pen<Point>) []) (state : State) =
        let a = state.theta * Math.PI / 180.0
        let point = getPoint(state.point)
        match moves with
          [] -> path
        | (m :: ms) -> match m with
                       Forward    -> let p' =
                                         let newPoint = Point(point.X + config.radius * cos a,
                                                              point.Y + config.radius * sin a)
                                         match state.point with
                                             Up p   -> (Up newPoint, Down newPoint)
                                           | Down p -> (Down newPoint, Down newPoint)
                                     turtle ms (Array.append path [|fst p'|]) ({state with point = snd p'})
                     | Right      -> turtle ms (path) {state with theta = state.theta - config.angle}
                     | Left       -> turtle ms (path) {state with theta = state.theta + config.angle}
                     | Push       -> stack.Push state
                                     turtle ms (path) state
                     | Pop        -> let state' = stack.Pop()
                                     turtle ms (path) {state' with point = Up(getPoint state'.point)}
                     | Other      -> turtle ms (path) state
    
    let pts = turtle (run config.seed 5 |> List.ofArray) [||] {point = Down(Point(100.0, 250.0)); theta = 0.0}
    let origin = pts.[0] |> getPoint
    Media.PathGeometry[Media.PathFigure(origin, Array.map line_to pts, false)]
do
  let group = Media.GeometryGroup()
  getPath () penrose |> group.Children.Add |> ignore
  let path = Shapes.Path(Data=group, Stroke=Media.SolidColorBrush Media.Colors.Black, StrokeThickness=0.2)
  let box = Controls.Viewbox(Child=path, Stretch=Media.Stretch.None)
  let window = Window(Content=box, Title="Penrose P3 tiling")
  (Application()).Run window
  |> ignore