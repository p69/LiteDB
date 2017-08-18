module Tests
open LiteDB
open Expecto

type Shape =
  | Circle of r:float
  | Rectangle of w:float*h:float
  | Point

type Single = A

type CompositeDU =
  | CA of s:Shape*name:string
  | CB of Single

[<Tests>]
let tests =
  testList "Shapes" [
    testCase "Circle" <| fun _ ->
      let shape = Circle(4.0)
      let mapper = LiteDB.FSharp.Mapper.instance
      let doc = mapper.ToDocument shape
      let no = mapper.ToObject<Shape>(doc)
      match no with
      | Circle r -> Expect.floatClose Accuracy.low r 4.0 "radius"
      | _ -> failtest "Should be Circle"

    testCase "Square" <| fun _ ->
      let shape = Rectangle(2.0,3.0)
      let mapper = LiteDB.FSharp.Mapper.instance
      let doc = mapper.ToDocument shape
      let no = mapper.ToObject<Shape>(doc)
      match no with
      | Rectangle (w,h) ->
          Expect.floatClose Accuracy.low w 2.0 "width"
          Expect.floatClose Accuracy.low h 3.0 "height"
      | _ -> failtest "Should be Rectangle"

    testCase "Point" <| fun _ ->
      let shape = Point
      let mapper = LiteDB.FSharp.Mapper.instance
      let doc = mapper.ToDocument shape
      let no = mapper.ToObject<Shape>(doc)
      match no with
      | Point -> Expect.isTrue true ""
      | _ -> failtest "Should be Point"

    testCase "Single" <| fun _ ->
      let mapper = LiteDB.FSharp.Mapper.instance
      let doc = mapper.ToDocument A
      let no = mapper.ToObject<Single>(doc)
      match no with
      | A -> Expect.isTrue true ""

    testCase "Composite" <| fun _ ->
      let mapper = LiteDB.FSharp.Mapper.instance
      let ca = CA(Shape.Circle(2.0), "circle")
      let doc = mapper.ToDocument ca
      let no = mapper.ToObject<CompositeDU>(doc)
      match no with
      | CA (s,n) ->
          Expect.equal n "circle" "name"
          match s with
          | Circle r -> Expect.floatClose Accuracy.low r 2.0 "radius"
          | _ -> failtest "Should be Circle"
      | _ -> failtest "Should be CA"
  ]