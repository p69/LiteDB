module Tests
open LiteDB
open Expecto

type Shape =
  | Circle of r:float
  | Rectangle of w:float*h:float
  | Point

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
      | _ -> failtest "Should be Circle"
  ]