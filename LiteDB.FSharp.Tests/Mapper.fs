module Tests
open LiteDB
open Expecto

type Shape =
  | Circle of r:float
  | Square of w:float*h:float

[<Tests>]
let tests =
  testList "Mapper" [
    testCase "DU" <| fun _ ->
      let shape = Circle(4.0)
      let mapper = LiteDB.FSharp.Mapper.instance
      let doc = mapper.ToDocument shape
      let no = mapper.ToObject<Shape>(doc)
      match no with
      | Circle r -> Expect.floatClose Accuracy.low r 4.0 "radius"
      | Square (_) -> failtest "Should be Circle"
  ]