namespace LiteDB.FSharp
open LiteDB
open System

module Mapper =
    let instance = { new BsonMapper(customTypeInstantiator=null) with
        override x.ToObject (typeInfo:Type, doc:BsonDocument) = "" :> obj
      }