namespace LiteDB.FSharp
open LiteDB
open System
open FSharp.Reflection

module Mapper =
    let private serialize t entity : BsonDocument =
        BsonDocument()

    let private deserialize t doc : obj =
        obj()

    let instance = { new BsonMapper(customTypeInstantiator=null) with
        override x.ToDocument (typeInfo:Type, entity:obj) =
            if (isNull entity) then raise <| ArgumentNullException "entity"
            if (entity :? BsonDocument)
            then entity :?> BsonDocument
            else
                match FSharpType.IsUnion typeInfo with
                | true -> serialize typeInfo entity
                | false -> x.Serialize(typeInfo, entity, 0).AsDocument
        override x.ToObject (typeInfo:Type, doc:BsonDocument) =
            if (isNull doc) then raise <| ArgumentNullException "doc"
            if (typeInfo = typeof<BsonDocument>)
            then doc :> obj
            else
                match FSharpType.IsUnion typeInfo with
                | true -> deserialize typeInfo doc
                | false -> x.Deserialize(typeInfo, doc)

      }