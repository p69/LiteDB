namespace LiteDB.FSharp
open LiteDB
open System
open FSharp.Reflection

module Mapper =
    let toString (x:'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString (t:Type) (s:string) =
        match FSharpType.GetUnionCases t |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None

    let private serialize t entity : BsonDocument =
        let doc = BsonDocument()
        let (case, fields) = FSharpValue.GetUnionFields(entity, t)
        doc.RawValue.["_du"] <- BsonValue(case.Name)
        doc

    let private deserialize t (doc:BsonDocument) : obj =
        match doc.TryGetValue "_du" with
        | (true, s) ->
            let duOption = fromString t s.AsString
            match duOption with
            | Some du -> du
            | None -> raise <| InvalidOperationException "Couldn't create DU instance. Probably serialization wasn't made by LiteDB.FSharp.BsonMapper"
        | (false, _) -> raise <| InvalidOperationException "Couldn't find '_du' prefix. Probably serialization wasn't made by LiteDB.FSharp.BsonMapper"


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