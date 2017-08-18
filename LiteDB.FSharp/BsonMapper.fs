namespace LiteDB.FSharp
open LiteDB
open System
open FSharp.Reflection
open System.Reflection

module Mapper =
    let private fromString (t:Type) (s:string) (fields:obj array) =
        match FSharpType.GetUnionCases t |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,fields) :?> 'a)
        |_ -> None

    let serializeUnion (typeInfo:Type) (o:obj) (depth:int) (mapper:BsonMapper) =
        let doc = BsonDocument()
        let (case, fields) = FSharpValue.GetUnionFields(o, typeInfo)
        doc.RawValue.["_du"] <- BsonValue(case.Name)
        doc.RawValue.["_type"] <- BsonValue(typeInfo.FullName + ", " + typeInfo.GetTypeInfo().Assembly.GetName().Name)
        if (fields.Length > 0)
        then
            let bsonArray = BsonArray(Array.empty<BsonValue>)
            fields
            |> Array.iter (fun i -> bsonArray.Add(mapper.Serialize(i.GetType(), i, depth+1)))
            |> ignore
            doc.RawValue.["_duf"] <- bsonArray
        doc :> BsonValue

    let desirializeUnion (typeInfo:Type) (value:BsonValue) (mapper:BsonMapper) =
        let doc = value.AsDocument
        match doc.TryGetValue "_du" with
        | (true, s) ->
            let bsonFields =
                match doc.TryGetValue "_duf" with
                | (true, f) -> f.AsArray
                | (false, _) -> BsonArray(Array.empty<BsonValue>)
            let fields =
                bsonFields
                |> Seq.map (fun i ->
                        let mutable t = typeof<obj>
                        if (i.IsDocument)
                        then
                            match i.AsDocument.TryGetValue "_type" with
                            | (true, typeName) -> t<-Type.GetType(typeName.AsString)
                            | (false, _) ->()
                        mapper.Deserialize(t, i)
                    )
                |> Seq.toArray
            let duOption = fromString typeInfo s.AsString fields
            match duOption with
            | Some du -> du
            | None -> raise <| InvalidOperationException "Couldn't create DU instance. Probably serialization wasn't made by LiteDB.FSharp.BsonMapper"
        | (false, _) -> raise <| InvalidOperationException "Couldn't find '_du' prefix. Probably serialization wasn't made by LiteDB.FSharp.BsonMapper"


    let instance = { new BsonMapper(customTypeInstantiator=null) with
        override x.Serialize(typeInfo:Type, o:obj, depth:int) =
            if (isNull o) then BsonValue.Null
            else
            match FSharpType.IsUnion typeInfo with
            | true -> serializeUnion typeInfo o depth x
            | false -> base.Serialize(typeInfo, o, depth)

        override x.Deserialize(typeInfo:Type, value:BsonValue) =
            if (isNull value) then null else
            if (typeInfo = typeof<BsonValue>)
            then value :> obj
            else
            match FSharpType.IsUnion typeInfo with
            | true -> desirializeUnion typeInfo value x
            | false ->
                if (value.IsDocument && value.AsDocument.ContainsKey("_du"))
                then desirializeUnion typeInfo value x
                else base.Deserialize(typeInfo, value)
      }