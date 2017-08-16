namespace LiteDB.FSharp
open LiteDB
open System
open FSharp.Reflection

module Mapper =
    let private toString (x:'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let private fromString (t:Type) (s:string) (fields:obj array) =
        match FSharpType.GetUnionCases t |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,fields) :?> 'a)
        |_ -> None

    let instance = { new BsonMapper(customTypeInstantiator=null) with
        override x.Serialize(typeInfo:Type, o:obj, depth:int) =
            if (isNull o) then BsonValue.Null
            else
                match FSharpType.IsUnion typeInfo with
                | true ->
                    let doc = BsonDocument()
                    let (case, fields) = FSharpValue.GetUnionFields(o, typeInfo)
                    doc.RawValue.["_du"] <- BsonValue(case.Name)
                    if (fields.Length > 0)
                    then
                        doc.RawValue.["_duf"] <- x.Serialize(typeof<obj array>, fields, depth+1)
                    doc :> BsonValue
                | false -> base.Serialize(typeInfo, o, depth)

        override x.Deserialize(typeInfo:Type, value:BsonValue) =
            if (isNull value) then null else
            if (typeInfo = typeof<BsonValue>)
            then value :> obj
            else
                match FSharpType.IsUnion typeInfo with
                | true ->
                    let doc = value.AsDocument
                    match doc.TryGetValue "_du" with
                    | (true, s) ->
                        let bsonFields =
                            match doc.TryGetValue "_duf" with
                            | (true, f) -> f.AsArray
                            | (false, _) -> BsonArray(Array.empty<BsonValue>)
                        let fields = x.DeserializeArray(typeof<obj>, bsonFields)
                        let ff = fields |> Seq.cast<obj> |> Seq.toArray
                        let duOption = fromString typeInfo s.AsString ff
                        match duOption with
                        | Some du -> du
                        | None -> raise <| InvalidOperationException "Couldn't create DU instance. Probably serialization wasn't made by LiteDB.FSharp.BsonMapper"
                    | (false, _) -> raise <| InvalidOperationException "Couldn't find '_du' prefix. Probably serialization wasn't made by LiteDB.FSharp.BsonMapper"
                | false -> base.Deserialize(typeInfo, value)

      }