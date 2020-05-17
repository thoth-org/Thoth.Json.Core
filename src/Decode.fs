
namespace Thoth.Json

[<RequireQualifiedAccess>]
module Decode =

    module Helpers =

        let prependPath (path: string) (err: DecoderError<'JsonValue>): DecoderError<'JsonValue> =
            let (oldPath, reason) = err
            (path + oldPath, reason)

        let inline prependPathToResult<'JsonValue, 'T> (path: string) (res: Result<'T, DecoderError<'JsonValue>>) =
            res |> Result.mapError(prependPath path)

    let private genericMsg (helpers : IDecoderHelpers<'JsonValue>) msg value newLine =
        try
            "Expecting "
                + msg
                + " but instead got:"
                + (if newLine then "\n" else " ")
                + (helpers.AnyToString value)
        with
            | _ ->
                "Expecting "
                + msg
                + " but decoder failed. Couldn't report given value due to circular structure."
                + (if newLine then "\n" else " ")

    let errorToString (helpers : IDecoderHelpers<'JsonValue>) (path : string, error) =
        let reason =
            match error with
            | BadPrimitive (msg, value) ->
                genericMsg helpers msg value false
            | BadType (msg, value) ->
                genericMsg helpers msg value true
            | BadPrimitiveExtra (msg, value, reason) ->
                genericMsg helpers msg value false + "\nReason: " + reason
            | BadField (msg, value) ->
                genericMsg helpers msg value true
            | BadPath (msg, value, fieldName) ->
                genericMsg helpers msg value true + ("\nNode `" + fieldName + "` is unknown.")
            | TooSmallArray (msg, value) ->
                "Expecting " + msg + ".\n" + (helpers.AnyToString value)
            | BadOneOf messages ->
                "The following errors were found:\n\n" + String.concat "\n\n" messages
            | FailMessage msg ->
                "The following `failure` occurred with the decoder: " + msg

        match error with
        | BadOneOf _ ->
            // Don't need to show the path here because each error case will show it's own path
            reason
        | _ ->
            "Error at: `" + path + "`\n" + reason

    ///////////////
    // Runners ///
    /////////////

    // let fromValue (helpers : IDecoderHelpers<'JsonValue>) (decoder : Decoder<'JsonValue, 'T>) =
    //     fun value ->
    //         match decoder helpers value with
    //         | Ok success ->
    //             Ok success
    //         | Error error ->
    //             Error (errorToString helpers error)

    // let fromString (helpers : IDecoderHelpers<'JsonValue>) (decoder : Decoder<'JsonValue, 'T>) =
    //     fun value ->
    //         try
    //             let json = JS.JSON.parse value
    //             match decoder helpers json with
    //             | Ok success -> Ok success
    //             | Error error ->
    //                 let finalError = error |> helpers.PrependPath "$"
    //                 Error (errorToString helpers finalError)
    //         with
    //             | ex when helpers.IsSyntaxError ex ->
    //                 Error("Given an invalid JSON: " + ex.Message)

    // let unsafeFromString (helpers : IDecoderHelpers<'JsonValue>) (decoder : Decoder<'JsonValue, 'T>) =
    //     fun value ->
    //         match fromString helpers decoder value with
    //         | Ok x -> x
    //         | Error msg -> failwith msg

    //////////////////
    // Primitives ///
    ////////////////

    let string : Decoder<'JsonValue, string> =
        fun helpers value ->
            if helpers.IsString value then
                Ok(helpers.AsString value)
            else
                ("", BadPrimitive("a string", value)) |> Error

    let guid : Decoder<'JsonValue, System.Guid> =
        fun helpers value ->
            if helpers.IsString value then
                match System.Guid.TryParse (helpers.AsString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("a guid", value)) |> Error
            else ("", BadPrimitive("a guid", value)) |> Error

    let unit : Decoder<'JsonValue, unit> =
        fun helpers value ->
            if helpers.IsNullValue value then
                Ok ()
            else
                ("", BadPrimitive("null", value)) |> Error

    let inline private integral
                    (name : string)
                    (tryParse : (string -> bool * 'T))
                    (min : 'T)
                    (max : 'T)
                    (conv : float -> 'T) : Decoder<'JsonValue, 'T > =

        fun helpers value ->
            if helpers.IsNumber value then
                if helpers.IsIntegralValue value then
                    let fValue = helpers.AsFloat value
                    if (float min) <= fValue && fValue <= (float max) then
                        Ok(conv fValue)
                    else
                        ("", BadPrimitiveExtra(name, value, "Value was either too large or too small for " + name)) |> Error
                else
                    ("", BadPrimitiveExtra(name, value, "Value is not an integral value")) |> Error
            elif helpers.IsString value then
                match tryParse (helpers.AsString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive(name, value)) |> Error
            else
                ("", BadPrimitive(name, value)) |> Error

    let sbyte<'JsonValue> : Decoder<'JsonValue, sbyte> =
        integral
            "a sbyte"
            System.SByte.TryParse
            System.SByte.MinValue
            System.SByte.MaxValue
            sbyte

    /// Alias to Decode.uint8
    let byte<'JsonValue> : Decoder<'JsonValue, byte> =
        integral
            "a byte"
            System.Byte.TryParse
            System.Byte.MinValue
            System.Byte.MaxValue
            byte

    let int16<'JsonValue> : Decoder<'JsonValue, int16> =
        integral
            "an int16"
            System.Int16.TryParse
            System.Int16.MinValue
            System.Int16.MaxValue
            int16

    let uint16<'JsonValue> : Decoder<'JsonValue, uint16> =
        integral
            "an uint16"
            System.UInt16.TryParse
            System.UInt16.MinValue
            System.UInt16.MaxValue
            uint16

    let int<'JsonValue> : Decoder<'JsonValue, int> =
        integral
            "an int"
            System.Int32.TryParse
            System.Int32.MinValue
            System.Int32.MaxValue
            int

    let uint32<'JsonValue> : Decoder<'JsonValue, uint32> =
        integral
            "an uint32"
            System.UInt32.TryParse
            System.UInt32.MinValue
            System.UInt32.MaxValue
            uint32

    let int64<'JsonValue> : Decoder<'JsonValue, int64> =
        integral
            "an int64"
            System.Int64.TryParse
            System.Int64.MinValue
            System.Int64.MaxValue
            int64

    let uint64<'JsonValue> : Decoder<'JsonValue, uint64> =
        integral
            "an uint64"
            System.UInt64.TryParse
            System.UInt64.MinValue
            System.UInt64.MaxValue
            uint64

    let bigint : Decoder<'JsonValue, bigint> =
        fun helpers value ->
            if helpers.IsNumber value then
                helpers.AsInt value |> bigint |> Ok
            elif helpers.IsString value then
                // TODO: BigInt.TryParse has been added in Fable 2.1.4
                // Don't use it for now to support lower Fable versions
                try
                    bigint.Parse (helpers.AsString value) |> Ok
                with _ ->
                    ("", BadPrimitive("a bigint", value)) |> Error
            else
                ("", BadPrimitive("a bigint", value)) |> Error

    let bool : Decoder<'JsonValue, bool> =
        fun helpers value ->
            if helpers.IsBoolean value then
                Ok(helpers.AsBool value)
            else
                ("", BadPrimitive("a boolean", value)) |> Error

    let float : Decoder<'JsonValue, float> =
        fun helpers value ->
            if helpers.IsNumber value then
                Ok(helpers.AsFloat value)
            else
                ("", BadPrimitive("a float", value)) |> Error

    let float32 : Decoder<'JsonValue, float32> =
        fun helpers value ->
            if helpers.IsNumber value then
                Ok(helpers.AsFloat32 value)
            else
                ("", BadPrimitive("a float32", value)) |> Error

    let decimal : Decoder<'JsonValue, decimal> =
        fun helpers value ->
            if helpers.IsNumber value then
                helpers.AsFloat value |> decimal |> Ok
            elif helpers.IsString value then
                match System.Decimal.TryParse (helpers.AsString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("a decimal", value)) |> Error
            else
                ("", BadPrimitive("a decimal", value)) |> Error

    let datetime : Decoder<'JsonValue, System.DateTime> =
        fun helpers value ->
            if helpers.IsString value then
                match System.DateTime.TryParse (helpers.AsString value) with
                | true, x -> x.ToUniversalTime() |> Ok
                | _ -> ("", BadPrimitive("a datetime", value)) |> Error
            else
                ("", BadPrimitive("a datetime", value)) |> Error

    let datetimeOffset : Decoder<'JsonValue, System.DateTimeOffset> =
        fun helpers value ->
            if helpers.IsString value then
                match System.DateTimeOffset.TryParse(helpers.AsString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("a datetimeoffset", value)) |> Error
            else
                ("", BadPrimitive("a datetime", value)) |> Error

    let timespan : Decoder<'JsonValue, System.TimeSpan> =
        fun helpers value ->
            if helpers.IsString value then
                match System.TimeSpan.TryParse(helpers.AsString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("a timespan", value)) |> Error
            else
                ("", BadPrimitive("a timespan", value)) |> Error

    /////////////////////////
    // Object primitives ///
    ///////////////////////

    // let private decodeMaybeNull path (decoder : Decoder<'JsonValue, 'T>) value =
    //     // The decoder may be an option decoder so give it an opportunity to check null values
    //     match decoder path value with
    //     | Ok v -> Ok(Some v)
    //     | Error _ when helpers.IsNullValue value -> Ok None
    //     | Error er -> Error er

    // let optional (fieldName : string) (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'value option> =
    //     fun helpers value ->
    //         if helpers.IsObject value then
    //             let fieldValue = helpers.GetField fieldName value
    //             if helpers.IsUndefined fieldValue then Ok None
    //             else decodeMaybeNull (path + "." + fieldName) decoder fieldValue
    //         else
    //             Error(path, BadType("an object", value))

    // let private badPathError fieldNames currentPath value =
    //     let currentPath = defaultArg currentPath ("$"::fieldNames |> String.concat ".")
    //     let msg = "an object with path `" + (String.concat "." fieldNames) + "`"
    //     Error(currentPath, BadPath (msg, value, List.tryLast fieldNames |> Option.defaultValue ""))

    // let optionalAt (fieldNames : string list) (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'value option> =
    //     fun firstPath firstValue ->
    //         ((firstPath, firstValue, None), fieldNames)
    //         ||> List.fold (fun (curPath, curValue, res) field ->
    //             match res with
    //             | Some _ -> curPath, curValue, res
    //             | None ->
    //                 if helpers.IsNullValue curValue then
    //                     let res = badPathError fieldNames (Some curPath) firstValue
    //                     curPath, curValue, Some res
    //                 elif helpers.IsObject curValue then
    //                     let curValue = helpers.GetField field curValue
    //                     curPath + "." + field, curValue, None
    //                 else
    //                     let res = Error(curPath, BadType("an object", curValue))
    //                     curPath, curValue, Some res)
    //         |> function
    //             | _, _, Some res -> res
    //             | lastPath, lastValue, None ->
    //                 if helpers.IsUndefined lastValue then Ok None
    //                 else decodeMaybeNull lastPath decoder lastValue

    let field (fieldName: string) (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'value> =
        fun helpers value ->
            if helpers.IsObject value then
                let fieldValue = helpers.GetField fieldName value
                if helpers.IsUndefined fieldValue then
                    Error("", BadField ("an object with a field named `" + fieldName + "`", value))
                else
                    match decoder helpers fieldValue with
                    | Ok _ as ok -> ok
                    | Error er ->
                        Error(er |> Helpers.prependPath ("." + fieldName))
            else
                Error("", BadType("an object", value))

    // let at (fieldNames: string list) (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'value> =
    //     fun firstPath firstValue ->
    //         ((firstPath, firstValue, None), fieldNames)
    //         ||> List.fold (fun (curPath, curValue, res) field ->
    //             match res with
    //             | Some _ -> curPath, curValue, res
    //             | None ->
    //                 if helpers.IsNullValue curValue then
    //                     let res = badPathError fieldNames (Some curPath) firstValue
    //                     curPath, curValue, Some res
    //                 elif helpers.IsObject curValue then
    //                     let curValue = helpers.GetField field curValue
    //                     if helpers.IsUndefined curValue then
    //                         let res = badPathError fieldNames None firstValue
    //                         curPath, curValue, Some res
    //                     else
    //                         curPath + "." + field, curValue, None
    //                 else
    //                     let res = Error(curPath, BadType("an object", curValue))
    //                     curPath, curValue, Some res)
    //         |> function
    //             | _, _, Some res -> res
    //             | lastPath, lastValue, None ->
    //                 decoder lastPath lastValue

    let index (requestedIndex: int) (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'value> =
        fun helpers value ->
            if helpers.IsArray value then
                let vArray = helpers.AsArray value
                if requestedIndex < vArray.Length then
                    decoder helpers (vArray.[requestedIndex])
                    |> Helpers.prependPathToResult (".[" + (Operators.string requestedIndex) + "]")
                else
                    let msg =
                        "a longer array. Need index `"
                            + (requestedIndex.ToString())
                            + "` but there are only `"
                            + (vArray.Length.ToString())
                            + "` entries"

                    ("", TooSmallArray(msg, value))
                    |> Error
                    |> Helpers.prependPathToResult (".[" + (Operators.string requestedIndex) + "]")
            else
                ("", BadPrimitive("an array", value))
                |> Error
                |> Helpers.prependPathToResult (".[" + (Operators.string requestedIndex) + "]")

    // let option (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'value option> =
    //     fun path value ->
    //         if helpers.IsNullValue value then Ok None
    //         else decoder path value |> Result.map Some

    //////////////////////
    // Data structure ///
    ////////////////////

    // let private arrayWith expectedMsg (mapping: 'value[] -> 'result) (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'result> =
    //     fun path value ->
    //         if helpers.IsArray value then
    //             let mutable i = -1
    //             let tokens = helpers.AsArray value
    //             let arr = Array.zeroCreate tokens.Length
    //             (Ok arr, tokens) ||> Array.fold (fun acc value ->
    //                 i <- i + 1
    //                 match acc with
    //                 | Error _ -> acc
    //                 | Ok acc ->
    //                     match decoder (path + ".[" + (i.ToString()) + "]") value with
    //                     | Error er -> Error er
    //                     | Ok value -> acc.[i] <- value; Ok acc)
    //             |> Result.map mapping
    //         else
    //             (path, BadPrimitive (expectedMsg, value))
    //             |> Error

    // let list (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'value list> =
    //     arrayWith "a list" List.ofArray decoder

    // let seq (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'value seq> =
    //     arrayWith "a seq" Seq.ofArray decoder

    // let array (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, 'value array> =
    //     arrayWith "an array" id decoder

    // let keys: Decoder<'JsonValue, string list> =
    //     fun path value ->
    //         if helpers.IsObject value then
    //             helpers.ObjectKeys value |> List.ofSeq |> Ok
    //         else
    //             (path, BadPrimitive ("an object", value))
    //             |> Error

    // let keyValuePairs (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, (string * 'value) list> =
    //     fun path value ->
    //         match keys path value with
    //         | Ok objectKeys ->
    //             (Ok [], objectKeys) ||> List.fold (fun acc prop ->
    //                 match acc with
    //                 | Error _ -> acc
    //                 | Ok acc ->
    //                     match helpers.GetField prop value |> decoder path with
    //                     | Error er -> Error er
    //                     | Ok value -> (prop, value)::acc |> Ok)
    //             |> Result.map List.rev
    //         | Error e -> Error e

    //////////////////////////////
    // Inconsistent Structure ///
    ////////////////////////////

    /// <summary>
    /// Internal implementation of fromValue for <c>oneOf</c> usage.
    /// <para>
    /// This is not the <c>fromValue</c> exposed to the end user
    /// </para>
    /// </summary>
    /// <param name="helpers"></param>
    /// <param name="decoder"></param>
    /// <param name="value"></param>
    /// <typeparam name="'JsonValue"></typeparam>
    /// <typeparam name="'T"></typeparam>
    /// <returns></returns>
    let internal fromValue (helpers : IDecoderHelpers<'JsonValue>) (decoder : Decoder<'JsonValue, 'T>) =
        fun value ->
            match decoder helpers value with
            | Ok success ->
                Ok success
            | Error error ->
                Error (errorToString helpers error)


    let oneOf (decoders : Decoder<'JsonValue, 'value> list) : Decoder<'JsonValue, 'value> =
        fun helpers value ->
            let rec runner (decoders : Decoder<'JsonValue, 'value> list) (errors : string list) =
                match decoders with
                | head::tail ->
                    match fromValue helpers head value with
                    | Ok v ->
                        Ok v
                    | Error error -> runner tail (errors @ [error])
                | [] -> ("", BadOneOf errors) |> Error

            runner decoders []

    //////////////////////
    // Fancy decoding ///
    ////////////////////

    let nil (output : 'a) : Decoder<'JsonValue, 'a> =
        fun helpers value ->
            if helpers.IsNullValue value then
                Ok output
            else
                ("", BadPrimitive("null", value)) |> Error

    let value _ v = Ok v

    let succeed (output : 'a) : Decoder<'JsonValue, 'a> =
        fun _ _ ->
            Ok output

    let fail (msg: string) : Decoder<'JsonValue, 'a> =
        fun _ _ ->
            ("", FailMessage msg) |> Error

    let andThen (cb: 'a -> Decoder<'JsonValue, 'b>) (decoder : Decoder<'JsonValue, 'a>) : Decoder<'JsonValue, 'b> =
        fun path value ->
            match decoder path value with
            | Error error -> Error error
            | Ok result -> cb result path value

    let all (decoders: Decoder<'JsonValue, 'a> list): Decoder<'JsonValue, 'a list> =
        fun path value ->
            let rec runner (decoders: Decoder<'JsonValue, 'a> list) (values: 'a list) =
                match decoders with
                | decoder :: tail ->
                    match decoder path value with
                    | Ok value -> runner tail (values @ [ value ])
                    | Error error -> Error error
                | [] -> Ok values

            runner decoders []

    /////////////////////
    // Map functions ///
    ///////////////////

    let map
        (ctor : 'a -> 'value)
        (d1 : Decoder<'JsonValue, 'a>) : Decoder<'JsonValue, 'value> =
        fun path value ->
            match d1 path value with
            | Ok v1 -> Ok (ctor v1)
            | Error er -> Error er

    let map2
        (ctor : 'a -> 'b -> 'value)
        (d1 : Decoder<'JsonValue, 'a>)
        (d2 : Decoder<'JsonValue, 'b>) : Decoder<'JsonValue, 'value> =
        fun path value ->
            match d1 path value, d2 path value with
            | Ok v1, Ok v2 -> Ok (ctor v1 v2)
            | Error er,_ -> Error er
            | _,Error er -> Error er

    let map3
        (ctor : 'a -> 'b -> 'c -> 'value)
        (d1 : Decoder<'JsonValue, 'a>)
        (d2 : Decoder<'JsonValue, 'b>)
        (d3 : Decoder<'JsonValue, 'c>) : Decoder<'JsonValue, 'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value with
            | Ok v1, Ok v2, Ok v3 -> Ok (ctor v1 v2 v3)
            | Error er,_,_ -> Error er
            | _,Error er,_ -> Error er
            | _,_,Error er -> Error er

    let map4
        (ctor : 'a -> 'b -> 'c -> 'd -> 'value)
        (d1 : Decoder<'JsonValue, 'a>)
        (d2 : Decoder<'JsonValue, 'b>)
        (d3 : Decoder<'JsonValue, 'c>)
        (d4 : Decoder<'JsonValue, 'd>) : Decoder<'JsonValue, 'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4 -> Ok (ctor v1 v2 v3 v4)
            | Error er,_,_,_ -> Error er
            | _,Error er,_,_ -> Error er
            | _,_,Error er,_ -> Error er
            | _,_,_,Error er -> Error er

    let map5
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'value)
        (d1 : Decoder<'JsonValue, 'a>)
        (d2 : Decoder<'JsonValue, 'b>)
        (d3 : Decoder<'JsonValue, 'c>)
        (d4 : Decoder<'JsonValue, 'd>)
        (d5 : Decoder<'JsonValue, 'e>) : Decoder<'JsonValue, 'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5 -> Ok (ctor v1 v2 v3 v4 v5)
            | Error er,_,_,_,_ -> Error er
            | _,Error er,_,_,_ -> Error er
            | _,_,Error er,_,_ -> Error er
            | _,_,_,Error er,_ -> Error er
            | _,_,_,_,Error er -> Error er

    let map6
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value)
        (d1 : Decoder<'JsonValue, 'a>)
        (d2 : Decoder<'JsonValue, 'b>)
        (d3 : Decoder<'JsonValue, 'c>)
        (d4 : Decoder<'JsonValue, 'd>)
        (d5 : Decoder<'JsonValue, 'e>)
        (d6 : Decoder<'JsonValue, 'f>) : Decoder<'JsonValue, 'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value, d6 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6 -> Ok (ctor v1 v2 v3 v4 v5 v6)
            | Error er,_,_,_,_,_ -> Error er
            | _,Error er,_,_,_,_ -> Error er
            | _,_,Error er,_,_,_ -> Error er
            | _,_,_,Error er,_,_ -> Error er
            | _,_,_,_,Error er,_ -> Error er
            | _,_,_,_,_,Error er -> Error er

    let map7
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value)
        (d1 : Decoder<'JsonValue, 'a>)
        (d2 : Decoder<'JsonValue, 'b>)
        (d3 : Decoder<'JsonValue, 'c>)
        (d4 : Decoder<'JsonValue, 'd>)
        (d5 : Decoder<'JsonValue, 'e>)
        (d6 : Decoder<'JsonValue, 'f>)
        (d7 : Decoder<'JsonValue, 'g>) : Decoder<'JsonValue, 'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value, d6 path value, d7 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6, Ok v7 -> Ok (ctor v1 v2 v3 v4 v5 v6 v7)
            | Error er,_,_,_,_,_,_ -> Error er
            | _,Error er,_,_,_,_,_ -> Error er
            | _,_,Error er,_,_,_,_ -> Error er
            | _,_,_,Error er,_,_,_ -> Error er
            | _,_,_,_,Error er,_,_ -> Error er
            | _,_,_,_,_,Error er,_ -> Error er
            | _,_,_,_,_,_,Error er -> Error er

    let map8
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value)
        (d1 : Decoder<'JsonValue, 'a>)
        (d2 : Decoder<'JsonValue, 'b>)
        (d3 : Decoder<'JsonValue, 'c>)
        (d4 : Decoder<'JsonValue, 'd>)
        (d5 : Decoder<'JsonValue, 'e>)
        (d6 : Decoder<'JsonValue, 'f>)
        (d7 : Decoder<'JsonValue, 'g>)
        (d8 : Decoder<'JsonValue, 'h>) : Decoder<'JsonValue, 'value> =
        fun path value ->
            match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value, d6 path value, d7 path value, d8 path value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6, Ok v7, Ok v8 -> Ok (ctor v1 v2 v3 v4 v5 v6 v7 v8)
            | Error er,_,_,_,_,_,_,_ -> Error er
            | _,Error er,_,_,_,_,_,_ -> Error er
            | _,_,Error er,_,_,_,_,_ -> Error er
            | _,_,_,Error er,_,_,_,_ -> Error er
            | _,_,_,_,Error er,_,_,_ -> Error er
            | _,_,_,_,_,Error er,_,_ -> Error er
            | _,_,_,_,_,_,Error er,_ -> Error er
            | _,_,_,_,_,_,_,Error er -> Error er

    // let dict (decoder : Decoder<'JsonValue, 'value>) : Decoder<'JsonValue, Map<string, 'value>> =
    //     map Map.ofList (keyValuePairs decoder)

    //////////////////////
    // Object builder ///
    ////////////////////

    // type IRequiredGetter =
    //     abstract Field : string -> Decoder<'a> -> 'a
    //     abstract At : List<string> -> Decoder<'a> -> 'a
    //     abstract Raw : Decoder<'a> -> 'a

    // type IOptionalGetter =
    //     abstract Field : string -> Decoder<'a> -> 'a option
    //     abstract At : List<string> -> Decoder<'a> -> 'a option
    //     abstract Raw : Decoder<'a> -> 'a option

    // type IGetters =
    //     abstract Required: IRequiredGetter
    //     abstract Optional: IOptionalGetter

    // let private unwrapWith (errors: ResizeArray<DecoderError>) path (decoder: Decoder<'JsonValue, 'T>) value: 'T =
    //     match decoder path value with
    //     | Ok v -> v
    //     | Error er -> errors.Add(er); Unchecked.defaultof<'T>

    // type Getters<'T>(path: string, v: 'T) =
    //     let mutable errors = ResizeArray<DecoderError>()
    //     let required =
    //         { new IRequiredGetter with
    //             member __.Field (fieldName : string) (decoder : Decoder<_>) =
    //                 unwrapWith errors path (field fieldName decoder) v
    //             member __.At (fieldNames : string list) (decoder : Decoder<_>) =
    //                 unwrapWith errors path (at fieldNames decoder) v
    //             member __.Raw (decoder: Decoder<_>) =
    //                 unwrapWith errors path decoder v }
    //     let optional =
    //         { new IOptionalGetter with
    //             member __.Field (fieldName : string) (decoder : Decoder<_>) =
    //                 unwrapWith errors path (optional fieldName decoder) v
    //             member __.At (fieldNames : string list) (decoder : Decoder<_>) =
    //                 unwrapWith errors path (optionalAt fieldNames decoder) v
    //             member __.Raw (decoder: Decoder<_>) =
    //                 match decoder path v with
    //                 | Ok v -> Some v
    //                 | Error((_, reason) as error) ->
    //                     match reason with
    //                     | BadPrimitive(_,v)
    //                     | BadPrimitiveExtra(_,v,_)
    //                     | BadType(_,v) ->
    //                         if helpers.IsNullValue v then None
    //                         else errors.Add(error); Unchecked.defaultof<_>
    //                     | BadField _
    //                     | BadPath _ -> None
    //                     | TooSmallArray _
    //                     | FailMessage _
    //                     | BadOneOf _ -> errors.Add(error); Unchecked.defaultof<_> }
    //     member __.Errors: _ list = Seq.toList errors
    //     interface IGetters with
    //         member __.Required = required
    //         member __.Optional = optional

    // let object (builder: IGetters -> 'value) : Decoder<'value> =
    //     fun path v ->
    //         let getters = Getters(path, v)
    //         let result = builder getters
    //         match getters.Errors with
    //         | [] -> Ok result
    //         | fst::_ as errors ->
    //             if errors.Length > 1 then
    //                 let errors = List.map errorToString errors
    //                 (path, BadOneOf errors) |> Error
    //             else
    //                 Error fst

    ///////////////////////
    // Tuples decoders ///
    ////////////////////

    let tuple2 (decoder1: Decoder<'JsonValue, 'T1>) (decoder2: Decoder<'JsonValue, 'T2>) : Decoder<'JsonValue, 'T1 * 'T2> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                succeed (v1, v2)
            )
        )

    let tuple3 (decoder1: Decoder<'JsonValue, 'T1>)
               (decoder2: Decoder<'JsonValue, 'T2>)
               (decoder3: Decoder<'JsonValue, 'T3>) : Decoder<'JsonValue, 'T1 * 'T2 * 'T3> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    succeed (v1, v2, v3)
                )
            )
        )

    let tuple4 (decoder1: Decoder<'JsonValue, 'T1>)
               (decoder2: Decoder<'JsonValue, 'T2>)
               (decoder3: Decoder<'JsonValue, 'T3>)
               (decoder4: Decoder<'JsonValue, 'T4>) : Decoder<'JsonValue, 'T1 * 'T2 * 'T3 * 'T4> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        succeed (v1, v2, v3, v4)
                    )
                )
            )
        )

    let tuple5 (decoder1: Decoder<'JsonValue, 'T1>)
               (decoder2: Decoder<'JsonValue, 'T2>)
               (decoder3: Decoder<'JsonValue, 'T3>)
               (decoder4: Decoder<'JsonValue, 'T4>)
               (decoder5: Decoder<'JsonValue, 'T5>) : Decoder<'JsonValue, 'T1 * 'T2 * 'T3 * 'T4 * 'T5> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            succeed (v1, v2, v3, v4, v5)
                        )
                    )
                )
            )
        )

    let tuple6 (decoder1: Decoder<'JsonValue, 'T1>)
               (decoder2: Decoder<'JsonValue, 'T2>)
               (decoder3: Decoder<'JsonValue, 'T3>)
               (decoder4: Decoder<'JsonValue, 'T4>)
               (decoder5: Decoder<'JsonValue, 'T5>)
               (decoder6: Decoder<'JsonValue, 'T6>) : Decoder<'JsonValue, 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                succeed (v1, v2, v3, v4, v5, v6)
                            )
                        )
                    )
                )
            )
        )

    let tuple7 (decoder1: Decoder<'JsonValue, 'T1>)
               (decoder2: Decoder<'JsonValue, 'T2>)
               (decoder3: Decoder<'JsonValue, 'T3>)
               (decoder4: Decoder<'JsonValue, 'T4>)
               (decoder5: Decoder<'JsonValue, 'T5>)
               (decoder6: Decoder<'JsonValue, 'T6>)
               (decoder7: Decoder<'JsonValue, 'T7>) : Decoder<'JsonValue, 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                index 6 decoder7
                                |> andThen (fun v7 ->
                                    succeed (v1, v2, v3, v4, v5, v6, v7)
                                )
                            )
                        )
                    )
                )
            )
        )

    let tuple8 (decoder1: Decoder<'JsonValue, 'T1>)
               (decoder2: Decoder<'JsonValue, 'T2>)
               (decoder3: Decoder<'JsonValue, 'T3>)
               (decoder4: Decoder<'JsonValue, 'T4>)
               (decoder5: Decoder<'JsonValue, 'T5>)
               (decoder6: Decoder<'JsonValue, 'T6>)
               (decoder7: Decoder<'JsonValue, 'T7>)
               (decoder8: Decoder<'JsonValue, 'T8>) : Decoder<'JsonValue, 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                index 6 decoder7
                                |> andThen (fun v7 ->
                                    index 7 decoder8
                                    |> andThen (fun v8 ->
                                        succeed (v1, v2, v3, v4, v5, v6, v7, v8)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )

    ////////////
    // Enum ///
    /////////

    #if !FABLE_REPL_LIB
    module Enum =

        #if FABLE_COMPILER
        let inline byte<'JsonValue, 'TEnum when 'TEnum : enum<byte>> : Decoder<'JsonValue, 'TEnum> =
        #else
        let byte<'JsonValue, 'TEnum when 'TEnum : enum<byte>> : Decoder<'JsonValue, 'TEnum> =
        #endif
            byte
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<byte, 'TEnum> value
                |> succeed
            )

        #if FABLE_COMPILER
        let inline sbyte<'JsonValue, 'TEnum when 'TEnum : enum<sbyte>> : Decoder<'JsonValue, 'TEnum> =
        #else
        let sbyte<'JsonValue, 'TEnum when 'TEnum : enum<sbyte>> : Decoder<'JsonValue, 'TEnum> =
        #endif
            sbyte
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<sbyte, 'TEnum> value
                |> succeed
            )

        #if FABLE_COMPILER
        let inline int16<'JsonValue, 'TEnum when 'TEnum : enum<int16>> : Decoder<'JsonValue, 'TEnum> =
        #else
        let int16<'JsonValue, 'TEnum when 'TEnum : enum<int16>> : Decoder<'JsonValue, 'TEnum> =
        #endif
            int16
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<int16, 'TEnum> value
                |> succeed
            )

        #if FABLE_COMPILER
        let inline uint16<'JsonValue, 'TEnum when 'TEnum : enum<uint16>> : Decoder<'JsonValue, 'TEnum> =
        #else
        let uint16<'JsonValue, 'TEnum when 'TEnum : enum<uint16>> : Decoder<'JsonValue, 'TEnum> =
        #endif
            uint16
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<uint16, 'TEnum> value
                |> succeed
            )

        #if FABLE_COMPILER
        let inline int<'JsonValue, 'TEnum when 'TEnum : enum<int>> : Decoder<'JsonValue, 'TEnum> =
        #else
        let int<'JsonValue, 'TEnum when 'TEnum : enum<int>> : Decoder<'JsonValue, 'TEnum> =
        #endif
            int
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<int, 'TEnum> value
                |> succeed
            )

        #if FABLE_COMPILER
        let inline uint32<'JsonValue, 'TEnum when 'TEnum : enum<uint32>> : Decoder<'JsonValue, 'TEnum> =
        #else
        let uint32<'JsonValue, 'TEnum when 'TEnum : enum<uint32>> : Decoder<'JsonValue, 'TEnum> =
        #endif
            uint32
            |> andThen (fun value ->
                LanguagePrimitives.EnumOfValue<uint32, 'TEnum> value
                |> succeed
            )
    #endif
