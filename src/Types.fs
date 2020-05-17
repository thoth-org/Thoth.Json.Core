namespace Thoth.Json

type IDecoderHelpers<'JsonValue> =
    abstract GetField : FieldName : string -> value : 'JsonValue -> 'JsonValue
    abstract IsString : value : 'JsonValue -> bool
    abstract IsBoolean : value : 'JsonValue -> bool
    abstract IsNumber : value : 'JsonValue -> bool
    abstract IsArray : value : 'JsonValue -> bool
    abstract IsObject : value : 'JsonValue -> bool
    abstract IsNullValue : value : 'JsonValue -> bool
    abstract IsIntegralValue : value : 'JsonValue -> bool
    abstract IsUndefined : value : 'JsonValue -> bool
    abstract AnyToString : value : 'JsonValue -> string

    abstract ObjectKeys : value : 'JsonValue -> string seq
    abstract AsBool : value : 'JsonValue -> bool
    abstract AsInt : value : 'JsonValue -> int
    abstract AsFloat : value : 'JsonValue -> float
    abstract AsFloat32 : value : 'JsonValue -> float32
    abstract AsString : value : 'JsonValue -> string
    abstract AsArray : value : 'JsonValue -> 'JsonValue[]

type ErrorReason<'JsonValue> =
    | BadPrimitive of string * 'JsonValue
    | BadPrimitiveExtra of string * 'JsonValue * string
    | BadType of string * 'JsonValue
    | BadField of string * 'JsonValue
    | BadPath of string * 'JsonValue * string
    | TooSmallArray of string * 'JsonValue
    | FailMessage of string
    | BadOneOf of string list

type DecoderError<'JsonValue> = string * ErrorReason<'JsonValue>

type Decoder<'JsonValue, 'T> = IDecoderHelpers<'JsonValue> -> 'JsonValue -> Result<'T, DecoderError<'JsonValue>>

type Encoder<'JsonValue, 'T> = 'T -> 'JsonValue
