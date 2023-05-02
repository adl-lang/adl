use crate::adlgen::sys::adlast2::PrimitiveType;

pub fn str_from_prim(prim: PrimitiveType) -> &'static str {
    match prim {
        PrimitiveType::Void => "Void",
        PrimitiveType::Bool => "Bool",
        PrimitiveType::Int8 => "Int8",
        PrimitiveType::Int16 => "Int16",
        PrimitiveType::Int32 => "Int32",
        PrimitiveType::Int64 => "Int64",
        PrimitiveType::Word8 => "Word8",
        PrimitiveType::Word16 => "Word16",
        PrimitiveType::Word32 => "Word32",
        PrimitiveType::Word64 => "Word64",
        PrimitiveType::Float => "Float",
        PrimitiveType::Double => "Double",
        PrimitiveType::Json => "Json",
        PrimitiveType::ByteVector => "Bytes",
        PrimitiveType::String => "String",
        PrimitiveType::Vector => "Vector",
        PrimitiveType::StringMap => "StringMap",
        PrimitiveType::Nullable => "Nullable",
        PrimitiveType::TypeToken => "TypeToken",
    }
}

pub fn prim_from_str(s: &str) -> Option<PrimitiveType> {
    match s {
        "Void" => Some(PrimitiveType::Void),
        "Bool" => Some(PrimitiveType::Bool),
        "Int8" => Some(PrimitiveType::Int8),
        "Int16" => Some(PrimitiveType::Int16),
        "Int32" => Some(PrimitiveType::Int32),
        "Int64" => Some(PrimitiveType::Int64),
        "Word8" => Some(PrimitiveType::Word8),
        "Word16" => Some(PrimitiveType::Word16),
        "Word32" => Some(PrimitiveType::Word32),
        "Word64" => Some(PrimitiveType::Word64),
        "Float" => Some(PrimitiveType::Float),
        "Double" => Some(PrimitiveType::Double),
        "Json" => Some(PrimitiveType::Json),
        "Bytes" => Some(PrimitiveType::ByteVector),
        "String" => Some(PrimitiveType::String),
        "Vector" => Some(PrimitiveType::Vector),
        "StringMap" => Some(PrimitiveType::StringMap),
        "Nullable" => Some(PrimitiveType::Nullable),
        "TypeToken" => Some(PrimitiveType::TypeToken),
        _ => None,
    }
}
