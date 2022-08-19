pub enum PrimitiveType {
    Void,
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    Word8,
    Word16,
    Word32,
    Word64,
    Float,
    Double,
    Json,
    ByteVector,
    String,
    Vector,
    StringMap,
    Nullable,
    TypeToken,
}

impl PrimitiveType {
    pub fn to_str(&self) -> &str {
        match self {
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
            PrimitiveType::ByteVector => "ByteVector",
            PrimitiveType::String => "String",
            PrimitiveType::Vector => "Vector",
            PrimitiveType::StringMap => "StringMap",
            PrimitiveType::Nullable => "Nullable",
            PrimitiveType::TypeToken => "TypeToken",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "Void" => Some(Self::Void),
            "Bool" => Some(Self::Bool),
            "Int8" => Some(Self::Int8),
            "Int16" => Some(Self::Int16),
            "Int32" => Some(Self::Int32),
            "Int64" => Some(Self::Int64),
            "Word8" => Some(Self::Word8),
            "Word16" => Some(Self::Word16),
            "Word32" => Some(Self::Word32),
            "Word64" => Some(Self::Word64),
            "Float" => Some(Self::Float),
            "Double" => Some(Self::Double),
            "Json" => Some(Self::Json),
            "ByteVector" => Some(Self::ByteVector),
            "String" => Some(Self::String),
            "Vector" => Some(Self::Vector),
            "StringMap" => Some(Self::StringMap),
            "Nullable" => Some(Self::Nullable),
            "TypeToken" => Some(Self::TypeToken),
            _ => None,
        }
    }
}
