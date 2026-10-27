//! The adl rust runtime.
//!
//! See the crate documentation; this module is its whole content, kept in
//! mod.rs so that adlc's --include-rt can copy it into a generated project
//! verbatim as a plain module.

/// Implementations of ADL declarations that map onto a rust type rather than
/// onto generated code. The path under here mirrors the ADL module.
pub mod custom;

use base64::{prelude::BASE64_STANDARD, Engine};
use serde::{Deserialize, Deserializer};
use serde::{Serialize, Serializer};

/// ADL's `Bytes`, serialized as a base64 string.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ByteVector(pub Vec<u8>);

impl ByteVector {
    /// The bytes a base64 string stands for.
    ///
    /// This is for the literals adlc generates, which it has already
    /// validated.
    ///
    /// # Panics
    ///
    /// If `s` is not valid base64. Use the [`Deserialize`] implementation for
    /// input that might not be.
    pub fn from_literal(s: &str) -> Self {
        let v: Vec<u8> = BASE64_STANDARD.decode(s).unwrap();
        ByteVector(v)
    }
}

impl Serialize for ByteVector {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let bvs = BASE64_STANDARD.encode(&self.0);
        bvs.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ByteVector {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let v: Vec<u8> = BASE64_STANDARD
            .decode(s)
            .map_err(|_| serde::de::Error::custom("expected a base64 string"))?;
        Ok(ByteVector(v))
    }
}
