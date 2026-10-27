use serde::Deserialize;
use serde::Serialize;

/// One entry of a [`Map`](super::map::Map).
///
/// ADL encodes a map as an array of these rather than as a json object, so
/// that a key can be of any type.
#[derive(Clone, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct MapEntry<K, V> {
    /// The entry's key.
    #[serde(rename = "k")]
    pub key: K,

    /// The entry's value.
    #[serde(rename = "v")]
    pub value: V,
}

impl<K, V> MapEntry<K, V> {
    /// An entry pairing `key` with `value`.
    pub fn new(key: K, value: V) -> MapEntry<K, V> {
        MapEntry { key, value }
    }
}
