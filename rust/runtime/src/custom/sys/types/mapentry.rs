use serde::{Deserialize};
use serde::{Serialize};

#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct MapEntry<K, V> {
  #[serde(rename="k")]
  pub key: K,

  #[serde(rename="v")]
  pub value: V,
}

impl<K, V> MapEntry<K, V> {
  pub fn new(key: K, value: V) -> MapEntry<K, V> {
    MapEntry {
      key: key,
      value: value,
    }
  }
}
