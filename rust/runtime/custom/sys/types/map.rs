use serde::{Deserialize, Deserializer};
use serde::{Serialize, Serializer};
use std::collections::HashMap;
use std::hash::Hash;
use std::result;
use super::mapentry::MapEntry;


#[derive(Clone,Eq,PartialEq)]
pub struct Map<K:Eq + Hash, V>(HashMap<K, V>);

impl<K:Eq + Hash, V> Map<K, V> {
    pub fn new(v: Vec<MapEntry<K, V>>) -> Map<K, V>
    {
        let mut hm = HashMap::new();
        for MapEntry{key,value} in v {
            hm.insert(key,value);
        }
        Map(hm)
    }
}

impl<K, V> Serialize for Map<K, V>
where
    K: Serialize + Eq + Hash,
    V: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut ventries: Vec<MapEntry<&K, &V>> = Vec::new();
        for (key, value) in &self.0 {
            ventries.push(MapEntry{key, value});
        }
        MapImpl(ventries).serialize(serializer)
    }
}

impl<'de, K, V> Deserialize<'de> for Map<K, V>
where
    K: Deserialize<'de> + Eq + Hash,
    V: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let m = MapImpl::<K, V>::deserialize(deserializer)?;
        Ok(Map::new(m.0))
    }
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
struct MapImpl<K, V>(pub Vec<MapEntry<K, V>>);

