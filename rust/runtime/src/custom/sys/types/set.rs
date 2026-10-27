use serde::{Deserialize, Deserializer};
use serde::{Serialize, Serializer};
use std::collections::HashSet;
use std::hash::Hash;
use std::result;

#[derive(Clone,Eq,PartialEq)]
pub struct Set<T:Eq + Hash>(HashSet<T>);

impl<T:Eq + Hash> Set<T> {
    pub fn new(v: Vec<T>) -> Set<T>
    {
        Set(v.into_iter().collect())
    }
}

impl<T> Serialize for Set<T>
where
    T: Serialize + Eq + Hash,
{
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut vpairs: Vec<&T> = Vec::new();
        for t in &self.0 {
            vpairs.push(t);
        }
        SetImpl(vpairs).serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for Set<T>
where
    T: Deserialize<'de> + Eq + Hash,
{
    fn deserialize<D>(deserializer: D) -> result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let m = SetImpl::<T>::deserialize(deserializer)?;
        Ok(Set(m.0.into_iter().collect()))
    }
}


#[derive(Deserialize,Serialize)]
struct SetImpl<T>(pub Vec<T>);
