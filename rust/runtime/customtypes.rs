use crate::test4::adl::sys::types::MapInternal;
use crate::test4::adl::sys::types::MaybeInternal;
use crate::test4::adl::sys::types::PairInternal;
use crate::test4::adl::sys::types::ResultInternal;
use crate::test4::adl::sys::types::SetInternal;
use serde::{Deserialize, Deserializer};
use serde::{Serialize, Serializer};
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::result;

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Maybe<T>(Option<T>);

impl<T> Maybe<T> {
    pub fn nothing() -> Maybe<T> {
        Maybe(Option::None)
    }

    pub fn just(t: T) -> Maybe<T> {
        Maybe(Option::Some(t))
    }
}

impl<T: Serialize> Serialize for Maybe<T> {
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let m: MaybeInternal<&T> = match self {
            Maybe(Option::None) => MaybeInternal::Nothing,
            Maybe(Option::Some(v)) => MaybeInternal::Just(v),
        };
        m.serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for Maybe<T> {
    fn deserialize<D>(deserializer: D) -> result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let m = MaybeInternal::deserialize(deserializer)?;
        match m {
            MaybeInternal::Just(v) => Ok(Maybe(Option::Some(v))),
            MaybeInternal::Nothing => Ok(Maybe(Option::None)),
        }
    }
}

#[derive(Clone,Eq,PartialEq)]
pub struct Map<K:Eq + Hash, V>(HashMap<K, V>);

impl<K:Eq + Hash, V> Map<K, V> {
    pub fn new(v: Vec<Pair<K, V>>) -> Map<K, V>
    {
        let mut hm = HashMap::new();
        for Pair((k, v)) in v {
            hm.insert(k, v);
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
        let mut vpairs: Vec<Pair<&K, &V>> = Vec::new();
        for (k, v) in &self.0 {
            vpairs.push(Pair::new(k, v));
        }
        MapInternal(vpairs).serialize(serializer)
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
        let m = MapInternal::<K, V>::deserialize(deserializer)?;
        Ok(Map::new(m.0))
    }
}

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
        SetInternal(vpairs).serialize(serializer)
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
        let m = SetInternal::<T>::deserialize(deserializer)?;
        Ok(Set::new(m.0))
    }
}

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Pair<A, B>((A, B));

impl<A, B> Pair<A, B> {
    pub fn new(a: A, b: B) -> Pair<A, B> {
        Pair((a, b))
    }
}

impl<A, B> Serialize for Pair<A, B>
where
    A: Serialize,
    B: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let Pair((a, b)) = self;
        PairInternal { v_1: a, v_2: b }.serialize(serializer)
    }
}

impl<'de, A, B> Deserialize<'de> for Pair<A, B>
where
    A: Deserialize<'de>,
    B: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let m = PairInternal::<A, B>::deserialize(deserializer)?;
        Ok(Pair((m.v_1, m.v_2)))
    }
}

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Result<T, E>(std::result::Result<T, E>);

impl<T, E> Result<T, E> {
    pub fn ok(value: T) -> Result<T, E> {
        Result(Ok(value))
    }

    pub fn err(e: E) -> Result<T, E> {
        Result(Err(e))
    }
}

impl<T, E> Serialize for Result<T, E>
where
    T: Serialize,
    E: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let m = match &self.0 {
            Ok(v) => ResultInternal::Ok(v),
            Err(e) => ResultInternal::Error(e),
        };
        m.serialize(serializer)
    }
}

impl<'de, T, E> Deserialize<'de> for Result<T, E>
where
    T: Deserialize<'de>,
    E: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let m = ResultInternal::deserialize(deserializer)?;
        Ok(match m {
            ResultInternal::Ok(v) => Result::ok(v),
            ResultInternal::Error(e) => Result::err(e),
        })
    }
}
