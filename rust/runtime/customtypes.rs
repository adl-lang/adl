use serde::{Serialize, Serializer};
use serde::{Deserialize, Deserializer};
use crate::test4::adl::sys::types::MaybeInternal;
use crate::test4::adl::sys::types::MapInternal;
use crate::test4::adl::sys::types::SetInternal;
use crate::test4::adl::sys::types::PairInternal;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;

pub struct Maybe<T>(Option<T>);

impl <T> Maybe<T> {
  pub fn nothing() -> Maybe<T> {
    Maybe(Option::None)
  }

  pub fn just(t :T) -> Maybe<T> {
    Maybe(Option::Some(t))
  }
}

impl<T: Serialize> Serialize for Maybe<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
     let m : MaybeInternal<&T> = match self {
       Maybe(Option::None) => MaybeInternal::Nothing,
       Maybe(Option::Some(v)) => MaybeInternal::Just(v),
     };
     m.serialize(serializer)
  }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for Maybe<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        let m = MaybeInternal::deserialize(deserializer)?;
        match m {
          MaybeInternal::Just(v) => Ok(Maybe(Option::Some(v))),
          MaybeInternal::Nothing => Ok(Maybe(Option::None)),
        }
    }
}

pub struct Map<K,V>(HashMap<K,V>);

impl<K,V> Map<K,V> {
  pub fn new(v: Vec<Pair<K,V>>) -> Map<K,V>
    where K: Eq + Hash
  {
    let mut hm = HashMap::new();
    for Pair((k,v)) in v {
      hm.insert(k,v);
    }
    Map(hm)
  }
}

impl<K,V> Serialize for Map<K,V>
  where K: Serialize + Eq + Hash,
        V: Serialize
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
     let mut vpairs : Vec<Pair<&K,&V>> = Vec::new();
     for (k,v) in &self.0 {
       vpairs.push(Pair::new(k,v));

     }
     MapInternal(vpairs).serialize(serializer)
  }
}

impl<'de, K,V> Deserialize<'de> for Map<K,V>
  where K: Deserialize<'de> + Eq + Hash,
        V: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        let m = MapInternal::<K,V>::deserialize(deserializer)?;
        Result::Ok(Map::new(m.0))
    }
}

pub struct Set<T>(HashSet<T>);

impl <T> Set<T> {
  pub fn new(v: Vec<T>) -> Set<T>
    where T: Eq + Hash
  {
    Set(v.into_iter().collect())
  }
}

impl<T> Serialize for Set<T>
  where T: Serialize + Eq + Hash,
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
     let mut vpairs : Vec<&T> = Vec::new();
     for t in &self.0 {
       vpairs.push(t);

     }
     SetInternal(vpairs).serialize(serializer)
  }
}

impl<'de, T> Deserialize<'de> for Set<T>
  where T: Deserialize<'de> + Eq + Hash,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        let m = SetInternal::<T>::deserialize(deserializer)?;
        Result::Ok(Set::new(m.0))
    }
}

pub struct Pair<A,B>((A,B));

impl <A,B> Pair<A,B> {
  pub fn new(a: A, b:B) -> Pair<A,B>
  {
    Pair((a,b))
  }
}

impl<A,B> Serialize for Pair<A,B>
  where A: Serialize,
        B: Serialize,
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
     let Pair((a,b)) = self;
     PairInternal{v_1:a,v_2:b}.serialize(serializer)
  }
}

impl<'de, A,B> Deserialize<'de> for Pair<A,B>
  where A: Deserialize<'de>,
        B: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        let m = PairInternal::<A,B>::deserialize(deserializer)?;
        Result::Ok(Pair((m.v_1,m.v_2)))
    }
}
