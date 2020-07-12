use serde::{Deserialize, Deserializer};
use serde::{Serialize, Serializer};
use std::result;

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Pair<A,B> (pub (A,B));

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
        PairImpl { v_1: a, v_2: b }.serialize(serializer)
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
        let m = PairImpl::<A, B>::deserialize(deserializer)?;
        Ok(Pair((m.v_1, m.v_2)))
    }
}

#[derive(Deserialize,Serialize)]
struct PairImpl<A, B> {
  #[serde(rename="v1")]
  pub v_1: A,

  #[serde(rename="v2")]
  pub v_2: B,
}
