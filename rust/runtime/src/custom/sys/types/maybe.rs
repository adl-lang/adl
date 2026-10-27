use serde::{Deserialize, Deserializer};
use serde::{Serialize, Serializer};
use std::result;

/// ADL's `sys.types.Maybe`, an [`Option`] serialized as `{"just": v}` or
/// `"nothing"` rather than as `v` or `null`, so that a present null is
/// distinguishable from an absent value.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Maybe<T>(pub Option<T>);

impl<T> Maybe<T> {
    /// The absent value.
    pub fn nothing() -> Maybe<T> {
        Maybe(Option::None)
    }

    /// A present `t`.
    pub fn just(t: T) -> Maybe<T> {
        Maybe(Option::Some(t))
    }
}

impl<T: Serialize> Serialize for Maybe<T> {
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let m: MaybeImpl<&T> = match self {
            Maybe(Option::None) => MaybeImpl::Nothing,
            Maybe(Option::Some(v)) => MaybeImpl::Just(v),
        };
        m.serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for Maybe<T> {
    fn deserialize<D>(deserializer: D) -> result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let m = MaybeImpl::deserialize(deserializer)?;
        match m {
            MaybeImpl::Just(v) => Ok(Maybe(Option::Some(v))),
            MaybeImpl::Nothing => Ok(Maybe(Option::None)),
        }
    }
}

#[derive(Deserialize, Serialize)]
enum MaybeImpl<T> {
    #[serde(rename = "nothing")]
    Nothing,

    #[serde(rename = "just")]
    Just(T),
}
