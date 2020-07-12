use serde::{Deserialize, Deserializer};
use serde::{Serialize, Serializer};


#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Result<T, E>(pub std::result::Result<T, E>);

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
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let m = match &self.0 {
            Ok(v) => ResultImpl::Ok(v),
            Err(e) => ResultImpl::Error(e),
        };
        m.serialize(serializer)
    }
}

impl<'de, T, E> Deserialize<'de> for Result<T, E>
where
    T: Deserialize<'de>,
    E: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let m = ResultImpl::deserialize(deserializer)?;
        Ok(match m {
            ResultImpl::Ok(v) => Result::ok(v),
            ResultImpl::Error(e) => Result::err(e),
        })
    }
}


#[derive(Deserialize,Serialize)]
enum ResultImpl<T, E> {
  #[serde(rename="ok")]
  Ok(T),

  #[serde(rename="error")]
  Error(E),
}

