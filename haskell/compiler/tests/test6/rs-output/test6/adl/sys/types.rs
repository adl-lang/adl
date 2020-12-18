// @generated from adl module sys.types

use serde::Deserialize;
use serde::Serialize;

#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum Either<T1, T2> {
  #[serde(rename="left")]
  Left(T1),

  #[serde(rename="right")]
  Right(T2),
}
