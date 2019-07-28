// @generated from adl module sys.types

pub struct Pair<T1, T2> {
  pub v_1: T1,
  pub v_2: T2,
}

impl<T1, T2> Pair<T1, T2> {
  pub fn new(v_1: T1, v_2: T2) -> Pair<T1, T2> {
    Pair {
      v_1: v_1,
      v_2: v_2,
    }
  }
}

pub enum Either<T1, T2> {
  Left(T1),
  Right(T2),
}

pub enum Maybe<T> {
  Nothing,
  Just(T),
}

pub enum Error<T> {
  Value(T),
  Error(String),
}

pub struct Map<K, V>(pub Vec<Pair<K, V>>);

pub struct Set<T>(pub Vec<T>);
