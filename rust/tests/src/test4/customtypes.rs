use chrono::naive::NaiveDate;
use chrono::Datelike;
use serde::de;
use serde::{Deserialize, Deserializer};
use serde::{Serialize, Serializer};
use std::result;

use crate::test4::adl::test4::CDate0;

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Date(NaiveDate);

impl Serialize for Date {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.format("%Y-%m-%d").to_string().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Date {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        match s.parse() {
            Ok(d) => Ok(Date(d)),
            Err(e) => Err(de::Error::custom(e.to_string())),
        }
    }
}

pub mod datehelpers {
    pub fn new(v: String) -> super::Date {
        super::Date(v.parse().unwrap())
    }
}

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct CDate(NaiveDate);

impl Serialize for CDate {
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let cdate0: CDate0 = CDate0 {
            day: self.0.day() as i16,
            month: self.0.month() as i16,
            year: self.0.year() as i16,
        };
        cdate0.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for CDate {
    fn deserialize<D>(deserializer: D) -> result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let cdate0 = CDate0::deserialize(deserializer)?;
        Ok(CDate(NaiveDate::from_ymd(
            cdate0.year as i32,
            cdate0.month as u32,
            cdate0.day as u32,
        )))
    }
}

pub mod cdatehelpers {
    pub fn new(year: i16, month: i16, day: i16) -> super::CDate {
        super::CDate(super::NaiveDate::from_ymd(
            year as i32,
            month as u32,
            day as u32,
        ))
    }
}
