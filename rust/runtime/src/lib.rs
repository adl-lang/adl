//! The adl rust runtime.
//!
//! The module tree lives in mod.rs, so that adlc's --include-rt can copy it
//! into a generated project verbatim as a plain module. This file is the
//! crate root, and exists only to add what makes sense for a crate but not
//! for a vendored copy.

#[path = "mod.rs"]
mod rt;

pub use rt::*;

/// Re-exported so that generated code needs only this one dependency, in the
/// same way that serde::Serialize is a re-export of serde_derive::Serialize.
pub use adl_rt_derive::AdlAnnotations;
