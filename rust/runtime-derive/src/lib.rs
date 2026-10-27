//! Derive macros supporting adlc generated rust code.

use proc_macro::TokenStream;

/// Introduces the `#[adl_annotation(..)]` attribute.
///
/// adlc emits that attribute for every ADL annotation whose declaration is
/// marked `adlc.config.rust.RustEmitAnnotation`, carrying the annotation's
/// scoped name and its value in ADL's json encoding. It names the derive
/// through `adl_rt`, which re-exports it, so that generated code needs only
/// that one dependency:
///
/// ```ignore
/// #[derive(Clone, /* ... */ adl_rt::AdlAnnotations)]
/// #[adl_annotation(name="common.http.HttpPath", value="\"/\"")]
/// pub struct ApiRequests { /* ... */ }
/// ```
///
/// Rust rejects an attribute that no derive on the item has declared as a
/// helper, so adlc adds this derive wherever it emits one. It generates no
/// code; its whole purpose is to make the attribute legal and inert. Other
/// derive macros on the same item can then read it, since helper attributes
/// are visible to every macro, not only the one that declared them.
#[proc_macro_derive(AdlAnnotations, attributes(adl_annotation))]
pub fn derive_adl_annotations(_input: TokenStream) -> TokenStream {
    TokenStream::new()
}
