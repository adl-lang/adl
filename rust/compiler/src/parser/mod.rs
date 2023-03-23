use crate::adlgen::sys::adlast2::{self as adlast};
use crate::adlgen::sys::adlast2::Spanned;
use std::iter::repeat;
use std::collections::HashMap;

use crate::adlrt::custom::sys::types::map::Map;
use crate::adlrt::custom::sys::types::maybe::Maybe;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1},
    character::complete::{digit1, satisfy},
    combinator::{cut, map, opt, recognize, value},
    error::{context, ErrorKind, ParseError, VerboseError, VerboseErrorKind},
    multi::{many0, many0_count, separated_list0},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated},
    *,
};

use nom_locate::{position, LocatedSpan};

#[cfg(test)]
mod tests;

type Res<I, T> = IResult<I, T, VerboseError<I>>;

type Input<'a> = LocatedSpan<&'a str>;

type TypeExpr0 = adlast::TypeExpr<adlast::ScopedName>;
type Module0 = adlast::Module<TypeExpr0>;
pub type RawModule = (Module0, Vec<ExplicitAnnotation>);

pub enum DeclOrAnnotation {
    DADecl(adlast::Decl<adlast::TypeExpr<adlast::ScopedName>>),
    DAAnnotation(ExplicitAnnotation),
}

#[derive(Eq, PartialEq, Debug)]
pub struct ExplicitAnnotation {
    pub refr: ExplicitAnnotationRef,
    pub scoped_name: adlast::ScopedName,
    pub value: serde_json::Value,
}

#[derive(Eq, PartialEq, Debug)]
pub enum ExplicitAnnotationRef {
    Module,
    Decl(String),
    Field((String, String)),
}

// Consumes whitespace and comments, but not docstrings
pub fn whitespace(i: Input) -> Res<Input, ()>
where
{
    let mut state = 0;
    let mut chars = i.char_indices();
    let mut first_slash_ci = 0;
    loop {
        if let Some((ci, c)) = chars.next() {
            match state {
                0 => match c {
                    '\n' => (),
                    '\r' => (),
                    '\t' => (),
                    ' ' => (),
                    '/' => {
                        first_slash_ci = ci;
                        state = 1;
                    }
                    _ => return Ok((i.take_split(ci).0, ())),
                },
                1 => match c {
                    '/' => {
                        state = 2;
                    }
                    _ => return Ok((i.take_split(first_slash_ci).0, ())),
                },
                2 => match c {
                    '/' => return Ok((i.take_split(first_slash_ci).0, ())),
                    _ => {
                        state = 3;
                    }
                },
                _ => match c {
                    '\n' => state = 0,
                    '\r' => state = 0,
                    _ => (),
                },
            }
        } else {
            return Ok((i.take_split(i.len()).0, ()));
        }
    }
}

pub fn docstring(i: Input) -> Res<Input, &str> {
    let (i, _) = ws(tag("///"))(i)?;
    let (i, text) = i.split_at_position_complete(|item| {
        let c = item.as_char();
        c == '\r' || c == '\n'
    })?;
    let (i, _) = i.take_split(1); // drop newline
    Ok((i, &text))
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// returning the output of `inner`.
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(Input<'a>) -> Res<Input<'a>, O>
where
    F: FnMut(Input<'a>) -> Res<Input<'a>, O>,
{
    preceded(whitespace, inner)
}

// Match a tag and preceeding whitespace
fn wtag<'a>(t: &'a str) -> impl FnMut(Input<'a>) -> Res<Input<'a>, ()> {
    move |i: Input<'a>| {
        let (i, _) = whitespace(i)?;
        let (i, _) = tag(t)(i)?;
        Ok((i, ()))
    }
}

// Run a parser, recording the span of it's result
fn _spanned<'a, P: 'a, O>(inner: P) -> impl FnMut(Input<'a>) -> Res<Input<'a>, adlast::Spanned<O>>
where
    P: Fn(Input<'a>) -> Res<Input<'a>, O>,
{
    move |i: Input<'a>| {
        let (i, pos1) = position(i)?;
        let (i, value) = inner(i)?;
        let (i, pos2) = position(i)?;
        let r = Spanned::new(
            value,
            adlast::Span::new(pos1.location_offset() as u64, pos2.location_offset() as u64),
        );
        Ok((i, r))
    }
}

pub fn ident0(i: Input) -> Res<Input, &str> {
    let (i, id) = recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_"))))))(i)?;
    Ok((i, id.fragment()))
}

pub fn module_name(i: Input) -> Res<Input, adlast::ModuleName> {
    let (i, m) = recognize(pair(ident0, many0(pair(satisfy(|c| c == '.'), ident0))))(i)?;
    Ok((i, m.to_string()))
}

pub fn scoped_name(i: Input) -> Res<Input, adlast::ScopedName> {
    let (i, (n0, mut ns)) = pair(ws(ident0), many0(preceded(satisfy(|c| c == '.'), ident0)))(i)?;
    ns.insert(0, n0);
    let ns: Vec<String> = ns.iter().map(|n| n.to_string()).collect();
    let (name, module_name) = ns.split_last().unwrap();
    let scoped_name = adlast::ScopedName {
        module_name: module_name.join("."),
        name: name.clone(),
    };
    Ok((i, scoped_name))
}
pub fn raw_module(i: Input) -> Res<Input, RawModule> {
    context("module", raw_module0)(i)
}

pub fn raw_module0(i: Input) -> Res<Input, RawModule> {
    let (i, annotations) = many0(prefix_annotation)(i)?;
    let ma = merge_annotations(annotations).map_err(|emsg| custom_error(i, emsg))?;
    let (i, _) = ws(tag("module"))(i)?;
    let (i, name) = ws(module_name)(i)?;
    let (i, (imports, decls_or_annotations)) = delimited(
        wtag("{"),
        pair(
            many0(terminated(r#import, wtag(";"))),
            many0(terminated(decl_or_annotation, wtag(";"))),
        ),
        wtag("}"),
    )(i)?;
    let mut decls: Vec<adlast::Decl<TypeExpr0>> = vec![];
    let mut explicit_annotations: Vec<ExplicitAnnotation> = Vec::new();
    for da in decls_or_annotations {
        match da {
            DeclOrAnnotation::DADecl(decl) => {
                let dname = decl.name.clone();
                if let Some(_) = decls.iter().find(|d| d.name == dname) {
                    return Err(custom_error(i, format!("found duplicate decl: {}", dname)));
                }
                decls.push(decl);
            }
            DeclOrAnnotation::DAAnnotation(ann) => {
                explicit_annotations.push(ann);
            }
        }
    }
    let module = adlast::Module::new(name, imports, decls, ma);

    Ok((i, (module, explicit_annotations)))
}

pub fn r#import(i: Input) -> Res<Input, adlast::Import> {
    let (i, _) = wtag("import")(i)?;
    let (i, import) = context(
        "import",
        cut(alt((
            map(wildcard_import, |mn| adlast::Import::ModuleName(mn)),
            map(scoped_name, |sn| adlast::Import::ScopedName(sn)),
        ))),
    )(i)?;
    Ok((i, import))
}

pub fn wildcard_import(i: Input) -> Res<Input, adlast::ModuleName> {
    let (i, m) = ws(recognize(pair(
        ident0,
        terminated(many0(pair(satisfy(|c| c == '.'), ident0)), tag(".*")),
    )))(i)?;
    Ok((i, m[..(m.len() - 2)].to_string()))
}

pub fn decl_or_annotation(i: Input) -> Res<Input, DeclOrAnnotation> {
    alt((
        map(explicit_annotation, |a| DeclOrAnnotation::DAAnnotation(a)),
        map(decl, |d| DeclOrAnnotation::DADecl(d)),
    ))(i)
}

pub fn decl(i: Input) -> Res<Input, adlast::Decl<TypeExpr0>> {
    let (i, annotations) = many0(prefix_annotation)(i)?;
    let ma = merge_annotations(annotations).map_err(|emsg| custom_error(i, emsg))?;
    let (i, (name, dtype)) = decl_type(i)?;
    let decl = adlast::Decl {
        name: name.to_owned(),
        r#type: dtype,
        annotations: ma,
        version: Maybe::nothing(),
    };
    Ok((i, decl))
}

pub fn prefix_annotation(i: Input) -> Res<Input, (adlast::ScopedName, serde_json::Value)> {
    alt((
        prefix_annotation_,
        map(docstring, |s| {
            (docstring_scoped_name(), serde_json::Value::from(s))
        }),
    ))(i)
}

pub fn prefix_annotation_(i: Input) -> Res<Input, (adlast::ScopedName, serde_json::Value)> {
    let (i, _) = wtag("@")(i)?;
    let (i, sn) = scoped_name(i)?;
    let (i, ojv) = opt(json)(i)?;
    Ok((i, (sn, ojv.unwrap_or(serde_json::Value::Null))))
}

pub fn merge_annotations(
    anns: Vec<(adlast::ScopedName, serde_json::Value)>,
) -> Result<adlast::Annotations,String> {
    let mut hm = HashMap::new();
    let mut ds = Vec::new();

    for (k, v) in anns {
        if k == docstring_scoped_name() {
            ds.push(v.as_str().unwrap().to_owned());
        } else {
            if let Some(_) = hm.insert(k.clone(), v) {
                return Err(format!(
                    "Error duplicate annotation '{}.{}'",
                    &k.module_name, &k.name
                ));
            }
        }
    }
    if !ds.is_empty() {
        // ADL Doc string is (in ADL) `type Doc = Vector<String>` not `type Doc = String`
        hm.insert(docstring_scoped_name(), serde_json::Value::from(ds));
    };

    Ok(Map(hm))
}

pub fn docstring_scoped_name() -> adlast::ScopedName {
    adlast::ScopedName::new("sys.annotations".to_owned(), "Doc".to_owned())
}

pub fn decl_type(i: Input) -> Res<Input, (&str, adlast::DeclType<TypeExpr0>)> {
    alt((
        context(
            "struct",
            map(struct_, |(name, s)| (name, adlast::DeclType::Struct(s))),
        ),
        context(
            "union",
            map(union, |(name, u)| (name, adlast::DeclType::Union(u))),
        ),
        context(
            "type",
            map(typedef, |(name, t)| (name, adlast::DeclType::Type(t))),
        ),
        context(
            "newtype",
            map(newtype, |(name, n)| (name, adlast::DeclType::Newtype(n))),
        ),
    ))(i)
}

pub fn struct_(i: Input) -> Res<Input, (&str, adlast::Struct<TypeExpr0>)> {
    let (i, _) = ws(tag("struct"))(i)?;
    cut(|i| {
        let (i, name) = ws(ident0)(i)?;
        let (i, _) = oversion(i)?;
        let (i, type_params) = type_params(i)?;
        let (i, fields) = delimited(wtag("{"), many0(terminated(field, wtag(";"))), wtag("}"))(i)?;
        let struct_ = adlast::Struct {
            fields,
            type_params,
        };
        Ok((i, (name, struct_)))
    })(i)
}

pub fn union(i: Input) -> Res<Input, (&str, adlast::Union<TypeExpr0>)> {
    let (i, _) = wtag("union")(i)?;
    cut(|i| {
        let (i, name) = ws(ident0)(i)?;
        let (i, _) = oversion(i)?;
        let (i, type_params) = type_params(i)?;
        let (i, fields) = delimited(wtag("{"), many0(terminated(field, wtag(";"))), wtag("}"))(i)?;
        let union = adlast::Union {
            fields,
            type_params,
        };
        Ok((i, (name, union)))
    })(i)
}

pub fn typedef(i: Input) -> Res<Input, (&str, adlast::TypeDef<TypeExpr0>)> {
    let (i, _) = wtag("type")(i)?;
    cut(|i| {
        let (i, name) = ws(ident0)(i)?;
        let (i, _) = oversion(i)?;
        let (i, type_params) = type_params(i)?;
        let (i, type_expr) = preceded(wtag("="), type_expr)(i)?;
        let typedef = adlast::TypeDef {
            type_params,
            type_expr,
        };
        Ok((i, (name, typedef)))
    })(i)
}

pub fn newtype(i: Input) -> Res<Input, (&str, adlast::NewType<TypeExpr0>)> {
    let (i, _) = ws(tag("newtype"))(i)?;
    cut(|i| {
        let (i, name) = ws(ident0)(i)?;
        let (i, _) = oversion(i)?;
        let (i, type_params) = type_params(i)?;
        let (i, type_expr) = preceded(wtag("="), type_expr)(i)?;
        let (i, default) = opt(preceded(wtag("="), json))(i)?;

        let newtype = adlast::NewType {
            type_params,
            type_expr,
            default: maybe_from_option(default),
        };
        Ok((i, (name, newtype)))
    })(i)
}

fn oversion(i: Input) -> Res<Input, Option<u64>> {
    opt(oversion_)(i)
}

fn oversion_(i: Input) -> Res<Input, u64> {
    let (i, _) = ws(tag("#"))(i)?;
    let (i, ds) = ws(digit1)(i)?;
    Ok((i, ds.parse::<u64>().unwrap()))
}

pub fn field(i: Input) -> Res<Input, adlast::Field<TypeExpr0>> {
    context("field", field0)(i)
}

pub fn field0(i: Input) -> Res<Input, adlast::Field<TypeExpr0>> {
    let (i, annotations) = many0(prefix_annotation)(i)?;
    let ma = merge_annotations(annotations).map_err(|emsg| custom_error(i, emsg))?;
    let (i, texpr) = ws(type_expr)(i)?;
    let (i, name) = ws(ident0)(i)?;
    let (i, default) = opt(preceded(wtag("="), json))(i)?;
    let field = adlast::Field {
        name: name.to_owned(),
        serialized_name: name.to_owned(),
        type_expr: texpr,
        default: maybe_from_option(default),
        annotations: ma,
    };
    Ok((i, field))
}

pub fn explicit_annotation(i: Input) -> Res<Input, ExplicitAnnotation> {
    preceded(
        wtag("annotation"),
        cut(alt((
            explicit_module_annotation,
            explicit_decl_annotation,
            explicit_field_annotation,
        ))),
    )(i)
}

pub fn explicit_module_annotation(i: Input) -> Res<Input, ExplicitAnnotation> {
    let (i, scoped_name) = ws(scoped_name)(i)?;
    let (i, value) = json(i)?;
    Ok((
        i,
        ExplicitAnnotation {
            refr: ExplicitAnnotationRef::Module,
            scoped_name,
            value,
        },
    ))
}

pub fn explicit_decl_annotation(i: Input) -> Res<Input, ExplicitAnnotation> {
    let (i, decl_name) = ws(ident0)(i)?;
    let (i, scoped_name) = ws(scoped_name)(i)?;
    let (i, value) = json(i)?;
    Ok((
        i,
        ExplicitAnnotation {
            refr: ExplicitAnnotationRef::Decl(decl_name.to_owned()),
            scoped_name,
            value,
        },
    ))
}

pub fn explicit_field_annotation(i: Input) -> Res<Input, ExplicitAnnotation> {
    let (i, decl_name) = ws(ident0)(i)?;
    let (i, _) = wtag("::")(i)?;
    let (i, field_name) = ws(ident0)(i)?;
    let (i, scoped_name) = ws(scoped_name)(i)?;
    let (i, value) = json(i)?;

    Ok((
        i,
        ExplicitAnnotation {
            refr: ExplicitAnnotationRef::Field((decl_name.to_owned(), field_name.to_owned())),
            scoped_name,
            value,
        },
    ))
}

pub fn type_params(i: Input) -> Res<Input, Vec<adlast::Ident>> {
    map(
        opt(delimited(
            wtag("<"),
            separated_list0(ws(tag(",")), map(ws(ident0), |i| i.to_string())),
            wtag(">"),
        )),
        |idents| idents.unwrap_or_else(|| Vec::new()),
    )(i)
}

pub fn type_expr(i: Input) -> Res<Input, TypeExpr0> {
    map(pair(scoped_name, type_expr_params), |(tref, params)| {
        adlast::TypeExpr::new(tref, params)
    })(i)
}

pub fn type_expr_params(i: Input) -> Res<Input, Vec<TypeExpr0>> {
    map(
        opt(delimited(
            wtag("<"),
            separated_list0(ws(tag(",")), type_expr),
            wtag(">"),
        )),
        |texpr| texpr.unwrap_or_else(|| Vec::new()),
    )(i)
}

pub fn json(i: Input) -> Res<Input, serde_json::Value> {
    alt((
        value(serde_json::Value::Null, ws(tag("null"))),
        value(serde_json::Value::Bool(true), ws(tag("true"))),
        value(serde_json::Value::Bool(false), ws(tag("false"))),
        json_string,
        json_number,
        json_object,
        json_array,
    ))(i)
}

pub fn json_string(i: Input) -> Res<Input, serde_json::Value> {
    map(json_string0, |s: String| serde_json::Value::from(s))(i)
}

pub fn json_string0(i: Input) -> Res<Input, String> {
    let mut result = String::new();
    let mut esc = false;

    let (i, _) = ws(tag("\""))(i)?;

    let mut chars = i.char_indices();
    loop {
        if let Some((ci, c)) = chars.next() {
            if esc {
                match c {
                    'b' => result.push(0x08 as char),
                    'f' => result.push(0xc as char),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    // TODO: implement \uXXXX
                    c => result.push(c),
                }
                esc = false;
            } else if c == '\\' {
                esc = true;
            } else if c == '"' {
                let (i, _) = i.take_split(ci + 1);
                return Ok((i, result));
            } else {
                result.push(c);
            }
        } else {
            return Err(Err::Failure(VerboseError {
                errors: vec![(
                    i,
                    nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Eof),
                )],
            }));
        }
    }
}

pub fn json_number(i: Input) -> Res<Input, serde_json::Value> {
    map(ws(double), |v| {
        if v.floor() == v {
            if v >= 0.0 {
                serde_json::Value::from(v as u64)
            } else {
                serde_json::Value::from(v as i64)
            }
        } else {
            serde_json::Value::from(v)
        }
    })(i)
}

pub fn json_object(i: Input) -> Res<Input, serde_json::Value> {
    let (i, fields) = preceded(
        ws(tag("{")),
        cut(terminated(
            separated_list0(wtag(","), pair(terminated(json_string0, wtag(":")), json)),
            wtag("}"),
        )),
    )(i)?;

    let mut map = serde_json::Map::new();
    for (k, v) in fields {
        map.insert(k, v);
    }
    Ok((i, serde_json::Value::Object(map)))
}

pub fn json_array(i: Input) -> Res<Input, serde_json::Value> {
    map(
        preceded(
            ws(tag("[")),
            cut(terminated(separated_list0(wtag(","), json), wtag("]"))),
        ),
        |jv| serde_json::Value::from(jv),
    )(i)
}

pub fn maybe_from_option<T>(v: Option<T>) -> Maybe<T> {
    match v {
        Some(v) => Maybe::just(v),
        None => Maybe::nothing(),
    }
}

pub fn map_spanned<F, A, B>(sa: Spanned<A>, f: F) -> Spanned<B>
where
    F: Fn(A) -> B,
{
    Spanned::new(f(sa.value), sa.span)
}

fn custom_error(i: Input, msg: String) -> nom::Err<VerboseError<Input>> {
    use log::error;
    error!("{}", msg);
    return nom::Err::Failure(VerboseError::from_error_kind(i, ErrorKind::Tag));
}

// Lifted from nom source, but with our custom input type.
pub fn convert_error(input: Input, e: VerboseError<Input>) -> String {
    let lines: Vec<_> = input.lines().map(String::from).collect();

    let mut result = String::new();

    for (i, (substring, kind)) in e.errors.iter().enumerate() {
        let mut offset = input.offset(substring);

        let mut line = 0;
        let mut column = 0;

        for (j, l) in lines.iter().enumerate() {
            if offset <= l.len() {
                line = j;
                column = offset;
                break;
            } else {
                offset = offset - l.len() - 1;
            }
        }

        match kind {
            VerboseErrorKind::Char(c) => {
                result += &format!("{}: at line {}:\n", i, line);
                result += &lines[line];
                result += "\n";

                if column > 0 {
                    result += &repeat(' ').take(column).collect::<String>();
                }
                result += "^\n";
                result += &format!(
                    "expected '{}', found {}\n\n",
                    c,
                    substring.chars().next().unwrap()
                );
            }
            VerboseErrorKind::Context(s) => {
                result += &format!("{}: at line {}, in {}:\n", i, line, s);
                result += &lines[line];
                result += "\n";
                if column > 0 {
                    result += &repeat(' ').take(column).collect::<String>();
                }
                result += "^\n\n";
            }
            VerboseErrorKind::Nom(e) => {
                result += &format!("{}: at line {}, in {:?}:\n", i, line, e);
                result += &lines[line];
                result += "\n";
                if column > 0 {
                    result += &repeat(' ').take(column).collect::<String>();
                }
                result += "^\n\n";
            }
        }
    }

    result
}
