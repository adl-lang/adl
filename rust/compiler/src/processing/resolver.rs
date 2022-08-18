use std::collections::{HashMap, HashSet};

use crate::adlgen::sys::adlast2 as adlast;
use crate::adlrt::custom::sys::types::map::Map;

use super::primitives::PrimitiveType;
use super::{Module0, TypeExpr0};

type Result<T> = std::result::Result<T, ResolveError>;

pub type TypeExpr1 = adlast::TypeExpr<TypeRef>;
pub type Decl1 = adlast::Decl<TypeExpr1>;
pub type Module1 = adlast::Module<TypeExpr1>;

pub enum TypeRef {
    ScopedName(adlast::ScopedName),
    Primitive(PrimitiveType),
    TypeParam(adlast::Ident),
}
pub trait AdlLoader {
    /// Find and parse the specified ADL module
    fn load(&self, module_name: &adlast::ModuleName) -> Result<Module0>;
}

pub struct Resolver {
    loader: Box<dyn AdlLoader>,
    modules: HashMap<adlast::ModuleName, Module1>,
}

impl Resolver {
    pub fn new(loader: Box<dyn AdlLoader>) -> Self {
        Self {
            loader,
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, module_name: &adlast::ModuleName) -> Result<()> {
        if self.modules.contains_key(module_name) {
            return Ok(());
        }
        let module0 = self.loader.load(module_name)?;
        let type_params = HashSet::new();
        let expanded_imports = HashMap::new();
        let mut ctx = ResolveCtx {
            resolver: self,
            module0: &module0,
            expanded_imports: &expanded_imports,
            type_params,
        };
        let module1 = resolve_module(&mut ctx, &module0)?;
        self.modules.insert(module_name.clone(), module1);
        Ok(())
    }

    pub fn get_module(&self, module_name: &adlast::ModuleName) -> Option<&Module1> {
        self.modules.get(module_name)
    }
    pub fn get_decl(&self, scoped_name: &adlast::ScopedName) -> Option<&Decl1> {
        match self.get_module(&scoped_name.module_name) {
            None => None,
            Some(module1) => return module1.decls.get(&scoped_name.name),
        }
    }
}

pub fn resolve_module(ctx: &mut ResolveCtx, module0: &Module0) -> Result<Module1> {
    let decls1 = module0
        .decls
        .iter()
        .map(|(n, decl0)| {
            let decl1 = resolve_decl(ctx, &decl0)?;
            Ok((n.clone(), decl1))
        })
        .collect::<Result<HashMap<_, _>>>()?;
    let annotations1 = resolve_annotations(ctx, &module0.annotations)?;
    let module1 = adlast::Module::new(
        module0.name.clone(),
        module0.imports.clone(),
        decls1,
        annotations1,
    );
    Ok(module1)
}

pub enum ResolveError {
    NoDeclForAnnotation,
    ModuleNotFound,
    DeclNotFound,
    LocalNotFound,
}

pub fn resolve_decl(
    ctx: &mut ResolveCtx,
    decl0: &adlast::Decl<TypeExpr0>,
) -> Result<adlast::Decl<TypeExpr1>> {
    let dtype = match &decl0.r#type {
        adlast::DeclType::Struct(s) => resolve_struct(ctx, &s)?,
        adlast::DeclType::Union(u) => resolve_union(ctx, &u)?,
        adlast::DeclType::Type(t) => resolve_type_alias(ctx, &t)?,
        adlast::DeclType::Newtype(n) => resolve_newtype(ctx, &n)?,
    };
    let annotations = resolve_annotations(ctx, &decl0.annotations)?;
    let decl = adlast::Decl::new(
        decl0.name.clone(),
        decl0.version.clone(),
        dtype,
        annotations,
    );
    Ok(decl)
}

pub fn resolve_struct(
    ctx0: &mut ResolveCtx,
    struct0: &adlast::Struct<TypeExpr0>,
) -> Result<adlast::DeclType<TypeExpr1>> {
    let ctx = with_type_params(ctx0, &struct0.type_params);
    let fields = resolve_fields(&ctx, &struct0.fields)?;
    let struct1 = adlast::Struct::new(struct0.type_params.clone(), fields);
    Ok(adlast::DeclType::Struct(struct1))
}

pub fn resolve_union(
    ctx0: &mut ResolveCtx,
    union0: &adlast::Union<TypeExpr0>,
) -> Result<adlast::DeclType<TypeExpr1>> {
    let ctx = with_type_params(ctx0, &union0.type_params);
    let fields = resolve_fields(&ctx, &union0.fields)?;
    let union1 = adlast::Union::new(union0.type_params.clone(), fields);
    Ok(adlast::DeclType::Union(union1))
}

pub fn resolve_type_alias(
    ctx0: &mut ResolveCtx,
    type0: &adlast::TypeDef<TypeExpr0>,
) -> Result<adlast::DeclType<TypeExpr1>> {
    let ctx = with_type_params(ctx0, &type0.type_params);
    let type_expr1 = resolve_type_expr(&ctx, &type0.type_expr)?;
    let type1 = adlast::TypeDef::new(type0.type_params.clone(), type_expr1);
    Ok(adlast::DeclType::Type(type1))
}

pub fn resolve_newtype(
    ctx0: &mut ResolveCtx,
    newtype0: &adlast::NewType<TypeExpr0>,
) -> Result<adlast::DeclType<TypeExpr1>> {
    let ctx = with_type_params(ctx0, &newtype0.type_params);
    let type_expr1 = resolve_type_expr(&ctx, &newtype0.type_expr)?;
    let newtype1 = adlast::NewType::new(
        newtype0.type_params.clone(),
        type_expr1,
        newtype0.default.clone(),
    );
    Ok(adlast::DeclType::Newtype(newtype1))
}

pub fn resolve_fields(
    ctx: &ResolveCtx,
    fields0: &Vec<adlast::Field<TypeExpr0>>,
) -> Result<Vec<adlast::Field<TypeExpr1>>> {
    fields0
        .iter()
        .map(|f| resolve_field(ctx, f))
        .collect::<Result<Vec<_>>>()
}

pub fn resolve_field(
    ctx: &ResolveCtx,
    field0: &adlast::Field<TypeExpr0>,
) -> Result<adlast::Field<TypeExpr1>> {
    let field1 = adlast::Field::new(
        field0.name.clone(),
        field0.serialized_name.clone(),
        resolve_type_expr(ctx, &field0.type_expr)?,
        field0.default.clone(),
        resolve_annotations(ctx, &field0.annotations)?,
    );
    Ok(field1)
}

pub fn resolve_annotations(
    ctx: &ResolveCtx,
    annotations0: &adlast::Annotations,
) -> Result<adlast::Annotations> {
    let hm1 = annotations0
        .0
        .iter()
        .map(|(sn0, jv)| {
            let tr1 = ctx.resolve_type_ref(sn0)?;
            if let TypeRef::ScopedName(sn1) = tr1 {
                Ok((sn1, jv.clone()))
            } else {
                Err(ResolveError::NoDeclForAnnotation)
            }
        })
        .collect::<Result<HashMap<_, _>>>()?;
    Ok(Map(hm1))
}

pub fn resolve_type_expr(ctx: &ResolveCtx, typeexpr0: &TypeExpr0) -> Result<TypeExpr1> {
    let type_ref = ctx.resolve_type_ref(&typeexpr0.type_ref)?;
    let parameters = typeexpr0
        .parameters
        .iter()
        .map(|p| resolve_type_expr(ctx, p))
        .collect::<Result<Vec<_>>>()?;
    let type_expr = adlast::TypeExpr::new(type_ref, parameters);
    Ok(type_expr)
}

pub struct ResolveCtx<'a> {
    resolver: &'a mut Resolver,
    module0: &'a Module0,
    expanded_imports: &'a HashMap<adlast::Ident, adlast::ScopedName>,
    type_params: HashSet<String>,
}

impl<'a> ResolveCtx<'a> {
    pub fn find_module(&self, _module_name: &adlast::ModuleName) -> Result<Option<&Module1>> {
        Ok(None) // FIXME
    }

    pub fn resolve_type_ref(&self, scoped_name0: &adlast::ScopedName) -> Result<TypeRef> {
        if scoped_name0.module_name.is_empty() {
            let name = &scoped_name0.name;
            if self.type_params.contains(name.as_str()) {
                return Ok(TypeRef::TypeParam(name.clone()));
            }
            if let Some(ptype) = PrimitiveType::from_str(&name) {
                return Ok(TypeRef::Primitive(ptype));
            }
            if self.module0.decls.contains_key(name) {
                return Ok(TypeRef::ScopedName(adlast::ScopedName::new(
                    self.module0.name.value.clone(),
                    name.clone(),
                )));
            }
            if let Some(scoped_name) = self.expanded_imports.get(name) {
                return Ok(TypeRef::ScopedName(scoped_name.clone()));
            }
            Err(ResolveError::LocalNotFound)
        } else {
            match self.find_module(&scoped_name0.module_name)? {
                None => return Err(ResolveError::ModuleNotFound),
                Some(module1) => match module1.decls.get(&scoped_name0.name) {
                    None => return Err(ResolveError::DeclNotFound),
                    Some(_) => return Ok(TypeRef::ScopedName(scoped_name0.clone())),
                },
            }
        }
    }
}

fn with_type_params<'a>(
    ctx0: &'a mut ResolveCtx,
    type_params: &'a Vec<adlast::Ident>,
) -> ResolveCtx<'a> {
    ResolveCtx {
        resolver: ctx0.resolver,
        module0: ctx0.module0,
        expanded_imports: ctx0.expanded_imports,
        type_params: type_params_set(type_params),
    }
}

fn type_params_set(type_params: &Vec<adlast::Ident>) -> HashSet<String> {
    type_params
        .iter()
        .map(|i| i.clone())
        .collect::<HashSet<_>>()
}