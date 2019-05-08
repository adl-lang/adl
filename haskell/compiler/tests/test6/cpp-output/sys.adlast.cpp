#include "sys.adlast.h"

namespace ADL {
namespace sys {
namespace adlast {

DeclType_::DeclType_()
    : d_(STRUCT_), p_(new Struct_())
{
}

DeclType_ DeclType_::mk_struct_( const Struct_ & v )
{
    return DeclType_( STRUCT_, new Struct_(v) );
}

DeclType_ DeclType_::mk_union_( const Union_ & v )
{
    return DeclType_( UNION_, new Union_(v) );
}

DeclType_ DeclType_::mk_type_( const TypeDef_ & v )
{
    return DeclType_( TYPE_, new TypeDef_(v) );
}

DeclType_ DeclType_::mk_newtype_( const NewType & v )
{
    return DeclType_( NEWTYPE_, new NewType(v) );
}

DeclType_::DeclType_( const DeclType_ & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

DeclType_::~DeclType_()
{
    free(d_,p_);
}

DeclType_ & DeclType_::operator=( const DeclType_ & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const Struct_ & DeclType_::set_struct_(const Struct_ &v)
{
    if( d_ == STRUCT_ )
    {
        *(Struct_ *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = STRUCT_;
        p_ = new Struct_(v);
    }
    return *(Struct_ *)p_;
}

const Union_ & DeclType_::set_union_(const Union_ &v)
{
    if( d_ == UNION_ )
    {
        *(Union_ *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = UNION_;
        p_ = new Union_(v);
    }
    return *(Union_ *)p_;
}

const TypeDef_ & DeclType_::set_type_(const TypeDef_ &v)
{
    if( d_ == TYPE_ )
    {
        *(TypeDef_ *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = TYPE_;
        p_ = new TypeDef_(v);
    }
    return *(TypeDef_ *)p_;
}

const NewType & DeclType_::set_newtype_(const NewType &v)
{
    if( d_ == NEWTYPE_ )
    {
        *(NewType *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = NEWTYPE_;
        p_ = new NewType(v);
    }
    return *(NewType *)p_;
}

DeclType_::DeclType_(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void DeclType_::free(DiscType d, void *p)
{
    switch( d )
    {
        case STRUCT_: delete (Struct_ *)p; return;
        case UNION_: delete (Union_ *)p; return;
        case TYPE_: delete (TypeDef_ *)p; return;
        case NEWTYPE_: delete (NewType *)p; return;
    }
}

void * DeclType_::copy( DiscType d, void *p )
{
    switch( d )
    {
        case STRUCT_: return new Struct_(*(Struct_ *)p);
        case UNION_: return new Union_(*(Union_ *)p);
        case TYPE_: return new TypeDef_(*(TypeDef_ *)p);
        case NEWTYPE_: return new NewType(*(NewType *)p);
    }
    return 0;
}

bool
operator<( const DeclType_ &a, const DeclType_ &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case DeclType_::STRUCT_: return a.struct_() < b.struct_();
        case DeclType_::UNION_: return a.union_() < b.union_();
        case DeclType_::TYPE_: return a.type_() < b.type_();
        case DeclType_::NEWTYPE_: return a.newtype_() < b.newtype_();
    }
    return false;
}

bool
operator==( const DeclType_ &a, const DeclType_ &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case DeclType_::STRUCT_: return a.struct_() == b.struct_();
        case DeclType_::UNION_: return a.union_() == b.union_();
        case DeclType_::TYPE_: return a.type_() == b.type_();
        case DeclType_::NEWTYPE_: return a.newtype_() == b.newtype_();
    }
    return false;
}

Import::Import()
    : d_(MODULENAME), p_(new std::string())
{
}

Import Import::mk_moduleName( const std::string & v )
{
    return Import( MODULENAME, new std::string(v) );
}

Import Import::mk_scopedName( const ScopedName & v )
{
    return Import( SCOPEDNAME, new ScopedName(v) );
}

Import::Import( const Import & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

Import::~Import()
{
    free(d_,p_);
}

Import & Import::operator=( const Import & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const std::string & Import::set_moduleName(const std::string &v)
{
    if( d_ == MODULENAME )
    {
        *(std::string *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = MODULENAME;
        p_ = new std::string(v);
    }
    return *(std::string *)p_;
}

const ScopedName & Import::set_scopedName(const ScopedName &v)
{
    if( d_ == SCOPEDNAME )
    {
        *(ScopedName *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = SCOPEDNAME;
        p_ = new ScopedName(v);
    }
    return *(ScopedName *)p_;
}

Import::Import(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void Import::free(DiscType d, void *p)
{
    switch( d )
    {
        case MODULENAME: delete (std::string *)p; return;
        case SCOPEDNAME: delete (ScopedName *)p; return;
    }
}

void * Import::copy( DiscType d, void *p )
{
    switch( d )
    {
        case MODULENAME: return new std::string(*(std::string *)p);
        case SCOPEDNAME: return new ScopedName(*(ScopedName *)p);
    }
    return 0;
}

bool
operator<( const Import &a, const Import &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case Import::MODULENAME: return a.moduleName() < b.moduleName();
        case Import::SCOPEDNAME: return a.scopedName() < b.scopedName();
    }
    return false;
}

bool
operator==( const Import &a, const Import &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case Import::MODULENAME: return a.moduleName() == b.moduleName();
        case Import::SCOPEDNAME: return a.scopedName() == b.scopedName();
    }
    return false;
}

Struct_::Struct_()
{
}

Struct_::Struct_(
    const std::vector<std::string>  & typeParams_,
    const std::vector<Field>  & fields_
    )
    : typeParams(typeParams_)
    , fields(fields_)
{
}

bool
operator<( const Struct_ &a, const Struct_ &b )
{
    if( a.typeParams < b.typeParams ) return true;
    if( b.typeParams < a.typeParams ) return false;
    if( a.fields < b.fields ) return true;
    if( b.fields < a.fields ) return false;
    return false;
}

bool
operator==( const Struct_ &a, const Struct_ &b )
{
    return
        a.typeParams == b.typeParams &&
        a.fields == b.fields ;
}

TypeRef::TypeRef()
    : d_(PRIMITIVE), p_(new std::string())
{
}

TypeRef TypeRef::mk_primitive( const std::string & v )
{
    return TypeRef( PRIMITIVE, new std::string(v) );
}

TypeRef TypeRef::mk_typeParam( const std::string & v )
{
    return TypeRef( TYPEPARAM, new std::string(v) );
}

TypeRef TypeRef::mk_reference( const ScopedName & v )
{
    return TypeRef( REFERENCE, new ScopedName(v) );
}

TypeRef::TypeRef( const TypeRef & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

TypeRef::~TypeRef()
{
    free(d_,p_);
}

TypeRef & TypeRef::operator=( const TypeRef & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const std::string & TypeRef::set_primitive(const std::string &v)
{
    if( d_ == PRIMITIVE )
    {
        *(std::string *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = PRIMITIVE;
        p_ = new std::string(v);
    }
    return *(std::string *)p_;
}

const std::string & TypeRef::set_typeParam(const std::string &v)
{
    if( d_ == TYPEPARAM )
    {
        *(std::string *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = TYPEPARAM;
        p_ = new std::string(v);
    }
    return *(std::string *)p_;
}

const ScopedName & TypeRef::set_reference(const ScopedName &v)
{
    if( d_ == REFERENCE )
    {
        *(ScopedName *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = REFERENCE;
        p_ = new ScopedName(v);
    }
    return *(ScopedName *)p_;
}

TypeRef::TypeRef(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void TypeRef::free(DiscType d, void *p)
{
    switch( d )
    {
        case PRIMITIVE: delete (std::string *)p; return;
        case TYPEPARAM: delete (std::string *)p; return;
        case REFERENCE: delete (ScopedName *)p; return;
    }
}

void * TypeRef::copy( DiscType d, void *p )
{
    switch( d )
    {
        case PRIMITIVE: return new std::string(*(std::string *)p);
        case TYPEPARAM: return new std::string(*(std::string *)p);
        case REFERENCE: return new ScopedName(*(ScopedName *)p);
    }
    return 0;
}

bool
operator<( const TypeRef &a, const TypeRef &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case TypeRef::PRIMITIVE: return a.primitive() < b.primitive();
        case TypeRef::TYPEPARAM: return a.typeParam() < b.typeParam();
        case TypeRef::REFERENCE: return a.reference() < b.reference();
    }
    return false;
}

bool
operator==( const TypeRef &a, const TypeRef &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case TypeRef::PRIMITIVE: return a.primitive() == b.primitive();
        case TypeRef::TYPEPARAM: return a.typeParam() == b.typeParam();
        case TypeRef::REFERENCE: return a.reference() == b.reference();
    }
    return false;
}

Union_::Union_()
{
}

Union_::Union_(
    const std::vector<std::string>  & typeParams_,
    const std::vector<Field>  & fields_
    )
    : typeParams(typeParams_)
    , fields(fields_)
{
}

bool
operator<( const Union_ &a, const Union_ &b )
{
    if( a.typeParams < b.typeParams ) return true;
    if( b.typeParams < a.typeParams ) return false;
    if( a.fields < b.fields ) return true;
    if( b.fields < a.fields ) return false;
    return false;
}

bool
operator==( const Union_ &a, const Union_ &b )
{
    return
        a.typeParams == b.typeParams &&
        a.fields == b.fields ;
}

ScopedName::ScopedName()
{
}

ScopedName::ScopedName(
    const ModuleName & moduleName_,
    const Ident & name_
    )
    : moduleName(moduleName_)
    , name(name_)
{
}

bool
operator<( const ScopedName &a, const ScopedName &b )
{
    if( a.moduleName < b.moduleName ) return true;
    if( b.moduleName < a.moduleName ) return false;
    if( a.name < b.name ) return true;
    if( b.name < a.name ) return false;
    return false;
}

bool
operator==( const ScopedName &a, const ScopedName &b )
{
    return
        a.moduleName == b.moduleName &&
        a.name == b.name ;
}

TypeExpr::TypeExpr()
{
}

TypeExpr::TypeExpr(
    const TypeRef & typeRef_,
    const std::vector<TypeExpr>  & parameters_
    )
    : typeRef(typeRef_)
    , parameters(parameters_)
{
}

bool
operator<( const TypeExpr &a, const TypeExpr &b )
{
    if( a.typeRef < b.typeRef ) return true;
    if( b.typeRef < a.typeRef ) return false;
    if( a.parameters < b.parameters ) return true;
    if( b.parameters < a.parameters ) return false;
    return false;
}

bool
operator==( const TypeExpr &a, const TypeExpr &b )
{
    return
        a.typeRef == b.typeRef &&
        a.parameters == b.parameters ;
}

NewType::NewType()
{
}

NewType::NewType(
    const std::vector<std::string>  & typeParams_,
    const TypeExpr & typeExpr_,
    const ADL::sys::types::Maybe<JsonValue>  & default__
    )
    : typeParams(typeParams_)
    , typeExpr(typeExpr_)
    , default_(default__)
{
}

bool
operator<( const NewType &a, const NewType &b )
{
    if( a.typeParams < b.typeParams ) return true;
    if( b.typeParams < a.typeParams ) return false;
    if( a.typeExpr < b.typeExpr ) return true;
    if( b.typeExpr < a.typeExpr ) return false;
    if( a.default_ < b.default_ ) return true;
    if( b.default_ < a.default_ ) return false;
    return false;
}

bool
operator==( const NewType &a, const NewType &b )
{
    return
        a.typeParams == b.typeParams &&
        a.typeExpr == b.typeExpr &&
        a.default_ == b.default_ ;
}

TypeDef_::TypeDef_()
{
}

TypeDef_::TypeDef_(
    const std::vector<std::string>  & typeParams_,
    const TypeExpr & typeExpr_
    )
    : typeParams(typeParams_)
    , typeExpr(typeExpr_)
{
}

bool
operator<( const TypeDef_ &a, const TypeDef_ &b )
{
    if( a.typeParams < b.typeParams ) return true;
    if( b.typeParams < a.typeParams ) return false;
    if( a.typeExpr < b.typeExpr ) return true;
    if( b.typeExpr < a.typeExpr ) return false;
    return false;
}

bool
operator==( const TypeDef_ &a, const TypeDef_ &b )
{
    return
        a.typeParams == b.typeParams &&
        a.typeExpr == b.typeExpr ;
}

Decl::Decl()
{
}

Decl::Decl(
    const Ident & name_,
    const ADL::sys::types::Maybe<uint32_t>  & version_,
    const DeclType_ & type__,
    const Annotations & annotations_
    )
    : name(name_)
    , version(version_)
    , type_(type__)
    , annotations(annotations_)
{
}

bool
operator<( const Decl &a, const Decl &b )
{
    if( a.name < b.name ) return true;
    if( b.name < a.name ) return false;
    if( a.version < b.version ) return true;
    if( b.version < a.version ) return false;
    if( a.type_ < b.type_ ) return true;
    if( b.type_ < a.type_ ) return false;
    if( a.annotations < b.annotations ) return true;
    if( b.annotations < a.annotations ) return false;
    return false;
}

bool
operator==( const Decl &a, const Decl &b )
{
    return
        a.name == b.name &&
        a.version == b.version &&
        a.type_ == b.type_ &&
        a.annotations == b.annotations ;
}

Field::Field()
{
}

Field::Field(
    const Ident & name_,
    const Ident & serializedName_,
    const TypeExpr & typeExpr_,
    const ADL::sys::types::Maybe<JsonValue>  & default__,
    const Annotations & annotations_
    )
    : name(name_)
    , serializedName(serializedName_)
    , typeExpr(typeExpr_)
    , default_(default__)
    , annotations(annotations_)
{
}

bool
operator<( const Field &a, const Field &b )
{
    if( a.name < b.name ) return true;
    if( b.name < a.name ) return false;
    if( a.serializedName < b.serializedName ) return true;
    if( b.serializedName < a.serializedName ) return false;
    if( a.typeExpr < b.typeExpr ) return true;
    if( b.typeExpr < a.typeExpr ) return false;
    if( a.default_ < b.default_ ) return true;
    if( b.default_ < a.default_ ) return false;
    if( a.annotations < b.annotations ) return true;
    if( b.annotations < a.annotations ) return false;
    return false;
}

bool
operator==( const Field &a, const Field &b )
{
    return
        a.name == b.name &&
        a.serializedName == b.serializedName &&
        a.typeExpr == b.typeExpr &&
        a.default_ == b.default_ &&
        a.annotations == b.annotations ;
}

Module::Module()
{
}

Module::Module(
    const ModuleName & name_,
    const std::vector<Import>  & imports_,
    const std::map<std::string,Decl> & decls_,
    const Annotations & annotations_
    )
    : name(name_)
    , imports(imports_)
    , decls(decls_)
    , annotations(annotations_)
{
}

bool
operator<( const Module &a, const Module &b )
{
    if( a.name < b.name ) return true;
    if( b.name < a.name ) return false;
    if( a.imports < b.imports ) return true;
    if( b.imports < a.imports ) return false;
    if( a.decls < b.decls ) return true;
    if( b.decls < a.decls ) return false;
    if( a.annotations < b.annotations ) return true;
    if( b.annotations < a.annotations ) return false;
    return false;
}

bool
operator==( const Module &a, const Module &b )
{
    return
        a.name == b.name &&
        a.imports == b.imports &&
        a.decls == b.decls &&
        a.annotations == b.annotations ;
}

ScopedDecl::ScopedDecl()
{
}

ScopedDecl::ScopedDecl(
    const ModuleName & moduleName_,
    const Decl & decl_
    )
    : moduleName(moduleName_)
    , decl(decl_)
{
}

bool
operator<( const ScopedDecl &a, const ScopedDecl &b )
{
    if( a.moduleName < b.moduleName ) return true;
    if( b.moduleName < a.moduleName ) return false;
    if( a.decl < b.decl ) return true;
    if( b.decl < a.decl ) return false;
    return false;
}

bool
operator==( const ScopedDecl &a, const ScopedDecl &b )
{
    return
        a.moduleName == b.moduleName &&
        a.decl == b.decl ;
}

}}}; // ADL::sys::adlast

namespace ADL {

typename Serialiser<ADL::sys::adlast::DeclType_>::Ptr
Serialisable<ADL::sys::adlast::DeclType_>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::DeclType_ _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<ADL::sys::adlast::Struct_>::Ptr struct__;
        mutable typename Serialiser<ADL::sys::adlast::Union_>::Ptr union__;
        mutable typename Serialiser<ADL::sys::adlast::TypeDef_>::Ptr type__;
        mutable typename Serialiser<ADL::sys::adlast::NewType>::Ptr newtype__;
        
        typename Serialiser<ADL::sys::adlast::Struct_>::Ptr struct__s() const
        {
            if( !struct__ )
                struct__ = Serialisable<ADL::sys::adlast::Struct_>::serialiser(sf_);
            return struct__;
        }
        
        typename Serialiser<ADL::sys::adlast::Union_>::Ptr union__s() const
        {
            if( !union__ )
                union__ = Serialisable<ADL::sys::adlast::Union_>::serialiser(sf_);
            return union__;
        }
        
        typename Serialiser<ADL::sys::adlast::TypeDef_>::Ptr type__s() const
        {
            if( !type__ )
                type__ = Serialisable<ADL::sys::adlast::TypeDef_>::serialiser(sf_);
            return type__;
        }
        
        typename Serialiser<ADL::sys::adlast::NewType>::Ptr newtype__s() const
        {
            if( !newtype__ )
                newtype__ = Serialisable<ADL::sys::adlast::NewType>::serialiser(sf_);
            return newtype__;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::sys::adlast::DeclType_::STRUCT_: json.startObject(); writeField( json, struct__s(), "struct_", v.struct_() ); json.endObject(); break;
                case ADL::sys::adlast::DeclType_::UNION_: json.startObject(); writeField( json, union__s(), "union_", v.union_() ); json.endObject(); break;
                case ADL::sys::adlast::DeclType_::TYPE_: json.startObject(); writeField( json, type__s(), "type_", v.type_() ); json.endObject(); break;
                case ADL::sys::adlast::DeclType_::NEWTYPE_: json.startObject(); writeField( json, newtype__s(), "newtype_", v.newtype_() ); json.endObject(); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::START_OBJECT )
            {
                match( json, JsonReader::START_OBJECT );
                if( json.type() == JsonReader::END_OBJECT )
                    throw json_parse_failure();
                while( !match0( json, JsonReader::END_OBJECT ) )
                {
                    if( matchField0( "struct_", json ) )
                        v.set_struct_(struct__s()->fromJson( json ));
                    else if( matchField0( "union_", json ) )
                        v.set_union_(union__s()->fromJson( json ));
                    else if( matchField0( "type_", json ) )
                        v.set_type_(type__s()->fromJson( json ));
                    else if( matchField0( "newtype_", json ) )
                        v.set_newtype_(newtype__s()->fromJson( json ));
                    else
                        throw json_parse_failure();
                }
                return;
            }
            throw json_parse_failure();
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

typename Serialiser<ADL::sys::adlast::Import>::Ptr
Serialisable<ADL::sys::adlast::Import>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::Import _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<std::string>::Ptr moduleName_;
        mutable typename Serialiser<ADL::sys::adlast::ScopedName>::Ptr scopedName_;
        
        typename Serialiser<std::string>::Ptr moduleName_s() const
        {
            if( !moduleName_ )
                moduleName_ = Serialisable<std::string>::serialiser(sf_);
            return moduleName_;
        }
        
        typename Serialiser<ADL::sys::adlast::ScopedName>::Ptr scopedName_s() const
        {
            if( !scopedName_ )
                scopedName_ = Serialisable<ADL::sys::adlast::ScopedName>::serialiser(sf_);
            return scopedName_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::sys::adlast::Import::MODULENAME: json.startObject(); writeField( json, moduleName_s(), "moduleName", v.moduleName() ); json.endObject(); break;
                case ADL::sys::adlast::Import::SCOPEDNAME: json.startObject(); writeField( json, scopedName_s(), "scopedName", v.scopedName() ); json.endObject(); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::START_OBJECT )
            {
                match( json, JsonReader::START_OBJECT );
                if( json.type() == JsonReader::END_OBJECT )
                    throw json_parse_failure();
                while( !match0( json, JsonReader::END_OBJECT ) )
                {
                    if( matchField0( "moduleName", json ) )
                        v.set_moduleName(moduleName_s()->fromJson( json ));
                    else if( matchField0( "scopedName", json ) )
                        v.set_scopedName(scopedName_s()->fromJson( json ));
                    else
                        throw json_parse_failure();
                }
                return;
            }
            throw json_parse_failure();
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

typename Serialiser<ADL::sys::adlast::Struct_>::Ptr
Serialisable<ADL::sys::adlast::Struct_>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::Struct_ _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : typeParams_s( Serialisable<std::vector<std::string> >::serialiser(sf) )
            , fields_s( Serialisable<std::vector<ADL::sys::adlast::Field> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<std::vector<std::string> >::Ptr typeParams_s;
        typename Serialiser<std::vector<ADL::sys::adlast::Field> >::Ptr fields_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::vector<std::string> >( json, typeParams_s, "typeParams", v.typeParams );
            writeField<std::vector<ADL::sys::adlast::Field> >( json, fields_s, "fields", v.fields );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( typeParams_s, v.typeParams, "typeParams", json ) ||
                readField( fields_s, v.fields, "fields", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::adlast::TypeRef>::Ptr
Serialisable<ADL::sys::adlast::TypeRef>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::TypeRef _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<std::string>::Ptr primitive_;
        mutable typename Serialiser<std::string>::Ptr typeParam_;
        mutable typename Serialiser<ADL::sys::adlast::ScopedName>::Ptr reference_;
        
        typename Serialiser<std::string>::Ptr primitive_s() const
        {
            if( !primitive_ )
                primitive_ = Serialisable<std::string>::serialiser(sf_);
            return primitive_;
        }
        
        typename Serialiser<std::string>::Ptr typeParam_s() const
        {
            if( !typeParam_ )
                typeParam_ = Serialisable<std::string>::serialiser(sf_);
            return typeParam_;
        }
        
        typename Serialiser<ADL::sys::adlast::ScopedName>::Ptr reference_s() const
        {
            if( !reference_ )
                reference_ = Serialisable<ADL::sys::adlast::ScopedName>::serialiser(sf_);
            return reference_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::sys::adlast::TypeRef::PRIMITIVE: json.startObject(); writeField( json, primitive_s(), "primitive", v.primitive() ); json.endObject(); break;
                case ADL::sys::adlast::TypeRef::TYPEPARAM: json.startObject(); writeField( json, typeParam_s(), "typeParam", v.typeParam() ); json.endObject(); break;
                case ADL::sys::adlast::TypeRef::REFERENCE: json.startObject(); writeField( json, reference_s(), "reference", v.reference() ); json.endObject(); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::START_OBJECT )
            {
                match( json, JsonReader::START_OBJECT );
                if( json.type() == JsonReader::END_OBJECT )
                    throw json_parse_failure();
                while( !match0( json, JsonReader::END_OBJECT ) )
                {
                    if( matchField0( "primitive", json ) )
                        v.set_primitive(primitive_s()->fromJson( json ));
                    else if( matchField0( "typeParam", json ) )
                        v.set_typeParam(typeParam_s()->fromJson( json ));
                    else if( matchField0( "reference", json ) )
                        v.set_reference(reference_s()->fromJson( json ));
                    else
                        throw json_parse_failure();
                }
                return;
            }
            throw json_parse_failure();
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

typename Serialiser<ADL::sys::adlast::Union_>::Ptr
Serialisable<ADL::sys::adlast::Union_>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::Union_ _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : typeParams_s( Serialisable<std::vector<std::string> >::serialiser(sf) )
            , fields_s( Serialisable<std::vector<ADL::sys::adlast::Field> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<std::vector<std::string> >::Ptr typeParams_s;
        typename Serialiser<std::vector<ADL::sys::adlast::Field> >::Ptr fields_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::vector<std::string> >( json, typeParams_s, "typeParams", v.typeParams );
            writeField<std::vector<ADL::sys::adlast::Field> >( json, fields_s, "fields", v.fields );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( typeParams_s, v.typeParams, "typeParams", json ) ||
                readField( fields_s, v.fields, "fields", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::adlast::ScopedName>::Ptr
Serialisable<ADL::sys::adlast::ScopedName>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::ScopedName _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : moduleName_s( Serialisable<ADL::sys::adlast::ModuleName>::serialiser(sf) )
            , name_s( Serialisable<ADL::sys::adlast::Ident>::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::sys::adlast::ModuleName>::Ptr moduleName_s;
        typename Serialiser<ADL::sys::adlast::Ident>::Ptr name_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::sys::adlast::ModuleName>( json, moduleName_s, "moduleName", v.moduleName );
            writeField<ADL::sys::adlast::Ident>( json, name_s, "name", v.name );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( moduleName_s, v.moduleName, "moduleName", json ) ||
                readField( name_s, v.name, "name", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::adlast::TypeExpr>::Ptr
Serialisable<ADL::sys::adlast::TypeExpr>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::TypeExpr _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : typeRef_s( Serialisable<ADL::sys::adlast::TypeRef>::serialiser(sf) )
            , parameters_s( Serialisable<std::vector<ADL::sys::adlast::TypeExpr> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::sys::adlast::TypeRef>::Ptr typeRef_s;
        typename Serialiser<std::vector<ADL::sys::adlast::TypeExpr> >::Ptr parameters_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::sys::adlast::TypeRef>( json, typeRef_s, "typeRef", v.typeRef );
            writeField<std::vector<ADL::sys::adlast::TypeExpr> >( json, parameters_s, "parameters", v.parameters );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( typeRef_s, v.typeRef, "typeRef", json ) ||
                readField( parameters_s, v.parameters, "parameters", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::adlast::NewType>::Ptr
Serialisable<ADL::sys::adlast::NewType>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::NewType _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : typeParams_s( Serialisable<std::vector<std::string> >::serialiser(sf) )
            , typeExpr_s( Serialisable<ADL::sys::adlast::TypeExpr>::serialiser(sf) )
            , default__s( Serialisable<ADL::sys::types::Maybe<JsonValue> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<std::vector<std::string> >::Ptr typeParams_s;
        typename Serialiser<ADL::sys::adlast::TypeExpr>::Ptr typeExpr_s;
        typename Serialiser<ADL::sys::types::Maybe<JsonValue> >::Ptr default__s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::vector<std::string> >( json, typeParams_s, "typeParams", v.typeParams );
            writeField<ADL::sys::adlast::TypeExpr>( json, typeExpr_s, "typeExpr", v.typeExpr );
            writeField<ADL::sys::types::Maybe<JsonValue> >( json, default__s, "default", v.default_ );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( typeParams_s, v.typeParams, "typeParams", json ) ||
                readField( typeExpr_s, v.typeExpr, "typeExpr", json ) ||
                readField( default__s, v.default_, "default", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::adlast::TypeDef_>::Ptr
Serialisable<ADL::sys::adlast::TypeDef_>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::TypeDef_ _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : typeParams_s( Serialisable<std::vector<std::string> >::serialiser(sf) )
            , typeExpr_s( Serialisable<ADL::sys::adlast::TypeExpr>::serialiser(sf) )
            {}
        
        
        typename Serialiser<std::vector<std::string> >::Ptr typeParams_s;
        typename Serialiser<ADL::sys::adlast::TypeExpr>::Ptr typeExpr_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::vector<std::string> >( json, typeParams_s, "typeParams", v.typeParams );
            writeField<ADL::sys::adlast::TypeExpr>( json, typeExpr_s, "typeExpr", v.typeExpr );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( typeParams_s, v.typeParams, "typeParams", json ) ||
                readField( typeExpr_s, v.typeExpr, "typeExpr", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::adlast::Decl>::Ptr
Serialisable<ADL::sys::adlast::Decl>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::Decl _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : name_s( Serialisable<ADL::sys::adlast::Ident>::serialiser(sf) )
            , version_s( Serialisable<ADL::sys::types::Maybe<uint32_t> >::serialiser(sf) )
            , type__s( Serialisable<ADL::sys::adlast::DeclType_>::serialiser(sf) )
            , annotations_s( Serialisable<ADL::sys::adlast::Annotations>::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::sys::adlast::Ident>::Ptr name_s;
        typename Serialiser<ADL::sys::types::Maybe<uint32_t> >::Ptr version_s;
        typename Serialiser<ADL::sys::adlast::DeclType_>::Ptr type__s;
        typename Serialiser<ADL::sys::adlast::Annotations>::Ptr annotations_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::sys::adlast::Ident>( json, name_s, "name", v.name );
            writeField<ADL::sys::types::Maybe<uint32_t> >( json, version_s, "version", v.version );
            writeField<ADL::sys::adlast::DeclType_>( json, type__s, "type_", v.type_ );
            writeField<ADL::sys::adlast::Annotations>( json, annotations_s, "annotations", v.annotations );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( name_s, v.name, "name", json ) ||
                readField( version_s, v.version, "version", json ) ||
                readField( type__s, v.type_, "type_", json ) ||
                readField( annotations_s, v.annotations, "annotations", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::adlast::Field>::Ptr
Serialisable<ADL::sys::adlast::Field>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::Field _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : name_s( Serialisable<ADL::sys::adlast::Ident>::serialiser(sf) )
            , serializedName_s( Serialisable<ADL::sys::adlast::Ident>::serialiser(sf) )
            , typeExpr_s( Serialisable<ADL::sys::adlast::TypeExpr>::serialiser(sf) )
            , default__s( Serialisable<ADL::sys::types::Maybe<JsonValue> >::serialiser(sf) )
            , annotations_s( Serialisable<ADL::sys::adlast::Annotations>::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::sys::adlast::Ident>::Ptr name_s;
        typename Serialiser<ADL::sys::adlast::Ident>::Ptr serializedName_s;
        typename Serialiser<ADL::sys::adlast::TypeExpr>::Ptr typeExpr_s;
        typename Serialiser<ADL::sys::types::Maybe<JsonValue> >::Ptr default__s;
        typename Serialiser<ADL::sys::adlast::Annotations>::Ptr annotations_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::sys::adlast::Ident>( json, name_s, "name", v.name );
            writeField<ADL::sys::adlast::Ident>( json, serializedName_s, "serializedName", v.serializedName );
            writeField<ADL::sys::adlast::TypeExpr>( json, typeExpr_s, "typeExpr", v.typeExpr );
            writeField<ADL::sys::types::Maybe<JsonValue> >( json, default__s, "default", v.default_ );
            writeField<ADL::sys::adlast::Annotations>( json, annotations_s, "annotations", v.annotations );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( name_s, v.name, "name", json ) ||
                readField( serializedName_s, v.serializedName, "serializedName", json ) ||
                readField( typeExpr_s, v.typeExpr, "typeExpr", json ) ||
                readField( default__s, v.default_, "default", json ) ||
                readField( annotations_s, v.annotations, "annotations", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::adlast::Module>::Ptr
Serialisable<ADL::sys::adlast::Module>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::Module _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : name_s( Serialisable<ADL::sys::adlast::ModuleName>::serialiser(sf) )
            , imports_s( Serialisable<std::vector<ADL::sys::adlast::Import> >::serialiser(sf) )
            , decls_s( stringMapSerialiser<ADL::sys::adlast::Decl>(sf) )
            , annotations_s( Serialisable<ADL::sys::adlast::Annotations>::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::sys::adlast::ModuleName>::Ptr name_s;
        typename Serialiser<std::vector<ADL::sys::adlast::Import> >::Ptr imports_s;
        typename Serialiser<std::map<std::string,ADL::sys::adlast::Decl>>::Ptr decls_s;
        typename Serialiser<ADL::sys::adlast::Annotations>::Ptr annotations_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::sys::adlast::ModuleName>( json, name_s, "name", v.name );
            writeField<std::vector<ADL::sys::adlast::Import> >( json, imports_s, "imports", v.imports );
            writeField<std::map<std::string,ADL::sys::adlast::Decl>>( json, decls_s, "decls", v.decls );
            writeField<ADL::sys::adlast::Annotations>( json, annotations_s, "annotations", v.annotations );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( name_s, v.name, "name", json ) ||
                readField( imports_s, v.imports, "imports", json ) ||
                readField( decls_s, v.decls, "decls", json ) ||
                readField( annotations_s, v.annotations, "annotations", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::adlast::ScopedDecl>::Ptr
Serialisable<ADL::sys::adlast::ScopedDecl>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::adlast::ScopedDecl _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : moduleName_s( Serialisable<ADL::sys::adlast::ModuleName>::serialiser(sf) )
            , decl_s( Serialisable<ADL::sys::adlast::Decl>::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::sys::adlast::ModuleName>::Ptr moduleName_s;
        typename Serialiser<ADL::sys::adlast::Decl>::Ptr decl_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::sys::adlast::ModuleName>( json, moduleName_s, "moduleName", v.moduleName );
            writeField<ADL::sys::adlast::Decl>( json, decl_s, "decl", v.decl );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( moduleName_s, v.moduleName, "moduleName", json ) ||
                readField( decl_s, v.decl, "decl", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL