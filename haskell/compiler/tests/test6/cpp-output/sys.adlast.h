// @generated from adl module sys.adlast
#ifndef SYS_ADLAST_H
#define SYS_ADLAST_H
#include <adl/adl.h>
#include <map>
#include <stdint.h>
#include <string>
#include "sys.types.h"
#include <vector>

namespace ADL {
namespace sys {
namespace adlast {

struct Decl;

struct Field;

struct Import;

struct NewType;

struct ScopedName;

struct Struct_;

struct TypeDef_;

struct Union_;

class DeclType_
{
public:
    DeclType_();
    static DeclType_ mk_struct_( const Struct_ & v );
    static DeclType_ mk_union_( const Union_ & v );
    static DeclType_ mk_type_( const TypeDef_ & v );
    static DeclType_ mk_newtype_( const NewType & v );
    
    DeclType_( const DeclType_ & );
    ~DeclType_();
    DeclType_ & operator=( const DeclType_ & );
    
    enum DiscType
    {
        STRUCT_,
        UNION_,
        TYPE_,
        NEWTYPE_
    };
    
    DiscType d() const;
    
    template<class Visitor>
    void visit(Visitor vis) const
    {
        switch (d())
        {
        case STRUCT_: { vis.struct_(struct_()); break; }
        case UNION_: { vis.union_(union_()); break; }
        case TYPE_: { vis.type_(type_()); break; }
        case NEWTYPE_: { vis.newtype_(newtype_()); break; }
        }
    }
    
    bool is_struct_() const { return d_ == STRUCT_; };
    bool is_union_() const { return d_ == UNION_; };
    bool is_type_() const { return d_ == TYPE_; };
    bool is_newtype_() const { return d_ == NEWTYPE_; };
    
    Struct_ & struct_() const;
    Union_ & union_() const;
    TypeDef_ & type_() const;
    NewType & newtype_() const;
    
    const Struct_ & set_struct_(const Struct_ & );
    const Union_ & set_union_(const Union_ & );
    const TypeDef_ & set_type_(const TypeDef_ & );
    const NewType & set_newtype_(const NewType & );
    
private:
    DeclType_( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const DeclType_ &a, const DeclType_ &b );
bool operator==( const DeclType_ &a, const DeclType_ &b );

inline DeclType_::DiscType DeclType_::d() const
{
    return d_;
}

inline Struct_ & DeclType_::struct_() const
{
    if( d_ == STRUCT_ )
    {
        return *(Struct_ *)p_;
    }
    throw invalid_union_access();
}

inline Union_ & DeclType_::union_() const
{
    if( d_ == UNION_ )
    {
        return *(Union_ *)p_;
    }
    throw invalid_union_access();
}

inline TypeDef_ & DeclType_::type_() const
{
    if( d_ == TYPE_ )
    {
        return *(TypeDef_ *)p_;
    }
    throw invalid_union_access();
}

inline NewType & DeclType_::newtype_() const
{
    if( d_ == NEWTYPE_ )
    {
        return *(NewType *)p_;
    }
    throw invalid_union_access();
}

using DeclVersions = std::vector<Decl> ;

using Ident = std::string;

class Import
{
public:
    Import();
    static Import mk_moduleName( const std::string & v );
    static Import mk_scopedName( const ScopedName & v );
    
    Import( const Import & );
    ~Import();
    Import & operator=( const Import & );
    
    enum DiscType
    {
        MODULENAME,
        SCOPEDNAME
    };
    
    DiscType d() const;
    
    template<class Visitor>
    void visit(Visitor vis) const
    {
        switch (d())
        {
        case MODULENAME: { vis.moduleName(moduleName()); break; }
        case SCOPEDNAME: { vis.scopedName(scopedName()); break; }
        }
    }
    
    bool is_moduleName() const { return d_ == MODULENAME; };
    bool is_scopedName() const { return d_ == SCOPEDNAME; };
    
    std::string & moduleName() const;
    ScopedName & scopedName() const;
    
    const std::string & set_moduleName(const std::string & );
    const ScopedName & set_scopedName(const ScopedName & );
    
private:
    Import( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const Import &a, const Import &b );
bool operator==( const Import &a, const Import &b );

inline Import::DiscType Import::d() const
{
    return d_;
}

inline std::string & Import::moduleName() const
{
    if( d_ == MODULENAME )
    {
        return *(std::string *)p_;
    }
    throw invalid_union_access();
}

inline ScopedName & Import::scopedName() const
{
    if( d_ == SCOPEDNAME )
    {
        return *(ScopedName *)p_;
    }
    throw invalid_union_access();
}

using ModuleName = std::string;

struct Struct_
{
    Struct_();
    
    Struct_(
        const std::vector<std::string>  & typeParams,
        const std::vector<Field>  & fields
        );
    
    std::vector<std::string>  typeParams;
    std::vector<Field>  fields;
};

bool operator<( const Struct_ &a, const Struct_ &b );
bool operator==( const Struct_ &a, const Struct_ &b );

class TypeRef
{
public:
    TypeRef();
    static TypeRef mk_primitive( const std::string & v );
    static TypeRef mk_typeParam( const std::string & v );
    static TypeRef mk_reference( const ScopedName & v );
    
    TypeRef( const TypeRef & );
    ~TypeRef();
    TypeRef & operator=( const TypeRef & );
    
    enum DiscType
    {
        PRIMITIVE,
        TYPEPARAM,
        REFERENCE
    };
    
    DiscType d() const;
    
    template<class Visitor>
    void visit(Visitor vis) const
    {
        switch (d())
        {
        case PRIMITIVE: { vis.primitive(primitive()); break; }
        case TYPEPARAM: { vis.typeParam(typeParam()); break; }
        case REFERENCE: { vis.reference(reference()); break; }
        }
    }
    
    bool is_primitive() const { return d_ == PRIMITIVE; };
    bool is_typeParam() const { return d_ == TYPEPARAM; };
    bool is_reference() const { return d_ == REFERENCE; };
    
    std::string & primitive() const;
    std::string & typeParam() const;
    ScopedName & reference() const;
    
    const std::string & set_primitive(const std::string & );
    const std::string & set_typeParam(const std::string & );
    const ScopedName & set_reference(const ScopedName & );
    
private:
    TypeRef( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const TypeRef &a, const TypeRef &b );
bool operator==( const TypeRef &a, const TypeRef &b );

inline TypeRef::DiscType TypeRef::d() const
{
    return d_;
}

inline std::string & TypeRef::primitive() const
{
    if( d_ == PRIMITIVE )
    {
        return *(std::string *)p_;
    }
    throw invalid_union_access();
}

inline std::string & TypeRef::typeParam() const
{
    if( d_ == TYPEPARAM )
    {
        return *(std::string *)p_;
    }
    throw invalid_union_access();
}

inline ScopedName & TypeRef::reference() const
{
    if( d_ == REFERENCE )
    {
        return *(ScopedName *)p_;
    }
    throw invalid_union_access();
}

struct Union_
{
    Union_();
    
    Union_(
        const std::vector<std::string>  & typeParams,
        const std::vector<Field>  & fields
        );
    
    std::vector<std::string>  typeParams;
    std::vector<Field>  fields;
};

bool operator<( const Union_ &a, const Union_ &b );
bool operator==( const Union_ &a, const Union_ &b );

struct ScopedName
{
    ScopedName();
    
    ScopedName(
        const ModuleName & moduleName,
        const Ident & name
        );
    
    ModuleName moduleName;
    Ident name;
};

bool operator<( const ScopedName &a, const ScopedName &b );
bool operator==( const ScopedName &a, const ScopedName &b );

struct TypeExpr
{
    TypeExpr();
    
    TypeExpr(
        const TypeRef & typeRef,
        const std::vector<TypeExpr>  & parameters
        );
    
    TypeRef typeRef;
    std::vector<TypeExpr>  parameters;
};

bool operator<( const TypeExpr &a, const TypeExpr &b );
bool operator==( const TypeExpr &a, const TypeExpr &b );

using Annotations = std::map<ScopedName,JsonValue> ;

struct NewType
{
    NewType();
    
    NewType(
        const std::vector<std::string>  & typeParams,
        const TypeExpr & typeExpr,
        const ADL::sys::types::Maybe<JsonValue>  & default_
        );
    
    std::vector<std::string>  typeParams;
    TypeExpr typeExpr;
    ADL::sys::types::Maybe<JsonValue>  default_;
};

bool operator<( const NewType &a, const NewType &b );
bool operator==( const NewType &a, const NewType &b );

struct TypeDef_
{
    TypeDef_();
    
    TypeDef_(
        const std::vector<std::string>  & typeParams,
        const TypeExpr & typeExpr
        );
    
    std::vector<std::string>  typeParams;
    TypeExpr typeExpr;
};

bool operator<( const TypeDef_ &a, const TypeDef_ &b );
bool operator==( const TypeDef_ &a, const TypeDef_ &b );

struct Decl
{
    Decl();
    
    Decl(
        const Ident & name,
        const ADL::sys::types::Maybe<uint32_t>  & version,
        const DeclType_ & type_,
        const Annotations & annotations
        );
    
    Ident name;
    ADL::sys::types::Maybe<uint32_t>  version;
    DeclType_ type_;
    Annotations annotations;
};

bool operator<( const Decl &a, const Decl &b );
bool operator==( const Decl &a, const Decl &b );

struct Field
{
    Field();
    
    Field(
        const Ident & name,
        const Ident & serializedName,
        const TypeExpr & typeExpr,
        const ADL::sys::types::Maybe<JsonValue>  & default_,
        const Annotations & annotations
        );
    
    Ident name;
    Ident serializedName;
    TypeExpr typeExpr;
    ADL::sys::types::Maybe<JsonValue>  default_;
    Annotations annotations;
};

bool operator<( const Field &a, const Field &b );
bool operator==( const Field &a, const Field &b );

struct Module
{
    Module();
    
    Module(
        const ModuleName & name,
        const std::vector<Import>  & imports,
        const std::map<std::string,Decl> & decls,
        const Annotations & annotations
        );
    
    ModuleName name;
    std::vector<Import>  imports;
    std::map<std::string,Decl> decls;
    Annotations annotations;
};

bool operator<( const Module &a, const Module &b );
bool operator==( const Module &a, const Module &b );

struct ScopedDecl
{
    ScopedDecl();
    
    ScopedDecl(
        const ModuleName & moduleName,
        const Decl & decl
        );
    
    ModuleName moduleName;
    Decl decl;
};

bool operator<( const ScopedDecl &a, const ScopedDecl &b );
bool operator==( const ScopedDecl &a, const ScopedDecl &b );

}}}; // ADL::sys::adlast

namespace ADL {

template <>
struct Serialisable<ADL::sys::adlast::DeclType_>
{
    static Serialiser<ADL::sys::adlast::DeclType_>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Import>
{
    static Serialiser<ADL::sys::adlast::Import>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Struct_>
{
    static Serialiser<ADL::sys::adlast::Struct_>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::TypeRef>
{
    static Serialiser<ADL::sys::adlast::TypeRef>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Union_>
{
    static Serialiser<ADL::sys::adlast::Union_>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::ScopedName>
{
    static Serialiser<ADL::sys::adlast::ScopedName>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::TypeExpr>
{
    static Serialiser<ADL::sys::adlast::TypeExpr>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::NewType>
{
    static Serialiser<ADL::sys::adlast::NewType>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::TypeDef_>
{
    static Serialiser<ADL::sys::adlast::TypeDef_>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Decl>
{
    static Serialiser<ADL::sys::adlast::Decl>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Field>
{
    static Serialiser<ADL::sys::adlast::Field>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Module>
{
    static Serialiser<ADL::sys::adlast::Module>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::ScopedDecl>
{
    static Serialiser<ADL::sys::adlast::ScopedDecl>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // SYS_ADLAST_H
