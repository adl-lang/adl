#ifndef SYS_ADLAST_H
#define SYS_ADLAST_H
#include <adl/adl.h>
#include "adl/sys.types.h"
#include <map>
#include <stdint.h>
#include <string>
#include <vector>

namespace ADL {
namespace sys {
namespace adlast {

using Ident = std::string;

class Literal
{
public:
    Literal();
    static Literal mk_null_();
    static Literal mk_integer( const int64_t & v );
    static Literal mk_double_( const double & v );
    static Literal mk_string( const std::string & v );
    static Literal mk_boolean( const bool & v );
    static Literal mk_array( const std::vector<Literal>  & v );
    static Literal mk_object( const std::map<std::string,Literal>  & v );
    
    Literal( const Literal & );
    ~Literal();
    Literal & operator=( const Literal & );
    
    enum DiscType
    {
        NULL_,
        INTEGER,
        DOUBLE_,
        STRING,
        BOOLEAN,
        ARRAY,
        OBJECT
    };
    
    DiscType d() const;
    int64_t & integer() const;
    double & double_() const;
    std::string & string() const;
    bool & boolean() const;
    std::vector<Literal>  & array() const;
    std::map<std::string,Literal>  & object() const;
    
    void set_null_();
    const int64_t & set_integer(const int64_t & );
    const double & set_double_(const double & );
    const std::string & set_string(const std::string & );
    const bool & set_boolean(const bool & );
    const std::vector<Literal>  & set_array(const std::vector<Literal>  & );
    const std::map<std::string,Literal>  & set_object(const std::map<std::string,Literal>  & );
    
private:
    Literal( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const Literal &a, const Literal &b );
bool operator==( const Literal &a, const Literal &b );

inline Literal::DiscType Literal::d() const
{
    return d_;
}

inline int64_t & Literal::integer() const
{
    if( d_ == INTEGER )
    {
        return *(int64_t *)p_;
    }
    throw invalid_union_access();
}

inline double & Literal::double_() const
{
    if( d_ == DOUBLE_ )
    {
        return *(double *)p_;
    }
    throw invalid_union_access();
}

inline std::string & Literal::string() const
{
    if( d_ == STRING )
    {
        return *(std::string *)p_;
    }
    throw invalid_union_access();
}

inline bool & Literal::boolean() const
{
    if( d_ == BOOLEAN )
    {
        return *(bool *)p_;
    }
    throw invalid_union_access();
}

inline std::vector<Literal>  & Literal::array() const
{
    if( d_ == ARRAY )
    {
        return *(std::vector<Literal>  *)p_;
    }
    throw invalid_union_access();
}

inline std::map<std::string,Literal>  & Literal::object() const
{
    if( d_ == OBJECT )
    {
        return *(std::map<std::string,Literal>  *)p_;
    }
    throw invalid_union_access();
}

using ModuleName = std::string;

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

class Import
{
public:
    Import();
    static Import mk_moduleName( const ModuleName & v );
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
    ModuleName & moduleName() const;
    ScopedName & scopedName() const;
    
    const ModuleName & set_moduleName(const ModuleName & );
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

inline ModuleName & Import::moduleName() const
{
    if( d_ == MODULENAME )
    {
        return *(ModuleName *)p_;
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

class TypeRef
{
public:
    TypeRef();
    static TypeRef mk_primitive( const Ident & v );
    static TypeRef mk_typeParam( const Ident & v );
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
    Ident & primitive() const;
    Ident & typeParam() const;
    ScopedName & reference() const;
    
    const Ident & set_primitive(const Ident & );
    const Ident & set_typeParam(const Ident & );
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

inline Ident & TypeRef::primitive() const
{
    if( d_ == PRIMITIVE )
    {
        return *(Ident *)p_;
    }
    throw invalid_union_access();
}

inline Ident & TypeRef::typeParam() const
{
    if( d_ == TYPEPARAM )
    {
        return *(Ident *)p_;
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

struct Field
{
    Field();
    
    Field(
        const Ident & name,
        const TypeExpr & typeExpr,
        const ADL::sys::types::Maybe<Literal>  & default_
        );
    
    Ident name;
    TypeExpr typeExpr;
    ADL::sys::types::Maybe<Literal>  default_;
};

bool operator<( const Field &a, const Field &b );
bool operator==( const Field &a, const Field &b );

struct NewType
{
    NewType();
    
    NewType(
        const std::vector<Ident>  & typeParams,
        const TypeExpr & typeExpr,
        const ADL::sys::types::Maybe<Literal>  & default_
        );
    
    std::vector<Ident>  typeParams;
    TypeExpr typeExpr;
    ADL::sys::types::Maybe<Literal>  default_;
};

bool operator<( const NewType &a, const NewType &b );
bool operator==( const NewType &a, const NewType &b );

struct TypeDef_
{
    TypeDef_();
    
    TypeDef_(
        const std::vector<Ident>  & typeParams,
        const TypeExpr & typeExpr
        );
    
    std::vector<Ident>  typeParams;
    TypeExpr typeExpr;
};

bool operator<( const TypeDef_ &a, const TypeDef_ &b );
bool operator==( const TypeDef_ &a, const TypeDef_ &b );

struct Struct_
{
    Struct_();
    
    Struct_(
        const std::vector<Ident>  & typeParams,
        const std::vector<Field>  & fields
        );
    
    std::vector<Ident>  typeParams;
    std::vector<Field>  fields;
};

bool operator<( const Struct_ &a, const Struct_ &b );
bool operator==( const Struct_ &a, const Struct_ &b );

struct Union_
{
    Union_();
    
    Union_(
        const std::vector<Ident>  & typeParams,
        const std::vector<Field>  & fields
        );
    
    std::vector<Ident>  typeParams;
    std::vector<Field>  fields;
};

bool operator<( const Union_ &a, const Union_ &b );
bool operator==( const Union_ &a, const Union_ &b );

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

struct Decl
{
    Decl();
    
    Decl(
        const Ident & name,
        const ADL::sys::types::Maybe<uint32_t>  & version,
        const DeclType_ & type_
        );
    
    Ident name;
    ADL::sys::types::Maybe<uint32_t>  version;
    DeclType_ type_;
};

bool operator<( const Decl &a, const Decl &b );
bool operator==( const Decl &a, const Decl &b );

using DeclVersions = std::vector<Decl> ;

struct Module
{
    Module();
    
    Module(
        const ModuleName & name,
        const std::vector<Import>  & imports,
        const std::map<Ident,Decl>  & decls
        );
    
    ModuleName name;
    std::vector<Import>  imports;
    std::map<Ident,Decl>  decls;
};

bool operator<( const Module &a, const Module &b );
bool operator==( const Module &a, const Module &b );

}}}; // ADL::sys::adlast

namespace ADL {

template <>
struct Serialisable<ADL::sys::adlast::Literal>
{
    static Serialiser<ADL::sys::adlast::Literal>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::ScopedName>
{
    static Serialiser<ADL::sys::adlast::ScopedName>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Import>
{
    static Serialiser<ADL::sys::adlast::Import>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::TypeRef>
{
    static Serialiser<ADL::sys::adlast::TypeRef>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::TypeExpr>
{
    static Serialiser<ADL::sys::adlast::TypeExpr>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Field>
{
    static Serialiser<ADL::sys::adlast::Field>::Ptr serialiser(const SerialiserFlags &);
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
struct Serialisable<ADL::sys::adlast::Struct_>
{
    static Serialiser<ADL::sys::adlast::Struct_>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Union_>
{
    static Serialiser<ADL::sys::adlast::Union_>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::DeclType_>
{
    static Serialiser<ADL::sys::adlast::DeclType_>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Decl>
{
    static Serialiser<ADL::sys::adlast::Decl>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::adlast::Module>
{
    static Serialiser<ADL::sys::adlast::Module>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // SYS_ADLAST_H