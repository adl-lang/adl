#include <adl/adl.h>
#include <string>

namespace ADL {
namespace sys {
namespace types {

template <class T1, class T2>
class Either
{
public:
    Either();
    static Either<T1,T2> mk_left( const T1 & v );
    static Either<T1,T2> mk_right( const T2 & v );
    
    Either( const Either<T1,T2> & );
    ~Either();
    Either<T1,T2> & operator=( const Either<T1,T2> & );
    
    enum DiscType
    {
        LEFT,
        RIGHT
    };
    
    DiscType d() const;
    T1 & left() const;
    T2 & right() const;
    
    const T1 & set_left(const T1 & );
    const T2 & set_right(const T2 & );
    
private:
    Either( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

template <class T1, class T2>
bool operator<( const Either<T1,T2> &a, const Either<T1,T2> &b );
template <class T1, class T2>
bool operator==( const Either<T1,T2> &a, const Either<T1,T2> &b );

}}} // ADL::sys::types

namespace ADL {

template <class T1, class T2>
struct JsonV<ADL::sys::types::Either<T1,T2>>
{
    static void toJson( JsonWriter &json, const ADL::sys::types::Either<T1,T2> & v );
    static void fromJson( ADL::sys::types::Either<T1,T2> &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace sys {
namespace types {

template <class T1, class T2>
typename Either<T1,T2>::DiscType Either<T1,T2>::d() const
{
    return d_;
}

template <class T1, class T2>
inline T1 & Either<T1,T2>::left() const
{
    if( d_ == LEFT )
    {
        return *(T1 *)p_;
    }
    throw invalid_union_access();
}

template <class T1, class T2>
inline T2 & Either<T1,T2>::right() const
{
    if( d_ == RIGHT )
    {
        return *(T2 *)p_;
    }
    throw invalid_union_access();
}

template <class T1, class T2>
Either<T1,T2>::Either()
    : d_(LEFT), p_(new T1())
{
}

template <class T1, class T2>
Either<T1,T2> Either<T1,T2>::mk_left( const T1 & v )
{
    return Either<T1,T2>( LEFT, new T1(v) );
}

template <class T1, class T2>
Either<T1,T2> Either<T1,T2>::mk_right( const T2 & v )
{
    return Either<T1,T2>( RIGHT, new T2(v) );
}

template <class T1, class T2>
Either<T1,T2>::Either( const Either<T1,T2> & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

template <class T1, class T2>
Either<T1,T2>::~Either()
{
    free(d_,p_);
}

template <class T1, class T2>
Either<T1,T2> & Either<T1,T2>::operator=( const Either<T1,T2> & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
}

template <class T1, class T2>
const T1 & Either<T1,T2>::set_left(const T1 &v)
{
    if( d_ == LEFT )
    {
        *(T1 *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = LEFT;
        p_ = new T1(v);
    }
    return *(T1 *)p_;
}

template <class T1, class T2>
const T2 & Either<T1,T2>::set_right(const T2 &v)
{
    if( d_ == RIGHT )
    {
        *(T2 *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = RIGHT;
        p_ = new T2(v);
    }
    return *(T2 *)p_;
}

template <class T1, class T2>
Either<T1,T2>::Either(DiscType d, void *p)
    : d_(d), p_(p)
{
}

template <class T1, class T2>
void Either<T1,T2>::free(DiscType d, void *p)
{
    switch( d )
    {
        case LEFT: delete (T1 *)p; return;
        case RIGHT: delete (T2 *)p; return;
    }
}

template <class T1, class T2>
void * Either<T1,T2>::copy( DiscType d, void *p )
{
    switch( d )
    {
        case LEFT: return new T1(*(T1 *)p);
        case RIGHT: return new T2(*(T2 *)p);
    }
}

template <class T1, class T2>
bool
operator<( const Either<T1,T2> &a, const Either<T1,T2> &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case Either<T1,T2>::LEFT: return a.left() < b.left();
        case Either<T1,T2>::RIGHT: return a.right() < b.right();
    }
}

template <class T1, class T2>
bool
operator==( const Either<T1,T2> &a, const Either<T1,T2> &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case Either<T1,T2>::LEFT: return a.left() == b.left();
        case Either<T1,T2>::RIGHT: return a.right() == b.right();
    }
}

}}} // ADL::sys::types

namespace ADL {

template <class T1, class T2>
void
JsonV<ADL::sys::types::Either<T1,T2>>::toJson( JsonWriter &json, const ADL::sys::types::Either<T1,T2> & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::sys::types::Either<T1,T2>::LEFT: writeField( json, "left", v.left() ); break;
        case ADL::sys::types::Either<T1,T2>::RIGHT: writeField( json, "right", v.right() ); break;
    }
    json.endObject();
}

template <class T1, class T2>
void
JsonV<ADL::sys::types::Either<T1,T2>>::fromJson( ADL::sys::types::Either<T1,T2> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( match0( json, JsonReader::FIELD ) )
    {
        if( json.fieldName() == "left" )
        {
            T1 fv;
            JsonV<T1>::fromJson( fv, json );
            v.set_left(fv);
        }
        else if( json.fieldName() == "right" )
        {
            T2 fv;
            JsonV<T2>::fromJson( fv, json );
            v.set_right(fv);
        }
        else
            throw json_parse_failure();
    }
    match( json, JsonReader::END_OBJECT );
}

} // ADL

namespace ADL {
namespace sys {
namespace types {

template <class T>
class Error
{
public:
    Error();
    static Error<T> mk_value( const T & v );
    static Error<T> mk_error( const std::string & v );
    
    Error( const Error<T> & );
    ~Error();
    Error<T> & operator=( const Error<T> & );
    
    enum DiscType
    {
        VALUE,
        ERROR
    };
    
    DiscType d() const;
    T & value() const;
    std::string & error() const;
    
    const T & set_value(const T & );
    const std::string & set_error(const std::string & );
    
private:
    Error( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

template <class T>
bool operator<( const Error<T> &a, const Error<T> &b );
template <class T>
bool operator==( const Error<T> &a, const Error<T> &b );

}}} // ADL::sys::types

namespace ADL {

template <class T>
struct JsonV<ADL::sys::types::Error<T>>
{
    static void toJson( JsonWriter &json, const ADL::sys::types::Error<T> & v );
    static void fromJson( ADL::sys::types::Error<T> &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace sys {
namespace types {

template <class T>
typename Error<T>::DiscType Error<T>::d() const
{
    return d_;
}

template <class T>
inline T & Error<T>::value() const
{
    if( d_ == VALUE )
    {
        return *(T *)p_;
    }
    throw invalid_union_access();
}

template <class T>
inline std::string & Error<T>::error() const
{
    if( d_ == ERROR )
    {
        return *(std::string *)p_;
    }
    throw invalid_union_access();
}

template <class T>
Error<T>::Error()
    : d_(VALUE), p_(new T())
{
}

template <class T>
Error<T> Error<T>::mk_value( const T & v )
{
    return Error<T>( VALUE, new T(v) );
}

template <class T>
Error<T> Error<T>::mk_error( const std::string & v )
{
    return Error<T>( ERROR, new std::string(v) );
}

template <class T>
Error<T>::Error( const Error<T> & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

template <class T>
Error<T>::~Error()
{
    free(d_,p_);
}

template <class T>
Error<T> & Error<T>::operator=( const Error<T> & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
}

template <class T>
const T & Error<T>::set_value(const T &v)
{
    if( d_ == VALUE )
    {
        *(T *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = VALUE;
        p_ = new T(v);
    }
    return *(T *)p_;
}

template <class T>
const std::string & Error<T>::set_error(const std::string &v)
{
    if( d_ == ERROR )
    {
        *(std::string *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = ERROR;
        p_ = new std::string(v);
    }
    return *(std::string *)p_;
}

template <class T>
Error<T>::Error(DiscType d, void *p)
    : d_(d), p_(p)
{
}

template <class T>
void Error<T>::free(DiscType d, void *p)
{
    switch( d )
    {
        case VALUE: delete (T *)p; return;
        case ERROR: delete (std::string *)p; return;
    }
}

template <class T>
void * Error<T>::copy( DiscType d, void *p )
{
    switch( d )
    {
        case VALUE: return new T(*(T *)p);
        case ERROR: return new std::string(*(std::string *)p);
    }
}

template <class T>
bool
operator<( const Error<T> &a, const Error<T> &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case Error<T>::VALUE: return a.value() < b.value();
        case Error<T>::ERROR: return a.error() < b.error();
    }
}

template <class T>
bool
operator==( const Error<T> &a, const Error<T> &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case Error<T>::VALUE: return a.value() == b.value();
        case Error<T>::ERROR: return a.error() == b.error();
    }
}

}}} // ADL::sys::types

namespace ADL {

template <class T>
void
JsonV<ADL::sys::types::Error<T>>::toJson( JsonWriter &json, const ADL::sys::types::Error<T> & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::sys::types::Error<T>::VALUE: writeField( json, "value", v.value() ); break;
        case ADL::sys::types::Error<T>::ERROR: writeField( json, "error", v.error() ); break;
    }
    json.endObject();
}

template <class T>
void
JsonV<ADL::sys::types::Error<T>>::fromJson( ADL::sys::types::Error<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( match0( json, JsonReader::FIELD ) )
    {
        if( json.fieldName() == "value" )
        {
            T fv;
            JsonV<T>::fromJson( fv, json );
            v.set_value(fv);
        }
        else if( json.fieldName() == "error" )
        {
            std::string fv;
            JsonV<std::string>::fromJson( fv, json );
            v.set_error(fv);
        }
        else
            throw json_parse_failure();
    }
    match( json, JsonReader::END_OBJECT );
}

} // ADL

namespace ADL {
namespace sys {
namespace types {

template <class T>
class Maybe
{
public:
    Maybe();
    static Maybe<T> mk_nothing();
    static Maybe<T> mk_just( const T & v );
    
    Maybe( const Maybe<T> & );
    ~Maybe();
    Maybe<T> & operator=( const Maybe<T> & );
    
    enum DiscType
    {
        NOTHING,
        JUST
    };
    
    DiscType d() const;
    T & just() const;
    
    void set_nothing();
    const T & set_just(const T & );
    
private:
    Maybe( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

template <class T>
bool operator<( const Maybe<T> &a, const Maybe<T> &b );
template <class T>
bool operator==( const Maybe<T> &a, const Maybe<T> &b );

}}} // ADL::sys::types

namespace ADL {

template <class T>
struct JsonV<ADL::sys::types::Maybe<T>>
{
    static void toJson( JsonWriter &json, const ADL::sys::types::Maybe<T> & v );
    static void fromJson( ADL::sys::types::Maybe<T> &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace sys {
namespace types {

template <class T>
typename Maybe<T>::DiscType Maybe<T>::d() const
{
    return d_;
}

template <class T>
inline T & Maybe<T>::just() const
{
    if( d_ == JUST )
    {
        return *(T *)p_;
    }
    throw invalid_union_access();
}

template <class T>
Maybe<T>::Maybe()
    : d_(NOTHING), p_(0)
{
}

template <class T>
Maybe<T> Maybe<T>::mk_nothing()
{
    return Maybe<T>( NOTHING, 0 );
}

template <class T>
Maybe<T> Maybe<T>::mk_just( const T & v )
{
    return Maybe<T>( JUST, new T(v) );
}

template <class T>
Maybe<T>::Maybe( const Maybe<T> & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

template <class T>
Maybe<T>::~Maybe()
{
    free(d_,p_);
}

template <class T>
Maybe<T> & Maybe<T>::operator=( const Maybe<T> & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
}

template <class T>
void Maybe<T>::set_nothing()
{
    if( d_ != NOTHING )
    {
        free(d_,p_);
        d_ = NOTHING;
        p_ = 0;
    }
}

template <class T>
const T & Maybe<T>::set_just(const T &v)
{
    if( d_ == JUST )
    {
        *(T *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = JUST;
        p_ = new T(v);
    }
    return *(T *)p_;
}

template <class T>
Maybe<T>::Maybe(DiscType d, void *p)
    : d_(d), p_(p)
{
}

template <class T>
void Maybe<T>::free(DiscType d, void *p)
{
    switch( d )
    {
        case NOTHING: return;
        case JUST: delete (T *)p; return;
    }
}

template <class T>
void * Maybe<T>::copy( DiscType d, void *p )
{
    switch( d )
    {
        case NOTHING: return 0;
        case JUST: return new T(*(T *)p);
    }
}

template <class T>
bool
operator<( const Maybe<T> &a, const Maybe<T> &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case Maybe<T>::NOTHING: return false;
        case Maybe<T>::JUST: return a.just() < b.just();
    }
}

template <class T>
bool
operator==( const Maybe<T> &a, const Maybe<T> &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case Maybe<T>::NOTHING: return true;
        case Maybe<T>::JUST: return a.just() == b.just();
    }
}

}}} // ADL::sys::types

namespace ADL {

template <class T>
void
JsonV<ADL::sys::types::Maybe<T>>::toJson( JsonWriter &json, const ADL::sys::types::Maybe<T> & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::sys::types::Maybe<T>::NOTHING: writeField( json, "nothing", Void() ); break;
        case ADL::sys::types::Maybe<T>::JUST: writeField( json, "just", v.just() ); break;
    }
    json.endObject();
}

template <class T>
void
JsonV<ADL::sys::types::Maybe<T>>::fromJson( ADL::sys::types::Maybe<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( match0( json, JsonReader::FIELD ) )
    {
        if( json.fieldName() == "nothing" )
        {
            Void fv;
            JsonV<Void>::fromJson( fv, json );
            v.set_nothing();
        }
        else if( json.fieldName() == "just" )
        {
            T fv;
            JsonV<T>::fromJson( fv, json );
            v.set_just(fv);
        }
        else
            throw json_parse_failure();
    }
    match( json, JsonReader::END_OBJECT );
}

} // ADL

namespace ADL {
namespace sys {
namespace types {

// Pair excluded due to custom definition

// Set excluded due to custom definition

// Map excluded due to custom definition
}}} // ADL::sys::types