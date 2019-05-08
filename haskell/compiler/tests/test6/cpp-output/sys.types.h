#ifndef SYS_TYPES_H
#define SYS_TYPES_H
#include <adl/adl.h>
#include <map>
#include <set>
#include <string>
#include <utility>

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
    return *this;
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
    return 0;
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
    return false;
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
    return false;
}

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
    return *this;
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
    return 0;
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
    return false;
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
    return false;
}

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
    return *this;
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
    return 0;
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
    return false;
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
    return false;
}

// Pair excluded due to custom definition

template <class A, class B>
using Pair = std::pair<A,B>;

// Set excluded due to custom definition

template <class A>
using Set = std::set<A>;

// Map excluded due to custom definition

template <class K, class V>
using Map = std::map<K,V>;

}}}; // ADL::sys::types

namespace ADL {

template <class T1, class T2>
struct Serialisable<ADL::sys::types::Either<T1,T2>>
{
    static typename Serialiser<ADL::sys::types::Either<T1,T2>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T1, class T2>
typename Serialiser<ADL::sys::types::Either<T1,T2>>::Ptr
Serialisable<ADL::sys::types::Either<T1,T2>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::types::Either<T1,T2> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<T1>::Ptr left_;
        mutable typename Serialiser<T2>::Ptr right_;
        
        typename Serialiser<T1>::Ptr left_s() const
        {
            if( !left_ )
                left_ = Serialisable<T1>::serialiser(sf_);
            return left_;
        }
        
        typename Serialiser<T2>::Ptr right_s() const
        {
            if( !right_ )
                right_ = Serialisable<T2>::serialiser(sf_);
            return right_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::sys::types::Either<T1,T2>::LEFT: json.startObject(); writeField( json, left_s(), "left", v.left() ); json.endObject(); break;
                case ADL::sys::types::Either<T1,T2>::RIGHT: json.startObject(); writeField( json, right_s(), "right", v.right() ); json.endObject(); break;
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
                    if( matchField0( "left", json ) )
                        v.set_left(left_s()->fromJson( json ));
                    else if( matchField0( "right", json ) )
                        v.set_right(right_s()->fromJson( json ));
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

template <class T>
struct Serialisable<ADL::sys::types::Error<T>>
{
    static typename Serialiser<ADL::sys::types::Error<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::sys::types::Error<T>>::Ptr
Serialisable<ADL::sys::types::Error<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::types::Error<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<T>::Ptr value_;
        mutable typename Serialiser<std::string>::Ptr error_;
        
        typename Serialiser<T>::Ptr value_s() const
        {
            if( !value_ )
                value_ = Serialisable<T>::serialiser(sf_);
            return value_;
        }
        
        typename Serialiser<std::string>::Ptr error_s() const
        {
            if( !error_ )
                error_ = Serialisable<std::string>::serialiser(sf_);
            return error_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::sys::types::Error<T>::VALUE: json.startObject(); writeField( json, value_s(), "value", v.value() ); json.endObject(); break;
                case ADL::sys::types::Error<T>::ERROR: json.startObject(); writeField( json, error_s(), "error", v.error() ); json.endObject(); break;
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
                    if( matchField0( "value", json ) )
                        v.set_value(value_s()->fromJson( json ));
                    else if( matchField0( "error", json ) )
                        v.set_error(error_s()->fromJson( json ));
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

template <class T>
struct Serialisable<ADL::sys::types::Maybe<T>>
{
    static typename Serialiser<ADL::sys::types::Maybe<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::sys::types::Maybe<T>>::Ptr
Serialisable<ADL::sys::types::Maybe<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::types::Maybe<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<Void>::Ptr nothing_;
        mutable typename Serialiser<T>::Ptr just_;
        
        typename Serialiser<Void>::Ptr nothing_s() const
        {
            if( !nothing_ )
                nothing_ = Serialisable<Void>::serialiser(sf_);
            return nothing_;
        }
        
        typename Serialiser<T>::Ptr just_s() const
        {
            if( !just_ )
                just_ = Serialisable<T>::serialiser(sf_);
            return just_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::sys::types::Maybe<T>::NOTHING: json.stringV( "nothing" ); break;
                case ADL::sys::types::Maybe<T>::JUST: json.startObject(); writeField( json, just_s(), "just", v.just() ); json.endObject(); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::STRING )
            {
                if( json.stringV() == "nothing" )
                    v.set_nothing();
                else
                    throw json_parse_failure();
                json.next();
                return;
            }
            if( json.type() == JsonReader::START_OBJECT )
            {
                match( json, JsonReader::START_OBJECT );
                if( json.type() == JsonReader::END_OBJECT )
                    throw json_parse_failure();
                while( !match0( json, JsonReader::END_OBJECT ) )
                {
                    if( matchField0( "just", json ) )
                        v.set_just(just_s()->fromJson( json ));
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

template <class A,class B>
struct Serialisable<std::pair<A,B>>
{
    typedef std::pair<A,B> P;

    static typename Serialiser<P>::Ptr serialiser(const SerialiserFlags &sf)
    {
        struct S : public Serialiser<P>
        {
            S( const SerialiserFlags &sf )
                : v1_s( Serialisable<A>::serialiser(sf) )
                , v2_s( Serialisable<B>::serialiser(sf) )
                {}

            typename Serialiser<A>::Ptr v1_s;
            typename Serialiser<B>::Ptr v2_s;

            void toJson( JsonWriter &json, const P & v ) const
            {
                json.startObject();
                writeField<A>( json, v1_s, "v1", v.first );
                writeField<B>( json, v2_s, "v2", v.second );
                json.endObject();
            }

            void fromJson( P &v, JsonReader &json ) const
            {
                match( json, JsonReader::START_OBJECT );
                while( !match0( json, JsonReader::END_OBJECT ) )
                {
                    readField<A>( v1_s, v.first, "v1", json ) ||
                        readField<B>( v2_s, v.second, "v2", json ) ||
                        ignoreField( json );
                }
            }
        };

        return typename Serialiser<P>::Ptr( new S(sf) );
    }
};

template <class A>
struct Serialisable<std::set<A>>
{
    typedef std::set<A> S;

    static typename Serialiser<S>::Ptr serialiser(const SerialiserFlags &sf)
    {
        struct S : public Serialiser<S>
        {
            S( const SerialiserFlags &sf )
                : s( Serialisable<A>::serialiser(sf) )
                {}

            typename Serialiser<A>::Ptr s;

            void toJson( JsonWriter &json, const S & v ) const
            {
                json.startArray();
                for( typename std::set<A>::const_iterator i = v.begin(); i != v.end(); i++ )
                    s->toJson( json, *i );
                json.endArray();
            }

            void fromJson( S &v, JsonReader &json ) const
            {
                match( json, JsonReader::START_ARRAY );
                while( !match0( json, JsonReader::END_ARRAY ) )
                    v.insert( s->fromJson(json) );
            }
        };

        return typename Serialiser<S>::Ptr( new S(sf) );
    }
};

template <class K,class V>
struct Serialisable<std::map<K,V>>
{
    typedef std::map<K,V> M;

    static typename Serialiser<M>::Ptr serialiser(const SerialiserFlags &sf)
    {
        struct S : public Serialiser<M>
        {
            S( const SerialiserFlags &sf )
                : s( Serialisable<std::pair<K,V>>::serialiser(sf) )
                {}

            typename Serialiser<std::pair<K,V>>::Ptr s;

            void toJson( JsonWriter &json, const M & v ) const
            {
                json.startArray();
                for( typename std::map<K,V>::const_iterator i = v.begin(); i != v.end(); i++ )
                    s->toJson( json, *i );
                json.endArray();
            }

            void fromJson( M &v, JsonReader &json ) const
            {
                std::pair<K,V> pv;
                s->fromJson( pv, json );
                v[pv.first] = pv.second;
            }
        };

        return typename Serialiser<M>::Ptr (new S(sf));
    }
};

}; // ADL
#endif // SYS_TYPES_H