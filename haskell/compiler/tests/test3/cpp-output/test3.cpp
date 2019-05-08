#include "test3.h"

namespace ADL {
namespace test3 {

A::A()
    : f_int(0)
    , f_bool(false)
{
}

A::A(
    const int16_t & f_int_,
    const std::string & f_string_,
    const bool & f_bool_
    )
    : f_int(f_int_)
    , f_string(f_string_)
    , f_bool(f_bool_)
{
}

bool
operator<( const A &a, const A &b )
{
    if( a.f_int < b.f_int ) return true;
    if( b.f_int < a.f_int ) return false;
    if( a.f_string < b.f_string ) return true;
    if( b.f_string < a.f_string ) return false;
    if( a.f_bool < b.f_bool ) return true;
    if( b.f_bool < a.f_bool ) return false;
    return false;
}

bool
operator==( const A &a, const A &b )
{
    return
        a.f_int == b.f_int &&
        a.f_string == b.f_string &&
        a.f_bool == b.f_bool ;
}

E::E()
    : d_(V1), p_(0)
{
}

E E::mk_v1()
{
    return E( V1, 0 );
}

E E::mk_v2()
{
    return E( V2, 0 );
}

E::E( const E & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

E::~E()
{
    free(d_,p_);
}

E & E::operator=( const E & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

void E::set_v1()
{
    if( d_ != V1 )
    {
        free(d_,p_);
        d_ = V1;
        p_ = 0;
    }
}

void E::set_v2()
{
    if( d_ != V2 )
    {
        free(d_,p_);
        d_ = V2;
        p_ = 0;
    }
}

E::E(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void E::free(DiscType d, void *p)
{
    switch( d )
    {
        case V1: return;
        case V2: return;
    }
}

void * E::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V1: return 0;
        case V2: return 0;
    }
    return 0;
}

bool
operator<( const E &a, const E &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case E::V1: return false;
        case E::V2: return false;
    }
    return false;
}

bool
operator==( const E &a, const E &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case E::V1: return true;
        case E::V2: return true;
    }
    return false;
}

U::U()
    : d_(F_INT), p_(new int16_t(0))
{
}

U U::mk_f_int( const int16_t & v )
{
    return U( F_INT, new int16_t(v) );
}

U U::mk_f_string( const std::string & v )
{
    return U( F_STRING, new std::string(v) );
}

U U::mk_f_void()
{
    return U( F_VOID, 0 );
}

U::U( const U & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

U::~U()
{
    free(d_,p_);
}

U & U::operator=( const U & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const int16_t & U::set_f_int(const int16_t &v)
{
    if( d_ == F_INT )
    {
        *(int16_t *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F_INT;
        p_ = new int16_t(v);
    }
    return *(int16_t *)p_;
}

const std::string & U::set_f_string(const std::string &v)
{
    if( d_ == F_STRING )
    {
        *(std::string *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F_STRING;
        p_ = new std::string(v);
    }
    return *(std::string *)p_;
}

void U::set_f_void()
{
    if( d_ != F_VOID )
    {
        free(d_,p_);
        d_ = F_VOID;
        p_ = 0;
    }
}

U::U(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void U::free(DiscType d, void *p)
{
    switch( d )
    {
        case F_INT: delete (int16_t *)p; return;
        case F_STRING: delete (std::string *)p; return;
        case F_VOID: return;
    }
}

void * U::copy( DiscType d, void *p )
{
    switch( d )
    {
        case F_INT: return new int16_t(*(int16_t *)p);
        case F_STRING: return new std::string(*(std::string *)p);
        case F_VOID: return 0;
    }
    return 0;
}

bool
operator<( const U &a, const U &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U::F_INT: return a.f_int() < b.f_int();
        case U::F_STRING: return a.f_string() < b.f_string();
        case U::F_VOID: return false;
    }
    return false;
}

bool
operator==( const U &a, const U &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U::F_INT: return a.f_int() == b.f_int();
        case U::F_STRING: return a.f_string() == b.f_string();
        case U::F_VOID: return true;
    }
    return false;
}

}}; // ADL::test3

namespace ADL {

typename Serialiser<ADL::test3::A>::Ptr
Serialisable<ADL::test3::A>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test3::A _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f_int_s( Serialisable<int16_t>::serialiser(sf) )
            , f_string_s( Serialisable<std::string>::serialiser(sf) )
            , f_bool_s( Serialisable<bool>::serialiser(sf) )
            {}
        
        
        typename Serialiser<int16_t>::Ptr f_int_s;
        typename Serialiser<std::string>::Ptr f_string_s;
        typename Serialiser<bool>::Ptr f_bool_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<int16_t>( json, f_int_s, "f_int", v.f_int );
            writeField<std::string>( json, f_string_s, "f_string", v.f_string );
            writeField<bool>( json, f_bool_s, "f_bool", v.f_bool );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f_int_s, v.f_int, "f_int", json ) ||
                readField( f_string_s, v.f_string, "f_string", json ) ||
                readField( f_bool_s, v.f_bool, "f_bool", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::test3::E>::Ptr
Serialisable<ADL::test3::E>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test3::E _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<Void>::Ptr v1_;
        mutable typename Serialiser<Void>::Ptr v2_;
        
        typename Serialiser<Void>::Ptr v1_s() const
        {
            if( !v1_ )
                v1_ = Serialisable<Void>::serialiser(sf_);
            return v1_;
        }
        
        typename Serialiser<Void>::Ptr v2_s() const
        {
            if( !v2_ )
                v2_ = Serialisable<Void>::serialiser(sf_);
            return v2_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test3::E::V1: json.stringV( "v1" ); break;
                case ADL::test3::E::V2: json.stringV( "v2" ); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::STRING )
            {
                if( json.stringV() == "v1" )
                    v.set_v1();
                else if( json.stringV() == "v2" )
                    v.set_v2();
                else
                    throw json_parse_failure();
                json.next();
                return;
            }
            throw json_parse_failure();
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

typename Serialiser<ADL::test3::U>::Ptr
Serialisable<ADL::test3::U>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test3::U _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<int16_t>::Ptr f_int_;
        mutable typename Serialiser<std::string>::Ptr f_string_;
        mutable typename Serialiser<Void>::Ptr f_void_;
        
        typename Serialiser<int16_t>::Ptr f_int_s() const
        {
            if( !f_int_ )
                f_int_ = Serialisable<int16_t>::serialiser(sf_);
            return f_int_;
        }
        
        typename Serialiser<std::string>::Ptr f_string_s() const
        {
            if( !f_string_ )
                f_string_ = Serialisable<std::string>::serialiser(sf_);
            return f_string_;
        }
        
        typename Serialiser<Void>::Ptr f_void_s() const
        {
            if( !f_void_ )
                f_void_ = Serialisable<Void>::serialiser(sf_);
            return f_void_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test3::U::F_INT: json.startObject(); writeField( json, f_int_s(), "f_int", v.f_int() ); json.endObject(); break;
                case ADL::test3::U::F_STRING: json.startObject(); writeField( json, f_string_s(), "f_string", v.f_string() ); json.endObject(); break;
                case ADL::test3::U::F_VOID: json.stringV( "f_void" ); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::STRING )
            {
                if( json.stringV() == "f_void" )
                    v.set_f_void();
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
                    if( matchField0( "f_int", json ) )
                        v.set_f_int(f_int_s()->fromJson( json ));
                    else if( matchField0( "f_string", json ) )
                        v.set_f_string(f_string_s()->fromJson( json ));
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

}; // ADL