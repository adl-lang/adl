#include "test5.h"

namespace ADL {
namespace test5 {

S1::S1()
    : f(100)
{
}

S1::S1(
    const int16_t & f_
    )
    : f(f_)
{
}

bool
operator<( const S1 &a, const S1 &b )
{
    if( a.f < b.f ) return true;
    if( b.f < a.f ) return false;
    return false;
}

bool
operator==( const S1 &a, const S1 &b )
{
    return
        a.f == b.f ;
}

U1::U1()
    : d_(V), p_(0)
{
}

U1 U1::mk_v()
{
    return U1( V, 0 );
}

U1::U1( const U1 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

U1::~U1()
{
    free(d_,p_);
}

U1 & U1::operator=( const U1 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

void U1::set_v()
{
    if( d_ != V )
    {
        free(d_,p_);
        d_ = V;
        p_ = 0;
    }
}

U1::U1(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void U1::free(DiscType d, void *p)
{
    switch( d )
    {
        case V: return;
    }
}

void * U1::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V: return 0;
    }
    return 0;
}

bool
operator<( const U1 &a, const U1 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U1::V: return false;
    }
    return false;
}

bool
operator==( const U1 &a, const U1 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U1::V: return true;
    }
    return false;
}

U2::U2()
    : d_(V), p_(new int16_t(0))
{
}

U2 U2::mk_v( const int16_t & v )
{
    return U2( V, new int16_t(v) );
}

U2::U2( const U2 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

U2::~U2()
{
    free(d_,p_);
}

U2 & U2::operator=( const U2 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const int16_t & U2::set_v(const int16_t &v)
{
    if( d_ == V )
    {
        *(int16_t *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V;
        p_ = new int16_t(v);
    }
    return *(int16_t *)p_;
}

U2::U2(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void U2::free(DiscType d, void *p)
{
    switch( d )
    {
        case V: delete (int16_t *)p; return;
    }
}

void * U2::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V: return new int16_t(*(int16_t *)p);
    }
    return 0;
}

bool
operator<( const U2 &a, const U2 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U2::V: return a.v() < b.v();
    }
    return false;
}

bool
operator==( const U2 &a, const U2 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U2::V: return a.v() == b.v();
    }
    return false;
}

U3::U3()
    : d_(V), p_(new int16_t(100))
{
}

U3 U3::mk_v( const int16_t & v )
{
    return U3( V, new int16_t(v) );
}

U3::U3( const U3 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

U3::~U3()
{
    free(d_,p_);
}

U3 & U3::operator=( const U3 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const int16_t & U3::set_v(const int16_t &v)
{
    if( d_ == V )
    {
        *(int16_t *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V;
        p_ = new int16_t(v);
    }
    return *(int16_t *)p_;
}

U3::U3(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void U3::free(DiscType d, void *p)
{
    switch( d )
    {
        case V: delete (int16_t *)p; return;
    }
}

void * U3::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V: return new int16_t(*(int16_t *)p);
    }
    return 0;
}

bool
operator<( const U3 &a, const U3 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U3::V: return a.v() < b.v();
    }
    return false;
}

bool
operator==( const U3 &a, const U3 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U3::V: return a.v() == b.v();
    }
    return false;
}

U4::U4()
    : d_(V), p_(new S1())
{
}

U4 U4::mk_v( const S1 & v )
{
    return U4( V, new S1(v) );
}

U4::U4( const U4 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

U4::~U4()
{
    free(d_,p_);
}

U4 & U4::operator=( const U4 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const S1 & U4::set_v(const S1 &v)
{
    if( d_ == V )
    {
        *(S1 *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V;
        p_ = new S1(v);
    }
    return *(S1 *)p_;
}

U4::U4(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void U4::free(DiscType d, void *p)
{
    switch( d )
    {
        case V: delete (S1 *)p; return;
    }
}

void * U4::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V: return new S1(*(S1 *)p);
    }
    return 0;
}

bool
operator<( const U4 &a, const U4 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U4::V: return a.v() < b.v();
    }
    return false;
}

bool
operator==( const U4 &a, const U4 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U4::V: return a.v() == b.v();
    }
    return false;
}

U5::U5()
    : d_(V), p_(new S1(200))
{
}

U5 U5::mk_v( const S1 & v )
{
    return U5( V, new S1(v) );
}

U5::U5( const U5 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

U5::~U5()
{
    free(d_,p_);
}

U5 & U5::operator=( const U5 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const S1 & U5::set_v(const S1 &v)
{
    if( d_ == V )
    {
        *(S1 *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V;
        p_ = new S1(v);
    }
    return *(S1 *)p_;
}

U5::U5(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void U5::free(DiscType d, void *p)
{
    switch( d )
    {
        case V: delete (S1 *)p; return;
    }
}

void * U5::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V: return new S1(*(S1 *)p);
    }
    return 0;
}

bool
operator<( const U5 &a, const U5 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U5::V: return a.v() < b.v();
    }
    return false;
}

bool
operator==( const U5 &a, const U5 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U5::V: return a.v() == b.v();
    }
    return false;
}

U6::U6()
    : d_(V), p_(new U3())
{
}

U6 U6::mk_v( const U3 & v )
{
    return U6( V, new U3(v) );
}

U6::U6( const U6 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

U6::~U6()
{
    free(d_,p_);
}

U6 & U6::operator=( const U6 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const U3 & U6::set_v(const U3 &v)
{
    if( d_ == V )
    {
        *(U3 *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V;
        p_ = new U3(v);
    }
    return *(U3 *)p_;
}

U6::U6(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void U6::free(DiscType d, void *p)
{
    switch( d )
    {
        case V: delete (U3 *)p; return;
    }
}

void * U6::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V: return new U3(*(U3 *)p);
    }
    return 0;
}

bool
operator<( const U6 &a, const U6 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U6::V: return a.v() < b.v();
    }
    return false;
}

bool
operator==( const U6 &a, const U6 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U6::V: return a.v() == b.v();
    }
    return false;
}

U7::U7()
    : d_(V), p_(new U3(U3::mk_v(75)))
{
}

U7 U7::mk_v( const U3 & v )
{
    return U7( V, new U3(v) );
}

U7::U7( const U7 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

U7::~U7()
{
    free(d_,p_);
}

U7 & U7::operator=( const U7 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const U3 & U7::set_v(const U3 &v)
{
    if( d_ == V )
    {
        *(U3 *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V;
        p_ = new U3(v);
    }
    return *(U3 *)p_;
}

U7::U7(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void U7::free(DiscType d, void *p)
{
    switch( d )
    {
        case V: delete (U3 *)p; return;
    }
}

void * U7::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V: return new U3(*(U3 *)p);
    }
    return 0;
}

bool
operator<( const U7 &a, const U7 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U7::V: return a.v() < b.v();
    }
    return false;
}

bool
operator==( const U7 &a, const U7 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U7::V: return a.v() == b.v();
    }
    return false;
}

U8::U8()
    : d_(V1), p_(new S1())
{
}

U8 U8::mk_v1( const S1 & v )
{
    return U8( V1, new S1(v) );
}

U8 U8::mk_v2( const int16_t & v )
{
    return U8( V2, new int16_t(v) );
}

U8::U8( const U8 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

U8::~U8()
{
    free(d_,p_);
}

U8 & U8::operator=( const U8 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const S1 & U8::set_v1(const S1 &v)
{
    if( d_ == V1 )
    {
        *(S1 *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V1;
        p_ = new S1(v);
    }
    return *(S1 *)p_;
}

const int16_t & U8::set_v2(const int16_t &v)
{
    if( d_ == V2 )
    {
        *(int16_t *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V2;
        p_ = new int16_t(v);
    }
    return *(int16_t *)p_;
}

U8::U8(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void U8::free(DiscType d, void *p)
{
    switch( d )
    {
        case V1: delete (S1 *)p; return;
        case V2: delete (int16_t *)p; return;
    }
}

void * U8::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V1: return new S1(*(S1 *)p);
        case V2: return new int16_t(*(int16_t *)p);
    }
    return 0;
}

bool
operator<( const U8 &a, const U8 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U8::V1: return a.v1() < b.v1();
        case U8::V2: return a.v2() < b.v2();
    }
    return false;
}

bool
operator==( const U8 &a, const U8 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U8::V1: return a.v1() == b.v1();
        case U8::V2: return a.v2() == b.v2();
    }
    return false;
}

}}; // ADL::test5

namespace ADL {

typename Serialiser<ADL::test5::S1>::Ptr
Serialisable<ADL::test5::S1>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::S1 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f_s( Serialisable<int16_t>::serialiser(sf) )
            {}
        
        
        typename Serialiser<int16_t>::Ptr f_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<int16_t>( json, f_s, "f", v.f );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f_s, v.f, "f", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::test5::U1>::Ptr
Serialisable<ADL::test5::U1>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::U1 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<Void>::Ptr v_;
        
        typename Serialiser<Void>::Ptr v_s() const
        {
            if( !v_ )
                v_ = Serialisable<Void>::serialiser(sf_);
            return v_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::U1::V: json.stringV( "v" ); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::STRING )
            {
                if( json.stringV() == "v" )
                    v.set_v();
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

typename Serialiser<ADL::test5::U2>::Ptr
Serialisable<ADL::test5::U2>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::U2 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<int16_t>::Ptr v_;
        
        typename Serialiser<int16_t>::Ptr v_s() const
        {
            if( !v_ )
                v_ = Serialisable<int16_t>::serialiser(sf_);
            return v_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::U2::V: json.startObject(); writeField( json, v_s(), "v", v.v() ); json.endObject(); break;
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
                    if( matchField0( "v", json ) )
                        v.set_v(v_s()->fromJson( json ));
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

typename Serialiser<ADL::test5::U3>::Ptr
Serialisable<ADL::test5::U3>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::U3 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<int16_t>::Ptr v_;
        
        typename Serialiser<int16_t>::Ptr v_s() const
        {
            if( !v_ )
                v_ = Serialisable<int16_t>::serialiser(sf_);
            return v_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::U3::V: json.startObject(); writeField( json, v_s(), "v", v.v() ); json.endObject(); break;
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
                    if( matchField0( "v", json ) )
                        v.set_v(v_s()->fromJson( json ));
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

typename Serialiser<ADL::test5::U4>::Ptr
Serialisable<ADL::test5::U4>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::U4 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<ADL::test5::S1>::Ptr v_;
        
        typename Serialiser<ADL::test5::S1>::Ptr v_s() const
        {
            if( !v_ )
                v_ = Serialisable<ADL::test5::S1>::serialiser(sf_);
            return v_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::U4::V: json.startObject(); writeField( json, v_s(), "v", v.v() ); json.endObject(); break;
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
                    if( matchField0( "v", json ) )
                        v.set_v(v_s()->fromJson( json ));
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

typename Serialiser<ADL::test5::U5>::Ptr
Serialisable<ADL::test5::U5>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::U5 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<ADL::test5::S1>::Ptr v_;
        
        typename Serialiser<ADL::test5::S1>::Ptr v_s() const
        {
            if( !v_ )
                v_ = Serialisable<ADL::test5::S1>::serialiser(sf_);
            return v_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::U5::V: json.startObject(); writeField( json, v_s(), "v", v.v() ); json.endObject(); break;
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
                    if( matchField0( "v", json ) )
                        v.set_v(v_s()->fromJson( json ));
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

typename Serialiser<ADL::test5::U6>::Ptr
Serialisable<ADL::test5::U6>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::U6 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<ADL::test5::U3>::Ptr v_;
        
        typename Serialiser<ADL::test5::U3>::Ptr v_s() const
        {
            if( !v_ )
                v_ = Serialisable<ADL::test5::U3>::serialiser(sf_);
            return v_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::U6::V: json.startObject(); writeField( json, v_s(), "v", v.v() ); json.endObject(); break;
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
                    if( matchField0( "v", json ) )
                        v.set_v(v_s()->fromJson( json ));
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

typename Serialiser<ADL::test5::U7>::Ptr
Serialisable<ADL::test5::U7>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::U7 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<ADL::test5::U3>::Ptr v_;
        
        typename Serialiser<ADL::test5::U3>::Ptr v_s() const
        {
            if( !v_ )
                v_ = Serialisable<ADL::test5::U3>::serialiser(sf_);
            return v_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::U7::V: json.startObject(); writeField( json, v_s(), "v", v.v() ); json.endObject(); break;
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
                    if( matchField0( "v", json ) )
                        v.set_v(v_s()->fromJson( json ));
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

typename Serialiser<ADL::test5::U8>::Ptr
Serialisable<ADL::test5::U8>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::U8 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<ADL::test5::S1>::Ptr v1_;
        mutable typename Serialiser<int16_t>::Ptr v2_;
        
        typename Serialiser<ADL::test5::S1>::Ptr v1_s() const
        {
            if( !v1_ )
                v1_ = Serialisable<ADL::test5::S1>::serialiser(sf_);
            return v1_;
        }
        
        typename Serialiser<int16_t>::Ptr v2_s() const
        {
            if( !v2_ )
                v2_ = Serialisable<int16_t>::serialiser(sf_);
            return v2_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::U8::V1: json.startObject(); writeField( json, v1_s(), "v1", v.v1() ); json.endObject(); break;
                case ADL::test5::U8::V2: json.startObject(); writeField( json, v2_s(), "v2", v.v2() ); json.endObject(); break;
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
                    if( matchField0( "v1", json ) )
                        v.set_v1(v1_s()->fromJson( json ));
                    else if( matchField0( "v2", json ) )
                        v.set_v2(v2_s()->fromJson( json ));
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