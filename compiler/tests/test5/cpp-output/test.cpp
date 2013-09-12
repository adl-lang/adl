#include "test.h"

namespace ADL {
namespace test {

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

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::S1>::toJson( JsonWriter &json, const ADL::test::S1 & v )
{
    json.startObject();
    writeField<int16_t>( json, "f", v.f );
    json.endObject();
}

void
JsonV<ADL::test::S1>::fromJson( ADL::test::S1 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        readField<int16_t>( v.f, "f", json ) ||
        ignoreField( json );
    }
}

} // ADL

namespace ADL {
namespace test {

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
}

bool
operator==( const U1 &a, const U1 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U1::V: return true;
    }
}

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::U1>::toJson( JsonWriter &json, const ADL::test::U1 & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::test::U1::V: writeField( json, "v", Void() ); break;
    }
    json.endObject();
}

void
JsonV<ADL::test::U1>::fromJson( ADL::test::U1 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        if( matchField0( "v", json ) )
            v.set_v();
        else
            throw json_parse_failure();
    }
}

} // ADL

namespace ADL {
namespace test {

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
}

bool
operator==( const U2 &a, const U2 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U2::V: return a.v() == b.v();
    }
}

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::U2>::toJson( JsonWriter &json, const ADL::test::U2 & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::test::U2::V: writeField( json, "v", v.v() ); break;
    }
    json.endObject();
}

void
JsonV<ADL::test::U2>::fromJson( ADL::test::U2 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        if( matchField0( "v", json ) )
            v.set_v(getFromJson<int16_t>( json ));
        else
            throw json_parse_failure();
    }
}

} // ADL

namespace ADL {
namespace test {

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
}

bool
operator==( const U3 &a, const U3 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U3::V: return a.v() == b.v();
    }
}

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::U3>::toJson( JsonWriter &json, const ADL::test::U3 & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::test::U3::V: writeField( json, "v", v.v() ); break;
    }
    json.endObject();
}

void
JsonV<ADL::test::U3>::fromJson( ADL::test::U3 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        if( matchField0( "v", json ) )
            v.set_v(getFromJson<int16_t>( json ));
        else
            throw json_parse_failure();
    }
}

} // ADL

namespace ADL {
namespace test {

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
}

bool
operator==( const U4 &a, const U4 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U4::V: return a.v() == b.v();
    }
}

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::U4>::toJson( JsonWriter &json, const ADL::test::U4 & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::test::U4::V: writeField( json, "v", v.v() ); break;
    }
    json.endObject();
}

void
JsonV<ADL::test::U4>::fromJson( ADL::test::U4 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        if( matchField0( "v", json ) )
            v.set_v(getFromJson<ADL::test::S1>( json ));
        else
            throw json_parse_failure();
    }
}

} // ADL

namespace ADL {
namespace test {

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
}

bool
operator==( const U5 &a, const U5 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U5::V: return a.v() == b.v();
    }
}

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::U5>::toJson( JsonWriter &json, const ADL::test::U5 & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::test::U5::V: writeField( json, "v", v.v() ); break;
    }
    json.endObject();
}

void
JsonV<ADL::test::U5>::fromJson( ADL::test::U5 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        if( matchField0( "v", json ) )
            v.set_v(getFromJson<ADL::test::S1>( json ));
        else
            throw json_parse_failure();
    }
}

} // ADL

namespace ADL {
namespace test {

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
}

bool
operator==( const U6 &a, const U6 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U6::V: return a.v() == b.v();
    }
}

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::U6>::toJson( JsonWriter &json, const ADL::test::U6 & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::test::U6::V: writeField( json, "v", v.v() ); break;
    }
    json.endObject();
}

void
JsonV<ADL::test::U6>::fromJson( ADL::test::U6 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        if( matchField0( "v", json ) )
            v.set_v(getFromJson<ADL::test::U3>( json ));
        else
            throw json_parse_failure();
    }
}

} // ADL

namespace ADL {
namespace test {

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
}

bool
operator==( const U7 &a, const U7 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U7::V: return a.v() == b.v();
    }
}

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::U7>::toJson( JsonWriter &json, const ADL::test::U7 & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::test::U7::V: writeField( json, "v", v.v() ); break;
    }
    json.endObject();
}

void
JsonV<ADL::test::U7>::fromJson( ADL::test::U7 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        if( matchField0( "v", json ) )
            v.set_v(getFromJson<ADL::test::U3>( json ));
        else
            throw json_parse_failure();
    }
}

} // ADL

namespace ADL {
namespace test {

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
}

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::U8>::toJson( JsonWriter &json, const ADL::test::U8 & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::test::U8::V1: writeField( json, "v1", v.v1() ); break;
        case ADL::test::U8::V2: writeField( json, "v2", v.v2() ); break;
    }
    json.endObject();
}

void
JsonV<ADL::test::U8>::fromJson( ADL::test::U8 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        if( matchField0( "v1", json ) )
            v.set_v1(getFromJson<ADL::test::S1>( json ));
        else if( matchField0( "v2", json ) )
            v.set_v2(getFromJson<int16_t>( json ));
        else
            throw json_parse_failure();
    }
}

} // ADL

namespace ADL {
namespace test {
}} // ADL::test