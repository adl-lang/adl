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

U1::DiscType U1::d() const
{
    return d_;
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

U2::DiscType U2::d() const
{
    return d_;
}

int16_t & U2::v() const
{
    if( d_ == V )
    {
        return *(int16_t *)p_;
    }
    throw invalid_union_access();
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
        case V: delete (int16_t *)p;
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

U3::DiscType U3::d() const
{
    return d_;
}

int16_t & U3::v() const
{
    if( d_ == V )
    {
        return *(int16_t *)p_;
    }
    throw invalid_union_access();
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
        case V: delete (int16_t *)p;
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

U4::DiscType U4::d() const
{
    return d_;
}

S1 & U4::v() const
{
    if( d_ == V )
    {
        return *(S1 *)p_;
    }
    throw invalid_union_access();
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
        case V: delete (S1 *)p;
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

U5::DiscType U5::d() const
{
    return d_;
}

S1 & U5::v() const
{
    if( d_ == V )
    {
        return *(S1 *)p_;
    }
    throw invalid_union_access();
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
        case V: delete (S1 *)p;
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

U6::DiscType U6::d() const
{
    return d_;
}

U3 & U6::v() const
{
    if( d_ == V )
    {
        return *(U3 *)p_;
    }
    throw invalid_union_access();
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
        case V: delete (U3 *)p;
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

U7::DiscType U7::d() const
{
    return d_;
}

U3 & U7::v() const
{
    if( d_ == V )
    {
        return *(U3 *)p_;
    }
    throw invalid_union_access();
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
        case V: delete (U3 *)p;
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

U8::DiscType U8::d() const
{
    return d_;
}

S1 & U8::v1() const
{
    if( d_ == V1 )
    {
        return *(S1 *)p_;
    }
    throw invalid_union_access();
}

int16_t & U8::v2() const
{
    if( d_ == V2 )
    {
        return *(int16_t *)p_;
    }
    throw invalid_union_access();
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
        case V1: delete (S1 *)p;
        case V2: delete (int16_t *)p;
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

}
}