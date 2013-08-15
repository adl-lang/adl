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
    if( a.f > b.f ) return false;
    return false;
}

U1::U1()
    : d_(V), v_(0)
{
}

U2::U2()
    : d_(V), v_(new int16_t(0))
{
}

U3::U3()
    : d_(V), v_(new int16_t(100))
{
}

U4::U4()
    : d_(V), v_(new S1())
{
}

U5::U5()
    : d_(V), v_(new S1(200))
{
}

U6::U6()
    : d_(V), v_(new U3())
{
}

U7::U7()
    : d_(V), v_(new U3(U3::mk_v(75)))
{
}

U8::U8()
    : d_(V1), v_(new S1())
{
}

}
}