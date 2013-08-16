#include "test.h"

namespace ADL {
namespace test {

S1::S1()
    : x(0)
{
}

S1::S1(
    const int32_t & x_,
    const std::string & y_
    )
    : x(x_)
    , y(y_)
{
}

bool
operator<( const S1 &a, const S1 &b )
{
    if( a.x < b.x ) return true;
    if( b.x < a.x ) return false;
    if( a.y < b.y ) return true;
    if( b.y < a.y ) return false;
    return false;
}

bool
operator==( const S1 &a, const S1 &b )
{
    return
        a.x == b.x &&
        a.y == b.y ;
}

}
}