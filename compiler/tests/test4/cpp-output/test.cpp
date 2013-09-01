#include "test.h"

namespace ADL {
namespace test {

S::S()
{
}

S::S(
    const Date & v1_
    )
    : v1(v1_)
{
}

bool
operator<( const S &a, const S &b )
{
    if( a.v1 < b.v1 ) return true;
    if( b.v1 < a.v1 ) return false;
    return false;
}

bool
operator==( const S &a, const S &b )
{
    return
        a.v1 == b.v1 ;
}

}
}