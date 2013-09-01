#include "test.h"

namespace ADL {
namespace test {

DateO::DateO()
    : date("1900-01-01")
{
}

DateO::DateO(
    const std::string & date_
    )
    : date(date_)
{
}

bool
operator<( const DateO &a, const DateO &b )
{
    if( a.date < b.date ) return true;
    if( b.date < a.date ) return false;
    return false;
}

bool
operator==( const DateO &a, const DateO &b )
{
    return
        a.date == b.date ;
}

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