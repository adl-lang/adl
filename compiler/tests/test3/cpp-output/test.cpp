#include "test.h"

namespace ADL {
namespace test {

A::A()
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
    if( a.f_int > b.f_int ) return false;
    if( a.f_string < b.f_string ) return true;
    if( a.f_string > b.f_string ) return false;
    if( a.f_bool < b.f_bool ) return true;
    if( a.f_bool > b.f_bool ) return false;
    return false;
}

}
}