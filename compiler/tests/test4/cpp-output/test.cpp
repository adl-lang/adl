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

}}; // ADL::test

namespace ADL {

void
JsonV<ADL::test::S>::toJson( JsonWriter &json, const ADL::test::S & v )
{
    json.startObject();
    writeField<Date>( json, "v1", v.v1 );
    json.endObject();
}

void
JsonV<ADL::test::S>::fromJson( ADL::test::S &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        readField<Date>( v.v1, "v1", json ) ||
        ignoreField( json );
    }
}

}; // ADL