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

}} // ADL::test

namespace ADL {

void
JsonV<ADL::test::S1>::toJson( JsonWriter &json, const ADL::test::S1 & v )
{
    json.startObject();
    writeField( json, "x", v.x );
    writeField( json, "y", v.y );
    json.endObject();
}

void
JsonV<ADL::test::S1>::fromJson( ADL::test::S1 &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( match0( json, JsonReader::FIELD ) )
    {
        if( json.fieldName() == "x" )
            JsonV<int32_t>::fromJson( v.x, json );
        else if( json.fieldName() == "y" )
            JsonV<std::string>::fromJson( v.y, json );
        else
            ignore( json );
    }
    match( json, JsonReader::END_OBJECT );
}

} // ADL

namespace ADL {
namespace test {
}} // ADL::test