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

}}; // ADL::test

namespace ADL {

void
JsonV<ADL::test::DateO>::toJson( JsonWriter &json, const ADL::test::DateO & v )
{
    json.startObject();
    writeField<std::string>( json, "date", v.date );
    json.endObject();
}

void
JsonV<ADL::test::DateO>::fromJson( ADL::test::DateO &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        readField<std::string>( v.date, "date", json ) ||
        ignoreField( json );
    }
}

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