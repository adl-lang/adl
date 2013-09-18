#ifndef TEST_H
#define TEST_H
#include "Date.h"
#include <adl/adl.h>
#include <string>

namespace ADL {
namespace test {

// Date generated as DateO due to custom definition


struct DateO
{
    DateO() : value("1900-01-01") {}
    explicit DateO(const std::string & v) : value(v) {}
    
    std::string value;
};

inline
bool operator<( const DateO &a, const DateO &b ) { return a.value < b.value; }
inline
bool operator==( const DateO &a, const DateO &b ) { return a.value == b.value; }

struct S
{
    S();
    
    S(
        const Date & v1
        );
    
    Date v1;
};

bool operator<( const S &a, const S &b );
bool operator==( const S &a, const S &b );

}}; // ADL::test

namespace ADL {

template <>
struct JsonV<ADL::test::DateO>
{
    static void toJson( JsonWriter &json, const ADL::test::DateO & v )
    {
        JsonV<std::string>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::DateO &v, JsonReader &json )
    {
        JsonV<std::string>::fromJson( v.value, json );
    }
};

template <>
struct JsonV<ADL::test::S>
{
    static void toJson( JsonWriter &json, const ADL::test::S & v );
    static void fromJson( ADL::test::S &v, JsonReader &json );
};

}; // ADL
#endif // TEST_H