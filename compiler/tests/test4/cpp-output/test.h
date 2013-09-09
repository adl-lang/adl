#include "Date.h"
#include "adl.h"
#include <string>

namespace ADL {
namespace test {

// Date generated as DateO due to custom definition


struct DateO
{
    DateO();
    
    DateO(
        const std::string & date
        );
    
    std::string date;
};

bool operator<( const DateO &a, const DateO &b );
bool operator==( const DateO &a, const DateO &b );

}} // ADL::test

namespace ADL {

template <>
struct JsonV<ADL::test::DateO>
{
    static void toJson( JsonWriter &json, const ADL::test::DateO & v );
    static void fromJson( ADL::test::DateO &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace test {

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

}} // ADL::test

namespace ADL {

template <>
struct JsonV<ADL::test::S>
{
    static void toJson( JsonWriter &json, const ADL::test::S & v );
    static void fromJson( ADL::test::S &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace test {
}} // ADL::test