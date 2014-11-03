#ifndef TEST_H
#define TEST_H
#include "Date.h"
#include <adl/adl.h>
#include <map>
#include <set>
#include <stdint.h>
#include <string>
#include "sys.types.h"
#include <utility>

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

struct S2
{
    S2();
    
    S2(
        const ADL::sys::types::Maybe<int32_t>  & f1,
        const ADL::sys::types::Maybe<int32_t>  & f2,
        const ADL::sys::types::Either<int32_t,std::string>  & f3,
        const ADL::sys::types::Either<int32_t,std::string>  & f4,
        const std::pair<int32_t,double>  & f5,
        const std::set<std::string>  & f6,
        const std::map<std::string,int32_t>  & f7
        );
    
    ADL::sys::types::Maybe<int32_t>  f1;
    ADL::sys::types::Maybe<int32_t>  f2;
    ADL::sys::types::Either<int32_t,std::string>  f3;
    ADL::sys::types::Either<int32_t,std::string>  f4;
    std::pair<int32_t,double>  f5;
    std::set<std::string>  f6;
    std::map<std::string,int32_t>  f7;
};

bool operator<( const S2 &a, const S2 &b );
bool operator==( const S2 &a, const S2 &b );

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
struct Serialisable<ADL::test::DateO>
{
    struct S : public Serialiser<ADL::test::DateO>
    {
        S( typename Serialiser<std::string>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test::DateO & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test::DateO &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<std::string>::Ptr s;
    };
    
    static typename Serialiser<ADL::test::DateO>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test::DateO>::Ptr(new S(Serialisable<std::string>::serialiser(sf)));
    }
};

template <>
struct Serialisable<ADL::test::S2>
{
    static Serialiser<ADL::test::S2>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test::S>
{
    static Serialiser<ADL::test::S>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // TEST_H