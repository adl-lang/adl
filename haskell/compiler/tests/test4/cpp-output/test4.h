// @generated from adl module test4
#ifndef TEST4_H
#define TEST4_H
#include "Date.h"
#include <adl/adl.h>
#include <map>
#include <set>
#include <stdint.h>
#include <string>
#include "sys.types.h"
#include <utility>

namespace ADL {
namespace test4 {

struct CDate
{
    CDate();
    
    CDate(
        const int16_t & year,
        const int16_t & month,
        const int16_t & day
        );
    
    int16_t year;
    int16_t month;
    int16_t day;
};

bool operator<( const CDate &a, const CDate &b );
bool operator==( const CDate &a, const CDate &b );

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
        const int32_t & intv
        );
    
    int32_t intv;
};

bool operator<( const S2 &a, const S2 &b );
bool operator==( const S2 &a, const S2 &b );

struct S3
{
    S3();
    
    S3(
        const int32_t & intv1,
        const int32_t & intv2
        );
    
    int32_t intv1;
    int32_t intv2;
};

bool operator<( const S3 &a, const S3 &b );
bool operator==( const S3 &a, const S3 &b );

struct S
{
    S();
    
    S(
        const Date & v1,
        const Date & v2,
        const CDate & v3,
        const CDate & v4,
        const ADL::sys::types::Maybe<std::string>  & v5,
        const ADL::sys::types::Maybe<std::string>  & v5a,
        const ADL::sys::types::Maybe<std::string>  & v5b,
        const std::pair<std::string,int32_t>  & v6,
        const std::set<int32_t>  & v7,
        const std::set<int32_t>  & v7a,
        const std::map<std::string,int32_t>  & v8,
        const std::map<std::string,int32_t>  & v8a
        );
    
    Date v1;
    Date v2;
    CDate v3;
    CDate v4;
    ADL::sys::types::Maybe<std::string>  v5;
    ADL::sys::types::Maybe<std::string>  v5a;
    ADL::sys::types::Maybe<std::string>  v5b;
    std::pair<std::string,int32_t>  v6;
    std::set<int32_t>  v7;
    std::set<int32_t>  v7a;
    std::map<std::string,int32_t>  v8;
    std::map<std::string,int32_t>  v8a;
};

bool operator<( const S &a, const S &b );
bool operator==( const S &a, const S &b );

}}; // ADL::test4

namespace ADL {

template <>
struct Serialisable<ADL::test4::CDate>
{
    static Serialiser<ADL::test4::CDate>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test4::DateO>
{
    struct S : public Serialiser<ADL::test4::DateO>
    {
        S( typename Serialiser<std::string>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test4::DateO & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test4::DateO &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<std::string>::Ptr s;
    };
    
    static typename Serialiser<ADL::test4::DateO>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test4::DateO>::Ptr(new S(Serialisable<std::string>::serialiser(sf)));
    }
};

template <>
struct Serialisable<ADL::test4::S2>
{
    static Serialiser<ADL::test4::S2>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test4::S3>
{
    static Serialiser<ADL::test4::S3>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test4::S>
{
    static Serialiser<ADL::test4::S>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // TEST4_H
