#ifndef TEST_H
#define TEST_H
#include <adl/adl.h>
#include "adl/test2.h"

namespace ADL {
namespace test {

struct X
{
    X() {}
    explicit X(const ADL::test2::A & v) : value(v) {}
    
    ADL::test2::A value;
};

inline
bool operator<( const X &a, const X &b ) { return a.value < b.value; }
inline
bool operator==( const X &a, const X &b ) { return a.value == b.value; }

}}; // ADL::test

namespace ADL {

template <>
struct Serialisable<ADL::test::X>
{
    struct S : public Serialiser<ADL::test::X>
    {
        S( typename Serialiser<ADL::test2::A>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test::X & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test::X &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<ADL::test2::A>::Ptr s;
    };
    
    static typename Serialiser<ADL::test::X>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test::X>::Ptr(new S(Serialisable<ADL::test2::A>::serialiser(sf)));
    }
};

}; // ADL
#endif // TEST_H