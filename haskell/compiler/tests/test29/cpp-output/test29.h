// @generated from adl module test29
#ifndef TEST29_H
#define TEST29_H
#include <adl/adl.h>
#include <map>
#include <string>

namespace ADL {
namespace test29 {

struct Test
{
    Test();
    
    Test(
        const std::map<std::string,std::string> & foo
        );
    
    std::map<std::string,std::string> foo;
};

bool operator<( const Test &a, const Test &b );
bool operator==( const Test &a, const Test &b );

}}; // ADL::test29

namespace ADL {

template <>
struct Serialisable<ADL::test29::Test>
{
    static Serialiser<ADL::test29::Test>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // TEST29_H
