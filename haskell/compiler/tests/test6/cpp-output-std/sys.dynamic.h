// @generated from adl module sys.dynamic
#ifndef SYS_DYNAMIC_H
#define SYS_DYNAMIC_H
#include <adl/adl.h>
#include "sys.adlast.h"

namespace ADL {
namespace sys {
namespace dynamic {

struct Dynamic
{
    Dynamic();
    
    Dynamic(
        const ADL::sys::adlast::TypeExpr & typeExpr,
        const JsonValue & value
        );
    
    ADL::sys::adlast::TypeExpr typeExpr;
    JsonValue value;
};

bool operator<( const Dynamic &a, const Dynamic &b );
bool operator==( const Dynamic &a, const Dynamic &b );

}}}; // ADL::sys::dynamic

namespace ADL {

template <>
struct Serialisable<ADL::sys::dynamic::Dynamic>
{
    static Serialiser<ADL::sys::dynamic::Dynamic>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // SYS_DYNAMIC_H
