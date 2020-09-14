// @generated from adl module test6
#ifndef TEST6_H
#define TEST6_H
#include <adl/adl.h>
#include <map>
#include <optional>
#include <set>
#include <stdint.h>
#include <string>
#include "sys.types.h"
#include <utility>

namespace ADL {
namespace test6 {

struct S
{
    S();
    
    S(
        const std::pair<int32_t,double>  & f_pair,
        const ADL::sys::types::Either<std::string,int32_t>  & f_either,
        const ADL::sys::types::Error<int32_t>  & f_error,
        const std::map<std::string,double>  & f_map,
        const std::set<std::string>  & f_set,
        const ADL::sys::types::Maybe<std::string>  & f_mstring,
        const ADL::sys::types::Maybe<std::string>  & f_mstring2,
        const std::optional<std::string> & f_nstring,
        const std::optional<std::string> & f_nstring2,
        const std::optional<int64_t> & f_int,
        const std::optional<int64_t> & f_int2,
        const std::optional<int64_t> & f_int3
        );
    
    std::pair<int32_t,double>  f_pair;
    ADL::sys::types::Either<std::string,int32_t>  f_either;
    ADL::sys::types::Error<int32_t>  f_error;
    std::map<std::string,double>  f_map;
    std::set<std::string>  f_set;
    ADL::sys::types::Maybe<std::string>  f_mstring;
    ADL::sys::types::Maybe<std::string>  f_mstring2;
    std::optional<std::string> f_nstring;
    std::optional<std::string> f_nstring2;
    std::optional<int64_t> f_int;
    std::optional<int64_t> f_int2;
    std::optional<int64_t> f_int3;
};

bool operator<( const S &a, const S &b );
bool operator==( const S &a, const S &b );

}}; // ADL::test6

namespace ADL {

template <>
struct Serialisable<ADL::test6::S>
{
    static Serialiser<ADL::test6::S>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // TEST6_H
