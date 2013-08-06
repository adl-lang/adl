#include "Void.h"
#include "stdint.h"
#include <string>
#include <vector>

namespace ADL {
namespace test {

struct A
{
    int16_t f_int;
    std::string f_string;
    bool f_bool;
};

struct U
{
   // FIXME UNION IMPL
};

template <class T>
struct XY
{
    T x;
    T y;
};

template <class T>
struct B
{
    T f_t;
    std::string f_string;
    std::vector<T>  f_tvec;
    XY<T>  f_xy;
};

template <class T>
struct S
{
    Void f_void;
    bool f_bool;
    int8_t f_int8;
    int16_t f_int16;
    int32_t f_int32;
    int64_t f_int64;
    uint8_t f_word8;
    uint16_t f_word16;
    uint32_t f_word32;
    uint64_t f_word64;
    float f_float;
    double f_double;
    std::string f_bytes;
    std::string f_string;
    std::vector<std::string>  f_vstring;
    A f_a;
    U f_u;
    T f_t;
    B<int16_t>  f_bint16;
};
}
}