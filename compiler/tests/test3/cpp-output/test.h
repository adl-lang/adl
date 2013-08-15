#include "adl.h"
#include <stdint.h>
#include <string>
#include <vector>

namespace ADL {
namespace test {

struct A
{
    A();
    
    A(
        const int16_t & f_int,
        const std::string & f_string,
        const bool & f_bool
        );
    
    int16_t f_int;
    std::string f_string;
    bool f_bool;
};

bool operator<( const A &a, const A &b );


class U
{
public:
    U();
    static U mk_f_int( const int16_t & v );
    static U mk_f_string( const std::string & v );
    
    U( const U & );
    ~U();
    U operator=( const U & );
    
    enum DiscType
    {
        F_INT,
        F_STRING
    };
    
    DiscType d() const;
    int16_t & f_int() const;
    std::string & f_string() const;
    
private:
    DiscType d_;
    void *v_;
    void clear();
};
bool operator<( const U &a, const U &b );

template <class T>
struct XY
{
    XY();
    
    XY(
        const T & x,
        const T & y
        );
    
    T x;
    T y;
};

template <class T>
bool operator<( const XY<T> &a, const XY<T> &b );


template <class T>
XY<T>::XY()
{
}

template <class T>
XY<T>::XY(
    const T & x_,
    const T & y_
    )
    : x(x_)
    , y(y_)
{
}

template <class T>
bool
operator<( const XY<T> &a, const XY<T> &b )
{
    if( a.x < b.x ) return true;
    if( b.x < a.x ) return false;
    if( a.y < b.y ) return true;
    if( b.y < a.y ) return false;
    return false;
}

template <class T>
struct B
{
    B();
    
    B(
        const T & f_t,
        const std::string & f_string,
        const std::vector<T>  & f_tvec,
        const XY<T>  & f_xy
        );
    
    T f_t;
    std::string f_string;
    std::vector<T>  f_tvec;
    XY<T>  f_xy;
};

template <class T>
bool operator<( const B<T> &a, const B<T> &b );


template <class T>
B<T>::B()
{
}

template <class T>
B<T>::B(
    const T & f_t_,
    const std::string & f_string_,
    const std::vector<T>  & f_tvec_,
    const XY<T>  & f_xy_
    )
    : f_t(f_t_)
    , f_string(f_string_)
    , f_tvec(f_tvec_)
    , f_xy(f_xy_)
{
}

template <class T>
bool
operator<( const B<T> &a, const B<T> &b )
{
    if( a.f_t < b.f_t ) return true;
    if( b.f_t < a.f_t ) return false;
    if( a.f_string < b.f_string ) return true;
    if( b.f_string < a.f_string ) return false;
    if( a.f_tvec < b.f_tvec ) return true;
    if( b.f_tvec < a.f_tvec ) return false;
    if( a.f_xy < b.f_xy ) return true;
    if( b.f_xy < a.f_xy ) return false;
    return false;
}

template <class T>
struct S
{
    S();
    
    S(
        const Void & f_void,
        const bool & f_bool,
        const int8_t & f_int8,
        const int16_t & f_int16,
        const int32_t & f_int32,
        const int64_t & f_int64,
        const uint8_t & f_word8,
        const uint16_t & f_word16,
        const uint32_t & f_word32,
        const uint64_t & f_word64,
        const float & f_float,
        const double & f_double,
        const std::string & f_bytes,
        const std::string & f_string,
        const std::vector<std::string>  & f_vstring,
        const A & f_a,
        const U & f_u,
        const T & f_t,
        const B<int16_t>  & f_bint16
        );
    
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

template <class T>
bool operator<( const S<T> &a, const S<T> &b );


template <class T>
S<T>::S()
    : f_void(Void())
    , f_bool(true)
    , f_int8(-5)
    , f_int16(-10000)
    , f_int32(56)
    , f_int64(40000)
    , f_word8(32)
    , f_word16(50000)
    , f_word32(124456)
    , f_word64(2344)
    , f_float(0.5)
    , f_double(0.45)
    , f_bytes("hello")
    , f_string("abcd")
    , f_vstring(mkvec<std::string>("xy","ab"))
    , f_a(A(0,"xyz",true))
    , f_u(U::mk_f_int(45))
    , f_bint16(B<int16_t> (56,"yikes",mkvec<int16_t>(1,2,3),XY<int16_t> (5,5)))
{
}

template <class T>
S<T>::S(
    const Void & f_void_,
    const bool & f_bool_,
    const int8_t & f_int8_,
    const int16_t & f_int16_,
    const int32_t & f_int32_,
    const int64_t & f_int64_,
    const uint8_t & f_word8_,
    const uint16_t & f_word16_,
    const uint32_t & f_word32_,
    const uint64_t & f_word64_,
    const float & f_float_,
    const double & f_double_,
    const std::string & f_bytes_,
    const std::string & f_string_,
    const std::vector<std::string>  & f_vstring_,
    const A & f_a_,
    const U & f_u_,
    const T & f_t_,
    const B<int16_t>  & f_bint16_
    )
    : f_void(f_void_)
    , f_bool(f_bool_)
    , f_int8(f_int8_)
    , f_int16(f_int16_)
    , f_int32(f_int32_)
    , f_int64(f_int64_)
    , f_word8(f_word8_)
    , f_word16(f_word16_)
    , f_word32(f_word32_)
    , f_word64(f_word64_)
    , f_float(f_float_)
    , f_double(f_double_)
    , f_bytes(f_bytes_)
    , f_string(f_string_)
    , f_vstring(f_vstring_)
    , f_a(f_a_)
    , f_u(f_u_)
    , f_t(f_t_)
    , f_bint16(f_bint16_)
{
}

template <class T>
bool
operator<( const S<T> &a, const S<T> &b )
{
    if( a.f_void < b.f_void ) return true;
    if( b.f_void < a.f_void ) return false;
    if( a.f_bool < b.f_bool ) return true;
    if( b.f_bool < a.f_bool ) return false;
    if( a.f_int8 < b.f_int8 ) return true;
    if( b.f_int8 < a.f_int8 ) return false;
    if( a.f_int16 < b.f_int16 ) return true;
    if( b.f_int16 < a.f_int16 ) return false;
    if( a.f_int32 < b.f_int32 ) return true;
    if( b.f_int32 < a.f_int32 ) return false;
    if( a.f_int64 < b.f_int64 ) return true;
    if( b.f_int64 < a.f_int64 ) return false;
    if( a.f_word8 < b.f_word8 ) return true;
    if( b.f_word8 < a.f_word8 ) return false;
    if( a.f_word16 < b.f_word16 ) return true;
    if( b.f_word16 < a.f_word16 ) return false;
    if( a.f_word32 < b.f_word32 ) return true;
    if( b.f_word32 < a.f_word32 ) return false;
    if( a.f_word64 < b.f_word64 ) return true;
    if( b.f_word64 < a.f_word64 ) return false;
    if( a.f_float < b.f_float ) return true;
    if( b.f_float < a.f_float ) return false;
    if( a.f_double < b.f_double ) return true;
    if( b.f_double < a.f_double ) return false;
    if( a.f_bytes < b.f_bytes ) return true;
    if( b.f_bytes < a.f_bytes ) return false;
    if( a.f_string < b.f_string ) return true;
    if( b.f_string < a.f_string ) return false;
    if( a.f_vstring < b.f_vstring ) return true;
    if( b.f_vstring < a.f_vstring ) return false;
    if( a.f_a < b.f_a ) return true;
    if( b.f_a < a.f_a ) return false;
    if( a.f_u < b.f_u ) return true;
    if( b.f_u < a.f_u ) return false;
    if( a.f_t < b.f_t ) return true;
    if( b.f_t < a.f_t ) return false;
    if( a.f_bint16 < b.f_bint16 ) return true;
    if( b.f_bint16 < a.f_bint16 ) return false;
    return false;
}

}
}