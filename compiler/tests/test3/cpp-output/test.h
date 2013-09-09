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
bool operator==( const A &a, const A &b );

}} // ADL::test

namespace ADL {

template <>
struct JsonV<ADL::test::A>
{
    static void toJson( JsonWriter &json, const ADL::test::A & v );
    static void fromJson( ADL::test::A &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace test {

class U
{
public:
    U();
    static U mk_f_int( const int16_t & v );
    static U mk_f_string( const std::string & v );
    
    U( const U & );
    ~U();
    U & operator=( const U & );
    
    enum DiscType
    {
        F_INT,
        F_STRING
    };
    
    DiscType d() const;
    int16_t & f_int() const;
    std::string & f_string() const;
    
    const int16_t & set_f_int(const int16_t & );
    const std::string & set_f_string(const std::string & );
    
private:
    U( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U &a, const U &b );
bool operator==( const U &a, const U &b );

}} // ADL::test

namespace ADL {

template <>
struct JsonV<ADL::test::U>
{
    static void toJson( JsonWriter &json, const ADL::test::U & v );
    static void fromJson( ADL::test::U &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace test {

inline U::DiscType U::d() const
{
    return d_;
}

inline int16_t & U::f_int() const
{
    if( d_ == F_INT )
    {
        return *(int16_t *)p_;
    }
    throw invalid_union_access();
}

inline std::string & U::f_string() const
{
    if( d_ == F_STRING )
    {
        return *(std::string *)p_;
    }
    throw invalid_union_access();
}

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
bool operator==( const XY<T> &a, const XY<T> &b );

}} // ADL::test

namespace ADL {

template <class T>
struct JsonV<ADL::test::XY<T>>
{
    static void toJson( JsonWriter &json, const ADL::test::XY<T> & v );
    static void fromJson( ADL::test::XY<T> &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace test {

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
bool
operator==( const XY<T> &a, const XY<T> &b )
{
    return
        a.x == b.x &&
        a.y == b.y ;
}

}} // ADL::test

namespace ADL {

template <class T>
void
JsonV<ADL::test::XY<T>>::toJson( JsonWriter &json, const ADL::test::XY<T> & v )
{
    json.startObject();
    writeField( json, "x", v.x );
    writeField( json, "y", v.y );
    json.endObject();
}

template <class T>
void
JsonV<ADL::test::XY<T>>::fromJson( ADL::test::XY<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( match0( json, JsonReader::FIELD ) )
    {
        if( json.fieldName() == "x" )
            JsonV<T>::fromJson( v.x, json );
        else if( json.fieldName() == "y" )
            JsonV<T>::fromJson( v.y, json );
        else
            ignore( json );
    }
    match( json, JsonReader::END_OBJECT );
}

} // ADL

namespace ADL {
namespace test {

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
bool operator==( const B<T> &a, const B<T> &b );

}} // ADL::test

namespace ADL {

template <class T>
struct JsonV<ADL::test::B<T>>
{
    static void toJson( JsonWriter &json, const ADL::test::B<T> & v );
    static void fromJson( ADL::test::B<T> &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace test {

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
bool
operator==( const B<T> &a, const B<T> &b )
{
    return
        a.f_t == b.f_t &&
        a.f_string == b.f_string &&
        a.f_tvec == b.f_tvec &&
        a.f_xy == b.f_xy ;
}

}} // ADL::test

namespace ADL {

template <class T>
void
JsonV<ADL::test::B<T>>::toJson( JsonWriter &json, const ADL::test::B<T> & v )
{
    json.startObject();
    writeField( json, "f_t", v.f_t );
    writeField( json, "f_string", v.f_string );
    writeField( json, "f_tvec", v.f_tvec );
    writeField( json, "f_xy", v.f_xy );
    json.endObject();
}

template <class T>
void
JsonV<ADL::test::B<T>>::fromJson( ADL::test::B<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( match0( json, JsonReader::FIELD ) )
    {
        if( json.fieldName() == "f_t" )
            JsonV<T>::fromJson( v.f_t, json );
        else if( json.fieldName() == "f_string" )
            JsonV<std::string>::fromJson( v.f_string, json );
        else if( json.fieldName() == "f_tvec" )
            JsonV<std::vector<T> >::fromJson( v.f_tvec, json );
        else if( json.fieldName() == "f_xy" )
            JsonV<ADL::test::XY<T> >::fromJson( v.f_xy, json );
        else
            ignore( json );
    }
    match( json, JsonReader::END_OBJECT );
}

} // ADL

namespace ADL {
namespace test {

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
bool operator==( const S<T> &a, const S<T> &b );

}} // ADL::test

namespace ADL {

template <class T>
struct JsonV<ADL::test::S<T>>
{
    static void toJson( JsonWriter &json, const ADL::test::S<T> & v );
    static void fromJson( ADL::test::S<T> &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace test {

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

template <class T>
bool
operator==( const S<T> &a, const S<T> &b )
{
    return
        a.f_void == b.f_void &&
        a.f_bool == b.f_bool &&
        a.f_int8 == b.f_int8 &&
        a.f_int16 == b.f_int16 &&
        a.f_int32 == b.f_int32 &&
        a.f_int64 == b.f_int64 &&
        a.f_word8 == b.f_word8 &&
        a.f_word16 == b.f_word16 &&
        a.f_word32 == b.f_word32 &&
        a.f_word64 == b.f_word64 &&
        a.f_float == b.f_float &&
        a.f_double == b.f_double &&
        a.f_bytes == b.f_bytes &&
        a.f_string == b.f_string &&
        a.f_vstring == b.f_vstring &&
        a.f_a == b.f_a &&
        a.f_u == b.f_u &&
        a.f_t == b.f_t &&
        a.f_bint16 == b.f_bint16 ;
}

}} // ADL::test

namespace ADL {

template <class T>
void
JsonV<ADL::test::S<T>>::toJson( JsonWriter &json, const ADL::test::S<T> & v )
{
    json.startObject();
    writeField( json, "f_void", v.f_void );
    writeField( json, "f_bool", v.f_bool );
    writeField( json, "f_int8", v.f_int8 );
    writeField( json, "f_int16", v.f_int16 );
    writeField( json, "f_int32", v.f_int32 );
    writeField( json, "f_int64", v.f_int64 );
    writeField( json, "f_word8", v.f_word8 );
    writeField( json, "f_word16", v.f_word16 );
    writeField( json, "f_word32", v.f_word32 );
    writeField( json, "f_word64", v.f_word64 );
    writeField( json, "f_float", v.f_float );
    writeField( json, "f_double", v.f_double );
    writeField( json, "f_bytes", v.f_bytes );
    writeField( json, "f_string", v.f_string );
    writeField( json, "f_vstring", v.f_vstring );
    writeField( json, "f_a", v.f_a );
    writeField( json, "f_u", v.f_u );
    writeField( json, "f_t", v.f_t );
    writeField( json, "f_bint16", v.f_bint16 );
    json.endObject();
}

template <class T>
void
JsonV<ADL::test::S<T>>::fromJson( ADL::test::S<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( match0( json, JsonReader::FIELD ) )
    {
        if( json.fieldName() == "f_void" )
            JsonV<Void>::fromJson( v.f_void, json );
        else if( json.fieldName() == "f_bool" )
            JsonV<bool>::fromJson( v.f_bool, json );
        else if( json.fieldName() == "f_int8" )
            JsonV<int8_t>::fromJson( v.f_int8, json );
        else if( json.fieldName() == "f_int16" )
            JsonV<int16_t>::fromJson( v.f_int16, json );
        else if( json.fieldName() == "f_int32" )
            JsonV<int32_t>::fromJson( v.f_int32, json );
        else if( json.fieldName() == "f_int64" )
            JsonV<int64_t>::fromJson( v.f_int64, json );
        else if( json.fieldName() == "f_word8" )
            JsonV<uint8_t>::fromJson( v.f_word8, json );
        else if( json.fieldName() == "f_word16" )
            JsonV<uint16_t>::fromJson( v.f_word16, json );
        else if( json.fieldName() == "f_word32" )
            JsonV<uint32_t>::fromJson( v.f_word32, json );
        else if( json.fieldName() == "f_word64" )
            JsonV<uint64_t>::fromJson( v.f_word64, json );
        else if( json.fieldName() == "f_float" )
            JsonV<float>::fromJson( v.f_float, json );
        else if( json.fieldName() == "f_double" )
            JsonV<double>::fromJson( v.f_double, json );
        else if( json.fieldName() == "f_bytes" )
            JsonV<std::string>::fromJson( v.f_bytes, json );
        else if( json.fieldName() == "f_string" )
            JsonV<std::string>::fromJson( v.f_string, json );
        else if( json.fieldName() == "f_vstring" )
            JsonV<std::vector<std::string> >::fromJson( v.f_vstring, json );
        else if( json.fieldName() == "f_a" )
            JsonV<ADL::test::A>::fromJson( v.f_a, json );
        else if( json.fieldName() == "f_u" )
            JsonV<ADL::test::U>::fromJson( v.f_u, json );
        else if( json.fieldName() == "f_t" )
            JsonV<T>::fromJson( v.f_t, json );
        else if( json.fieldName() == "f_bint16" )
            JsonV<ADL::test::B<int16_t> >::fromJson( v.f_bint16, json );
        else
            ignore( json );
    }
    match( json, JsonReader::END_OBJECT );
}

} // ADL

namespace ADL {
namespace test {
}} // ADL::test