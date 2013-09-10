#include <adl/adl.h>
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
    writeField<T>( json, "x", v.x );
    writeField<T>( json, "y", v.y );
    json.endObject();
}

template <class T>
void
JsonV<ADL::test::XY<T>>::fromJson( ADL::test::XY<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        readField<T>( v.x, "x", json ) ||
        readField<T>( v.y, "y", json ) ||
        ignoreField( json );
    }
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
    writeField<T>( json, "f_t", v.f_t );
    writeField<std::string>( json, "f_string", v.f_string );
    writeField<std::vector<T> >( json, "f_tvec", v.f_tvec );
    writeField<ADL::test::XY<T> >( json, "f_xy", v.f_xy );
    json.endObject();
}

template <class T>
void
JsonV<ADL::test::B<T>>::fromJson( ADL::test::B<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        readField<T>( v.f_t, "f_t", json ) ||
        readField<std::string>( v.f_string, "f_string", json ) ||
        readField<std::vector<T> >( v.f_tvec, "f_tvec", json ) ||
        readField<ADL::test::XY<T> >( v.f_xy, "f_xy", json ) ||
        ignoreField( json );
    }
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
        const ByteVector & f_bytes,
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
    ByteVector f_bytes;
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
    , f_bytes(ByteVector::fromLiteral("hello"))
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
    const ByteVector & f_bytes_,
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
    writeField<Void>( json, "f_void", v.f_void );
    writeField<bool>( json, "f_bool", v.f_bool );
    writeField<int8_t>( json, "f_int8", v.f_int8 );
    writeField<int16_t>( json, "f_int16", v.f_int16 );
    writeField<int32_t>( json, "f_int32", v.f_int32 );
    writeField<int64_t>( json, "f_int64", v.f_int64 );
    writeField<uint8_t>( json, "f_word8", v.f_word8 );
    writeField<uint16_t>( json, "f_word16", v.f_word16 );
    writeField<uint32_t>( json, "f_word32", v.f_word32 );
    writeField<uint64_t>( json, "f_word64", v.f_word64 );
    writeField<float>( json, "f_float", v.f_float );
    writeField<double>( json, "f_double", v.f_double );
    writeField<ByteVector>( json, "f_bytes", v.f_bytes );
    writeField<std::string>( json, "f_string", v.f_string );
    writeField<std::vector<std::string> >( json, "f_vstring", v.f_vstring );
    writeField<ADL::test::A>( json, "f_a", v.f_a );
    writeField<ADL::test::U>( json, "f_u", v.f_u );
    writeField<T>( json, "f_t", v.f_t );
    writeField<ADL::test::B<int16_t> >( json, "f_bint16", v.f_bint16 );
    json.endObject();
}

template <class T>
void
JsonV<ADL::test::S<T>>::fromJson( ADL::test::S<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        readField<Void>( v.f_void, "f_void", json ) ||
        readField<bool>( v.f_bool, "f_bool", json ) ||
        readField<int8_t>( v.f_int8, "f_int8", json ) ||
        readField<int16_t>( v.f_int16, "f_int16", json ) ||
        readField<int32_t>( v.f_int32, "f_int32", json ) ||
        readField<int64_t>( v.f_int64, "f_int64", json ) ||
        readField<uint8_t>( v.f_word8, "f_word8", json ) ||
        readField<uint16_t>( v.f_word16, "f_word16", json ) ||
        readField<uint32_t>( v.f_word32, "f_word32", json ) ||
        readField<uint64_t>( v.f_word64, "f_word64", json ) ||
        readField<float>( v.f_float, "f_float", json ) ||
        readField<double>( v.f_double, "f_double", json ) ||
        readField<ByteVector>( v.f_bytes, "f_bytes", json ) ||
        readField<std::string>( v.f_string, "f_string", json ) ||
        readField<std::vector<std::string> >( v.f_vstring, "f_vstring", json ) ||
        readField<ADL::test::A>( v.f_a, "f_a", json ) ||
        readField<ADL::test::U>( v.f_u, "f_u", json ) ||
        readField<T>( v.f_t, "f_t", json ) ||
        readField<ADL::test::B<int16_t> >( v.f_bint16, "f_bint16", json ) ||
        ignoreField( json );
    }
}

} // ADL

namespace ADL {
namespace test {
}} // ADL::test