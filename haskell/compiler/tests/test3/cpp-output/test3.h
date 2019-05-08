#ifndef TEST3_H
#define TEST3_H
#include <adl/adl.h>
#include <map>
#include <stdint.h>
#include <string>
#include <vector>

namespace ADL {
namespace test3 {

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

class E
{
public:
    E();
    static E mk_v1();
    static E mk_v2();
    
    E( const E & );
    ~E();
    E & operator=( const E & );
    
    enum DiscType
    {
        V1,
        V2
    };
    
    DiscType d() const;
    
    void set_v1();
    void set_v2();
    
private:
    E( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const E &a, const E &b );
bool operator==( const E &a, const E &b );

inline E::DiscType E::d() const
{
    return d_;
}

class U
{
public:
    U();
    static U mk_f_int( const int16_t & v );
    static U mk_f_string( const std::string & v );
    static U mk_f_void();
    
    U( const U & );
    ~U();
    U & operator=( const U & );
    
    enum DiscType
    {
        F_INT,
        F_STRING,
        F_VOID
    };
    
    DiscType d() const;
    int16_t & f_int() const;
    std::string & f_string() const;
    
    const int16_t & set_f_int(const int16_t & );
    const std::string & set_f_string(const std::string & );
    void set_f_void();
    
private:
    U( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U &a, const U &b );
bool operator==( const U &a, const U &b );

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
        const U & f_u1,
        const E & f_e,
        const T & f_t,
        const B<int16_t>  & f_bint16,
        const std::map<std::string,int32_t> & f_smap,
        const JsonValue & f_json1,
        const JsonValue & f_json2
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
    U f_u1;
    E f_e;
    T f_t;
    B<int16_t>  f_bint16;
    std::map<std::string,int32_t> f_smap;
    JsonValue f_json1;
    JsonValue f_json2;
};

template <class T>
bool operator<( const S<T> &a, const S<T> &b );
template <class T>
bool operator==( const S<T> &a, const S<T> &b );

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
    , f_a(A(0,"xyz",false))
    , f_u(U::mk_f_int(45))
    , f_u1(U::mk_f_void())
    , f_e(E::mk_v2())
    , f_bint16(B<int16_t> (56,"yikes",mkvec<int16_t>(1,2,3),XY<int16_t> (5,5)))
    , f_smap(MapBuilder<std::string,int32_t>().add("a",45).add("b",47).result())
    , f_json1(JsonValue.parseString("null"))
    , f_json2(JsonValue.parseString("[{\"v1\":27,\"v2\":\"abcde\"},true]"))
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
    const U & f_u1_,
    const E & f_e_,
    const T & f_t_,
    const B<int16_t>  & f_bint16_,
    const std::map<std::string,int32_t> & f_smap_,
    const JsonValue & f_json1_,
    const JsonValue & f_json2_
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
    , f_u1(f_u1_)
    , f_e(f_e_)
    , f_t(f_t_)
    , f_bint16(f_bint16_)
    , f_smap(f_smap_)
    , f_json1(f_json1_)
    , f_json2(f_json2_)
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
    if( a.f_u1 < b.f_u1 ) return true;
    if( b.f_u1 < a.f_u1 ) return false;
    if( a.f_e < b.f_e ) return true;
    if( b.f_e < a.f_e ) return false;
    if( a.f_t < b.f_t ) return true;
    if( b.f_t < a.f_t ) return false;
    if( a.f_bint16 < b.f_bint16 ) return true;
    if( b.f_bint16 < a.f_bint16 ) return false;
    if( a.f_smap < b.f_smap ) return true;
    if( b.f_smap < a.f_smap ) return false;
    if( a.f_json1 < b.f_json1 ) return true;
    if( b.f_json1 < a.f_json1 ) return false;
    if( a.f_json2 < b.f_json2 ) return true;
    if( b.f_json2 < a.f_json2 ) return false;
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
        a.f_u1 == b.f_u1 &&
        a.f_e == b.f_e &&
        a.f_t == b.f_t &&
        a.f_bint16 == b.f_bint16 &&
        a.f_smap == b.f_smap &&
        a.f_json1 == b.f_json1 &&
        a.f_json2 == b.f_json2 ;
}

}}; // ADL::test3

namespace ADL {

template <>
struct Serialisable<ADL::test3::A>
{
    static Serialiser<ADL::test3::A>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test3::E>
{
    static Serialiser<ADL::test3::E>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test3::U>
{
    static Serialiser<ADL::test3::U>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
struct Serialisable<ADL::test3::XY<T>>
{
    static typename Serialiser<ADL::test3::XY<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test3::XY<T>>::Ptr
Serialisable<ADL::test3::XY<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test3::XY<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : x_s( Serialisable<T>::serialiser(sf) )
            , y_s( Serialisable<T>::serialiser(sf) )
            {}
        
        
        typename Serialiser<T>::Ptr x_s;
        typename Serialiser<T>::Ptr y_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<T>( json, x_s, "x", v.x );
            writeField<T>( json, y_s, "y", v.y );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( x_s, v.x, "x", json ) ||
                readField( y_s, v.y, "y", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

template <class T>
struct Serialisable<ADL::test3::B<T>>
{
    static typename Serialiser<ADL::test3::B<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test3::B<T>>::Ptr
Serialisable<ADL::test3::B<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test3::B<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f_t_s( Serialisable<T>::serialiser(sf) )
            , f_string_s( Serialisable<std::string>::serialiser(sf) )
            , f_tvec_s( Serialisable<std::vector<T> >::serialiser(sf) )
            , f_xy_s( Serialisable<ADL::test3::XY<T> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<T>::Ptr f_t_s;
        typename Serialiser<std::string>::Ptr f_string_s;
        typename Serialiser<std::vector<T> >::Ptr f_tvec_s;
        typename Serialiser<ADL::test3::XY<T> >::Ptr f_xy_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<T>( json, f_t_s, "f_t", v.f_t );
            writeField<std::string>( json, f_string_s, "f_string", v.f_string );
            writeField<std::vector<T> >( json, f_tvec_s, "f_tvec", v.f_tvec );
            writeField<ADL::test3::XY<T> >( json, f_xy_s, "f_xy", v.f_xy );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f_t_s, v.f_t, "f_t", json ) ||
                readField( f_string_s, v.f_string, "f_string", json ) ||
                readField( f_tvec_s, v.f_tvec, "f_tvec", json ) ||
                readField( f_xy_s, v.f_xy, "f_xy", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

template <class T>
struct Serialisable<ADL::test3::S<T>>
{
    static typename Serialiser<ADL::test3::S<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test3::S<T>>::Ptr
Serialisable<ADL::test3::S<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test3::S<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f_void_s( Serialisable<Void>::serialiser(sf) )
            , f_bool_s( Serialisable<bool>::serialiser(sf) )
            , f_int8_s( Serialisable<int8_t>::serialiser(sf) )
            , f_int16_s( Serialisable<int16_t>::serialiser(sf) )
            , f_int32_s( Serialisable<int32_t>::serialiser(sf) )
            , f_int64_s( Serialisable<int64_t>::serialiser(sf) )
            , f_word8_s( Serialisable<uint8_t>::serialiser(sf) )
            , f_word16_s( Serialisable<uint16_t>::serialiser(sf) )
            , f_word32_s( Serialisable<uint32_t>::serialiser(sf) )
            , f_word64_s( Serialisable<uint64_t>::serialiser(sf) )
            , f_float_s( Serialisable<float>::serialiser(sf) )
            , f_double_s( Serialisable<double>::serialiser(sf) )
            , f_bytes_s( Serialisable<ByteVector>::serialiser(sf) )
            , f_string_s( Serialisable<std::string>::serialiser(sf) )
            , f_vstring_s( Serialisable<std::vector<std::string> >::serialiser(sf) )
            , f_a_s( Serialisable<ADL::test3::A>::serialiser(sf) )
            , f_u_s( Serialisable<ADL::test3::U>::serialiser(sf) )
            , f_u1_s( Serialisable<ADL::test3::U>::serialiser(sf) )
            , f_e_s( Serialisable<ADL::test3::E>::serialiser(sf) )
            , f_t_s( Serialisable<T>::serialiser(sf) )
            , f_bint16_s( Serialisable<ADL::test3::B<int16_t> >::serialiser(sf) )
            , f_smap_s( stringMapSerialiser<int32_t>(sf) )
            , f_json1_s( Serialisable<JsonValue>::serialiser(sf) )
            , f_json2_s( Serialisable<JsonValue>::serialiser(sf) )
            {}
        
        
        typename Serialiser<Void>::Ptr f_void_s;
        typename Serialiser<bool>::Ptr f_bool_s;
        typename Serialiser<int8_t>::Ptr f_int8_s;
        typename Serialiser<int16_t>::Ptr f_int16_s;
        typename Serialiser<int32_t>::Ptr f_int32_s;
        typename Serialiser<int64_t>::Ptr f_int64_s;
        typename Serialiser<uint8_t>::Ptr f_word8_s;
        typename Serialiser<uint16_t>::Ptr f_word16_s;
        typename Serialiser<uint32_t>::Ptr f_word32_s;
        typename Serialiser<uint64_t>::Ptr f_word64_s;
        typename Serialiser<float>::Ptr f_float_s;
        typename Serialiser<double>::Ptr f_double_s;
        typename Serialiser<ByteVector>::Ptr f_bytes_s;
        typename Serialiser<std::string>::Ptr f_string_s;
        typename Serialiser<std::vector<std::string> >::Ptr f_vstring_s;
        typename Serialiser<ADL::test3::A>::Ptr f_a_s;
        typename Serialiser<ADL::test3::U>::Ptr f_u_s;
        typename Serialiser<ADL::test3::U>::Ptr f_u1_s;
        typename Serialiser<ADL::test3::E>::Ptr f_e_s;
        typename Serialiser<T>::Ptr f_t_s;
        typename Serialiser<ADL::test3::B<int16_t> >::Ptr f_bint16_s;
        typename Serialiser<std::map<std::string,int32_t>>::Ptr f_smap_s;
        typename Serialiser<JsonValue>::Ptr f_json1_s;
        typename Serialiser<JsonValue>::Ptr f_json2_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<Void>( json, f_void_s, "f_void", v.f_void );
            writeField<bool>( json, f_bool_s, "f_bool", v.f_bool );
            writeField<int8_t>( json, f_int8_s, "f_int8", v.f_int8 );
            writeField<int16_t>( json, f_int16_s, "f_int16", v.f_int16 );
            writeField<int32_t>( json, f_int32_s, "f_int32", v.f_int32 );
            writeField<int64_t>( json, f_int64_s, "f_int64", v.f_int64 );
            writeField<uint8_t>( json, f_word8_s, "f_word8", v.f_word8 );
            writeField<uint16_t>( json, f_word16_s, "f_word16", v.f_word16 );
            writeField<uint32_t>( json, f_word32_s, "f_word32", v.f_word32 );
            writeField<uint64_t>( json, f_word64_s, "f_word64", v.f_word64 );
            writeField<float>( json, f_float_s, "f_float", v.f_float );
            writeField<double>( json, f_double_s, "f_double", v.f_double );
            writeField<ByteVector>( json, f_bytes_s, "f_bytes", v.f_bytes );
            writeField<std::string>( json, f_string_s, "f_string", v.f_string );
            writeField<std::vector<std::string> >( json, f_vstring_s, "f_vstring", v.f_vstring );
            writeField<ADL::test3::A>( json, f_a_s, "f_a", v.f_a );
            writeField<ADL::test3::U>( json, f_u_s, "f_u", v.f_u );
            writeField<ADL::test3::U>( json, f_u1_s, "f_u1", v.f_u1 );
            writeField<ADL::test3::E>( json, f_e_s, "f_e", v.f_e );
            writeField<T>( json, f_t_s, "f_t", v.f_t );
            writeField<ADL::test3::B<int16_t> >( json, f_bint16_s, "f_bint16", v.f_bint16 );
            writeField<std::map<std::string,int32_t>>( json, f_smap_s, "f_smap", v.f_smap );
            writeField<JsonValue>( json, f_json1_s, "f_json1", v.f_json1 );
            writeField<JsonValue>( json, f_json2_s, "f_json2", v.f_json2 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f_void_s, v.f_void, "f_void", json ) ||
                readField( f_bool_s, v.f_bool, "f_bool", json ) ||
                readField( f_int8_s, v.f_int8, "f_int8", json ) ||
                readField( f_int16_s, v.f_int16, "f_int16", json ) ||
                readField( f_int32_s, v.f_int32, "f_int32", json ) ||
                readField( f_int64_s, v.f_int64, "f_int64", json ) ||
                readField( f_word8_s, v.f_word8, "f_word8", json ) ||
                readField( f_word16_s, v.f_word16, "f_word16", json ) ||
                readField( f_word32_s, v.f_word32, "f_word32", json ) ||
                readField( f_word64_s, v.f_word64, "f_word64", json ) ||
                readField( f_float_s, v.f_float, "f_float", json ) ||
                readField( f_double_s, v.f_double, "f_double", json ) ||
                readField( f_bytes_s, v.f_bytes, "f_bytes", json ) ||
                readField( f_string_s, v.f_string, "f_string", json ) ||
                readField( f_vstring_s, v.f_vstring, "f_vstring", json ) ||
                readField( f_a_s, v.f_a, "f_a", json ) ||
                readField( f_u_s, v.f_u, "f_u", json ) ||
                readField( f_u1_s, v.f_u1, "f_u1", json ) ||
                readField( f_e_s, v.f_e, "f_e", json ) ||
                readField( f_t_s, v.f_t, "f_t", json ) ||
                readField( f_bint16_s, v.f_bint16, "f_bint16", json ) ||
                readField( f_smap_s, v.f_smap, "f_smap", json ) ||
                readField( f_json1_s, v.f_json1, "f_json1", json ) ||
                readField( f_json2_s, v.f_json2, "f_json2", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL
#endif // TEST3_H