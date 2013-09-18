#ifndef TEST_H
#define TEST_H
#include <adl/adl.h>
#include <stdint.h>
#include <string>

namespace ADL {
namespace test {

using Int1 = int64_t;

struct Int2
{
    Int2() : value(0) {}
    explicit Int2(const int64_t & v) : value(v) {}
    
    int64_t value;
};

inline
bool operator<( const Int2 &a, const Int2 &b ) { return a.value < b.value; }
inline
bool operator==( const Int2 &a, const Int2 &b ) { return a.value == b.value; }

struct Int3
{
    Int3() : value(42) {}
    explicit Int3(const int64_t & v) : value(v) {}
    
    int64_t value;
};

inline
bool operator<( const Int3 &a, const Int3 &b ) { return a.value < b.value; }
inline
bool operator==( const Int3 &a, const Int3 &b ) { return a.value == b.value; }

template <class X>
using Int4 = int64_t;

template <class X>
struct Int5
{
    Int5() : value(0) {}
    explicit Int5(const int64_t & v) : value(v) {}
    
    int64_t value;
};

template <class X>
bool operator<( const Int5<X> &a, const Int5<X> &b ) { return a.value < b.value; }
template <class X>
bool operator==( const Int5<X> &a, const Int5<X> &b ) { return a.value == b.value; }

template <class X>
struct Int6
{
    Int6() : value(43) {}
    explicit Int6(const int64_t & v) : value(v) {}
    
    int64_t value;
};

template <class X>
bool operator<( const Int6<X> &a, const Int6<X> &b ) { return a.value < b.value; }
template <class X>
bool operator==( const Int6<X> &a, const Int6<X> &b ) { return a.value == b.value; }

template <class T>
struct Point
{
    Point();
    
    Point(
        const T & x,
        const T & y
        );
    
    T x;
    T y;
};

template <class T>
bool operator<( const Point<T> &a, const Point<T> &b );
template <class T>
bool operator==( const Point<T> &a, const Point<T> &b );

template <class T>
Point<T>::Point()
{
}

template <class T>
Point<T>::Point(
    const T & x_,
    const T & y_
    )
    : x(x_)
    , y(y_)
{
}

template <class T>
bool
operator<( const Point<T> &a, const Point<T> &b )
{
    if( a.x < b.x ) return true;
    if( b.x < a.x ) return false;
    if( a.y < b.y ) return true;
    if( b.y < a.y ) return false;
    return false;
}

template <class T>
bool
operator==( const Point<T> &a, const Point<T> &b )
{
    return
        a.x == b.x &&
        a.y == b.y ;
}

using String1 = std::string;

struct String2
{
    String2() {}
    explicit String2(const std::string & v) : value(v) {}
    
    std::string value;
};

inline
bool operator<( const String2 &a, const String2 &b ) { return a.value < b.value; }
inline
bool operator==( const String2 &a, const String2 &b ) { return a.value == b.value; }

struct String3
{
    String3() : value("hello") {}
    explicit String3(const std::string & v) : value(v) {}
    
    std::string value;
};

inline
bool operator<( const String3 &a, const String3 &b ) { return a.value < b.value; }
inline
bool operator==( const String3 &a, const String3 &b ) { return a.value == b.value; }

template <class X>
using String4 = std::string;

template <class X>
struct String5
{
    String5() {}
    explicit String5(const std::string & v) : value(v) {}
    
    std::string value;
};

template <class X>
bool operator<( const String5<X> &a, const String5<X> &b ) { return a.value < b.value; }
template <class X>
bool operator==( const String5<X> &a, const String5<X> &b ) { return a.value == b.value; }

template <class X>
struct String6
{
    String6() : value("goodbye") {}
    explicit String6(const std::string & v) : value(v) {}
    
    std::string value;
};

template <class X>
bool operator<( const String6<X> &a, const String6<X> &b ) { return a.value < b.value; }
template <class X>
bool operator==( const String6<X> &a, const String6<X> &b ) { return a.value == b.value; }

using IntPoint1 = Point<int64_t> ;

struct IntPoint2
{
    IntPoint2() {}
    explicit IntPoint2(const Point<int64_t>  & v) : value(v) {}
    
    Point<int64_t>  value;
};

inline
bool operator<( const IntPoint2 &a, const IntPoint2 &b ) { return a.value < b.value; }
inline
bool operator==( const IntPoint2 &a, const IntPoint2 &b ) { return a.value == b.value; }

struct IntPoint3
{
    IntPoint3() : value(Point<int64_t> (5,27)) {}
    explicit IntPoint3(const Point<int64_t>  & v) : value(v) {}
    
    Point<int64_t>  value;
};

inline
bool operator<( const IntPoint3 &a, const IntPoint3 &b ) { return a.value < b.value; }
inline
bool operator==( const IntPoint3 &a, const IntPoint3 &b ) { return a.value == b.value; }

template <class X>
using Point1 = Point<X> ;

template <class X>
struct Point2
{
    Point2() {}
    explicit Point2(const Point<X>  & v) : value(v) {}
    
    Point<X>  value;
};

template <class X>
bool operator<( const Point2<X> &a, const Point2<X> &b ) { return a.value < b.value; }
template <class X>
bool operator==( const Point2<X> &a, const Point2<X> &b ) { return a.value == b.value; }

}}; // ADL::test

namespace ADL {

template <>
struct JsonV<ADL::test::Int2>
{
    static void toJson( JsonWriter &json, const ADL::test::Int2 & v )
    {
        JsonV<int64_t>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::Int2 &v, JsonReader &json )
    {
        JsonV<int64_t>::fromJson( v.value, json );
    }
};

template <>
struct JsonV<ADL::test::Int3>
{
    static void toJson( JsonWriter &json, const ADL::test::Int3 & v )
    {
        JsonV<int64_t>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::Int3 &v, JsonReader &json )
    {
        JsonV<int64_t>::fromJson( v.value, json );
    }
};

template <class X>
struct JsonV<ADL::test::Int5<X>>
{
    static void toJson( JsonWriter &json, const ADL::test::Int5<X> & v )
    {
        JsonV<int64_t>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::Int5<X> &v, JsonReader &json )
    {
        JsonV<int64_t>::fromJson( v.value, json );
    }
};

template <class X>
struct JsonV<ADL::test::Int6<X>>
{
    static void toJson( JsonWriter &json, const ADL::test::Int6<X> & v )
    {
        JsonV<int64_t>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::Int6<X> &v, JsonReader &json )
    {
        JsonV<int64_t>::fromJson( v.value, json );
    }
};

template <class T>
struct JsonV<ADL::test::Point<T>>
{
    static void toJson( JsonWriter &json, const ADL::test::Point<T> & v );
    static void fromJson( ADL::test::Point<T> &v, JsonReader &json );
};

template <class T>
void
JsonV<ADL::test::Point<T>>::toJson( JsonWriter &json, const ADL::test::Point<T> & v )
{
    json.startObject();
    writeField<T>( json, "x", v.x );
    writeField<T>( json, "y", v.y );
    json.endObject();
}

template <class T>
void
JsonV<ADL::test::Point<T>>::fromJson( ADL::test::Point<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        readField<T>( v.x, "x", json ) ||
        readField<T>( v.y, "y", json ) ||
        ignoreField( json );
    }
}

template <>
struct JsonV<ADL::test::String2>
{
    static void toJson( JsonWriter &json, const ADL::test::String2 & v )
    {
        JsonV<std::string>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::String2 &v, JsonReader &json )
    {
        JsonV<std::string>::fromJson( v.value, json );
    }
};

template <>
struct JsonV<ADL::test::String3>
{
    static void toJson( JsonWriter &json, const ADL::test::String3 & v )
    {
        JsonV<std::string>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::String3 &v, JsonReader &json )
    {
        JsonV<std::string>::fromJson( v.value, json );
    }
};

template <class X>
struct JsonV<ADL::test::String5<X>>
{
    static void toJson( JsonWriter &json, const ADL::test::String5<X> & v )
    {
        JsonV<std::string>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::String5<X> &v, JsonReader &json )
    {
        JsonV<std::string>::fromJson( v.value, json );
    }
};

template <class X>
struct JsonV<ADL::test::String6<X>>
{
    static void toJson( JsonWriter &json, const ADL::test::String6<X> & v )
    {
        JsonV<std::string>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::String6<X> &v, JsonReader &json )
    {
        JsonV<std::string>::fromJson( v.value, json );
    }
};

template <>
struct JsonV<ADL::test::IntPoint2>
{
    static void toJson( JsonWriter &json, const ADL::test::IntPoint2 & v )
    {
        JsonV<ADL::test::Point<int64_t> >::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::IntPoint2 &v, JsonReader &json )
    {
        JsonV<ADL::test::Point<int64_t> >::fromJson( v.value, json );
    }
};

template <>
struct JsonV<ADL::test::IntPoint3>
{
    static void toJson( JsonWriter &json, const ADL::test::IntPoint3 & v )
    {
        JsonV<ADL::test::Point<int64_t> >::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::IntPoint3 &v, JsonReader &json )
    {
        JsonV<ADL::test::Point<int64_t> >::fromJson( v.value, json );
    }
};

template <class X>
struct JsonV<ADL::test::Point2<X>>
{
    static void toJson( JsonWriter &json, const ADL::test::Point2<X> & v )
    {
        JsonV<ADL::test::Point<X> >::toJson( json, v.value );
    }
    
    static void fromJson( ADL::test::Point2<X> &v, JsonReader &json )
    {
        JsonV<ADL::test::Point<X> >::fromJson( v.value, json );
    }
};

}; // ADL
#endif // TEST_H