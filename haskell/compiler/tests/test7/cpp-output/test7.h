// @generated from adl module test7
#ifndef TEST7_H
#define TEST7_H
#include <adl/adl.h>
#include <stdint.h>
#include <string>

namespace ADL {
namespace test7 {

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
    typedef T TType;
    
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

using IntPoint1A = IntPoint1;

struct S
{
    S();
    
    S(
        const IntPoint1A & f1
        );
    
    IntPoint1A f1;
};

bool operator<( const S &a, const S &b );
bool operator==( const S &a, const S &b );

}}; // ADL::test7

namespace ADL {

template <>
struct Serialisable<ADL::test7::Int2>
{
    struct S : public Serialiser<ADL::test7::Int2>
    {
        S( typename Serialiser<int64_t>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::Int2 & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::Int2 &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<int64_t>::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::Int2>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::Int2>::Ptr(new S(Serialisable<int64_t>::serialiser(sf)));
    }
};

template <>
struct Serialisable<ADL::test7::Int3>
{
    struct S : public Serialiser<ADL::test7::Int3>
    {
        S( typename Serialiser<int64_t>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::Int3 & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::Int3 &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<int64_t>::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::Int3>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::Int3>::Ptr(new S(Serialisable<int64_t>::serialiser(sf)));
    }
};

template <class X>
struct Serialisable<ADL::test7::Int5<X>>
{
    struct S : public Serialiser<ADL::test7::Int5<X>>
    {
        S( typename Serialiser<int64_t>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::Int5<X> & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::Int5<X> &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<int64_t>::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::Int5<X>>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::Int5<X>>::Ptr(new S(Serialisable<int64_t>::serialiser(sf)));
    }
};

template <class X>
struct Serialisable<ADL::test7::Int6<X>>
{
    struct S : public Serialiser<ADL::test7::Int6<X>>
    {
        S( typename Serialiser<int64_t>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::Int6<X> & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::Int6<X> &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<int64_t>::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::Int6<X>>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::Int6<X>>::Ptr(new S(Serialisable<int64_t>::serialiser(sf)));
    }
};

template <class T>
struct Serialisable<ADL::test7::Point<T>>
{
    static typename Serialiser<ADL::test7::Point<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test7::Point<T>>::Ptr
Serialisable<ADL::test7::Point<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test7::Point<T> _T;
    
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

template <>
struct Serialisable<ADL::test7::String2>
{
    struct S : public Serialiser<ADL::test7::String2>
    {
        S( typename Serialiser<std::string>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::String2 & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::String2 &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<std::string>::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::String2>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::String2>::Ptr(new S(Serialisable<std::string>::serialiser(sf)));
    }
};

template <>
struct Serialisable<ADL::test7::String3>
{
    struct S : public Serialiser<ADL::test7::String3>
    {
        S( typename Serialiser<std::string>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::String3 & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::String3 &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<std::string>::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::String3>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::String3>::Ptr(new S(Serialisable<std::string>::serialiser(sf)));
    }
};

template <class X>
struct Serialisable<ADL::test7::String5<X>>
{
    struct S : public Serialiser<ADL::test7::String5<X>>
    {
        S( typename Serialiser<std::string>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::String5<X> & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::String5<X> &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<std::string>::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::String5<X>>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::String5<X>>::Ptr(new S(Serialisable<std::string>::serialiser(sf)));
    }
};

template <class X>
struct Serialisable<ADL::test7::String6<X>>
{
    struct S : public Serialiser<ADL::test7::String6<X>>
    {
        S( typename Serialiser<std::string>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::String6<X> & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::String6<X> &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<std::string>::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::String6<X>>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::String6<X>>::Ptr(new S(Serialisable<std::string>::serialiser(sf)));
    }
};

template <>
struct Serialisable<ADL::test7::IntPoint2>
{
    struct S : public Serialiser<ADL::test7::IntPoint2>
    {
        S( typename Serialiser<ADL::test7::Point<int64_t> >::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::IntPoint2 & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::IntPoint2 &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<ADL::test7::Point<int64_t> >::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::IntPoint2>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::IntPoint2>::Ptr(new S(Serialisable<ADL::test7::Point<int64_t> >::serialiser(sf)));
    }
};

template <>
struct Serialisable<ADL::test7::IntPoint3>
{
    struct S : public Serialiser<ADL::test7::IntPoint3>
    {
        S( typename Serialiser<ADL::test7::Point<int64_t> >::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::IntPoint3 & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::IntPoint3 &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<ADL::test7::Point<int64_t> >::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::IntPoint3>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::IntPoint3>::Ptr(new S(Serialisable<ADL::test7::Point<int64_t> >::serialiser(sf)));
    }
};

template <class X>
struct Serialisable<ADL::test7::Point2<X>>
{
    struct S : public Serialiser<ADL::test7::Point2<X>>
    {
        S( typename Serialiser<ADL::test7::Point<X> >::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test7::Point2<X> & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test7::Point2<X> &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<ADL::test7::Point<X> >::Ptr s;
    };
    
    static typename Serialiser<ADL::test7::Point2<X>>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test7::Point2<X>>::Ptr(new S(Serialisable<ADL::test7::Point<X> >::serialiser(sf)));
    }
};

template <>
struct Serialisable<ADL::test7::S>
{
    static Serialiser<ADL::test7::S>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // TEST7_H
