// @generated from adl module test2
#ifndef TEST2_H
#define TEST2_H
#include <adl/adl.h>
#include <stdint.h>
#include <string>
#include <vector>

namespace ADL {
namespace test2 {

struct S0
{
    S0();
    
};

bool operator<( const S0 &a, const S0 &b );
bool operator==( const S0 &a, const S0 &b );

struct S1
{
    S1();
    
    S1(
        const int32_t & x,
        const std::string & y
        );
    
    int32_t x;
    std::string y;
};

bool operator<( const S1 &a, const S1 &b );
bool operator==( const S1 &a, const S1 &b );

struct S2
{
    S2();
    
    S2(
        const std::string & f1,
        const double & f2,
        const std::vector<int32_t>  & f3
        );
    
    std::string f1;
    double f2;
    std::vector<int32_t>  f3;
};

bool operator<( const S2 &a, const S2 &b );
bool operator==( const S2 &a, const S2 &b );

template <class T>
struct S3
{
    typedef T TType;
    
    S3();
    
    S3(
        const std::string & f1,
        const double & f2,
        const T & f3,
        const std::vector<T>  & f4
        );
    
    std::string f1;
    double f2;
    T f3;
    std::vector<T>  f4;
};

template <class T>
bool operator<( const S3<T> &a, const S3<T> &b );
template <class T>
bool operator==( const S3<T> &a, const S3<T> &b );

template <class T>
S3<T>::S3()
    : f2(0.0)
{
}

template <class T>
S3<T>::S3(
    const std::string & f1_,
    const double & f2_,
    const T & f3_,
    const std::vector<T>  & f4_
    )
    : f1(f1_)
    , f2(f2_)
    , f3(f3_)
    , f4(f4_)
{
}

template <class T>
bool
operator<( const S3<T> &a, const S3<T> &b )
{
    if( a.f1 < b.f1 ) return true;
    if( b.f1 < a.f1 ) return false;
    if( a.f2 < b.f2 ) return true;
    if( b.f2 < a.f2 ) return false;
    if( a.f3 < b.f3 ) return true;
    if( b.f3 < a.f3 ) return false;
    if( a.f4 < b.f4 ) return true;
    if( b.f4 < a.f4 ) return false;
    return false;
}

template <class T>
bool
operator==( const S3<T> &a, const S3<T> &b )
{
    return
        a.f1 == b.f1 &&
        a.f2 == b.f2 &&
        a.f3 == b.f3 &&
        a.f4 == b.f4 ;
}

template <class T>
struct Tree
{
    typedef T TType;
    
    Tree();
    
    Tree(
        const T & value,
        const std::vector<Tree<T> >  & children
        );
    
    T value;
    std::vector<Tree<T> >  children;
};

template <class T>
bool operator<( const Tree<T> &a, const Tree<T> &b );
template <class T>
bool operator==( const Tree<T> &a, const Tree<T> &b );

template <class T>
Tree<T>::Tree()
{
}

template <class T>
Tree<T>::Tree(
    const T & value_,
    const std::vector<Tree<T> >  & children_
    )
    : value(value_)
    , children(children_)
{
}

template <class T>
bool
operator<( const Tree<T> &a, const Tree<T> &b )
{
    if( a.value < b.value ) return true;
    if( b.value < a.value ) return false;
    if( a.children < b.children ) return true;
    if( b.children < a.children ) return false;
    return false;
}

template <class T>
bool
operator==( const Tree<T> &a, const Tree<T> &b )
{
    return
        a.value == b.value &&
        a.children == b.children ;
}

using IntTree = Tree<int32_t> ;

template <class T>
struct S4
{
    typedef T TType;
    
    S4();
    
    S4(
        const S3<std::string>  & f1,
        const S3<T>  & f2
        );
    
    S3<std::string>  f1;
    S3<T>  f2;
};

template <class T>
bool operator<( const S4<T> &a, const S4<T> &b );
template <class T>
bool operator==( const S4<T> &a, const S4<T> &b );

template <class T>
S4<T>::S4()
{
}

template <class T>
S4<T>::S4(
    const S3<std::string>  & f1_,
    const S3<T>  & f2_
    )
    : f1(f1_)
    , f2(f2_)
{
}

template <class T>
bool
operator<( const S4<T> &a, const S4<T> &b )
{
    if( a.f1 < b.f1 ) return true;
    if( b.f1 < a.f1 ) return false;
    if( a.f2 < b.f2 ) return true;
    if( b.f2 < a.f2 ) return false;
    return false;
}

template <class T>
bool
operator==( const S4<T> &a, const S4<T> &b )
{
    return
        a.f1 == b.f1 &&
        a.f2 == b.f2 ;
}

}}; // ADL::test2

namespace ADL {

template <>
struct Serialisable<ADL::test2::S0>
{
    static Serialiser<ADL::test2::S0>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test2::S1>
{
    static Serialiser<ADL::test2::S1>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test2::S2>
{
    static Serialiser<ADL::test2::S2>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
struct Serialisable<ADL::test2::S3<T>>
{
    static typename Serialiser<ADL::test2::S3<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test2::S3<T>>::Ptr
Serialisable<ADL::test2::S3<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test2::S3<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f1_s( Serialisable<std::string>::serialiser(sf) )
            , f2_s( Serialisable<double>::serialiser(sf) )
            , f3_s( Serialisable<T>::serialiser(sf) )
            , f4_s( Serialisable<std::vector<T> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<std::string>::Ptr f1_s;
        typename Serialiser<double>::Ptr f2_s;
        typename Serialiser<T>::Ptr f3_s;
        typename Serialiser<std::vector<T> >::Ptr f4_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::string>( json, f1_s, "f1", v.f1 );
            writeField<double>( json, f2_s, "f2", v.f2 );
            writeField<T>( json, f3_s, "f3", v.f3 );
            writeField<std::vector<T> >( json, f4_s, "f4", v.f4 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f1_s, v.f1, "f1", json ) ||
                readField( f2_s, v.f2, "f2", json ) ||
                readField( f3_s, v.f3, "f3", json ) ||
                readField( f4_s, v.f4, "f4", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

template <class T>
struct Serialisable<ADL::test2::Tree<T>>
{
    static typename Serialiser<ADL::test2::Tree<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test2::Tree<T>>::Ptr
Serialisable<ADL::test2::Tree<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test2::Tree<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : value_s( Serialisable<T>::serialiser(sf) )
            , children_s( Serialisable<std::vector<ADL::test2::Tree<T> > >::serialiser(sf) )
            {}
        
        
        typename Serialiser<T>::Ptr value_s;
        typename Serialiser<std::vector<ADL::test2::Tree<T> > >::Ptr children_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<T>( json, value_s, "value", v.value );
            writeField<std::vector<ADL::test2::Tree<T> > >( json, children_s, "children", v.children );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( value_s, v.value, "value", json ) ||
                readField( children_s, v.children, "children", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

template <class T>
struct Serialisable<ADL::test2::S4<T>>
{
    static typename Serialiser<ADL::test2::S4<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test2::S4<T>>::Ptr
Serialisable<ADL::test2::S4<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test2::S4<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f1_s( Serialisable<ADL::test2::S3<std::string> >::serialiser(sf) )
            , f2_s( Serialisable<ADL::test2::S3<T> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::test2::S3<std::string> >::Ptr f1_s;
        typename Serialiser<ADL::test2::S3<T> >::Ptr f2_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::test2::S3<std::string> >( json, f1_s, "f1", v.f1 );
            writeField<ADL::test2::S3<T> >( json, f2_s, "f2", v.f2 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f1_s, v.f1, "f1", json ) ||
                readField( f2_s, v.f2, "f2", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL
#endif // TEST2_H
