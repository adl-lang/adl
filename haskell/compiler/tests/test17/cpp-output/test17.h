// @generated from adl module test17
#ifndef TEST17_H
#define TEST17_H
#include <adl/adl.h>
#include <stdint.h>
#include <string>
#include <vector>

namespace ADL {
namespace test17 {

template <class A, class B>
struct Pair;

template <class A, class B>
struct Pair
{
    Pair();
    
    Pair(
        const A & v1,
        const B & v2
        );
    
    A v1;
    B v2;
};

template <class A, class B>
bool operator<( const Pair<A,B> &a, const Pair<A,B> &b );
template <class A, class B>
bool operator==( const Pair<A,B> &a, const Pair<A,B> &b );

template <class A, class B>
Pair<A,B>::Pair()
{
}

template <class A, class B>
Pair<A,B>::Pair(
    const A & v1_,
    const B & v2_
    )
    : v1(v1_)
    , v2(v2_)
{
}

template <class A, class B>
bool
operator<( const Pair<A,B> &a, const Pair<A,B> &b )
{
    if( a.v1 < b.v1 ) return true;
    if( b.v1 < a.v1 ) return false;
    if( a.v2 < b.v2 ) return true;
    if( b.v2 < a.v2 ) return false;
    return false;
}

template <class A, class B>
bool
operator==( const Pair<A,B> &a, const Pair<A,B> &b )
{
    return
        a.v1 == b.v1 &&
        a.v2 == b.v2 ;
}

using T1 = int32_t;

class X2
{
public:
    X2();
    static X2 mk_f1( const int32_t & v );
    static X2 mk_f2( const int32_t & v );
    static X2 mk_f3( const Pair<std::string,int32_t>  & v );
    static X2 mk_f4( const Pair<std::string,std::string>  & v );
    static X2 mk_f5( const std::vector<int32_t>  & v );
    static X2 mk_f6( const std::vector<Pair<std::string,int32_t> >  & v );
    static X2 mk_f7( const std::vector<Pair<std::string,std::string> >  & v );
    
    X2( const X2 & );
    ~X2();
    X2 & operator=( const X2 & );
    
    enum DiscType
    {
        F1,
        F2,
        F3,
        F4,
        F5,
        F6,
        F7
    };
    
    DiscType d() const;
    bool is_f1() const { return d_ == F1; };
    bool is_f2() const { return d_ == F2; };
    bool is_f3() const { return d_ == F3; };
    bool is_f4() const { return d_ == F4; };
    bool is_f5() const { return d_ == F5; };
    bool is_f6() const { return d_ == F6; };
    bool is_f7() const { return d_ == F7; };
    
    int32_t & f1() const;
    int32_t & f2() const;
    Pair<std::string,int32_t>  & f3() const;
    Pair<std::string,std::string>  & f4() const;
    std::vector<int32_t>  & f5() const;
    std::vector<Pair<std::string,int32_t> >  & f6() const;
    std::vector<Pair<std::string,std::string> >  & f7() const;
    
    const int32_t & set_f1(const int32_t & );
    const int32_t & set_f2(const int32_t & );
    const Pair<std::string,int32_t>  & set_f3(const Pair<std::string,int32_t>  & );
    const Pair<std::string,std::string>  & set_f4(const Pair<std::string,std::string>  & );
    const std::vector<int32_t>  & set_f5(const std::vector<int32_t>  & );
    const std::vector<Pair<std::string,int32_t> >  & set_f6(const std::vector<Pair<std::string,int32_t> >  & );
    const std::vector<Pair<std::string,std::string> >  & set_f7(const std::vector<Pair<std::string,std::string> >  & );
    
private:
    X2( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const X2 &a, const X2 &b );
bool operator==( const X2 &a, const X2 &b );

inline X2::DiscType X2::d() const
{
    return d_;
}

inline int32_t & X2::f1() const
{
    if( d_ == F1 )
    {
        return *(int32_t *)p_;
    }
    throw invalid_union_access();
}

inline int32_t & X2::f2() const
{
    if( d_ == F2 )
    {
        return *(int32_t *)p_;
    }
    throw invalid_union_access();
}

inline Pair<std::string,int32_t>  & X2::f3() const
{
    if( d_ == F3 )
    {
        return *(Pair<std::string,int32_t>  *)p_;
    }
    throw invalid_union_access();
}

inline Pair<std::string,std::string>  & X2::f4() const
{
    if( d_ == F4 )
    {
        return *(Pair<std::string,std::string>  *)p_;
    }
    throw invalid_union_access();
}

inline std::vector<int32_t>  & X2::f5() const
{
    if( d_ == F5 )
    {
        return *(std::vector<int32_t>  *)p_;
    }
    throw invalid_union_access();
}

inline std::vector<Pair<std::string,int32_t> >  & X2::f6() const
{
    if( d_ == F6 )
    {
        return *(std::vector<Pair<std::string,int32_t> >  *)p_;
    }
    throw invalid_union_access();
}

inline std::vector<Pair<std::string,std::string> >  & X2::f7() const
{
    if( d_ == F7 )
    {
        return *(std::vector<Pair<std::string,std::string> >  *)p_;
    }
    throw invalid_union_access();
}

template <class A>
using T2 = Pair<A,int32_t> ;

template <class A, class B>
using T3 = Pair<A,B> ;

struct X1
{
    X1();
    
    X1(
        const int32_t & f1,
        const T1 & f2,
        const T2<std::string>  & f3,
        const T3<std::string,std::string>  & f4,
        const std::vector<int32_t>  & f5,
        const std::vector<Pair<std::string,int32_t> >  & f6,
        const std::vector<Pair<std::string,std::string> >  & f7
        );
    
    int32_t f1;
    T1 f2;
    T2<std::string>  f3;
    T3<std::string,std::string>  f4;
    std::vector<int32_t>  f5;
    std::vector<Pair<std::string,int32_t> >  f6;
    std::vector<Pair<std::string,std::string> >  f7;
};

bool operator<( const X1 &a, const X1 &b );
bool operator==( const X1 &a, const X1 &b );

}}; // ADL::test17

namespace ADL {

template <class A, class B>
struct Serialisable<ADL::test17::Pair<A,B>>
{
    static typename Serialiser<ADL::test17::Pair<A,B>>::Ptr serialiser(const SerialiserFlags &);
};

template <class A, class B>
typename Serialiser<ADL::test17::Pair<A,B>>::Ptr
Serialisable<ADL::test17::Pair<A,B>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test17::Pair<A,B> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : v1_s( Serialisable<A>::serialiser(sf) )
            , v2_s( Serialisable<B>::serialiser(sf) )
            {}
        
        
        typename Serialiser<A>::Ptr v1_s;
        typename Serialiser<B>::Ptr v2_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<A>( json, v1_s, "v1", v.v1 );
            writeField<B>( json, v2_s, "v2", v.v2 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( v1_s, v.v1, "v1", json ) ||
                readField( v2_s, v.v2, "v2", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

template <>
struct Serialisable<ADL::test17::X2>
{
    static Serialiser<ADL::test17::X2>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test17::X1>
{
    static Serialiser<ADL::test17::X1>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // TEST17_H
