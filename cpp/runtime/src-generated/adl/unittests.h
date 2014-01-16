#ifndef UNITTESTS_H
#define UNITTESTS_H
#include <adl/adl.h>
#include <vector>

namespace ADL {
namespace unittests {

template <class A, class B>
struct Pair
{
    Pair();
    
    Pair(
        const A & first,
        const B & second
        );
    
    A first;
    B second;
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
    const A & first_,
    const B & second_
    )
    : first(first_)
    , second(second_)
{
}

template <class A, class B>
bool
operator<( const Pair<A,B> &a, const Pair<A,B> &b )
{
    if( a.first < b.first ) return true;
    if( b.first < a.first ) return false;
    if( a.second < b.second ) return true;
    if( b.second < a.second ) return false;
    return false;
}

template <class A, class B>
bool
operator==( const Pair<A,B> &a, const Pair<A,B> &b )
{
    return
        a.first == b.first &&
        a.second == b.second ;
}

struct T1
{
    T1();
    
    T1(
        const double & value,
        const std::vector<T1>  & children
        );
    
    double value;
    std::vector<T1>  children;
};

bool operator<( const T1 &a, const T1 &b );
bool operator==( const T1 &a, const T1 &b );

class L1
{
public:
    L1();
    static L1 mk_null_();
    static L1 mk_value( const Pair<double,L1>  & v );
    
    L1( const L1 & );
    ~L1();
    L1 & operator=( const L1 & );
    
    enum DiscType
    {
        NULL_,
        VALUE
    };
    
    DiscType d() const;
    Pair<double,L1>  & value() const;
    
    void set_null_();
    const Pair<double,L1>  & set_value(const Pair<double,L1>  & );
    
private:
    L1( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const L1 &a, const L1 &b );
bool operator==( const L1 &a, const L1 &b );

inline L1::DiscType L1::d() const
{
    return d_;
}

inline Pair<double,L1>  & L1::value() const
{
    if( d_ == VALUE )
    {
        return *(Pair<double,L1>  *)p_;
    }
    throw invalid_union_access();
}

}}; // ADL::unittests

namespace ADL {

template <class A, class B>
struct Serialisable<ADL::unittests::Pair<A,B>>
{
    static typename Serialiser<ADL::unittests::Pair<A,B>>::Ptr serialiser(const SerialiserFlags &);
};

template <class A, class B>
typename Serialiser<ADL::unittests::Pair<A,B>>::Ptr
Serialisable<ADL::unittests::Pair<A,B>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::unittests::Pair<A,B> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : first_s( Serialisable<A>::serialiser(sf) )
            , second_s( Serialisable<B>::serialiser(sf) )
            {}
        
        
        typename Serialiser<A>::Ptr first_s;
        typename Serialiser<B>::Ptr second_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<A>( json, first_s, "first", v.first );
            writeField<B>( json, second_s, "second", v.second );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( first_s, v.first, "first", json ) ||
                readField( second_s, v.second, "second", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

template <>
struct Serialisable<ADL::unittests::T1>
{
    static Serialiser<ADL::unittests::T1>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::unittests::L1>
{
    static Serialiser<ADL::unittests::L1>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // UNITTESTS_H