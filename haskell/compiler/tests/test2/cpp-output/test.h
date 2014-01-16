#ifndef TEST_H
#define TEST_H
#include <adl/adl.h>
#include <stdint.h>
#include <string>
#include <vector>

namespace ADL {
namespace test {

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

template <class T>
struct Tree
{
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

}}; // ADL::test

namespace ADL {

template <>
struct Serialisable<ADL::test::S1>
{
    static Serialiser<ADL::test::S1>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
struct Serialisable<ADL::test::Tree<T>>
{
    static typename Serialiser<ADL::test::Tree<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test::Tree<T>>::Ptr
Serialisable<ADL::test::Tree<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test::Tree<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : value_s( Serialisable<T>::serialiser(sf) )
            , children_s( Serialisable<std::vector<ADL::test::Tree<T> > >::serialiser(sf) )
            {}
        
        
        typename Serialiser<T>::Ptr value_s;
        typename Serialiser<std::vector<ADL::test::Tree<T> > >::Ptr children_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<T>( json, value_s, "value", v.value );
            writeField<std::vector<ADL::test::Tree<T> > >( json, children_s, "children", v.children );
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

}; // ADL
#endif // TEST_H