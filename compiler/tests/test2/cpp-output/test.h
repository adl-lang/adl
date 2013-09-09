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

}} // ADL::test

namespace ADL {

template <>
struct JsonV<ADL::test::S1>
{
    static void toJson( JsonWriter &json, const ADL::test::S1 & v );
    static void fromJson( ADL::test::S1 &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace test {

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

}} // ADL::test

namespace ADL {

template <class T>
struct JsonV<ADL::test::Tree<T>>
{
    static void toJson( JsonWriter &json, const ADL::test::Tree<T> & v );
    static void fromJson( ADL::test::Tree<T> &v, JsonReader &json );
};

} // ADL

namespace ADL {
namespace test {

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

}} // ADL::test

namespace ADL {

template <class T>
void
JsonV<ADL::test::Tree<T>>::toJson( JsonWriter &json, const ADL::test::Tree<T> & v )
{
    json.startObject();
    writeField( json, "value", v.value );
    writeField( json, "children", v.children );
    json.endObject();
}

template <class T>
void
JsonV<ADL::test::Tree<T>>::fromJson( ADL::test::Tree<T> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( match0( json, JsonReader::FIELD ) )
    {
        if( json.fieldName() == "value" )
            JsonV<T>::fromJson( v.value, json );
        else if( json.fieldName() == "children" )
            JsonV<std::vector<ADL::test::Tree<T> > >::fromJson( v.children, json );
        else
            ignore( json );
    }
    match( json, JsonReader::END_OBJECT );
}

} // ADL

namespace ADL {
namespace test {

using IntTree = Tree<int32_t> ;
}} // ADL::test