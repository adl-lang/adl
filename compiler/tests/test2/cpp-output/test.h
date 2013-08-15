#include "adl.h"
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

using IntTree = Tree<int32_t> ;

}
}