#include "stdint.h"
#include <string>
#include <vector>

namespace ADL {
namespace test {


struct S1
{
    int32_t x;
    std::string y;
};

template <class T>
struct Tree
{
    T value;
    std::vector<Tree<T> >  children;
};
}
}