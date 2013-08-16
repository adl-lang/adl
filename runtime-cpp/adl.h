#ifndef ADL_H
#define ADL_H

#include <vector>
#include <string>
#include <exception>
#include <stdint.h>

namespace ADL {

enum Ordering
{
    LT, EQ, GT
};


// Void type

struct Void
{
};

bool operator<( const Void &a, const Void &b ) { return false; }


class invalid_union_access : public std::exception
{
};

template <class T>
std::vector<T> mkvec() {
    return std::vector<T>();
}

template <class T>
std::vector<T> mkvec(const T & v1 ) {
    std::vector<T> vec;
    vec.push_back(v1);
    return vec;
}

template <class T>
std::vector<T> mkvec(const T & v1, const T & v2 ) {
    std::vector<T> vec;
    vec.push_back(v1);
    vec.push_back(v2);
    return vec;
}

template <class T>
std::vector<T> mkvec(const T & v1, const T & v2, const T & v3 ) {
    std::vector<T> vec;
    vec.push_back(v1);
    vec.push_back(v2);
    vec.push_back(v3);
    return vec;
}

template <class T>
std::vector<T> mkvec(const T & v1, const T & v2, const T & v3, const T &v4 ) {
    std::vector<T> vec;
    vec.push_back(v1);
    vec.push_back(v2);
    vec.push_back(v3);
    vec.push_back(v4);
    return vec;
}

}

#endif
