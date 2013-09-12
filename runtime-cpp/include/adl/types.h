#ifndef ADL_TYPES_H
#define ADL_TYPES_H

#include <vector>
#include <stdint.h>
#include <exception>

namespace ADL {

//----------------------------------------------------------------------
// Void type

struct Void
{
};

inline
bool operator<( const Void &a, const Void &b ) { return false; }

//----------------------------------------------------------------------
// ByteVector

struct ByteVector
{
    std::vector<uint8_t> bytes;

    static ByteVector fromLiteral( const std::string & v );
};

//----------------------------------------------------------------------
// Sink type

template <class T>
struct Sink {
};

//----------------------------------------------------------------------
// Helpers for unions

class invalid_union_access : public std::exception
{
};

//----------------------------------------------------------------------
// Helpers for vectors

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

} // namespace ADL

#endif
