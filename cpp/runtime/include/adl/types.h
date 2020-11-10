#ifndef ADL_TYPES_H
#define ADL_TYPES_H

#include <vector>
#include <map>
#include <string>
#include <stdint.h>
#include <exception>
#include <memory>

#include <nlohmann/json.hpp>


namespace ADL {

//----------------------------------------------------------------------
// Void type

struct Void
{
};

inline bool operator==( const Void &a, const Void &b ) { return true; }
inline bool operator<( const Void &a, const Void &b ) { return false; }

//----------------------------------------------------------------------
// ByteVector

struct ByteVector
{
    std::vector<uint8_t> bytes;

    static std::string toLiteral( const ByteVector & v );
    static ByteVector fromLiteral( const std::string & v );
};

inline bool operator==( const ByteVector &a, const ByteVector &b ) { return a.bytes == b.bytes; }
inline bool operator<( const ByteVector &a, const ByteVector &b ) { return a.bytes < b.bytes; }

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

//----------------------------------------------------------------------
// Helpers for maps

template <class K, class V>
class MapBuilder
{
public:
    MapBuilder<K,V> &add( const K &k, const V &v) {
        map_[k] = v;
        return *this;
    }
    const std::map<K,V> &result() const {
        return map_;
    }
private:
    std::map<K,V> map_;
};


template <class V>
using StringMap = std::map<std::string,V>;

struct JsonValue : public nlohmann::json {
  static const JsonValue null;
};


} // namespace ADL

#endif
