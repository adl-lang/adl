#ifndef ADL_JSON_H
#define ADL_JSON_H

#include <stdint.h>
#include <string> 

namespace ADL {

class JsonWriter
{
public:
    virtual void startObject() = 0;
    virtual void field( const std::string &f ) = 0;
    virtual void endObject() = 0;

    virtual void startArray() = 0;
    virtual void endArray() = 0;

    virtual void stringV( const std::string &v ) = 0;
    virtual void intV( int64_t v ) = 0;
    virtual void uintV( uint64_t v ) = 0;
    virtual void doubleV( double v ) = 0;
};

class JsonReader
{
public:
    enum Type {
        START_OBJECT,
        FIELD,
        END_OBJECT,
        START_ARRAY,
        END_ARRAY,
        STRING,
        NUMBER,
        END_OF_STREAM
    };

    // Get the type of the current item
    Type type() = 0;

    // Step to the next item
    void step() = 0;
};

template <class T>
struct JsonV
{
    void toJson( JsonWriter &json, const T & v );
    void fromJson( T &v, JsonReader &json );
};

template <>
struct JsonV<int64_t>
{
    void toJson( JsonWriter &json, const int64_t & v );
    void fromJson( int64_t &v, JsonReader &json );
};

template <>
struct JsonV<std::string>
{
    void toJson( JsonWriter &json, const std::string & v );
    void fromJson( std::string &v, JsonReader &json );
};

template <class T>
void
writeField( JsonWriter &json, const std::string &f, const T &v )
{
    json.field( f );
    JsonV<T>::toJson( json, v );
};

};

#endif
