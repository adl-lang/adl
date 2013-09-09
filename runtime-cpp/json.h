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
    virtual Type type() = 0;

    // Step to the next stream item
    virtual void next() = 0;

    // When type() == FIELD, this function is
    // to access the fieldname
    virtual const std::string & fieldName() = 0;
};

template <class T>
struct JsonV
{
    static void toJson( JsonWriter &json, const T & v );
    static void fromJson( T &v, JsonReader &json );
};

template <>
struct JsonV<int64_t>
{
    static void toJson( JsonWriter &json, const int64_t & v );
    static void fromJson( int64_t &v, JsonReader &json );
};

template <>
struct JsonV<std::string>
{
    static void toJson( JsonWriter &json, const std::string & v );
    static void fromJson( std::string &v, JsonReader &json );
};

template <class T>
void
writeField( JsonWriter &json, const std::string &f, const T &v )
{
    json.field( f );
    JsonV<T>::toJson( json, v );
};

struct json_parse_failure : public std::exception
{
};

inline
bool match0( JsonReader &json, JsonReader::Type t )
{
    if( json.type() == t )
    {
        json.next();
        return true;
    }
    return false;
};

inline
void match( JsonReader &json, JsonReader::Type t )
{
    if( json.type() == t )
        json.next();
    else
        throw json_parse_failure();
};

// Skip over the complete json object currently pointed at.
void ignore( JsonReader &json );

};

#endif
