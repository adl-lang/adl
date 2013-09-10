#ifndef ADL_JSON_H
#define ADL_JSON_H

#include <stdint.h>
#include <string> 

#include <adl/types.h>

namespace ADL {

class JsonWriter
{
public:
    virtual void startObject() = 0;
    virtual void field( const std::string &f ) = 0;
    virtual void endObject() = 0;

    virtual void startArray() = 0;
    virtual void endArray() = 0;

    virtual void nullV() = 0;
    virtual void boolV( bool v ) = 0;
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
        NULLV,
        BOOLEAN,
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

    // When type() == BOOLEAN, this function is
    // to access the numeric value
    virtual bool boolV() = 0;

    // When type() == NUMBER, this function is
    // to access the numeric value
    virtual int64_t intV() = 0;

    // When type() == NUMBER, this function is
    // to access the numeric value
    virtual uint64_t uintV() = 0;

    // When type() == NUMBER, this function is
    // to access the numeric value
    virtual double doubleV() = 0;

    // When type() == STRING, this function is
    // to access the string value
    virtual const std::string & stringV() = 0;
};

template <class T>
struct JsonV
{
    static void toJson( JsonWriter &json, const T & v );
    static void fromJson( T &v, JsonReader &json );
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

inline bool
matchField0( const std::string &f, JsonReader &json )
{
    if( json.type() != JsonReader::FIELD )
        throw json_parse_failure();
    bool match = json.fieldName() == f;
    if( match )
        json.next();
    return match;
}

template <class T>
void
writeField( JsonWriter &json, const std::string &f, const T &v )
{
    json.field( f );
    JsonV<T>::toJson( json, v );
};

template <class T>
bool
readField( T &v, const std::string &f, JsonReader &json )
{
    if( json.type() != JsonReader::FIELD )
        throw json_parse_failure();
    if( json.fieldName() != f )
        return false;
    json.next();
    JsonV<T>::fromJson( v, json );
};

bool ignoreField( JsonReader &json );


//----------------------------------------------------------------------
// Serialisation for primitive/builtin types

template <>
struct JsonV<Void>
{
    static void toJson( JsonWriter &json, const Void & v );
    static void fromJson( Void &v, JsonReader &json );
};

template <>
struct JsonV<bool>
{
    static void toJson( JsonWriter &json, const bool & v );
    static void fromJson( bool &v, JsonReader &json );
};

template <>
struct JsonV<int8_t>
{
    static void toJson( JsonWriter &json, const int8_t & v );
    static void fromJson( int8_t &v, JsonReader &json );
};

template <>
struct JsonV<int16_t>
{
    static void toJson( JsonWriter &json, const int16_t & v );
    static void fromJson( int16_t &v, JsonReader &json );
};

template <>
struct JsonV<int32_t>
{
    static void toJson( JsonWriter &json, const int32_t & v );
    static void fromJson( int32_t &v, JsonReader &json );
};

template <>
struct JsonV<int64_t>
{
    static void toJson( JsonWriter &json, const int64_t & v );
    static void fromJson( int64_t &v, JsonReader &json );
};

template <>
struct JsonV<uint8_t>
{
    static void toJson( JsonWriter &json, const uint8_t & v );
    static void fromJson( uint8_t &v, JsonReader &json );
};

template <>
struct JsonV<uint16_t>
{
    static void toJson( JsonWriter &json, const uint16_t & v );
    static void fromJson( uint16_t &v, JsonReader &json );
};

template <>
struct JsonV<uint32_t>
{
    static void toJson( JsonWriter &json, const uint32_t & v );
    static void fromJson( uint32_t &v, JsonReader &json );
};

template <>
struct JsonV<uint64_t>
{
    static void toJson( JsonWriter &json, const uint64_t & v );
    static void fromJson( uint64_t &v, JsonReader &json );
};

template <>
struct JsonV<float>
{
    static void toJson( JsonWriter &json, const float & v );
    static void fromJson( float &v, JsonReader &json );
};

template <>
struct JsonV<double>
{
    static void toJson( JsonWriter &json, const double & v );
    static void fromJson( double &v, JsonReader &json );
};

template <>
struct JsonV<std::string>
{
    static void toJson( JsonWriter &json, const std::string & v );
    static void fromJson( std::string &v, JsonReader &json );
};

template <>
struct JsonV<ByteVector>
{
    static void toJson( JsonWriter &json, const ByteVector & v );
    static void fromJson( ByteVector &v, JsonReader &json );
};

template <class T>
struct JsonV<std::vector<T>>
{
    static void toJson( JsonWriter &json, const std::vector<T> & v );
    static void fromJson( std::vector<T> &v, JsonReader &json );
};

template <class T>
void
JsonV<std::vector<T>>::toJson( JsonWriter &json, const std::vector<T> & v )
{
    json.startArray();
    for( typename std::vector<T>::const_iterator vi = v.begin(); vi != v.end(); vi++ )
        JsonV<T>::toJson( json, *vi );
    json.endArray();
}

template <class T>
void
JsonV<std::vector<T>>::fromJson( std::vector<T> & v, JsonReader &json  )
{
    match( json, JsonReader::START_ARRAY );
    while( !match0( json, JsonReader::END_ARRAY ) )
    {
        T el;
        JsonV<T>::fromJson( el, json );
        v.push_back(el);
    }
}

};

#endif
