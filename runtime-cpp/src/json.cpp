#include <adl/json.h>

namespace ADL {

bool
ignoreField( JsonReader &json )
{
    if( json.type() != JsonReader::FIELD )
        throw json_parse_failure();
    json.next();
    ignore( json );
    return true;
}

void
ignore( JsonReader &json )
{
    switch( json.type() )
    {
    case JsonReader::START_OBJECT:
        match(json, JsonReader::START_OBJECT);
        while( json.type() != JsonReader::END_OBJECT )
        {
            match(json, JsonReader::FIELD);
            ignore( json );
        }
        match(json, JsonReader::END_OBJECT);
        break;

    case JsonReader::START_ARRAY:
        match(json, JsonReader::START_ARRAY);
        while( json.type() != JsonReader::END_ARRAY )
        {
            ignore( json );
        }
        match(json, JsonReader::END_ARRAY);
        break;

    case JsonReader::STRING:
    case JsonReader::NUMBER:
    case JsonReader::NULLV:
        json.next();
        break;

    default:
        throw json_parse_failure();
    }
}

void
JsonV<Void>::toJson( JsonWriter &json, const Void & v )
{
    json.nullV();
}

void
JsonV<Void>::fromJson( Void &v, JsonReader &json )
{
    match( json, JsonReader::NULLV );
}

void
JsonV<bool>::toJson( JsonWriter &json, const bool & v )
{
    json.boolV(v);
}

void
JsonV<bool>::fromJson( bool &v, JsonReader &json )
{
    if( json.type() == JsonReader::BOOLEAN )
    {
        v = json.boolV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<int8_t>::toJson( JsonWriter &json, const int8_t & v )
{
    json.intV(v);
}

void
JsonV<int8_t>::fromJson( int8_t &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.intV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<int16_t>::toJson( JsonWriter &json, const int16_t & v )
{
    json.intV(v);
}

void
JsonV<int16_t>::fromJson( int16_t &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.intV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<int32_t>::toJson( JsonWriter &json, const int32_t & v )
{
    json.intV(v);
}

void
JsonV<int32_t>::fromJson( int32_t &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.intV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<int64_t>::toJson( JsonWriter &json, const int64_t & v )
{
    json.intV(v);
}

void
JsonV<int64_t>::fromJson( int64_t &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.intV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<uint8_t>::toJson( JsonWriter &json, const uint8_t & v )
{
    json.uintV(v);
}

void
JsonV<uint8_t>::fromJson( uint8_t &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.uintV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<uint16_t>::toJson( JsonWriter &json, const uint16_t & v )
{
    json.uintV(v);
}

void
JsonV<uint16_t>::fromJson( uint16_t &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.uintV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<uint32_t>::toJson( JsonWriter &json, const uint32_t & v )
{
    json.uintV(v);
}

void
JsonV<uint32_t>::fromJson( uint32_t &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.uintV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<uint64_t>::toJson( JsonWriter &json, const uint64_t & v )
{
    json.uintV(v);
}

void
JsonV<uint64_t>::fromJson( uint64_t &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.uintV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<float>::toJson( JsonWriter &json, const float & v )
{
    json.doubleV(v);
}

void
JsonV<float>::fromJson( float &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.doubleV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<double>::toJson( JsonWriter &json, const double & v )
{
    json.doubleV(v);
}

void
JsonV<double>::fromJson( double &v, JsonReader &json )
{
    if( json.type() == JsonReader::NUMBER )
    {
        v = json.doubleV();
        json.next();
    }
    else
        throw json_parse_failure();
}

void
JsonV<std::string>::toJson( JsonWriter &json, const std::string & v )
{
    json.stringV(v);
}

void
JsonV<std::string>::fromJson( std::string &v, JsonReader &json )
{
    if( json.type() == JsonReader::STRING )
    {
        v = json.stringV();
        json.next();
    }
    else
        throw json_parse_failure();
}

};
