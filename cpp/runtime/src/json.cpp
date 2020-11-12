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

Serialiser<Void>::Ptr
Serialisable<Void>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<Void>
    {
        void toJson( JsonWriter &json, const Void & v ) const
        {
            json.nullV();
        }

        void fromJson( Void &v, JsonReader &json )const
        {
            match( json, JsonReader::NULLV );
        }
    };

    static Serialiser<Void>::Ptr s;
    if( !s )
        s = Serialiser<Void>::Ptr( new S() );
    return s;
}

Serialiser<bool>::Ptr
Serialisable<bool>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<bool>
    {
        void toJson( JsonWriter &json, const bool & v ) const
        {
            json.boolV(v);
        }

        void fromJson( bool &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::BOOLEAN )
            {
                v = json.boolV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<bool>::Ptr s;
    if( !s )
        s = Serialiser<bool>::Ptr( new S() );
    return s;
}


Serialiser<int8_t>::Ptr
Serialisable<int8_t>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<int8_t>
    {
        void toJson( JsonWriter &json, const int8_t & v ) const
        {
            json.intV(v);
        }

        void fromJson( int8_t &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.intV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<int8_t>::Ptr s;
    if( !s )
        s = Serialiser<int8_t>::Ptr( new S() );
    return s;
}

Serialiser<int16_t>::Ptr
Serialisable<int16_t>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<int16_t>
    {
        void toJson( JsonWriter &json, const int16_t & v ) const
        {
            json.intV(v);
        }

        void fromJson( int16_t &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.intV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<int16_t>::Ptr s;
    if( !s )
        s = Serialiser<int16_t>::Ptr( new S() );
    return s;
}

Serialiser<int32_t>::Ptr
Serialisable<int32_t>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<int32_t>
    {
        void toJson( JsonWriter &json, const int32_t & v ) const
        {
            json.intV(v);
        }

        void fromJson( int32_t &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.intV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<int32_t>::Ptr s;
    if( !s )
        s = Serialiser<int32_t>::Ptr( new S() );
    return s;
}

Serialiser<int64_t>::Ptr
Serialisable<int64_t>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<int64_t>
    {
        void toJson( JsonWriter &json, const int64_t & v ) const
        {
            json.intV(v);
        }

        void fromJson( int64_t &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.intV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<int64_t>::Ptr s;
    if( !s )
        s = Serialiser<int64_t>::Ptr( new S() );
    return s;
}

Serialiser<uint8_t>::Ptr
Serialisable<uint8_t>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<uint8_t>
    {
        void toJson( JsonWriter &json, const uint8_t & v ) const
        {
            json.uintV(v);
        }

        void fromJson( uint8_t &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.uintV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<uint8_t>::Ptr s;
    if( !s )
        s = Serialiser<uint8_t>::Ptr( new S() );
    return s;
}

Serialiser<uint16_t>::Ptr
Serialisable<uint16_t>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<uint16_t>
    {
        void toJson( JsonWriter &json, const uint16_t & v ) const
        {
            json.uintV(v);
        }

        void fromJson( uint16_t &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.uintV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<uint16_t>::Ptr s;
    if( !s )
        s = Serialiser<uint16_t>::Ptr( new S() );
    return s;
}

Serialiser<uint32_t>::Ptr
Serialisable<uint32_t>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<uint32_t>
    {
        void toJson( JsonWriter &json, const uint32_t & v ) const
        {
            json.uintV(v);
        }

        void fromJson( uint32_t &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.uintV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<uint32_t>::Ptr s;
    if( !s )
        s = Serialiser<uint32_t>::Ptr( new S() );
    return s;
}

Serialiser<uint64_t>::Ptr
Serialisable<uint64_t>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<uint64_t>
    {
        void toJson( JsonWriter &json, const uint64_t & v ) const
        {
            json.uintV(v);
        }

        void fromJson( uint64_t &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.uintV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<uint64_t>::Ptr s;
    if( !s )
        s = Serialiser<uint64_t>::Ptr( new S() );
    return s;
}

Serialiser<float>::Ptr
Serialisable<float>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<float>
    {
        void toJson( JsonWriter &json, const float & v ) const
        {
            json.doubleV(v);
        }

        void fromJson( float &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.doubleV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<float>::Ptr s;
    if( !s )
        s = Serialiser<float>::Ptr( new S() );
    return s;
}

Serialiser<double>::Ptr
Serialisable<double>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<double>
    {
        void toJson( JsonWriter &json, const double & v ) const
        {
            json.doubleV(v);
        }

        void fromJson( double &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::NUMBER )
            {
                v = json.doubleV();
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<double>::Ptr s;
    if( !s )
        s = Serialiser<double>::Ptr( new S() );
    return s;
}

Serialiser<std::string>::Ptr
Serialisable<std::string>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<std::string>
    {
        void toJson( JsonWriter &json, const std::string & v ) const
        {
            json.stringV(v);
        }

        void fromJson( std::string &v, JsonReader &json )const
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

    static Serialiser<std::string>::Ptr s;
    if( !s )
        s = Serialiser<std::string>::Ptr( new S() );
    return s;
}

Serialiser<ByteVector>::Ptr
Serialisable<ByteVector>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<ByteVector>
    {
        void toJson( JsonWriter &json, const ByteVector & v ) const
        {
            json.stringV(ByteVector::toLiteral(v));
        }

        void fromJson( ByteVector &v, JsonReader &json )const
        {
            if( json.type() == JsonReader::STRING )
            {
                v = ByteVector::fromLiteral( json.stringV() );
                json.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<ByteVector>::Ptr s;
    if( !s )
        s = Serialiser<ByteVector>::Ptr( new S() );
    return s;
}

Serialiser<nlohmann::json>::Ptr
Serialisable<nlohmann::json>::serialiser( const SerialiserFlags &  )
{
    struct S : public Serialiser<nlohmann::json>
    {
        void toJson(JsonWriter &writer, const nlohmann::json & v) const
        {
            if (v.is_null()) {
              writer.nullV();
            } else if (v.is_boolean()) {
              writer.boolV(v);
            } else if (v.is_number()) {
              writer.doubleV(v);
            } else if (v.is_object()) {
              writer.startObject();
              for(auto& el : v.items()) {
                writer.field(el.key());
                toJson(writer, el.value());
              }
              writer.endObject();
            } else if (v.is_array()) {
              writer.startArray();
              for(auto& el : v) {
                toJson(writer, el);
              }
              writer.endArray();
            } else if (v.is_string()) {
              writer.stringV(v);
            }
        }

        void fromJson(nlohmann::json &v, JsonReader &reader) const
        {
            if( reader.type() == JsonReader::START_OBJECT) {
              reader.next();
              v = nlohmann::json::object();
              while(reader.type() == JsonReader::FIELD) {
                std::string field = reader.fieldName();
                reader.next();
                nlohmann::json value;
                fromJson(value, reader);
                v[field] = value;
              }
              if (reader.type() != JsonReader::END_OBJECT) {
                throw json_parse_failure();
              }
              reader.next();
            } else if( reader.type() == JsonReader::START_ARRAY) {
              reader.next();
              v = nlohmann::json::array();
              while(reader.type() !=  JsonReader::END_ARRAY) {
                nlohmann::json value;
                fromJson(value, reader);
                v.push_back(value);
              }
              reader.next();
            } else if( reader.type() == JsonReader::NULLV) {
                v = nullptr;
                reader.next();
            } else if( reader.type() == JsonReader::BOOLEAN) {
                v = reader.boolV();
                reader.next();
            } else if( reader.type() == JsonReader::STRING ) {
                v = reader.stringV();
                reader.next();
            } else if( reader.type() == JsonReader::NUMBER) {
                v = reader.doubleV();
                reader.next();
            }
            else
                throw json_parse_failure();
        }
    };

    static Serialiser<nlohmann::json>::Ptr s;
    if( !s )
        s = Serialiser<nlohmann::json>::Ptr( new S() );
      return s;
  }

  };
