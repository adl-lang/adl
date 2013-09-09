#include "json.h"

namespace ADL {

void
JsonV<int64_t>::toJson( JsonWriter &json, const int64_t & v )
{
    json.intV(v);
}


void
JsonV<int64_t>::fromJson( int64_t &v, JsonReader &json )
{
}

void
JsonV<std::string>::toJson( JsonWriter &json, const std::string & v )
{
    json.stringV(v);
}


void
JsonV<std::string>::fromJson( std::string &v, JsonReader &json )
{
}

};
