#ifndef ADL_SINK_H
#define ADL_SINK_H

#include <adl/json.h>
#include <sys.sinkimpl.h>

namespace ADL {

//----------------------------------------------------------------------
// Sink type

template <class T>
struct Sink {
    std::shared_ptr<ADL::sys::sinkimpl::SinkData> data;
};

template <class T>
struct JsonV<Sink<T>>
{
    static void toJson( JsonWriter &json, const Sink<T> & v )
    {
        JsonV<ADL::sys::sinkimpl::SinkData>::toJson( json, v.data );
    }

    static void fromJson( std::vector<T> &v, JsonReader &json )
    {
        JsonV<ADL::sys::sinkimpl::SinkData>::fromJson( v.data, json );
    }
};

};

#endif


