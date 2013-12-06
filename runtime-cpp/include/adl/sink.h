#ifndef ADL_SINK_H
#define ADL_SINK_H

#include <adl/json.h>
#include <adl/sys.sinkimpl.h>

namespace ADL {

//----------------------------------------------------------------------
// Sink type

template <class T>
struct Sink {
    std::shared_ptr<ADL::sys::sinkimpl::SinkData> data;
};

template <class T>
struct Serialisable<Sink<T>>
{
    struct S : public Serialiser<Sink<T>>
    {
        S( const SerialiserFlags &sf )
            : s( Serialisable<ADL::sys::sinkimpl::SinkData>::serialiser(sf) )
        {}

        void toJson( JsonWriter &json, const Sink<T> & v )
        {
            s->toJson( json, v.data );
        }

        void fromJson( std::vector<T> &v, JsonReader &json )
        {
            s->fromJson( v.data, json );
        }

        Serialiser<ADL::sys::sinkimpl::SinkData>::Ptr s;
    };

    static typename Serialiser<Sink<T>>::Ptr serialiser( const SerialiserFlags & sf )
    {
        return Serialiser<Sink<T>>::Ptr( new S(sf) );
    }
};

}

#endif


