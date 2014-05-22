#ifndef ADL_SINK_H
#define ADL_SINK_H

#include <adl/json.h>
#include <adl/sys.sinkimpl.h>

namespace ADL {

//----------------------------------------------------------------------
// Sink type

template <class T>
struct Sink
{
    Sink() : data( std::make_shared<ADL::sys::sinkimpl::SinkData>() ) {}

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

        void toJson( JsonWriter &json, const Sink<T> & v ) const
        {
            s->toJson( json, *v.data );
        }

        void fromJson( Sink<T> &v, JsonReader &json ) const
        {
            s->fromJson( *v.data, json );
        }

        Serialiser<ADL::sys::sinkimpl::SinkData>::Ptr s;
    };

    static typename Serialiser<Sink<T>>::Ptr serialiser( const SerialiserFlags & sf )
    {
        return typename Serialiser<Sink<T>>::Ptr( new S(sf) );
    }
};

}

#endif


