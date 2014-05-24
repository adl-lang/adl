#ifndef ADL_COMMSIMPL_H
#define ADL_COMMSIMPL_H

namespace ADL {
namespace comms {

class RawSinkDetails
{
public:
    RawSinkDetails( const TransportName &t, const TransportAddr &a, Closeable::Ptr c) 
	: transport(t), address(a), closef(c)
    {}

    TransportName transport;
    TransportAddr address;
    Closeable::Ptr closef;
};

template <class T>
class BSerialiser
{
public:
    virtual RawBufferPtr serialise( const T& ) = 0;
    virtual void deserialise( T&, RawBufferPtr b ) = 0;

    typedef std::shared_ptr<BSerialiser> Ptr;
};

template<class T>
class SinkConnection : public Connection<T>
{
public:
    SinkConnection( Connection<RawBufferPtr>::Ptr c, typename BSerialiser<T>::Ptr s );
    ~SinkConnection();

    void close0();
    void send( const T & );

    typedef std::shared_ptr<SinkConnection> Ptr;

private:
    Connection<RawBufferPtr>::Ptr connection_;
    typename BSerialiser<T>::Ptr serialiser_;
};

template <class T>
SinkConnection<T>::~SinkConnection()
{
    Closeable::close();
}

template <class T>
SinkConnection<T>::SinkConnection( Connection<RawBufferPtr>::Ptr c, typename BSerialiser<T>::Ptr s )
    : connection_(c), serialiser_(s)
{
}

template <class T>
void
SinkConnection<T>::close0()
{
    connection_->close();
}

template <class T>
void
SinkConnection<T>::send( const T &v )
{
    RawBufferPtr b = serialiser_->serialise( v );
    connection_->send( b );
}

template <class T>
class JsonBSerialiser : public BSerialiser<T>
{
public:
    JsonBSerialiser( typename ADL::Serialiser<T>::Ptr s )
        : s_(s) {}

    RawBufferPtr serialise( const T &v )
    {
        std::stringstream str;
        StreamJsonWriter jw( str, false );
        s_->toJson( jw, v );
	return std::make_shared<std::string>( str.str() );
    }

    void deserialise( T&v, RawBufferPtr b )
    {
        std::istringstream is(*b);
        StreamJsonReader jr(is);
        s_->fromJson( v, jr );
    }

private:
    typename ADL::Serialiser<T>::Ptr s_;
};


template <class T>
typename BSerialiser<T>::Ptr
getSerialiser( const std::string &method )
{
    typename ADL::Serialiser<T>::Ptr ser = ADL::Serialisable<T>::serialiser( commsSerialiserFlags );
    typename BSerialiser<T>::Ptr bs;
    if( method == "json" )
        return std::make_shared<JsonBSerialiser<T>>( ser );
    else
        assert( false ); // Unknown serialisation method
}

template <class T>
typename Connection<T>::Ptr
ConnectionFactory::connect( const Sink<T> &s )
{
    // Get the serialiser for T
    typename BSerialiser<T>::Ptr bs = getSerialiser<T>( s.data->serialisation.value );

    // And construct the typed connection
    Connection<RawBufferPtr>::Ptr connection = connect0( s.data );
    return std::make_shared<SinkConnection<T>>( connection, bs );
}

template <class T>
void
rawCallbackImpl( typename BSerialiser<T>::Ptr bs, EndPoint::Callback<T> callback, RawBufferPtr b )
{
    T v;
    bs->deserialise( v, b );
    callback( v );
}

template <class T>
typename LocalSink<T>::Ptr
EndPoint::newUniqueLocalSink( Callback<T> callback )
{
    // Get the serialiser for T
    typename BSerialiser<T>::Ptr bs = getSerialiser<T>( serialisationType_.value );

    // Build and connect a callback expecting RawBufferPtrs
    RawSinkDetails rsd = newUniqueRawSink( std::bind( rawCallbackImpl<T>, bs, callback, std::placeholders::_1 ) );
    Sink<T> sink;
    sink.data->transport = rsd.transport;
    sink.data->address = rsd.address;
    sink.data->serialisation = serialisationType_;
    return std::make_shared<LocalSink<T>>( sink, rsd.closef );
}

template <class T>
typename LocalSink<T>::Ptr
    EndPoint::newLocalSink( const std::string &name, Callback<T> callback )
{
    // Get the serialiser for T
    typename BSerialiser<T>::Ptr bs = getSerialiser<T>( serialisationType_.value );

    // Build and connect a callback expecting RawBufferPtrs
    RawSinkDetails rsd = newRawSink( name,  std::bind( rawCallbackImpl<T>, bs, callback, std::placeholders::_1 ));
    Sink<T> sink;
    sink.data->transport = rsd.transport;
    sink.data->address = rsd.address;
    sink.data->serialisation = serialisationType_;
    return std::make_shared<LocalSink<T>>( sink, rsd.closef );
}

};
};

#endif
