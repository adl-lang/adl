#ifndef ADL_COMMS_H
#define ADL_COMMS_H

#include <memory>
#include <assert.h>
#include <sstream>
#include <map>
#include <functional>
#include <adl/types.h>
#include <adl/sink.h>
#include <adl/jsonimpl.h>
#include <adl/sys.sinkimpl.h>

namespace ADL {

using TransportName = ADL::sys::sinkimpl::TransportName;
using TransportAddr = ADL::sys::sinkimpl::TransportAddr;
using SerialisationType = ADL::sys::sinkimpl::SerialisationType;

// Base class for any type that needs to be closed()
// Derived class must implement close0(), and this
// class ensures close0() will be called exactly once.

class Closeable
{
public:
    Closeable();
    virtual ~Closeable();
    void close();

    typedef std::shared_ptr<Closeable> Ptr;

protected:
    virtual void close0() = 0;
    bool closed_;
};

template <class T>
class BSerialiser
{
public:
    virtual void toBytes( ByteVector &vs, const T& ) = 0;
    virtual void fromBytes( T&, const ByteVector &vs ) = 0;

    typedef std::shared_ptr<BSerialiser> Ptr;
};

class Connection : public Closeable
{
public:
    virtual void send( const ByteVector &v ) = 0;

    typedef std::shared_ptr<Connection> Ptr;
};


class Transport : public Closeable
{
public:
    virtual TransportName name() = 0;
    virtual Connection::Ptr connect( const TransportAddr & addr ) = 0;

    typedef std::shared_ptr<Transport> Ptr;
};

template <class T>
using Callback = std::function<void(T)>;

class RawBuffer
{
public:
    std::string buffer;

    typedef std::shared_ptr<RawBuffer> Ptr;
};    

class EndPoint : public Closeable
{
public:
    struct RawSinkDetails
    {
        TransportName name;
        TransportAddr addr;
        Closeable::Ptr closef;
    };

    virtual RawSinkDetails newUniqueRawSink( Callback<RawBuffer::Ptr> ) = 0;
    virtual RawSinkDetails newRawSink( std::string name, Callback<RawBuffer::Ptr> ) = 0;

    typedef std::shared_ptr<EndPoint> Ptr;
};

template<class T>
class SinkConnection : public Closeable
{
public:
    SinkConnection( Connection::Ptr c, typename BSerialiser<T>::Ptr s );

    void close0();
    void send( const T & );

    typedef std::shared_ptr<SinkConnection> Ptr;

private:
    Connection::Ptr connection_;
    typename BSerialiser<T>::Ptr serialiser_;
};

template<class T>
class LocalSink : public Closeable
{
public:
    virtual Sink<T> sink() = 0;

    typedef std::shared_ptr<LocalSink> Ptr;
};

class CommsContext : public Closeable
{
public:
    void close0();
    void registerTransport( Transport::Ptr transport );

    template <class T>
    typename SinkConnection<T>::Ptr
    connect( const Sink<T> &s );

    template <class T>
    typename LocalSink<T>::Ptr
    newUniqueLocalSink( EndPoint::Ptr, Callback<T> callback );

    template <class T>
    typename LocalSink<T>::Ptr
    newLocalSink( EndPoint::Ptr, const std::string & name, Callback<T> callback );

    typedef std::shared_ptr<CommsContext> Ptr;

    Connection::Ptr connect0( std::shared_ptr<ADL::sys::sinkimpl::SinkData> );

private:

    std::map<TransportName,Transport::Ptr> transportRegister_;
    ADL::SerialiserFlags flags_;
};

template <class T>
SinkConnection<T>::SinkConnection( Connection::Ptr c, typename BSerialiser<T>::Ptr s )
    : connection_(c), serialiser_(s)
{
}

template <class T>
void
SinkConnection<T>::send( const T &v )
{
    ByteVector bs;
    serialiser_->toBytes( bs, v );
    connection_->send( bs );
}

template <class T>
class JsonBSerialiser : public BSerialiser<T>
{
public:
    JsonBSerialiser( typename ADL::Serialiser<T>::Ptr s )
        : s_(s) {}

    void toBytes( ByteVector &vs, const T &v )
    {
        std::stringstream str;
        StreamJsonWriter jw( str, false );
        s_->toJson( jw, v );

        // FIXME: Should actually encode here
        // vs = str
    }

    void fromBytes( T&v, const ByteVector &vs )
    {
        // FIXME: Should actually decode here
        // std::istringstream is(vs);
        // nStreamJsonReader jr(is);
        // s_->fromJson( v, jr );

    }

private:
    typename ADL::Serialiser<T>::Ptr s_;
};

template <class T>
typename SinkConnection<T>::Ptr
CommsContext::connect( const Sink<T> &s )
{
    Connection::Ptr connection = connect0( s.data );

    // Get the serialiser
    typename ADL::Serialiser<T>::Ptr ser = ADL::Serialisable<T>::serialiser( flags_ );

    // And construct the typed connection
    if( s.data->serialisation.value == "json" )
    {
        typename JsonBSerialiser<T>::Ptr js = std::make_shared<JsonBSerialiser<T>>( ser );
        return std::make_shared<SinkConnection<T>>( connection, js );
    }
    else
        assert( false ); // Unknown serialisation method
}

}; // namespace ADL

#endif
