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
namespace comms {

using TransportName = ADL::sys::sinkimpl::TransportName;
using TransportAddr = ADL::sys::sinkimpl::TransportAddr;
using SerialisationType = ADL::sys::sinkimpl::SerialisationType;

typedef std::shared_ptr<std::string> RawBufferPtr;

// Closeable is a base class for any type that needs to be closed()
// Derived class must implement close0(), and should call close() from
// their destructor. This class ensures that close0() will be called
// exactly once.

class Closeable
{
public:
    Closeable();
    virtual ~Closeable() = 0;
    void close();

    typedef std::shared_ptr<Closeable> Ptr;

protected:
    virtual void close0() = 0;
    bool closed_;
};


// A Connection sends messages of type T to a remote sink

template <class T>
class Connection : public Closeable
{
public:
    virtual void send( const T &v ) = 0;

    typedef std::shared_ptr<Connection> Ptr;
};

// Each transport must provide a RawConnectionFactory, which
// is used to establish connections based upon addresses

class RawConnectionFactory : public Closeable
{
public:
    virtual typename Connection<RawBufferPtr>::Ptr connect( const TransportAddr & addr ) = 0;

    typedef std::shared_ptr<RawConnectionFactory> Ptr;
};

// A LocalSink is an object with an implementation in the current
// process that handles messages of type T. The sink() methods returns
// a Sink which references this LocalSink, and can be serialised
// and transmitted to remote processes.

template<class T>
class LocalSink : public Closeable
{
public:
    LocalSink( const Sink<T> &sink, Closeable::Ptr closef );

    virtual Sink<T> sink() ;
    virtual void close0();

    typedef std::shared_ptr<LocalSink> Ptr;

private:
    Sink<T> sink_;
    Closeable::Ptr closef_;
};

class RawSinkDetails;

// An Endpoint is used to construct LocalSinks and make them visible
// to the network.

class EndPoint : public Closeable
{
public:
    template <class T>
    using Callback = std::function<void(T)>;

    template <class T>
    typename LocalSink<T>::Ptr
    newUniqueLocalSink( Callback<T> callback );

    template <class T>
    typename LocalSink<T>::Ptr
    newLocalSink( const std::string & name, Callback<T> callback );

    typedef std::shared_ptr<EndPoint> Ptr;

protected:

    sys::sinkimpl::SerialisationType serialisationType_;

    virtual RawSinkDetails newUniqueRawSink( Callback<RawBufferPtr> ) = 0;
    virtual RawSinkDetails newRawSink( const std::string &name, Callback<RawBufferPtr> ) = 0;
};


// A ConnectionFactory is a concrete class used to establish
// connections with remote Sinks.

class ConnectionFactory : public Closeable
{
public:
    ~ConnectionFactory();
    void close0();
    void registerTransport( const TransportName &, RawConnectionFactory::Ptr );

    template <class T>
    typename Connection<T>::Ptr
    connect( const Sink<T> &s );

    typedef std::shared_ptr<ConnectionFactory> Ptr;

private:
    Connection<RawBufferPtr>::Ptr connect0( std::shared_ptr<ADL::sys::sinkimpl::SinkData> );

    std::map<TransportName,RawConnectionFactory::Ptr> transportFactories_;
};

const extern ADL::SerialiserFlags commsSerialiserFlags;


}; // namespace comms
}; // namespace ADL

#include <adl/commsimpl.h>

#endif
