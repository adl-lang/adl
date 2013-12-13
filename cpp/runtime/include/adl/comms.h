#ifndef ADL_COMMS_H
#define ADL_COMMS_H

#include <memory>
#include <adl/types.h>
#include <adl/sink.h>
#include <adl/sys.sinkimpl.h>

namespace ADL {

using TransportName = ADL::sys::sinkimpl::TransportName;
using TransportAddr = ADL::sys::sinkimpl::TransportAddr;

class Closeable
{
public:
    virtual void close() = 0;

    typedef std::shared_ptr<Closeable> Ptr;
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

template<class T>
class Callback
{
public:
    virtual void exec( const T &  ) = 0;

    typedef std::shared_ptr<Callback<T>> Ptr;
};

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

    RawSinkDetails newUniqueRawSink( Callback<RawBuffer::Ptr>::Ptr );
    RawSinkDetails newRawSink( std::string name, Callback<RawBuffer::Ptr>::Ptr );
};

template<class T>
class SinkConnection : public Closeable
{
public:
    virtual void send( const T & ) = 0;
};

template<class T>
class LocalSink : public Closeable
{
public:
    virtual Sink<T> sink() = 0;
};

class CommsContext : public Closeable
{
    void registerTransport( Transport::Ptr transport );

    template <class T>
    SinkConnection<T> connect( const Sink<T> &s );

    template <class T>
    typename LocalSink<T>::Ptr
    newUniqueLocalSink( EndPoint::Ptr, typename Callback<T>::Ptr callback );
};

}; // namespace ADL

#endif
