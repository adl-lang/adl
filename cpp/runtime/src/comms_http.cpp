#include <adl/comms_http.h>

namespace ADL {

class HttpConnection : public Connection
{
public:
    virtual void send( const ByteVector &v );
};

struct HttpEndPoint : public EndPoint
{
    void close0();
    RawSinkDetails newUniqueRawSink( Callback<RawBuffer::Ptr> );
    RawSinkDetails newRawSink( const std::string & name, Callback<RawBuffer::Ptr> );
};

TransportName
HttpTransport::name()
{
    return TransportName("http");
}

Connection::Ptr
HttpTransport::connect( const TransportAddr & addr )
{
    return Connection::Ptr();
}

};
