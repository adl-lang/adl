#include <boost/network/protocol/http/client.hpp>
#include <boost/network/protocol/http/server.hpp>
#include <boost/network/uri.hpp>

#include <adl/comms_http.h>


namespace ADL {
namespace comms {
namespace http {

namespace bhttp = boost::network::http;
namespace uri = boost::network::uri;

uri::uri uriFromAddr( const TransportAddr & taddr )
{
    assert( taddr.d() == TransportAddr::ARRAYV );
    assert( taddr.arrayv().size() == 3 );
    assert( taddr.arrayv()[0].d() == TransportAddr::STRINGV );
    assert( taddr.arrayv()[1].d() == TransportAddr::INTV );
    assert( taddr.arrayv()[2].d() == TransportAddr::STRINGV );
    
    uri::uri uri;
    uri << uri::scheme( "http" );
    uri << uri::host( taddr.arrayv()[0].stringv() );
    uri << uri::port( taddr.arrayv()[1].intv() );
    uri << uri::path( taddr.arrayv()[2].stringv() );
    return uri;
}

class HttpConnection : public Connection<RawBufferPtr>
{
public:
    HttpConnection( const uri::uri &uri ) : uri_(uri)
    {}

    virtual void send( const RawBufferPtr &b )
    {
	using namespace boost::network;

        bhttp::client client;
        bhttp::client::request request(uri_);
	request << body(*b);
        bhttp::client::response response = client.put(request);
    }
    
    uri::uri uri_;
};

class HttpConnectionFactory : public RawConnectionFactory
{
public:
    virtual typename Connection<RawBufferPtr>::Ptr connect( const TransportAddr & addr )
    {
	return std::make_shared<HttpConnection>( uriFromAddr(addr) );
    }
};


TransportName transportName()
{
    return TransportName("http");
}

RawConnectionFactory::Ptr connector()
{
    return std::make_shared<HttpConnectionFactory>();
}


class Handler;
typedef bhttp::server<Handler> Server;

class Handler 
{
public:
    using Map = std::map<std::string,EndPoint::Callback<RawBufferPtr>>;
    using MapPtr = std::shared_ptr<Map>;

    Handler( const std::string &host, int port )
	: host_(host), port_(port), callbacks_(std::make_shared<Map>()), nextid_(0)
    {}

    void operator() (Server::request const &request, Server::response &response)
    {
        response = Server::response::stock_reply(
            Server::response::ok, "Hello, world!");
    }

    void log(Server::string_type const &info) {
        std::cerr << "ERROR: " << info << '\n';
    }

    RawSinkDetails newUniqueRawSink( EndPoint::Callback<RawBufferPtr> cb )
    {
	std::ostringstream os;
	os << nextid_++;
	return newRawSink(os.str(),cb);
    }

    RawSinkDetails newRawSink( const std::string & name, EndPoint::Callback<RawBufferPtr> cb )
    {
	assert( callbacks_->count( name) == 0 ); // Name is no talready bound
	(*callbacks_)[name] = cb;
	return RawSinkDetails( transportName(), transportAddress(name), closefn(name) );
    }

private:
    TransportAddr transportAddress( const std::string & name )
    {
	std::vector<TransportAddr> vs;
	vs.push_back( TransportAddr::mk_stringv( host_ ) );
	vs.push_back( TransportAddr::mk_intv( port_ ) );
	vs.push_back( TransportAddr::mk_stringv( name ) );
	return TransportAddr::mk_arrayv( vs );
    }

    static void closeRawSink( MapPtr callbacks, std::string name )
    {
	callbacks->erase(name);
    }

    CloseFn closefn( const std::string & name )
    {
	return std::bind( closeRawSink, callbacks_, name );
    }

    const std::string host_;
    const int port_;
    MapPtr callbacks_;
    int nextid_;
};

class HttpEndPointI : public HttpEndPoint
{
public:
    HttpEndPointI( const std::string & host, int minPort, int maxPort )
    {
	for( int i= minPort; i < maxPort; i++ )
	{
	    try {
		init(host,i);
	    }
	    catch( const std::runtime_error & ) {
		// It would be better if there was something more
		// specific than runtime_error to catch here.  Assume
		// the problem is that the port is already bound, so
		// loop and try the next port.
	    }
	}

	// Try the last port, and let any exception propagate
	init(host,maxPort);
    }

    
    sys::sinkimpl::SerialisationType serialisationType() {
	return sys::sinkimpl::SerialisationType( "json" );
    }

    RawSinkDetails newUniqueRawSink( Callback<RawBufferPtr> cb ) {
	return handler_->newUniqueRawSink( cb );
    }

    RawSinkDetails newRawSink( const std::string & name, Callback<RawBufferPtr> cb ) {
	return handler_->newRawSink( name, cb );
    }

    void run() {
	server_->run();
    }

    void close0() {
	server_->stop();
    }

private:
    void init( const std::string &host, int port )
    {
	std::ostringstream os;
	os << port;
	std::string ports = os.str();

	handler_.reset( new Handler( host, port ) );
	bhttp::server<Handler>::options options(*handler_);
	options.address( host ).port( ports );
	server_.reset( new Server( options ) );
	server_->listen();
    }

    std::unique_ptr<Handler> handler_;
    std::unique_ptr<Server> server_;
};

HttpEndPoint::Ptr createEndPoint( const std::string & host, int minPort, int maxPort )
{
    return std::make_shared<HttpEndPointI>( host, minPort, maxPort );
}
    
};
};
};
