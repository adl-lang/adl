#include <functional>
#include <iostream>
#include <fstream>

#include <adl/comms.h>
#include <adl/comms_http.h>
#include <adl/sys.rpc.h>

using namespace ADL;
using namespace ADL::comms;
using namespace std::placeholders; 

typedef sys::rpc::Rpc<int,int> RpcReq;

class App
{
public:
    App( ConnectionFactory::Ptr cf ) : cf_(cf) {}

    void processRpcReq( const RpcReq &m ) {
        int result = m.params + 1;

        cf_->connect( m.replyTo )->send( result );
    }

    ConnectionFactory::Ptr cf_;
};

const char *sinkRefFile = "/tmp/test.sink";
const char *host = "localhost";
const int port = 5500;

SerialiserFlags sflags;

int
main( int argc, const char * argv[] )
{
    ConnectionFactory::Ptr cf = std::make_shared<ConnectionFactory>();
    cf->registerTransport( http::transportName(), http::connector() );

    http::HttpEndPoint::Ptr ep = http::createEndPoint( host, port, port );

    App app(cf);

    EndPoint::Callback<RpcReq> callback = std::bind( &App::processRpcReq, app, _1 );
    ep->newLocalSink<RpcReq>( "test", callback );

    ep->run();
}
