#include <functional>
#include <iostream>
#include <fstream>

#include <adl/comms.h>
#include <adl/comms_http.h>
#include <adl/sys.rpc.h>

using namespace ADL;
using namespace std::placeholders; 

typedef sys::rpc::Rpc<int,int> RpcReq;

class App
{
public:
    App( CommsContext::Ptr ctx ) : ctx_(ctx) {}

    void processRpcReq( const RpcReq &m ) {
        int result = m.params + 1;

        ctx_->connect( m.replyTo )->send( result );
    }

    CommsContext::Ptr ctx_;
};

const char *sinkRefFile = "/tmp/test.sink";
SerialiserFlags sflags;

int
main( int argc, const char * argv[] )
{
    CommsContext::Ptr ctx = std::make_shared<CommsContext>();
    HttpTransport::Ptr http = std::make_shared<HttpTransport>(5600,5600);
    ctx->registerTransport( http );

    App app(ctx);

    Callback<RpcReq> callback = std::bind( &App::processRpcReq, app, _1 );
    LocalSink<RpcReq>::Ptr lsink = ctx->newLocalSink<RpcReq>( http->getEndpoint(), "test", callback );

    // Write the sink reference to a file
    std::ofstream out(sinkRefFile);
    out << toJsonString( lsink->sink(), true, SerialiserFlags() );
    out.close();

    // And run the mainloop
    http->mainloop();
}
