#include "catch/catch.hpp"

#include <unistd.h>
#include <future>
#include <adl/comms.h>
#include <adl/comms_http.h>

template <class T>
struct Handler
{
    Handler( const T & v_ ) : v(v_) {}

    void operator()( const T & v_ ) {
	v = v_;
    }

    T v;
};

TEST_CASE( "Instantiate an http local sink", "[comms]" )
{
    using namespace ADL;
    using namespace ADL::comms;
    using namespace ADL::comms::http;

    HttpEndPoint::Ptr ep = createEndPoint( "localhost", 5000, 5100 );
    Handler<int> h(0);
    LocalSink<int>::Ptr lsink = ep->newLocalSink<int>( "test", std::function<void(int)>( h ) );
};

TEST_CASE( "Instantiate a ConnectionFactory, configured for http transport", "[comms]" )
{
    using namespace ADL;
    using namespace ADL::comms;
    using namespace ADL::comms::http;

    ConnectionFactory::Ptr cf = std::make_shared<ConnectionFactory>();
    cf->registerTransport( transportName(), connector() );
};

TEST_CASE( "Transmit an int value, via HTTP", "[comms]" )
{
    using namespace ADL;
    using namespace ADL::comms;
    using namespace ADL::comms::http;

    HttpEndPoint::Ptr ep = createEndPoint( "localhost", 52500, 52600 );
    Handler<int> h(0);
    LocalSink<int>::Ptr lsink = ep->newLocalSink<int>( "test", std::function<void(int)>( h ) );

    ConnectionFactory::Ptr cf = std::make_shared<ConnectionFactory>();
    cf->registerTransport( transportName(), connector() );
    Connection<int>::Ptr c = cf->connect( lsink->sink() );

    // Need to
    //    1) start the endpoint in the background
    //    2) wait for the endpoint to be live
    //    3) send a new value for the integer
    //    4) wait for the value to arrive in h
    //    5) shut down the endpoint

    //    throw std::runtime_error( "test not yet implemented" );
};
