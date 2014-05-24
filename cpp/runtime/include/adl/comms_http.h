#ifndef ADL_COMMS_HTTP_H
#define ADL_COMMS_HTTP_H

#include <adl/comms.h>

namespace ADL {
namespace comms {
namespace http {

// Get the identifier for the http transport

TransportName transportName();

// Get a raw connector for http connections

RawConnectionFactory::Ptr connector();


// An HttpEndPoint is a regular endpoint extended
// with a run() method to execute the main server
// loop

class HttpEndPoint : public EndPoint
{
public:
    // Start the processing loop 
    virtual void run() = 0;

    typedef std::shared_ptr<HttpEndPoint> Ptr;
};


// Construct an HttpEndPoint. The server listends on a free port
// between minPort and maxPort. The port will be fixed in minPort ==
// maxPort.

HttpEndPoint::Ptr createEndPoint( const std::string & host, int minPort, int maxPort );

};
};
};


#endif
