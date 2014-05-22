#ifndef ADL_COMMS_HTTP_H
#define ADL_COMMS_HTTP_H

#include <adl/comms.h>

namespace ADL {

class HttpTransport : public Transport
{
public:
    // Configure an http transport, with port chosen
    // between minPort and maxPort
    HttpTransport( int minPort, int maxPort );

    // 
    EndPoint::Ptr getEndpoint();

    virtual void close0();
    virtual TransportName name();
    virtual Connection::Ptr connect( const TransportAddr & addr );


    void mainloop();

    typedef std::shared_ptr<HttpTransport> Ptr;
};

};


#endif
