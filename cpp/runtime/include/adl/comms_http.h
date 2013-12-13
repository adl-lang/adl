#ifndef ADL_COMMS_HTTP_H
#define ADL_COMMS_HTTP_H

#include <adl/comms.h>

namespace ADL {

class HttpTransport : public Transport
{
public:
    virtual TransportName name();
    virtual Connection::Ptr connect( const TransportAddr & addr );

    EndPoint::Ptr newEndpoint( int minPort, int maxPort );
};

};


#endif
