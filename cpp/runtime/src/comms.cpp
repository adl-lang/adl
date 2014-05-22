#include <adl/comms.h>

namespace ADL {

Closeable::Closeable() : closed_(false)
{
}

Closeable::~Closeable()
{
}

void
Closeable::close()
{
    if( !closed_) 
    {
        close0();
        closed_ = true;
    }
}

CommsContext::~CommsContext()
{
    close();
}

void
CommsContext::close0()
{
}


void
CommsContext::registerTransport( Transport::Ptr transport )
{
    if( transportRegister_.find(transport->name()) != transportRegister_.end() )
        assert( false ); // Illegal re-registration of transport

    transportRegister_[transport->name()] = transport;
}

Connection::Ptr
CommsContext::connect0( std::shared_ptr<ADL::sys::sinkimpl::SinkData> sdata )
{
    // Locate the transport
    auto mi = transportRegister_.find( sdata->transport );
    if( mi == transportRegister_.end() )
        assert( false );  // No registered transport found
    Transport::Ptr t = mi->second;

    // Make a connection
    return t->connect( sdata->address );
}


};
