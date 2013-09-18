#ifndef SYS_RPC_H
#define SYS_RPC_H
#include <adl/adl.h>
#include <adl/sink.h>

namespace ADL {
namespace sys {
namespace rpc {

template <class I, class O>
struct Rpc
{
    Rpc();
    
    Rpc(
        const I & params,
        const Sink<O>  & replyTo
        );
    
    I params;
    Sink<O>  replyTo;
};

template <class I, class O>
bool operator<( const Rpc<I,O> &a, const Rpc<I,O> &b );
template <class I, class O>
bool operator==( const Rpc<I,O> &a, const Rpc<I,O> &b );

template <class I, class O>
Rpc<I,O>::Rpc()
{
}

template <class I, class O>
Rpc<I,O>::Rpc(
    const I & params_,
    const Sink<O>  & replyTo_
    )
    : params(params_)
    , replyTo(replyTo_)
{
}

template <class I, class O>
bool
operator<( const Rpc<I,O> &a, const Rpc<I,O> &b )
{
    if( a.params < b.params ) return true;
    if( b.params < a.params ) return false;
    if( a.replyTo < b.replyTo ) return true;
    if( b.replyTo < a.replyTo ) return false;
    return false;
}

template <class I, class O>
bool
operator==( const Rpc<I,O> &a, const Rpc<I,O> &b )
{
    return
        a.params == b.params &&
        a.replyTo == b.replyTo ;
}

template <class I, class O>
using RpcSvc = Sink<Rpc<I,O> > ;

}}}; // ADL::sys::rpc

namespace ADL {

template <class I, class O>
struct JsonV<ADL::sys::rpc::Rpc<I,O>>
{
    static void toJson( JsonWriter &json, const ADL::sys::rpc::Rpc<I,O> & v );
    static void fromJson( ADL::sys::rpc::Rpc<I,O> &v, JsonReader &json );
};

template <class I, class O>
void
JsonV<ADL::sys::rpc::Rpc<I,O>>::toJson( JsonWriter &json, const ADL::sys::rpc::Rpc<I,O> & v )
{
    json.startObject();
    writeField<I>( json, "params", v.params );
    writeField<Sink<O> >( json, "replyTo", v.replyTo );
    json.endObject();
}

template <class I, class O>
void
JsonV<ADL::sys::rpc::Rpc<I,O>>::fromJson( ADL::sys::rpc::Rpc<I,O> &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        readField<I>( v.params, "params", json ) ||
        readField<Sink<O> >( v.replyTo, "replyTo", json ) ||
        ignoreField( json );
    }
}

}; // ADL
#endif // SYS_RPC_H