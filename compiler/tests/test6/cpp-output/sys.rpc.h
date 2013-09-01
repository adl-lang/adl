#include "adl.h"

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

}
}
}