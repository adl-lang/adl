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
struct Serialisable<ADL::sys::rpc::Rpc<I,O>>
{
    static typename Serialiser<ADL::sys::rpc::Rpc<I,O>>::Ptr serialiser(const SerialiserFlags &);
};

template <class I, class O>
typename Serialiser<ADL::sys::rpc::Rpc<I,O>>::Ptr
Serialisable<ADL::sys::rpc::Rpc<I,O>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::rpc::Rpc<I,O> _T;
    
    struct _S : public Serialiser<_T>
    {
        _S( const SerialiserFlags & sf )
            : params_s( Serialisable<I>::serialiser(sf) )
            , replyTo_s( Serialisable<Sink<O> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<I>::Ptr params_s;
        typename Serialiser<Sink<O> >::Ptr replyTo_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<I>( json, params_s, "params", v.params );
            writeField<Sink<O> >( json, replyTo_s, "replyTo", v.replyTo );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( params_s, v.params, "params", json ) ||
                readField( replyTo_s, v.replyTo, "replyTo", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new _S(sf) );
};

}; // ADL
#endif // SYS_RPC_H