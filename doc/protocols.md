ADL can be used to describe a variety of application level
communication protocols. This document describes several of them.

# Asynchronous Messaging

Asynchronous message sends are built into the ADL language core, via
the Sink primitive. A value of type `Sink<T>` represents some target
to which a value of type `T` may be sent. The details of the
communications (eg serialisation, protocol, reliability etc) are left
to the runtime implementation and are under the control the
application. A Sink value may be unforgeable - in these circumstances
it can be considered to be a [security capability][1].

# Remote Procedure Calls

A remote procedure call (RPC) reflects a call-response protocol.
The payload for an RPC request has type `Rpc<I,O>`. `I` is the type of of
the call parameters, and `O` is the type of the response. In ADL an RPC
payload has the definition:

```
struct Rpc<I,O>
{
    I params;
    Sink<O> replyTo;
};
```

Hence an Rpc _payload_ for a request taking a String and expecting a
Bool in response would have type `Rpc<String,Bool>`. An RPC _service_
that handles such requests would be a sink to which we can send such
payloads, and have type `Sink<Rpc<String,Bool>>`.

To implement an RPC, a service must:

  1. instantiate a sink of type `Sink<Rpc<I,O>>`, and somehow publish it.
  2. await a message of type `Rpc<I,O>` on the sink
  3. extract the I value and perform the local function to produce the O value.
  4. send the O value back to the caller via the replyTo sink.

Whereas an RPC client must:

  1. construct a local `Sink<O>` to receive the results
  2. populate the `Rpc<I,O>` payload with the I value and local sink,
     and send it to the service sink.
  4. await the response of type O on the local sink.

Both the server and client side implementation can be abstracted in
appropriate library functions.

An RPC request can fail for application specific reasons. If only a
failure string is required, then an error type like:

    union Error<T>
    {
        T value;
        String error;
    };

can be used to capture the possibility of failure with an associated
error message. Hence an RPC for a request taking a String and with a
Bool result, with possible failure would have type
`Rpc<String,Error<Bool>>`. More complex failure representations (eg
returning error codes in addition messages) are of course possible.

A single service may handle a variety of RPC requests. This can be
specified using an appropriate ADL union definition. For example, a
service that stored key-value pairs, might have the following RPC
requests:

   - put a key-value pair
   - delete a key
   - retrieve the value associated with a specific key
   - retrieve all active keys

Such a service could be defined as follows:

    union KVRequest<K,V>
    {
        Pair<K,V>           put;
        K                   delete;
        Rpc<K,Maybe<V>>     get;
        Rpc<Void,Vector<K>> getkeys; 
    };

    type KVService<K,V> = Sink<KVRequest<K,V>>;

This service actually combines asynchronous messaging with RPC
requests. No response is necessary (or expected) for the put and
delete selectors. The above definitions are abstract in the types of
the keys and values. A service would be configured and deployed for a
concrete type of course, eg for string keys and values:

    type MyKVService = KVService<String,String>;

# Streaming Results

An RPC requires that a single message is sent for the request, and a
single message sent back for the response. This may not be appropriate
when the results are of unknown size and potentially large. In these
circumstances it can be useful to stream the results back as
successive messages:

    union StreamItem<V>
    {
        V value;
        Void endStream;       
    };

    struct StreamReq0<I,O>
    {
        I params;
        Sink<StreamItem<O>> resultsTo;
    };

    type Signal = Sink<Void>;

    type StreamReq<I,O> = Rpc<StreamReq0<I,O>,Error<Signal>>;

A `StreamItem<V>` is either a value of type V, or a marker to indicate
the end of data associated with a stream.

A `Signal` is a contentless message used to indicate that some event
has occurred, perhaps to trigger some state change.

A StreamReq is an RPC which attempts to start a streaming request. If
successful, a `Signal` is returned immediately to the caller, and the
results begin to be sent to the `resultsTo` sink provided. When all
results have been sent, an `endStream` value is sent. The caller can
use the Signal to cancel the request early.

The KVService described previously could use a stream request to
better handle the potentially large results vector from `getKeys`:

    union KVRequest<K,V>
    {
        Pair<K,V>           put;
        K                   delete;
        Rpc<K,Maybe<V>>     get;
        StreamReq<Void,K>   getkeys; 
    };

# Continuous Queries

A continuous query can be considered an extention of a streaming
request: given some request parameters it produces an initial result,
and then continues to stream updates to that result until
cancelled. For this example we assume that the query parameters are of
type Q, and the results are a map with keys of type K, and values of
type V. Consider the following ADL defintions:

    union MapUpdate<K,V>
    {
        Pair<K,V> update;
        K delete;
    };

    type CtsQueryItem<K,V> = StreamItem<MapUpdate<K,V>>;

    struct CtsQueryReq<Q,K,V>
    {
        Q query;
        Sink<CtsQueryItem<K,V>> resultsTo;
    };

    type CtsQuery<Q,K,V> = Rpc<CtsQueryReq<Q,K,V>,Error<Signal>>;

`CtsQueryItem<K,V>` defines the stream of results that will be
received be the client. The client will receive:

  1. multiple updates, one for every value initially matching the query
  2. a single endStream value, indicating that the initial set is complete
  3. subsequent update and delete values reflecting real-time changes
     to the matching values

`CtsQueryReq<Q,K,V>` defines the data that a client must provide to a
service for a continuous query. Specifically, it must include the
query parameters themselves, and the sink to which the stream of
results are to be sent.

As for a streaming query, the `Signal` is used to cancel a continuous
query.

Hence to execute a continuous query, a client must:

  1. instantiate a sink of type `Sink<CtsQueryItem<K,V>>` to receive
     the results.
  2. Make an Rpc request with call parameter type `CtsQueryReq<Q,K,V>`,
     saving the resulting Signal
  3. process the results received on the sink, as long as is required
  4. Trigger the Signal to cancel the query.

The previously defined abstract KVService could be extended to handle
continuous queries:

    union KVRequest<Q,K,V>
    {
        Rpc<Pair<K,V>,Void> put;
        Rpc<K,Void>         delete;
        Rpc<K,Maybe<V>>     get;
        Rpc<Void,Vector<K>> getkeys; 
        CtsQueryReq<Q,K,V>  query;
    };

    type KVService<Q,K,V> = Sink<KVRequest<Q,K,V>>;

    type KeyPattern = String;
    type MyKVService = KVService<KeyPattern,String,String>;

     
[1]: http://en.wikipedia.org/wiki/Capability-based_security
