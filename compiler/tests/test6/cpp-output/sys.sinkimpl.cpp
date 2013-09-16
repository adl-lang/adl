#include "sys.sinkimpl.h"

namespace ADL {
namespace sys {
namespace sinkimpl {

TransportAddr::TransportAddr()
    : d_(STRINGV), p_(new std::string())
{
}

TransportAddr TransportAddr::mk_stringv( const std::string & v )
{
    return TransportAddr( STRINGV, new std::string(v) );
}

TransportAddr TransportAddr::mk_intv( const uint64_t & v )
{
    return TransportAddr( INTV, new uint64_t(v) );
}

TransportAddr TransportAddr::mk_arrayv( const std::vector<TransportAddr>  & v )
{
    return TransportAddr( ARRAYV, new std::vector<TransportAddr> (v) );
}

TransportAddr::TransportAddr( const TransportAddr & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

TransportAddr::~TransportAddr()
{
    free(d_,p_);
}

TransportAddr & TransportAddr::operator=( const TransportAddr & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
}

const std::string & TransportAddr::set_stringv(const std::string &v)
{
    if( d_ == STRINGV )
    {
        *(std::string *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = STRINGV;
        p_ = new std::string(v);
    }
    return *(std::string *)p_;
}

const uint64_t & TransportAddr::set_intv(const uint64_t &v)
{
    if( d_ == INTV )
    {
        *(uint64_t *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = INTV;
        p_ = new uint64_t(v);
    }
    return *(uint64_t *)p_;
}

const std::vector<TransportAddr>  & TransportAddr::set_arrayv(const std::vector<TransportAddr>  &v)
{
    if( d_ == ARRAYV )
    {
        *(std::vector<TransportAddr>  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = ARRAYV;
        p_ = new std::vector<TransportAddr> (v);
    }
    return *(std::vector<TransportAddr>  *)p_;
}

TransportAddr::TransportAddr(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void TransportAddr::free(DiscType d, void *p)
{
    switch( d )
    {
        case STRINGV: delete (std::string *)p; return;
        case INTV: delete (uint64_t *)p; return;
        case ARRAYV: delete (std::vector<TransportAddr>  *)p; return;
    }
}

void * TransportAddr::copy( DiscType d, void *p )
{
    switch( d )
    {
        case STRINGV: return new std::string(*(std::string *)p);
        case INTV: return new uint64_t(*(uint64_t *)p);
        case ARRAYV: return new std::vector<TransportAddr> (*(std::vector<TransportAddr>  *)p);
    }
}

bool
operator<( const TransportAddr &a, const TransportAddr &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case TransportAddr::STRINGV: return a.stringv() < b.stringv();
        case TransportAddr::INTV: return a.intv() < b.intv();
        case TransportAddr::ARRAYV: return a.arrayv() < b.arrayv();
    }
}

bool
operator==( const TransportAddr &a, const TransportAddr &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case TransportAddr::STRINGV: return a.stringv() == b.stringv();
        case TransportAddr::INTV: return a.intv() == b.intv();
        case TransportAddr::ARRAYV: return a.arrayv() == b.arrayv();
    }
}

SinkData::SinkData()
    : transport(TransportName("null"))
    , address(TransportAddr::mk_stringv(""))
    , serialisation(SerialisationType("json"))
{
}

SinkData::SinkData(
    const TransportName & transport_,
    const TransportAddr & address_,
    const SerialisationType & serialisation_
    )
    : transport(transport_)
    , address(address_)
    , serialisation(serialisation_)
{
}

bool
operator<( const SinkData &a, const SinkData &b )
{
    if( a.transport < b.transport ) return true;
    if( b.transport < a.transport ) return false;
    if( a.address < b.address ) return true;
    if( b.address < a.address ) return false;
    if( a.serialisation < b.serialisation ) return true;
    if( b.serialisation < a.serialisation ) return false;
    return false;
}

bool
operator==( const SinkData &a, const SinkData &b )
{
    return
        a.transport == b.transport &&
        a.address == b.address &&
        a.serialisation == b.serialisation ;
}

}}}; // ADL::sys::sinkimpl

namespace ADL {

void
JsonV<ADL::sys::sinkimpl::TransportAddr>::toJson( JsonWriter &json, const ADL::sys::sinkimpl::TransportAddr & v )
{
    json.startObject();
    switch( v.d() )
    {
        case ADL::sys::sinkimpl::TransportAddr::STRINGV: writeField( json, "stringv", v.stringv() ); break;
        case ADL::sys::sinkimpl::TransportAddr::INTV: writeField( json, "intv", v.intv() ); break;
        case ADL::sys::sinkimpl::TransportAddr::ARRAYV: writeField( json, "arrayv", v.arrayv() ); break;
    }
    json.endObject();
}

void
JsonV<ADL::sys::sinkimpl::TransportAddr>::fromJson( ADL::sys::sinkimpl::TransportAddr &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        if( matchField0( "stringv", json ) )
            v.set_stringv(getFromJson<std::string>( json ));
        else if( matchField0( "intv", json ) )
            v.set_intv(getFromJson<uint64_t>( json ));
        else if( matchField0( "arrayv", json ) )
            v.set_arrayv(getFromJson<std::vector<ADL::sys::sinkimpl::TransportAddr> >( json ));
        else
            throw json_parse_failure();
    }
}

void
JsonV<ADL::sys::sinkimpl::SinkData>::toJson( JsonWriter &json, const ADL::sys::sinkimpl::SinkData & v )
{
    json.startObject();
    writeField<ADL::sys::sinkimpl::TransportName>( json, "transport", v.transport );
    writeField<ADL::sys::sinkimpl::TransportAddr>( json, "address", v.address );
    writeField<ADL::sys::sinkimpl::SerialisationType>( json, "serialisation", v.serialisation );
    json.endObject();
}

void
JsonV<ADL::sys::sinkimpl::SinkData>::fromJson( ADL::sys::sinkimpl::SinkData &v, JsonReader &json )
{
    match( json, JsonReader::START_OBJECT );
    while( !match0( json, JsonReader::END_OBJECT ) )
    {
        readField<ADL::sys::sinkimpl::TransportName>( v.transport, "transport", json ) ||
        readField<ADL::sys::sinkimpl::TransportAddr>( v.address, "address", json ) ||
        readField<ADL::sys::sinkimpl::SerialisationType>( v.serialisation, "serialisation", json ) ||
        ignoreField( json );
    }
}

}; // ADL