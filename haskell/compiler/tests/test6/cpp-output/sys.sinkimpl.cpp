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

typename Serialiser<ADL::sys::sinkimpl::TransportAddr>::Ptr
Serialisable<ADL::sys::sinkimpl::TransportAddr>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::sinkimpl::TransportAddr _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<std::string>::Ptr stringv_;
        mutable typename Serialiser<uint64_t>::Ptr intv_;
        mutable typename Serialiser<std::vector<ADL::sys::sinkimpl::TransportAddr> >::Ptr arrayv_;
        
        typename Serialiser<std::string>::Ptr stringv_s() const
        {
            if( !stringv_ )
                stringv_ = Serialisable<std::string>::serialiser(sf_);
            return stringv_;
        }
        
        typename Serialiser<uint64_t>::Ptr intv_s() const
        {
            if( !intv_ )
                intv_ = Serialisable<uint64_t>::serialiser(sf_);
            return intv_;
        }
        
        typename Serialiser<std::vector<ADL::sys::sinkimpl::TransportAddr> >::Ptr arrayv_s() const
        {
            if( !arrayv_ )
                arrayv_ = Serialisable<std::vector<ADL::sys::sinkimpl::TransportAddr> >::serialiser(sf_);
            return arrayv_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            switch( v.d() )
            {
                case ADL::sys::sinkimpl::TransportAddr::STRINGV: writeField( json, stringv_s(), "stringv", v.stringv() ); break;
                case ADL::sys::sinkimpl::TransportAddr::INTV: writeField( json, intv_s(), "intv", v.intv() ); break;
                case ADL::sys::sinkimpl::TransportAddr::ARRAYV: writeField( json, arrayv_s(), "arrayv", v.arrayv() ); break;
            }
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                if( matchField0( "stringv", json ) )
                    v.set_stringv(stringv_s()->fromJson( json ));
                else if( matchField0( "intv", json ) )
                    v.set_intv(intv_s()->fromJson( json ));
                else if( matchField0( "arrayv", json ) )
                    v.set_arrayv(arrayv_s()->fromJson( json ));
                else
                    throw json_parse_failure();
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

typename Serialiser<ADL::sys::sinkimpl::SinkData>::Ptr
Serialisable<ADL::sys::sinkimpl::SinkData>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::sinkimpl::SinkData _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : transport_s( Serialisable<ADL::sys::sinkimpl::TransportName>::serialiser(sf) )
            , address_s( Serialisable<ADL::sys::sinkimpl::TransportAddr>::serialiser(sf) )
            , serialisation_s( Serialisable<ADL::sys::sinkimpl::SerialisationType>::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::sys::sinkimpl::TransportName>::Ptr transport_s;
        typename Serialiser<ADL::sys::sinkimpl::TransportAddr>::Ptr address_s;
        typename Serialiser<ADL::sys::sinkimpl::SerialisationType>::Ptr serialisation_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::sys::sinkimpl::TransportName>( json, transport_s, "transport", v.transport );
            writeField<ADL::sys::sinkimpl::TransportAddr>( json, address_s, "address", v.address );
            writeField<ADL::sys::sinkimpl::SerialisationType>( json, serialisation_s, "serialisation", v.serialisation );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( transport_s, v.transport, "transport", json ) ||
                readField( address_s, v.address, "address", json ) ||
                readField( serialisation_s, v.serialisation, "serialisation", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL