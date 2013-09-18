#ifndef SYS_SINKIMPL_H
#define SYS_SINKIMPL_H
#include <adl/adl.h>
#include <stdint.h>
#include <string>
#include <vector>

namespace ADL {
namespace sys {
namespace sinkimpl {

struct SerialisationType
{
    SerialisationType() {}
    explicit SerialisationType(const std::string & v) : value(v) {}
    
    std::string value;
};

inline
bool operator<( const SerialisationType &a, const SerialisationType &b ) { return a.value < b.value; }
inline
bool operator==( const SerialisationType &a, const SerialisationType &b ) { return a.value == b.value; }

class TransportAddr
{
public:
    TransportAddr();
    static TransportAddr mk_stringv( const std::string & v );
    static TransportAddr mk_intv( const uint64_t & v );
    static TransportAddr mk_arrayv( const std::vector<TransportAddr>  & v );
    
    TransportAddr( const TransportAddr & );
    ~TransportAddr();
    TransportAddr & operator=( const TransportAddr & );
    
    enum DiscType
    {
        STRINGV,
        INTV,
        ARRAYV
    };
    
    DiscType d() const;
    std::string & stringv() const;
    uint64_t & intv() const;
    std::vector<TransportAddr>  & arrayv() const;
    
    const std::string & set_stringv(const std::string & );
    const uint64_t & set_intv(const uint64_t & );
    const std::vector<TransportAddr>  & set_arrayv(const std::vector<TransportAddr>  & );
    
private:
    TransportAddr( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const TransportAddr &a, const TransportAddr &b );
bool operator==( const TransportAddr &a, const TransportAddr &b );

inline TransportAddr::DiscType TransportAddr::d() const
{
    return d_;
}

inline std::string & TransportAddr::stringv() const
{
    if( d_ == STRINGV )
    {
        return *(std::string *)p_;
    }
    throw invalid_union_access();
}

inline uint64_t & TransportAddr::intv() const
{
    if( d_ == INTV )
    {
        return *(uint64_t *)p_;
    }
    throw invalid_union_access();
}

inline std::vector<TransportAddr>  & TransportAddr::arrayv() const
{
    if( d_ == ARRAYV )
    {
        return *(std::vector<TransportAddr>  *)p_;
    }
    throw invalid_union_access();
}

struct TransportName
{
    TransportName() {}
    explicit TransportName(const std::string & v) : value(v) {}
    
    std::string value;
};

inline
bool operator<( const TransportName &a, const TransportName &b ) { return a.value < b.value; }
inline
bool operator==( const TransportName &a, const TransportName &b ) { return a.value == b.value; }

struct SinkData
{
    SinkData();
    
    SinkData(
        const TransportName & transport,
        const TransportAddr & address,
        const SerialisationType & serialisation
        );
    
    TransportName transport;
    TransportAddr address;
    SerialisationType serialisation;
};

bool operator<( const SinkData &a, const SinkData &b );
bool operator==( const SinkData &a, const SinkData &b );

}}}; // ADL::sys::sinkimpl

namespace ADL {

template <>
struct JsonV<ADL::sys::sinkimpl::SerialisationType>
{
    static void toJson( JsonWriter &json, const ADL::sys::sinkimpl::SerialisationType & v )
    {
        JsonV<std::string>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::sys::sinkimpl::SerialisationType &v, JsonReader &json )
    {
        JsonV<std::string>::fromJson( v.value, json );
    }
};

template <>
struct JsonV<ADL::sys::sinkimpl::TransportAddr>
{
    static void toJson( JsonWriter &json, const ADL::sys::sinkimpl::TransportAddr & v );
    static void fromJson( ADL::sys::sinkimpl::TransportAddr &v, JsonReader &json );
};

template <>
struct JsonV<ADL::sys::sinkimpl::TransportName>
{
    static void toJson( JsonWriter &json, const ADL::sys::sinkimpl::TransportName & v )
    {
        JsonV<std::string>::toJson( json, v.value );
    }
    
    static void fromJson( ADL::sys::sinkimpl::TransportName &v, JsonReader &json )
    {
        JsonV<std::string>::fromJson( v.value, json );
    }
};

template <>
struct JsonV<ADL::sys::sinkimpl::SinkData>
{
    static void toJson( JsonWriter &json, const ADL::sys::sinkimpl::SinkData & v );
    static void fromJson( ADL::sys::sinkimpl::SinkData &v, JsonReader &json );
};

}; // ADL
#endif // SYS_SINKIMPL_H