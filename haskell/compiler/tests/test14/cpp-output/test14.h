#ifndef TEST14_H
#define TEST14_H
#include <adl/adl.h>
#include <stdint.h>
#include <string>

namespace ADL {
namespace test14 {

struct Factory
{
    Factory() {}
    explicit Factory(const std::string & v) : value(v) {}
    
    std::string value;
};

inline
bool operator<( const Factory &a, const Factory &b ) { return a.value < b.value; }
inline
bool operator==( const Factory &a, const Factory &b ) { return a.value == b.value; }

struct switch_
{
    switch_();
    
    switch_(
        const double & double_,
        const int32_t & int_,
        const std::string & string,
        const bool & for_,
        const std::string & Objects
        );
    
    double double_;
    int32_t int_;
    std::string string;
    bool for_;
    std::string Objects;
};

bool operator<( const switch_ &a, const switch_ &b );
bool operator==( const switch_ &a, const switch_ &b );

class unsigned_
{
public:
    unsigned_();
    static unsigned_ mk_null_();
    
    unsigned_( const unsigned_ & );
    ~unsigned_();
    unsigned_ & operator=( const unsigned_ & );
    
    enum DiscType
    {
        NULL_
    };
    
    DiscType d() const;
    
    void set_null_();
    
private:
    unsigned_( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const unsigned_ &a, const unsigned_ &b );
bool operator==( const unsigned_ &a, const unsigned_ &b );

inline unsigned_::DiscType unsigned_::d() const
{
    return d_;
}

}}; // ADL::test14

namespace ADL {

template <>
struct Serialisable<ADL::test14::Factory>
{
    struct S : public Serialiser<ADL::test14::Factory>
    {
        S( typename Serialiser<std::string>::Ptr s_ ) : s(s_) {}
        
        void toJson( JsonWriter &json, const ADL::test14::Factory & v ) const
        {
            s->toJson( json, v.value );
        }
        
        void fromJson( ADL::test14::Factory &v, JsonReader &json ) const
        {
            s->fromJson( v.value, json );
        }
        
        typename Serialiser<std::string>::Ptr s;
    };
    
    static typename Serialiser<ADL::test14::Factory>::Ptr serialiser(const SerialiserFlags &sf)
    {
        return typename Serialiser<ADL::test14::Factory>::Ptr(new S(Serialisable<std::string>::serialiser(sf)));
    }
};

template <>
struct Serialisable<ADL::test14::switch_>
{
    static Serialiser<ADL::test14::switch_>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test14::unsigned_>
{
    static Serialiser<ADL::test14::unsigned_>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // TEST14_H