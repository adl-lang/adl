#include "test14.h"

namespace ADL {
namespace test14 {

switch_::switch_()
    : double_(0.0)
    , int_(0)
    , for_(false)
{
}

switch_::switch_(
    const double & double__,
    const int32_t & int__,
    const std::string & string_,
    const bool & for__,
    const std::string & Objects_
    )
    : double_(double__)
    , int_(int__)
    , string(string_)
    , for_(for__)
    , Objects(Objects_)
{
}

bool
operator<( const switch_ &a, const switch_ &b )
{
    if( a.double_ < b.double_ ) return true;
    if( b.double_ < a.double_ ) return false;
    if( a.int_ < b.int_ ) return true;
    if( b.int_ < a.int_ ) return false;
    if( a.string < b.string ) return true;
    if( b.string < a.string ) return false;
    if( a.for_ < b.for_ ) return true;
    if( b.for_ < a.for_ ) return false;
    if( a.Objects < b.Objects ) return true;
    if( b.Objects < a.Objects ) return false;
    return false;
}

bool
operator==( const switch_ &a, const switch_ &b )
{
    return
        a.double_ == b.double_ &&
        a.int_ == b.int_ &&
        a.string == b.string &&
        a.for_ == b.for_ &&
        a.Objects == b.Objects ;
}

unsigned_::unsigned_()
    : d_(NULL_), p_(0)
{
}

unsigned_ unsigned_::mk_null_()
{
    return unsigned_( NULL_, 0 );
}

unsigned_::unsigned_( const unsigned_ & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

unsigned_::~unsigned_()
{
    free(d_,p_);
}

unsigned_ & unsigned_::operator=( const unsigned_ & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

void unsigned_::set_null_()
{
    if( d_ != NULL_ )
    {
        free(d_,p_);
        d_ = NULL_;
        p_ = 0;
    }
}

unsigned_::unsigned_(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void unsigned_::free(DiscType d, void *p)
{
    switch( d )
    {
        case NULL_: return;
    }
}

void * unsigned_::copy( DiscType d, void *p )
{
    switch( d )
    {
        case NULL_: return 0;
    }
    return 0;
}

bool
operator<( const unsigned_ &a, const unsigned_ &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case unsigned_::NULL_: return false;
    }
    return false;
}

bool
operator==( const unsigned_ &a, const unsigned_ &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case unsigned_::NULL_: return true;
    }
    return false;
}

}}; // ADL::test14

namespace ADL {

typename Serialiser<ADL::test14::switch_>::Ptr
Serialisable<ADL::test14::switch_>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test14::switch_ _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : double__s( Serialisable<double>::serialiser(sf) )
            , int__s( Serialisable<int32_t>::serialiser(sf) )
            , string_s( Serialisable<std::string>::serialiser(sf) )
            , for__s( Serialisable<bool>::serialiser(sf) )
            , Objects_s( Serialisable<std::string>::serialiser(sf) )
            {}
        
        
        typename Serialiser<double>::Ptr double__s;
        typename Serialiser<int32_t>::Ptr int__s;
        typename Serialiser<std::string>::Ptr string_s;
        typename Serialiser<bool>::Ptr for__s;
        typename Serialiser<std::string>::Ptr Objects_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<double>( json, double__s, "double", v.double_ );
            writeField<int32_t>( json, int__s, "int", v.int_ );
            writeField<std::string>( json, string_s, "string", v.string );
            writeField<bool>( json, for__s, "for", v.for_ );
            writeField<std::string>( json, Objects_s, "Objects", v.Objects );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( double__s, v.double_, "double", json ) ||
                readField( int__s, v.int_, "int", json ) ||
                readField( string_s, v.string, "string", json ) ||
                readField( for__s, v.for_, "for", json ) ||
                readField( Objects_s, v.Objects, "Objects", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::test14::unsigned_>::Ptr
Serialisable<ADL::test14::unsigned_>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test14::unsigned_ _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<Void>::Ptr null_;
        
        typename Serialiser<Void>::Ptr null_s() const
        {
            if( !null_ )
                null_ = Serialisable<Void>::serialiser(sf_);
            return null_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test14::unsigned_::NULL_: json.stringV( "null" ); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::STRING )
            {
                if( json.stringV() == "null" )
                    v.set_null_();
                else
                    throw json_parse_failure();
                json.next();
                return;
            }
            throw json_parse_failure();
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

}; // ADL