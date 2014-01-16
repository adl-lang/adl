#include "test.h"

namespace ADL {
namespace test {

S1::S1()
    : x(0)
{
}

S1::S1(
    const int32_t & x_,
    const std::string & y_
    )
    : x(x_)
    , y(y_)
{
}

bool
operator<( const S1 &a, const S1 &b )
{
    if( a.x < b.x ) return true;
    if( b.x < a.x ) return false;
    if( a.y < b.y ) return true;
    if( b.y < a.y ) return false;
    return false;
}

bool
operator==( const S1 &a, const S1 &b )
{
    return
        a.x == b.x &&
        a.y == b.y ;
}

}}; // ADL::test

namespace ADL {

typename Serialiser<ADL::test::S1>::Ptr
Serialisable<ADL::test::S1>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test::S1 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : x_s( Serialisable<int32_t>::serialiser(sf) )
            , y_s( Serialisable<std::string>::serialiser(sf) )
            {}
        
        
        typename Serialiser<int32_t>::Ptr x_s;
        typename Serialiser<std::string>::Ptr y_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<int32_t>( json, x_s, "x", v.x );
            writeField<std::string>( json, y_s, "y", v.y );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( x_s, v.x, "x", json ) ||
                readField( y_s, v.y, "y", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL