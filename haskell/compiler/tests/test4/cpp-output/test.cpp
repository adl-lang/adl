#include "test.h"

namespace ADL {
namespace test {

S::S()
{
}

S::S(
    const Date & v1_
    )
    : v1(v1_)
{
}

bool
operator<( const S &a, const S &b )
{
    if( a.v1 < b.v1 ) return true;
    if( b.v1 < a.v1 ) return false;
    return false;
}

bool
operator==( const S &a, const S &b )
{
    return
        a.v1 == b.v1 ;
}

}}; // ADL::test

namespace ADL {

typename Serialiser<ADL::test::S>::Ptr
Serialisable<ADL::test::S>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test::S _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : v1_s( Serialisable<Date>::serialiser(sf) )
            {}
        
        
        typename Serialiser<Date>::Ptr v1_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<Date>( json, v1_s, "v1", v.v1 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( v1_s, v.v1, "v1", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL