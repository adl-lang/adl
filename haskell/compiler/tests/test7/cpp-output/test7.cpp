// @generated from adl module test7
#include "test7.h"

namespace ADL {
namespace test7 {

S::S()
{
}

S::S(
    const IntPoint1A & f1_
    )
    : f1(f1_)
{
}

bool
operator<( const S &a, const S &b )
{
    if( a.f1 < b.f1 ) return true;
    if( b.f1 < a.f1 ) return false;
    return false;
}

bool
operator==( const S &a, const S &b )
{
    return
        a.f1 == b.f1 ;
}

}}; // ADL::test7

namespace ADL {

typename Serialiser<ADL::test7::S>::Ptr
Serialisable<ADL::test7::S>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test7::S _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f1_s( Serialisable<ADL::test7::IntPoint1A>::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::test7::IntPoint1A>::Ptr f1_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::test7::IntPoint1A>( json, f1_s, "f1", v.f1 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f1_s, v.f1, "f1", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL
