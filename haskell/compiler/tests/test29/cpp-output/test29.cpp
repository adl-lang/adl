// @generated from adl module test29
#include "test29.h"

namespace ADL {
namespace test29 {

Test::Test()
    : foo(MapBuilder<std::string,std::string>().add(" ","baz").add("\"","baz").add("$","bar").add("'","baz").add("degrees","°").result())
{
}

Test::Test(
    const std::map<std::string,std::string> & foo_
    )
    : foo(foo_)
{
}

bool
operator<( const Test &a, const Test &b )
{
    if( a.foo < b.foo ) return true;
    if( b.foo < a.foo ) return false;
    return false;
}

bool
operator==( const Test &a, const Test &b )
{
    return
        a.foo == b.foo ;
}

}}; // ADL::test29

namespace ADL {

typename Serialiser<ADL::test29::Test>::Ptr
Serialisable<ADL::test29::Test>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test29::Test _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : foo_s( stringMapSerialiser<std::string>(sf) )
            {}
        
        
        typename Serialiser<std::map<std::string,std::string>>::Ptr foo_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::map<std::string,std::string>>( json, foo_s, "foo", v.foo );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( foo_s, v.foo, "foo", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL
