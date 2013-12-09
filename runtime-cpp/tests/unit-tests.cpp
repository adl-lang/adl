#define CATCH_CONFIG_MAIN
#include "catch/catch.hpp"

#include <sstream>
#include <adl/jsonimpl.h>
#include <adl/sys.adlast.h>

using namespace ADL;

template <class T>
std::string toJsonString( const T & t, bool pretty )
{
    SerialiserFlags flags;

    typename Serialiser<T>::Ptr s = Serialisable<T>::serialiser( flags );
    std::stringstream str;
    StreamJsonWriter jw(str,pretty);
    s->toJson( jw, t );
    return str.str();
}

TEST_CASE( "Serialisation of structs", "[serialisation]" )
{
    sys::adlast::ScopedName n;
    n.moduleName = "a";
    n.name = "v";
    
    CHECK( toJsonString(n,false) == "{\"moduleName\":\"a\",\"name\":\"v\"}" );
    CHECK( toJsonString(n,true) == "{\n\t\"moduleName\" : \"a\",\n\t\"name\" : \"v\"\n}" );
}

TEST_CASE( "Serialisation of unions", "[serialisation]" )
{
    using namespace sys::adlast;
    {
        TypeRef t = TypeRef::mk_primitive( "a" );
        CHECK( toJsonString(t,false) == "{\"primitive\":\"a\"}" );
        CHECK( toJsonString(t,true) == "{\n\t\"primitive\" : \"a\"\n}" );
    }

    {
        TypeRef t = TypeRef::mk_reference( ScopedName( "test", "x" ) );
        CHECK( toJsonString(t,false) == "{\"reference\":{\"moduleName\":\"test\",\"name\":\"x\"}}" );
        CHECK( toJsonString(t,true) == "{\n\t\"reference\" : {\n\t\t\"moduleName\" : \"test\",\n\t\t\"name\" : \"x\"\n\t}\n}" );
    }
}
