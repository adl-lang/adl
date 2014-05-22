#define CATCH_CONFIG_MAIN
#include "catch/catch.hpp"

#include <sstream>
#include <adl/jsonimpl.h>
#include <adl/sys.adlast.h>
#include <adl/unittests.h>
#include <adl/comms.h>

using namespace ADL;

template <class T>
std::string toJsonString( const T & t, bool pretty )
{
    return toJsonString( t, pretty, SerialiserFlags() );
}

template <class T>
T fromJsonString( const std::string &str  )
{
    return fromJsonString( str, SerialiserFlags() );
}

template <class T>
T jsonRoundTrip( const T & t, bool pretty )
{
    return fromJsonString<T>(toJsonString(t,pretty));
}

TEST_CASE( "basic json reader operation" "[serialisation]" )
{
    {
        std::istringstream is("[]");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::START_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        std::istringstream is("[null]");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::START_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::NULLV );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        std::istringstream is("[null,true]");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::START_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::NULLV );
        jr.next();
        REQUIRE( jr.type() == JsonReader::BOOLEAN );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        std::istringstream is("[null,[true,false]]");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::START_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::NULLV );
        jr.next();
        REQUIRE( jr.type() == JsonReader::START_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::BOOLEAN );
        jr.next();
        REQUIRE( jr.type() == JsonReader::BOOLEAN );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        std::istringstream is("{}");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::START_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        std::istringstream is("\"text\"");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::STRING );
        REQUIRE( jr.stringV() == "text" );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        std::istringstream is("\"\\\"text\\\"\"");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::STRING );
        REQUIRE( jr.stringV() == "\"text\"" );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        std::istringstream is("{\"hello\":true}");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::START_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::FIELD );
        jr.next();
        REQUIRE( jr.type() == JsonReader::BOOLEAN );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        std::istringstream is("{\"hello\" : true, \"goodbye\" : null}");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::START_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::FIELD );
        jr.next();
        REQUIRE( jr.type() == JsonReader::BOOLEAN );
        jr.next();
        REQUIRE( jr.type() == JsonReader::FIELD );
        jr.next();
        REQUIRE( jr.type() == JsonReader::NULLV );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        std::istringstream is("{\"value\":54321,\"children\":[]}");
        StreamJsonReader jr(is);
        REQUIRE( jr.type() == JsonReader::START_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::FIELD );
        jr.next();
        REQUIRE( jr.type() == JsonReader::NUMBER );
        jr.next();
        REQUIRE( jr.type() == JsonReader::FIELD );
        jr.next();
        REQUIRE( jr.type() == JsonReader::START_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }
}

TEST_CASE( "Serialisation of structs", "[serialisation]" )
{
    sys::adlast::ScopedName n;
    n.moduleName = "a";
    n.name = "v";
    
    CHECK( toJsonString(n,false) == "{\"moduleName\":\"a\",\"name\":\"v\"}" );
    CHECK( toJsonString(n,true) == "{\n\t\"moduleName\" : \"a\",\n\t\"name\" : \"v\"\n}" );
}

TEST_CASE( "Roundtrip structs", "[serialisation]" )
{
    sys::adlast::ScopedName n;
    n.moduleName = "a";
    n.name = "v";

    CHECK( n == jsonRoundTrip(n,false) );
    CHECK( n == jsonRoundTrip(n,true) );
}

TEST_CASE( "Roundtrip primitives", "[serialisation]" )
{
    // Test each primitive type within a Maybe
    using namespace sys::types;

    {
        Maybe<Void> v = Maybe<Void>::mk_just( Void() );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<bool> v = Maybe<bool>::mk_just( true );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<int8_t> v = Maybe<int8_t>::mk_just( 4 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<int16_t> v = Maybe<int16_t>::mk_just( 1033 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<int32_t> v = Maybe<int32_t>::mk_just( 212345 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<int64_t> v = Maybe<int64_t>::mk_just( -40 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<uint8_t> v = Maybe<uint8_t>::mk_just( 4 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<uint16_t> v = Maybe<uint16_t>::mk_just( 1033 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<uint32_t> v = Maybe<uint32_t>::mk_just( 212345 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<uint64_t> v = Maybe<uint64_t>::mk_just( 17856890 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<double> v = Maybe<double>::mk_just( 3.5 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<float> v = Maybe<float>::mk_just( 15.25 );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        Maybe<std::string> v = Maybe<std::string>::mk_just( "hi" );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        ByteVector b;
        b.bytes.push_back( 0 );
        b.bytes.push_back( 255 );
        b.bytes.push_back( 0 );
        Maybe<ByteVector> v = Maybe<ByteVector>::mk_just( b );
        CHECK( v == jsonRoundTrip(v,true) );
    }

    {
        std::vector<bool> vb;
        vb.push_back( true );
        vb.push_back( false );
        vb.push_back( true );
        Maybe<std::vector<bool>> v = Maybe<std::vector<bool>>::mk_just( vb );
        CHECK( v == jsonRoundTrip(v,true) );
    }

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

TEST_CASE( "Roundtrip unions", "[serialisation]" )
{
    using namespace sys::adlast;
    {
        TypeRef t = TypeRef::mk_primitive( "a" );
        CHECK( t == jsonRoundTrip(t,false) );
        CHECK( t == jsonRoundTrip(t,true) );
    }

    {
        TypeRef t = TypeRef::mk_reference( ScopedName( "test", "x" ) );
        CHECK( t == jsonRoundTrip(t,false) );
        CHECK( t == jsonRoundTrip(t,true) );
    }
}

TEST_CASE( "Serialisation of a type recursive via a vector", "[serialisation]" )
{
    using namespace unittests;
    T1 t1( 0, std::vector<T1>() );
    CHECK( toJsonString(t1,false) == "{\"value\":0,\"children\":[]}" );
    CHECK( t1 == jsonRoundTrip( t1, false ) );
}

TEST_CASE( "Serialisation of a type recursive via a union", "[serialisation]" )
{
    using namespace unittests;
    L1 l1 = L1::mk_value( Pair<double,L1>( 5, L1::mk_null_() ) );
    CHECK( toJsonString(l1,false) == "{\"value\":{\"first\":5,\"second\":{\"null\":null}}}" );
    CHECK( l1 == jsonRoundTrip( l1, false ) );
}

TEST_CASE( "Instantiate a client connection" )
{
    using namespace unittests;

    CommsContext::Ptr ctx( new CommsContext() );
};
