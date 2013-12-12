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

template <class T>
T fromJsonString( const std::string &str  )
{
    SerialiserFlags flags;

    typename Serialiser<T>::Ptr s = Serialisable<T>::serialiser( flags );
    T t;
    StringJsonReader jr(str);
    s->fromJson( t, jr );
    return t;
}

template <class T>
T jsonRoundTrip( const T & t, bool pretty )
{
    return fromJsonString<T>(toJsonString(t,pretty));
}

TEST_CASE( "basic json reader operation" "[serialisation]" )
{
    {
        StringJsonReader jr("[]");
        REQUIRE( jr.type() == JsonReader::START_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        StringJsonReader jr("[null]");
        REQUIRE( jr.type() == JsonReader::START_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::NULLV );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_ARRAY );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        StringJsonReader jr("[null,true]");
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
        StringJsonReader jr("[null,[true,false]]");
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
        StringJsonReader jr("{}");
        REQUIRE( jr.type() == JsonReader::START_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OBJECT );
        jr.next();
        REQUIRE( jr.type() == JsonReader::END_OF_STREAM );
    }

    {
        StringJsonReader jr("{\"hello\":true}");
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
        StringJsonReader jr("{\"hello\" : true, \"goodbye\" : null}");
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
        Maybe<uint64_t> v = Maybe<uint64_t>::mk_just( -40 );
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

