#include "test.h"

namespace ADL {
namespace test {

S0::S0()
{
}

S0::S0(
    )
{
}

bool
operator<( const S0 &a, const S0 &b )
{
    return false;
}

bool
operator==( const S0 &a, const S0 &b )
{
    return
}

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

S2::S2()
    : f2(0.0)
{
}

S2::S2(
    const std::string & f1_,
    const double & f2_,
    const std::vector<int32_t>  & f3_
    )
    : f1(f1_)
    , f2(f2_)
    , f3(f3_)
{
}

bool
operator<( const S2 &a, const S2 &b )
{
    if( a.f1 < b.f1 ) return true;
    if( b.f1 < a.f1 ) return false;
    if( a.f2 < b.f2 ) return true;
    if( b.f2 < a.f2 ) return false;
    if( a.f3 < b.f3 ) return true;
    if( b.f3 < a.f3 ) return false;
    return false;
}

bool
operator==( const S2 &a, const S2 &b )
{
    return
        a.f1 == b.f1 &&
        a.f2 == b.f2 &&
        a.f3 == b.f3 ;
}

}}; // ADL::test

namespace ADL {

typename Serialiser<ADL::test::S0>::Ptr
Serialisable<ADL::test::S0>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test::S0 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            {}
        
        
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

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

typename Serialiser<ADL::test::S2>::Ptr
Serialisable<ADL::test::S2>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test::S2 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f1_s( Serialisable<std::string>::serialiser(sf) )
            , f2_s( Serialisable<double>::serialiser(sf) )
            , f3_s( Serialisable<std::vector<int32_t> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<std::string>::Ptr f1_s;
        typename Serialiser<double>::Ptr f2_s;
        typename Serialiser<std::vector<int32_t> >::Ptr f3_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::string>( json, f1_s, "f1", v.f1 );
            writeField<double>( json, f2_s, "f2", v.f2 );
            writeField<std::vector<int32_t> >( json, f3_s, "f3", v.f3 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f1_s, v.f1, "f1", json ) ||
                readField( f2_s, v.f2, "f2", json ) ||
                readField( f3_s, v.f3, "f3", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL