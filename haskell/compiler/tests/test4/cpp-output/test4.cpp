#include "test4.h"

namespace ADL {
namespace test4 {

CDate::CDate()
    : year(0)
    , month(0)
    , day(0)
{
}

CDate::CDate(
    const int16_t & year_,
    const int16_t & month_,
    const int16_t & day_
    )
    : year(year_)
    , month(month_)
    , day(day_)
{
}

bool
operator<( const CDate &a, const CDate &b )
{
    if( a.year < b.year ) return true;
    if( b.year < a.year ) return false;
    if( a.month < b.month ) return true;
    if( b.month < a.month ) return false;
    if( a.day < b.day ) return true;
    if( b.day < a.day ) return false;
    return false;
}

bool
operator==( const CDate &a, const CDate &b )
{
    return
        a.year == b.year &&
        a.month == b.month &&
        a.day == b.day ;
}

S2::S2()
    : intv(0)
{
}

S2::S2(
    const int32_t & intv_
    )
    : intv(intv_)
{
}

bool
operator<( const S2 &a, const S2 &b )
{
    if( a.intv < b.intv ) return true;
    if( b.intv < a.intv ) return false;
    return false;
}

bool
operator==( const S2 &a, const S2 &b )
{
    return
        a.intv == b.intv ;
}

S::S()
    : v2(Date("2000-01-01"))
    , v4(CDate(2000,1,1))
    , v5a(ADL::sys::types::Maybe<std::string> ::mk_nothing())
    , v5b(ADL::sys::types::Maybe<std::string> ::mk_just("hello"))
    , v7(std::set<int32_t> (mkvec<int32_t>(1,2,3)))
    , v8a(std::map<std::string,int32_t> (mkvec<std::pair<std::string,int32_t> >(std::pair<std::string,int32_t> ("X",1),std::pair<std::string,int32_t> ("Y",2))))
{
}

S::S(
    const Date & v1_,
    const Date & v2_,
    const CDate & v3_,
    const CDate & v4_,
    const ADL::sys::types::Maybe<std::string>  & v5_,
    const ADL::sys::types::Maybe<std::string>  & v5a_,
    const ADL::sys::types::Maybe<std::string>  & v5b_,
    const std::pair<std::string,int32_t>  & v6_,
    const std::set<int32_t>  & v7_,
    const std::set<int32_t>  & v7a_,
    const std::map<std::string,int32_t>  & v8_,
    const std::map<std::string,int32_t>  & v8a_
    )
    : v1(v1_)
    , v2(v2_)
    , v3(v3_)
    , v4(v4_)
    , v5(v5_)
    , v5a(v5a_)
    , v5b(v5b_)
    , v6(v6_)
    , v7(v7_)
    , v7a(v7a_)
    , v8(v8_)
    , v8a(v8a_)
{
}

bool
operator<( const S &a, const S &b )
{
    if( a.v1 < b.v1 ) return true;
    if( b.v1 < a.v1 ) return false;
    if( a.v2 < b.v2 ) return true;
    if( b.v2 < a.v2 ) return false;
    if( a.v3 < b.v3 ) return true;
    if( b.v3 < a.v3 ) return false;
    if( a.v4 < b.v4 ) return true;
    if( b.v4 < a.v4 ) return false;
    if( a.v5 < b.v5 ) return true;
    if( b.v5 < a.v5 ) return false;
    if( a.v5a < b.v5a ) return true;
    if( b.v5a < a.v5a ) return false;
    if( a.v5b < b.v5b ) return true;
    if( b.v5b < a.v5b ) return false;
    if( a.v6 < b.v6 ) return true;
    if( b.v6 < a.v6 ) return false;
    if( a.v7 < b.v7 ) return true;
    if( b.v7 < a.v7 ) return false;
    if( a.v7a < b.v7a ) return true;
    if( b.v7a < a.v7a ) return false;
    if( a.v8 < b.v8 ) return true;
    if( b.v8 < a.v8 ) return false;
    if( a.v8a < b.v8a ) return true;
    if( b.v8a < a.v8a ) return false;
    return false;
}

bool
operator==( const S &a, const S &b )
{
    return
        a.v1 == b.v1 &&
        a.v2 == b.v2 &&
        a.v3 == b.v3 &&
        a.v4 == b.v4 &&
        a.v5 == b.v5 &&
        a.v5a == b.v5a &&
        a.v5b == b.v5b &&
        a.v6 == b.v6 &&
        a.v7 == b.v7 &&
        a.v7a == b.v7a &&
        a.v8 == b.v8 &&
        a.v8a == b.v8a ;
}

}}; // ADL::test4

namespace ADL {

typename Serialiser<ADL::test4::CDate>::Ptr
Serialisable<ADL::test4::CDate>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test4::CDate _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : year_s( Serialisable<int16_t>::serialiser(sf) )
            , month_s( Serialisable<int16_t>::serialiser(sf) )
            , day_s( Serialisable<int16_t>::serialiser(sf) )
            {}
        
        
        typename Serialiser<int16_t>::Ptr year_s;
        typename Serialiser<int16_t>::Ptr month_s;
        typename Serialiser<int16_t>::Ptr day_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<int16_t>( json, year_s, "year", v.year );
            writeField<int16_t>( json, month_s, "month", v.month );
            writeField<int16_t>( json, day_s, "day", v.day );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( year_s, v.year, "year", json ) ||
                readField( month_s, v.month, "month", json ) ||
                readField( day_s, v.day, "day", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::test4::S2>::Ptr
Serialisable<ADL::test4::S2>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test4::S2 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : intv_s( Serialisable<int32_t>::serialiser(sf) )
            {}
        
        
        typename Serialiser<int32_t>::Ptr intv_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<int32_t>( json, intv_s, "intv", v.intv );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( intv_s, v.intv, "intv", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::test4::S>::Ptr
Serialisable<ADL::test4::S>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test4::S _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : v1_s( Serialisable<Date>::serialiser(sf) )
            , v2_s( Serialisable<Date>::serialiser(sf) )
            , v3_s( Serialisable<ADL::test4::CDate>::serialiser(sf) )
            , v4_s( Serialisable<ADL::test4::CDate>::serialiser(sf) )
            , v5_s( Serialisable<ADL::sys::types::Maybe<std::string> >::serialiser(sf) )
            , v5a_s( Serialisable<ADL::sys::types::Maybe<std::string> >::serialiser(sf) )
            , v5b_s( Serialisable<ADL::sys::types::Maybe<std::string> >::serialiser(sf) )
            , v6_s( Serialisable<std::pair<std::string,int32_t> >::serialiser(sf) )
            , v7_s( Serialisable<std::set<int32_t> >::serialiser(sf) )
            , v7a_s( Serialisable<std::set<int32_t> >::serialiser(sf) )
            , v8_s( Serialisable<std::map<std::string,int32_t> >::serialiser(sf) )
            , v8a_s( Serialisable<std::map<std::string,int32_t> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<Date>::Ptr v1_s;
        typename Serialiser<Date>::Ptr v2_s;
        typename Serialiser<ADL::test4::CDate>::Ptr v3_s;
        typename Serialiser<ADL::test4::CDate>::Ptr v4_s;
        typename Serialiser<ADL::sys::types::Maybe<std::string> >::Ptr v5_s;
        typename Serialiser<ADL::sys::types::Maybe<std::string> >::Ptr v5a_s;
        typename Serialiser<ADL::sys::types::Maybe<std::string> >::Ptr v5b_s;
        typename Serialiser<std::pair<std::string,int32_t> >::Ptr v6_s;
        typename Serialiser<std::set<int32_t> >::Ptr v7_s;
        typename Serialiser<std::set<int32_t> >::Ptr v7a_s;
        typename Serialiser<std::map<std::string,int32_t> >::Ptr v8_s;
        typename Serialiser<std::map<std::string,int32_t> >::Ptr v8a_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<Date>( json, v1_s, "v1", v.v1 );
            writeField<Date>( json, v2_s, "v2", v.v2 );
            writeField<ADL::test4::CDate>( json, v3_s, "v3", v.v3 );
            writeField<ADL::test4::CDate>( json, v4_s, "v4", v.v4 );
            writeField<ADL::sys::types::Maybe<std::string> >( json, v5_s, "v5", v.v5 );
            writeField<ADL::sys::types::Maybe<std::string> >( json, v5a_s, "v5a", v.v5a );
            writeField<ADL::sys::types::Maybe<std::string> >( json, v5b_s, "v5b", v.v5b );
            writeField<std::pair<std::string,int32_t> >( json, v6_s, "v6", v.v6 );
            writeField<std::set<int32_t> >( json, v7_s, "v7", v.v7 );
            writeField<std::set<int32_t> >( json, v7a_s, "v7a", v.v7a );
            writeField<std::map<std::string,int32_t> >( json, v8_s, "v8", v.v8 );
            writeField<std::map<std::string,int32_t> >( json, v8a_s, "v8a", v.v8a );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( v1_s, v.v1, "v1", json ) ||
                readField( v2_s, v.v2, "v2", json ) ||
                readField( v3_s, v.v3, "v3", json ) ||
                readField( v4_s, v.v4, "v4", json ) ||
                readField( v5_s, v.v5, "v5", json ) ||
                readField( v5a_s, v.v5a, "v5a", json ) ||
                readField( v5b_s, v.v5b, "v5b", json ) ||
                readField( v6_s, v.v6, "v6", json ) ||
                readField( v7_s, v.v7, "v7", json ) ||
                readField( v7a_s, v.v7a, "v7a", json ) ||
                readField( v8_s, v.v8, "v8", json ) ||
                readField( v8a_s, v.v8a, "v8a", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL