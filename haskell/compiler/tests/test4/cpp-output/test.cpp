#include "test.h"

namespace ADL {
namespace test {

S2::S2()
    : f1(ADL::sys::types::Maybe<int32_t> ::mk_just(5))
    , f2(ADL::sys::types::Maybe<int32_t> ::mk_nothing(Void()))
    , f3(ADL::sys::types::Either<int32_t,std::string> ::mk_left(7))
    , f4(ADL::sys::types::Either<int32_t,std::string> ::mk_right("go"))
    , f5(std::pair<int32_t,double> (5,7.8))
{
}

S2::S2(
    const ADL::sys::types::Maybe<int32_t>  & f1_,
    const ADL::sys::types::Maybe<int32_t>  & f2_,
    const ADL::sys::types::Either<int32_t,std::string>  & f3_,
    const ADL::sys::types::Either<int32_t,std::string>  & f4_,
    const std::pair<int32_t,double>  & f5_
    )
    : f1(f1_)
    , f2(f2_)
    , f3(f3_)
    , f4(f4_)
    , f5(f5_)
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
    if( a.f4 < b.f4 ) return true;
    if( b.f4 < a.f4 ) return false;
    if( a.f5 < b.f5 ) return true;
    if( b.f5 < a.f5 ) return false;
    return false;
}

bool
operator==( const S2 &a, const S2 &b )
{
    return
        a.f1 == b.f1 &&
        a.f2 == b.f2 &&
        a.f3 == b.f3 &&
        a.f4 == b.f4 &&
        a.f5 == b.f5 ;
}

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

typename Serialiser<ADL::test::S2>::Ptr
Serialisable<ADL::test::S2>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test::S2 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f1_s( Serialisable<ADL::sys::types::Maybe<int32_t> >::serialiser(sf) )
            , f2_s( Serialisable<ADL::sys::types::Maybe<int32_t> >::serialiser(sf) )
            , f3_s( Serialisable<ADL::sys::types::Either<int32_t,std::string> >::serialiser(sf) )
            , f4_s( Serialisable<ADL::sys::types::Either<int32_t,std::string> >::serialiser(sf) )
            , f5_s( Serialisable<std::pair<int32_t,double> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::sys::types::Maybe<int32_t> >::Ptr f1_s;
        typename Serialiser<ADL::sys::types::Maybe<int32_t> >::Ptr f2_s;
        typename Serialiser<ADL::sys::types::Either<int32_t,std::string> >::Ptr f3_s;
        typename Serialiser<ADL::sys::types::Either<int32_t,std::string> >::Ptr f4_s;
        typename Serialiser<std::pair<int32_t,double> >::Ptr f5_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::sys::types::Maybe<int32_t> >( json, f1_s, "f1", v.f1 );
            writeField<ADL::sys::types::Maybe<int32_t> >( json, f2_s, "f2", v.f2 );
            writeField<ADL::sys::types::Either<int32_t,std::string> >( json, f3_s, "f3", v.f3 );
            writeField<ADL::sys::types::Either<int32_t,std::string> >( json, f4_s, "f4", v.f4 );
            writeField<std::pair<int32_t,double> >( json, f5_s, "f5", v.f5 );
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
                readField( f4_s, v.f4, "f4", json ) ||
                readField( f5_s, v.f5, "f5", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

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