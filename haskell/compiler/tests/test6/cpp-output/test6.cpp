// @generated from adl module test6
#include "test6.h"

namespace ADL {
namespace test6 {

S::S()
    : f_mstring2(ADL::sys::types::Maybe<std::string> ::mk_just("sukpeepolup"))
    , f_nstring2(std::optional<std::string>("abcde"))
    , f_int2(std::optional<int64_t>(100))
    , f_int3(std::optional<int64_t>())
{
}

S::S(
    const std::pair<int32_t,double>  & f_pair_,
    const ADL::sys::types::Either<std::string,int32_t>  & f_either_,
    const std::map<std::string,double>  & f_map_,
    const std::set<std::string>  & f_set_,
    const ADL::sys::types::Maybe<std::string>  & f_mstring_,
    const ADL::sys::types::Maybe<std::string>  & f_mstring2_,
    const std::optional<std::string> & f_nstring_,
    const std::optional<std::string> & f_nstring2_,
    const std::optional<int64_t> & f_int_,
    const std::optional<int64_t> & f_int2_,
    const std::optional<int64_t> & f_int3_
    )
    : f_pair(f_pair_)
    , f_either(f_either_)
    , f_map(f_map_)
    , f_set(f_set_)
    , f_mstring(f_mstring_)
    , f_mstring2(f_mstring2_)
    , f_nstring(f_nstring_)
    , f_nstring2(f_nstring2_)
    , f_int(f_int_)
    , f_int2(f_int2_)
    , f_int3(f_int3_)
{
}

bool
operator<( const S &a, const S &b )
{
    if( a.f_pair < b.f_pair ) return true;
    if( b.f_pair < a.f_pair ) return false;
    if( a.f_either < b.f_either ) return true;
    if( b.f_either < a.f_either ) return false;
    if( a.f_map < b.f_map ) return true;
    if( b.f_map < a.f_map ) return false;
    if( a.f_set < b.f_set ) return true;
    if( b.f_set < a.f_set ) return false;
    if( a.f_mstring < b.f_mstring ) return true;
    if( b.f_mstring < a.f_mstring ) return false;
    if( a.f_mstring2 < b.f_mstring2 ) return true;
    if( b.f_mstring2 < a.f_mstring2 ) return false;
    if( a.f_nstring < b.f_nstring ) return true;
    if( b.f_nstring < a.f_nstring ) return false;
    if( a.f_nstring2 < b.f_nstring2 ) return true;
    if( b.f_nstring2 < a.f_nstring2 ) return false;
    if( a.f_int < b.f_int ) return true;
    if( b.f_int < a.f_int ) return false;
    if( a.f_int2 < b.f_int2 ) return true;
    if( b.f_int2 < a.f_int2 ) return false;
    if( a.f_int3 < b.f_int3 ) return true;
    if( b.f_int3 < a.f_int3 ) return false;
    return false;
}

bool
operator==( const S &a, const S &b )
{
    return
        a.f_pair == b.f_pair &&
        a.f_either == b.f_either &&
        a.f_map == b.f_map &&
        a.f_set == b.f_set &&
        a.f_mstring == b.f_mstring &&
        a.f_mstring2 == b.f_mstring2 &&
        a.f_nstring == b.f_nstring &&
        a.f_nstring2 == b.f_nstring2 &&
        a.f_int == b.f_int &&
        a.f_int2 == b.f_int2 &&
        a.f_int3 == b.f_int3 ;
}

}}; // ADL::test6

namespace ADL {

typename Serialiser<ADL::test6::S>::Ptr
Serialisable<ADL::test6::S>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test6::S _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f_pair_s( Serialisable<std::pair<int32_t,double> >::serialiser(sf) )
            , f_either_s( Serialisable<ADL::sys::types::Either<std::string,int32_t> >::serialiser(sf) )
            , f_map_s( Serialisable<std::map<std::string,double> >::serialiser(sf) )
            , f_set_s( Serialisable<std::set<std::string> >::serialiser(sf) )
            , f_mstring_s( Serialisable<ADL::sys::types::Maybe<std::string> >::serialiser(sf) )
            , f_mstring2_s( Serialisable<ADL::sys::types::Maybe<std::string> >::serialiser(sf) )
            , f_nstring_s( optionalSerialiser<std::string>(sf) )
            , f_nstring2_s( optionalSerialiser<std::string>(sf) )
            , f_int_s( optionalSerialiser<int64_t>(sf) )
            , f_int2_s( optionalSerialiser<int64_t>(sf) )
            , f_int3_s( optionalSerialiser<int64_t>(sf) )
            {}
        
        
        typename Serialiser<std::pair<int32_t,double> >::Ptr f_pair_s;
        typename Serialiser<ADL::sys::types::Either<std::string,int32_t> >::Ptr f_either_s;
        typename Serialiser<std::map<std::string,double> >::Ptr f_map_s;
        typename Serialiser<std::set<std::string> >::Ptr f_set_s;
        typename Serialiser<ADL::sys::types::Maybe<std::string> >::Ptr f_mstring_s;
        typename Serialiser<ADL::sys::types::Maybe<std::string> >::Ptr f_mstring2_s;
        typename Serialiser<std::optional<std::string>>::Ptr f_nstring_s;
        typename Serialiser<std::optional<std::string>>::Ptr f_nstring2_s;
        typename Serialiser<std::optional<int64_t>>::Ptr f_int_s;
        typename Serialiser<std::optional<int64_t>>::Ptr f_int2_s;
        typename Serialiser<std::optional<int64_t>>::Ptr f_int3_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::pair<int32_t,double> >( json, f_pair_s, "f_pair", v.f_pair );
            writeField<ADL::sys::types::Either<std::string,int32_t> >( json, f_either_s, "f_either", v.f_either );
            writeField<std::map<std::string,double> >( json, f_map_s, "f_map", v.f_map );
            writeField<std::set<std::string> >( json, f_set_s, "f_set", v.f_set );
            writeField<ADL::sys::types::Maybe<std::string> >( json, f_mstring_s, "f_mstring", v.f_mstring );
            writeField<ADL::sys::types::Maybe<std::string> >( json, f_mstring2_s, "f_mstring2", v.f_mstring2 );
            writeField<std::optional<std::string>>( json, f_nstring_s, "f_nstring", v.f_nstring );
            writeField<std::optional<std::string>>( json, f_nstring2_s, "f_nstring2", v.f_nstring2 );
            writeField<std::optional<int64_t>>( json, f_int_s, "f_int", v.f_int );
            writeField<std::optional<int64_t>>( json, f_int2_s, "f_int2", v.f_int2 );
            writeField<std::optional<int64_t>>( json, f_int3_s, "f_int3", v.f_int3 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f_pair_s, v.f_pair, "f_pair", json ) ||
                readField( f_either_s, v.f_either, "f_either", json ) ||
                readField( f_map_s, v.f_map, "f_map", json ) ||
                readField( f_set_s, v.f_set, "f_set", json ) ||
                readField( f_mstring_s, v.f_mstring, "f_mstring", json ) ||
                readField( f_mstring2_s, v.f_mstring2, "f_mstring2", json ) ||
                readField( f_nstring_s, v.f_nstring, "f_nstring", json ) ||
                readField( f_nstring2_s, v.f_nstring2, "f_nstring2", json ) ||
                readField( f_int_s, v.f_int, "f_int", json ) ||
                readField( f_int2_s, v.f_int2, "f_int2", json ) ||
                readField( f_int3_s, v.f_int3, "f_int3", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL
