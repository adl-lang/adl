#include "sys.dynamic.h"

namespace ADL {
namespace sys {
namespace dynamic {

Dynamic::Dynamic()
    : value(JsonValue::null)
{
}

Dynamic::Dynamic(
    const ADL::sys::adlast::TypeExpr & typeExpr_,
    const JsonValue & value_
    )
    : typeExpr(typeExpr_)
    , value(value_)
{
}

bool
operator<( const Dynamic &a, const Dynamic &b )
{
    if( a.typeExpr < b.typeExpr ) return true;
    if( b.typeExpr < a.typeExpr ) return false;
    if( a.value < b.value ) return true;
    if( b.value < a.value ) return false;
    return false;
}

bool
operator==( const Dynamic &a, const Dynamic &b )
{
    return
        a.typeExpr == b.typeExpr &&
        a.value == b.value ;
}

}}}; // ADL::sys::dynamic

namespace ADL {

typename Serialiser<ADL::sys::dynamic::Dynamic>::Ptr
Serialisable<ADL::sys::dynamic::Dynamic>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::dynamic::Dynamic _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : typeExpr_s( Serialisable<ADL::sys::adlast::TypeExpr>::serialiser(sf) )
            , value_s( Serialisable<JsonValue>::serialiser(sf) )
            {}
        
        
        typename Serialiser<ADL::sys::adlast::TypeExpr>::Ptr typeExpr_s;
        typename Serialiser<JsonValue>::Ptr value_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<ADL::sys::adlast::TypeExpr>( json, typeExpr_s, "typeExpr", v.typeExpr );
            writeField<JsonValue>( json, value_s, "value", v.value );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( typeExpr_s, v.typeExpr, "typeExpr", json ) ||
                readField( value_s, v.value, "value", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL