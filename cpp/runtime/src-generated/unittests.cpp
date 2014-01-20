#include "adl/unittests.h"

namespace ADL {
namespace unittests {

T1::T1()
    : value(0.0)
{
}

T1::T1(
    const double & value_,
    const std::vector<T1>  & children_
    )
    : value(value_)
    , children(children_)
{
}

bool
operator<( const T1 &a, const T1 &b )
{
    if( a.value < b.value ) return true;
    if( b.value < a.value ) return false;
    if( a.children < b.children ) return true;
    if( b.children < a.children ) return false;
    return false;
}

bool
operator==( const T1 &a, const T1 &b )
{
    return
        a.value == b.value &&
        a.children == b.children ;
}

L1::L1()
    : d_(NULL_), p_(0)
{
}

L1 L1::mk_null_()
{
    return L1( NULL_, 0 );
}

L1 L1::mk_value( const Pair<double,L1>  & v )
{
    return L1( VALUE, new Pair<double,L1> (v) );
}

L1::L1( const L1 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

L1::~L1()
{
    free(d_,p_);
}

L1 & L1::operator=( const L1 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

void L1::set_null_()
{
    if( d_ != NULL_ )
    {
        free(d_,p_);
        d_ = NULL_;
        p_ = 0;
    }
}

const Pair<double,L1>  & L1::set_value(const Pair<double,L1>  &v)
{
    if( d_ == VALUE )
    {
        *(Pair<double,L1>  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = VALUE;
        p_ = new Pair<double,L1> (v);
    }
    return *(Pair<double,L1>  *)p_;
}

L1::L1(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void L1::free(DiscType d, void *p)
{
    switch( d )
    {
        case NULL_: return;
        case VALUE: delete (Pair<double,L1>  *)p; return;
    }
}

void * L1::copy( DiscType d, void *p )
{
    switch( d )
    {
        case NULL_: return 0;
        case VALUE: return new Pair<double,L1> (*(Pair<double,L1>  *)p);
    }
    return 0;
}

bool
operator<( const L1 &a, const L1 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case L1::NULL_: return false;
        case L1::VALUE: return a.value() < b.value();
    }
    return false;
}

bool
operator==( const L1 &a, const L1 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case L1::NULL_: return true;
        case L1::VALUE: return a.value() == b.value();
    }
    return false;
}

}}; // ADL::unittests

namespace ADL {

typename Serialiser<ADL::unittests::T1>::Ptr
Serialisable<ADL::unittests::T1>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::unittests::T1 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : value_s( Serialisable<double>::serialiser(sf) )
            , children_s( Serialisable<std::vector<ADL::unittests::T1> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<double>::Ptr value_s;
        typename Serialiser<std::vector<ADL::unittests::T1> >::Ptr children_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<double>( json, value_s, "value", v.value );
            writeField<std::vector<ADL::unittests::T1> >( json, children_s, "children", v.children );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( value_s, v.value, "value", json ) ||
                readField( children_s, v.children, "children", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::unittests::L1>::Ptr
Serialisable<ADL::unittests::L1>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::unittests::L1 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<Void>::Ptr null_;
        mutable typename Serialiser<ADL::unittests::Pair<double,ADL::unittests::L1> >::Ptr value_;
        
        typename Serialiser<Void>::Ptr null_s() const
        {
            if( !null_ )
                null_ = Serialisable<Void>::serialiser(sf_);
            return null_;
        }
        
        typename Serialiser<ADL::unittests::Pair<double,ADL::unittests::L1> >::Ptr value_s() const
        {
            if( !value_ )
                value_ = Serialisable<ADL::unittests::Pair<double,ADL::unittests::L1> >::serialiser(sf_);
            return value_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            switch( v.d() )
            {
                case ADL::unittests::L1::NULL_: writeField( json, null_s(), "null", Void() ); break;
                case ADL::unittests::L1::VALUE: writeField( json, value_s(), "value", v.value() ); break;
            }
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                if( matchField0( "null", json ) )
                {
                    null_s()->fromJson( json );
                    v.set_null_();
                }
                else if( matchField0( "value", json ) )
                    v.set_value(value_s()->fromJson( json ));
                else
                    throw json_parse_failure();
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

}; // ADL