#include "test18.h"

namespace ADL {
namespace test18 {

X1::X1()
    : d_(F1), p_(new double(0.0))
{
}

X1 X1::mk_f1( const double & v )
{
    return X1( F1, new double(v) );
}

X1 X1::mk_f2( const Y1 & v )
{
    return X1( F2, new Y1(v) );
}

X1::X1( const X1 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

X1::~X1()
{
    free(d_,p_);
}

X1 & X1::operator=( const X1 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const double & X1::set_f1(const double &v)
{
    if( d_ == F1 )
    {
        *(double *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F1;
        p_ = new double(v);
    }
    return *(double *)p_;
}

const Y1 & X1::set_f2(const Y1 &v)
{
    if( d_ == F2 )
    {
        *(Y1 *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F2;
        p_ = new Y1(v);
    }
    return *(Y1 *)p_;
}

X1::X1(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void X1::free(DiscType d, void *p)
{
    switch( d )
    {
        case F1: delete (double *)p; return;
        case F2: delete (Y1 *)p; return;
    }
}

void * X1::copy( DiscType d, void *p )
{
    switch( d )
    {
        case F1: return new double(*(double *)p);
        case F2: return new Y1(*(Y1 *)p);
    }
    return 0;
}

bool
operator<( const X1 &a, const X1 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case X1::F1: return a.f1() < b.f1();
        case X1::F2: return a.f2() < b.f2();
    }
    return false;
}

bool
operator==( const X1 &a, const X1 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case X1::F1: return a.f1() == b.f1();
        case X1::F2: return a.f2() == b.f2();
    }
    return false;
}

X2::X2()
    : f1(0.0)
{
}

X2::X2(
    const double & f1_,
    const std::vector<Y2>  & f2_
    )
    : f1(f1_)
    , f2(f2_)
{
}

bool
operator<( const X2 &a, const X2 &b )
{
    if( a.f1 < b.f1 ) return true;
    if( b.f1 < a.f1 ) return false;
    if( a.f2 < b.f2 ) return true;
    if( b.f2 < a.f2 ) return false;
    return false;
}

bool
operator==( const X2 &a, const X2 &b )
{
    return
        a.f1 == b.f1 &&
        a.f2 == b.f2 ;
}

Y1::Y1()
    : d_(F1), p_(new std::string())
{
}

Y1 Y1::mk_f1( const std::string & v )
{
    return Y1( F1, new std::string(v) );
}

Y1 Y1::mk_f2( const X1 & v )
{
    return Y1( F2, new X1(v) );
}

Y1::Y1( const Y1 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

Y1::~Y1()
{
    free(d_,p_);
}

Y1 & Y1::operator=( const Y1 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const std::string & Y1::set_f1(const std::string &v)
{
    if( d_ == F1 )
    {
        *(std::string *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F1;
        p_ = new std::string(v);
    }
    return *(std::string *)p_;
}

const X1 & Y1::set_f2(const X1 &v)
{
    if( d_ == F2 )
    {
        *(X1 *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F2;
        p_ = new X1(v);
    }
    return *(X1 *)p_;
}

Y1::Y1(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void Y1::free(DiscType d, void *p)
{
    switch( d )
    {
        case F1: delete (std::string *)p; return;
        case F2: delete (X1 *)p; return;
    }
}

void * Y1::copy( DiscType d, void *p )
{
    switch( d )
    {
        case F1: return new std::string(*(std::string *)p);
        case F2: return new X1(*(X1 *)p);
    }
    return 0;
}

bool
operator<( const Y1 &a, const Y1 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case Y1::F1: return a.f1() < b.f1();
        case Y1::F2: return a.f2() < b.f2();
    }
    return false;
}

bool
operator==( const Y1 &a, const Y1 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case Y1::F1: return a.f1() == b.f1();
        case Y1::F2: return a.f2() == b.f2();
    }
    return false;
}

Y2::Y2()
{
}

Y2::Y2(
    const std::string & f1_,
    const std::vector<X2>  & f2_
    )
    : f1(f1_)
    , f2(f2_)
{
}

bool
operator<( const Y2 &a, const Y2 &b )
{
    if( a.f1 < b.f1 ) return true;
    if( b.f1 < a.f1 ) return false;
    if( a.f2 < b.f2 ) return true;
    if( b.f2 < a.f2 ) return false;
    return false;
}

bool
operator==( const Y2 &a, const Y2 &b )
{
    return
        a.f1 == b.f1 &&
        a.f2 == b.f2 ;
}

}}; // ADL::test18

namespace ADL {

typename Serialiser<ADL::test18::X1>::Ptr
Serialisable<ADL::test18::X1>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test18::X1 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<double>::Ptr f1_;
        mutable typename Serialiser<ADL::test18::Y1>::Ptr f2_;
        
        typename Serialiser<double>::Ptr f1_s() const
        {
            if( !f1_ )
                f1_ = Serialisable<double>::serialiser(sf_);
            return f1_;
        }
        
        typename Serialiser<ADL::test18::Y1>::Ptr f2_s() const
        {
            if( !f2_ )
                f2_ = Serialisable<ADL::test18::Y1>::serialiser(sf_);
            return f2_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test18::X1::F1: json.startObject(); writeField( json, f1_s(), "f1", v.f1() ); json.endObject(); break;
                case ADL::test18::X1::F2: json.startObject(); writeField( json, f2_s(), "f2", v.f2() ); json.endObject(); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::START_OBJECT )
            {
                match( json, JsonReader::START_OBJECT );
                if( json.type() == JsonReader::END_OBJECT )
                    throw json_parse_failure();
                while( !match0( json, JsonReader::END_OBJECT ) )
                {
                    if( matchField0( "f1", json ) )
                        v.set_f1(f1_s()->fromJson( json ));
                    else if( matchField0( "f2", json ) )
                        v.set_f2(f2_s()->fromJson( json ));
                    else
                        throw json_parse_failure();
                }
                return;
            }
            throw json_parse_failure();
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

typename Serialiser<ADL::test18::X2>::Ptr
Serialisable<ADL::test18::X2>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test18::X2 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f1_s( Serialisable<double>::serialiser(sf) )
            , f2_s( Serialisable<std::vector<ADL::test18::Y2> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<double>::Ptr f1_s;
        typename Serialiser<std::vector<ADL::test18::Y2> >::Ptr f2_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<double>( json, f1_s, "f1", v.f1 );
            writeField<std::vector<ADL::test18::Y2> >( json, f2_s, "f2", v.f2 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f1_s, v.f1, "f1", json ) ||
                readField( f2_s, v.f2, "f2", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::test18::Y1>::Ptr
Serialisable<ADL::test18::Y1>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test18::Y1 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<std::string>::Ptr f1_;
        mutable typename Serialiser<ADL::test18::X1>::Ptr f2_;
        
        typename Serialiser<std::string>::Ptr f1_s() const
        {
            if( !f1_ )
                f1_ = Serialisable<std::string>::serialiser(sf_);
            return f1_;
        }
        
        typename Serialiser<ADL::test18::X1>::Ptr f2_s() const
        {
            if( !f2_ )
                f2_ = Serialisable<ADL::test18::X1>::serialiser(sf_);
            return f2_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test18::Y1::F1: json.startObject(); writeField( json, f1_s(), "f1", v.f1() ); json.endObject(); break;
                case ADL::test18::Y1::F2: json.startObject(); writeField( json, f2_s(), "f2", v.f2() ); json.endObject(); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::START_OBJECT )
            {
                match( json, JsonReader::START_OBJECT );
                if( json.type() == JsonReader::END_OBJECT )
                    throw json_parse_failure();
                while( !match0( json, JsonReader::END_OBJECT ) )
                {
                    if( matchField0( "f1", json ) )
                        v.set_f1(f1_s()->fromJson( json ));
                    else if( matchField0( "f2", json ) )
                        v.set_f2(f2_s()->fromJson( json ));
                    else
                        throw json_parse_failure();
                }
                return;
            }
            throw json_parse_failure();
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

typename Serialiser<ADL::test18::Y2>::Ptr
Serialisable<ADL::test18::Y2>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test18::Y2 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f1_s( Serialisable<std::string>::serialiser(sf) )
            , f2_s( Serialisable<std::vector<ADL::test18::X2> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<std::string>::Ptr f1_s;
        typename Serialiser<std::vector<ADL::test18::X2> >::Ptr f2_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::string>( json, f1_s, "f1", v.f1 );
            writeField<std::vector<ADL::test18::X2> >( json, f2_s, "f2", v.f2 );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( f1_s, v.f1, "f1", json ) ||
                readField( f2_s, v.f2, "f2", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL