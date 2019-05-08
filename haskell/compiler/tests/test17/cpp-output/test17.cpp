#include "test17.h"

namespace ADL {
namespace test17 {

X2::X2()
    : d_(F1), p_(new int32_t(0))
{
}

X2 X2::mk_f1( const int32_t & v )
{
    return X2( F1, new int32_t(v) );
}

X2 X2::mk_f2( const int32_t & v )
{
    return X2( F2, new int32_t(v) );
}

X2 X2::mk_f3( const Pair<std::string,int32_t>  & v )
{
    return X2( F3, new Pair<std::string,int32_t> (v) );
}

X2 X2::mk_f4( const Pair<std::string,std::string>  & v )
{
    return X2( F4, new Pair<std::string,std::string> (v) );
}

X2 X2::mk_f5( const std::vector<int32_t>  & v )
{
    return X2( F5, new std::vector<int32_t> (v) );
}

X2 X2::mk_f6( const std::vector<Pair<std::string,int32_t> >  & v )
{
    return X2( F6, new std::vector<Pair<std::string,int32_t> > (v) );
}

X2 X2::mk_f7( const std::vector<Pair<std::string,std::string> >  & v )
{
    return X2( F7, new std::vector<Pair<std::string,std::string> > (v) );
}

X2::X2( const X2 & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

X2::~X2()
{
    free(d_,p_);
}

X2 & X2::operator=( const X2 & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const int32_t & X2::set_f1(const int32_t &v)
{
    if( d_ == F1 )
    {
        *(int32_t *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F1;
        p_ = new int32_t(v);
    }
    return *(int32_t *)p_;
}

const int32_t & X2::set_f2(const int32_t &v)
{
    if( d_ == F2 )
    {
        *(int32_t *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F2;
        p_ = new int32_t(v);
    }
    return *(int32_t *)p_;
}

const Pair<std::string,int32_t>  & X2::set_f3(const Pair<std::string,int32_t>  &v)
{
    if( d_ == F3 )
    {
        *(Pair<std::string,int32_t>  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F3;
        p_ = new Pair<std::string,int32_t> (v);
    }
    return *(Pair<std::string,int32_t>  *)p_;
}

const Pair<std::string,std::string>  & X2::set_f4(const Pair<std::string,std::string>  &v)
{
    if( d_ == F4 )
    {
        *(Pair<std::string,std::string>  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F4;
        p_ = new Pair<std::string,std::string> (v);
    }
    return *(Pair<std::string,std::string>  *)p_;
}

const std::vector<int32_t>  & X2::set_f5(const std::vector<int32_t>  &v)
{
    if( d_ == F5 )
    {
        *(std::vector<int32_t>  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F5;
        p_ = new std::vector<int32_t> (v);
    }
    return *(std::vector<int32_t>  *)p_;
}

const std::vector<Pair<std::string,int32_t> >  & X2::set_f6(const std::vector<Pair<std::string,int32_t> >  &v)
{
    if( d_ == F6 )
    {
        *(std::vector<Pair<std::string,int32_t> >  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F6;
        p_ = new std::vector<Pair<std::string,int32_t> > (v);
    }
    return *(std::vector<Pair<std::string,int32_t> >  *)p_;
}

const std::vector<Pair<std::string,std::string> >  & X2::set_f7(const std::vector<Pair<std::string,std::string> >  &v)
{
    if( d_ == F7 )
    {
        *(std::vector<Pair<std::string,std::string> >  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = F7;
        p_ = new std::vector<Pair<std::string,std::string> > (v);
    }
    return *(std::vector<Pair<std::string,std::string> >  *)p_;
}

X2::X2(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void X2::free(DiscType d, void *p)
{
    switch( d )
    {
        case F1: delete (int32_t *)p; return;
        case F2: delete (int32_t *)p; return;
        case F3: delete (Pair<std::string,int32_t>  *)p; return;
        case F4: delete (Pair<std::string,std::string>  *)p; return;
        case F5: delete (std::vector<int32_t>  *)p; return;
        case F6: delete (std::vector<Pair<std::string,int32_t> >  *)p; return;
        case F7: delete (std::vector<Pair<std::string,std::string> >  *)p; return;
    }
}

void * X2::copy( DiscType d, void *p )
{
    switch( d )
    {
        case F1: return new int32_t(*(int32_t *)p);
        case F2: return new int32_t(*(int32_t *)p);
        case F3: return new Pair<std::string,int32_t> (*(Pair<std::string,int32_t>  *)p);
        case F4: return new Pair<std::string,std::string> (*(Pair<std::string,std::string>  *)p);
        case F5: return new std::vector<int32_t> (*(std::vector<int32_t>  *)p);
        case F6: return new std::vector<Pair<std::string,int32_t> > (*(std::vector<Pair<std::string,int32_t> >  *)p);
        case F7: return new std::vector<Pair<std::string,std::string> > (*(std::vector<Pair<std::string,std::string> >  *)p);
    }
    return 0;
}

bool
operator<( const X2 &a, const X2 &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case X2::F1: return a.f1() < b.f1();
        case X2::F2: return a.f2() < b.f2();
        case X2::F3: return a.f3() < b.f3();
        case X2::F4: return a.f4() < b.f4();
        case X2::F5: return a.f5() < b.f5();
        case X2::F6: return a.f6() < b.f6();
        case X2::F7: return a.f7() < b.f7();
    }
    return false;
}

bool
operator==( const X2 &a, const X2 &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case X2::F1: return a.f1() == b.f1();
        case X2::F2: return a.f2() == b.f2();
        case X2::F3: return a.f3() == b.f3();
        case X2::F4: return a.f4() == b.f4();
        case X2::F5: return a.f5() == b.f5();
        case X2::F6: return a.f6() == b.f6();
        case X2::F7: return a.f7() == b.f7();
    }
    return false;
}

X1::X1()
    : f1(0)
{
}

X1::X1(
    const int32_t & f1_,
    const T1 & f2_,
    const T2<std::string>  & f3_,
    const T3<std::string,std::string>  & f4_,
    const std::vector<int32_t>  & f5_,
    const std::vector<Pair<std::string,int32_t> >  & f6_,
    const std::vector<Pair<std::string,std::string> >  & f7_
    )
    : f1(f1_)
    , f2(f2_)
    , f3(f3_)
    , f4(f4_)
    , f5(f5_)
    , f6(f6_)
    , f7(f7_)
{
}

bool
operator<( const X1 &a, const X1 &b )
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
    if( a.f6 < b.f6 ) return true;
    if( b.f6 < a.f6 ) return false;
    if( a.f7 < b.f7 ) return true;
    if( b.f7 < a.f7 ) return false;
    return false;
}

bool
operator==( const X1 &a, const X1 &b )
{
    return
        a.f1 == b.f1 &&
        a.f2 == b.f2 &&
        a.f3 == b.f3 &&
        a.f4 == b.f4 &&
        a.f5 == b.f5 &&
        a.f6 == b.f6 &&
        a.f7 == b.f7 ;
}

}}; // ADL::test17

namespace ADL {

typename Serialiser<ADL::test17::X2>::Ptr
Serialisable<ADL::test17::X2>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test17::X2 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<int32_t>::Ptr f1_;
        mutable typename Serialiser<int32_t>::Ptr f2_;
        mutable typename Serialiser<ADL::test17::Pair<std::string,int32_t> >::Ptr f3_;
        mutable typename Serialiser<ADL::test17::Pair<std::string,std::string> >::Ptr f4_;
        mutable typename Serialiser<std::vector<int32_t> >::Ptr f5_;
        mutable typename Serialiser<std::vector<ADL::test17::Pair<std::string,int32_t> > >::Ptr f6_;
        mutable typename Serialiser<std::vector<ADL::test17::Pair<std::string,std::string> > >::Ptr f7_;
        
        typename Serialiser<int32_t>::Ptr f1_s() const
        {
            if( !f1_ )
                f1_ = Serialisable<int32_t>::serialiser(sf_);
            return f1_;
        }
        
        typename Serialiser<int32_t>::Ptr f2_s() const
        {
            if( !f2_ )
                f2_ = Serialisable<int32_t>::serialiser(sf_);
            return f2_;
        }
        
        typename Serialiser<ADL::test17::Pair<std::string,int32_t> >::Ptr f3_s() const
        {
            if( !f3_ )
                f3_ = Serialisable<ADL::test17::Pair<std::string,int32_t> >::serialiser(sf_);
            return f3_;
        }
        
        typename Serialiser<ADL::test17::Pair<std::string,std::string> >::Ptr f4_s() const
        {
            if( !f4_ )
                f4_ = Serialisable<ADL::test17::Pair<std::string,std::string> >::serialiser(sf_);
            return f4_;
        }
        
        typename Serialiser<std::vector<int32_t> >::Ptr f5_s() const
        {
            if( !f5_ )
                f5_ = Serialisable<std::vector<int32_t> >::serialiser(sf_);
            return f5_;
        }
        
        typename Serialiser<std::vector<ADL::test17::Pair<std::string,int32_t> > >::Ptr f6_s() const
        {
            if( !f6_ )
                f6_ = Serialisable<std::vector<ADL::test17::Pair<std::string,int32_t> > >::serialiser(sf_);
            return f6_;
        }
        
        typename Serialiser<std::vector<ADL::test17::Pair<std::string,std::string> > >::Ptr f7_s() const
        {
            if( !f7_ )
                f7_ = Serialisable<std::vector<ADL::test17::Pair<std::string,std::string> > >::serialiser(sf_);
            return f7_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test17::X2::F1: json.startObject(); writeField( json, f1_s(), "f1", v.f1() ); json.endObject(); break;
                case ADL::test17::X2::F2: json.startObject(); writeField( json, f2_s(), "f2", v.f2() ); json.endObject(); break;
                case ADL::test17::X2::F3: json.startObject(); writeField( json, f3_s(), "f3", v.f3() ); json.endObject(); break;
                case ADL::test17::X2::F4: json.startObject(); writeField( json, f4_s(), "f4", v.f4() ); json.endObject(); break;
                case ADL::test17::X2::F5: json.startObject(); writeField( json, f5_s(), "f5", v.f5() ); json.endObject(); break;
                case ADL::test17::X2::F6: json.startObject(); writeField( json, f6_s(), "f6", v.f6() ); json.endObject(); break;
                case ADL::test17::X2::F7: json.startObject(); writeField( json, f7_s(), "f7", v.f7() ); json.endObject(); break;
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
                    else if( matchField0( "f3", json ) )
                        v.set_f3(f3_s()->fromJson( json ));
                    else if( matchField0( "f4", json ) )
                        v.set_f4(f4_s()->fromJson( json ));
                    else if( matchField0( "f5", json ) )
                        v.set_f5(f5_s()->fromJson( json ));
                    else if( matchField0( "f6", json ) )
                        v.set_f6(f6_s()->fromJson( json ));
                    else if( matchField0( "f7", json ) )
                        v.set_f7(f7_s()->fromJson( json ));
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

typename Serialiser<ADL::test17::X1>::Ptr
Serialisable<ADL::test17::X1>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test17::X1 _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : f1_s( Serialisable<int32_t>::serialiser(sf) )
            , f2_s( Serialisable<ADL::test17::T1>::serialiser(sf) )
            , f3_s( Serialisable<ADL::test17::T2<std::string> >::serialiser(sf) )
            , f4_s( Serialisable<ADL::test17::T3<std::string,std::string> >::serialiser(sf) )
            , f5_s( Serialisable<std::vector<int32_t> >::serialiser(sf) )
            , f6_s( Serialisable<std::vector<ADL::test17::Pair<std::string,int32_t> > >::serialiser(sf) )
            , f7_s( Serialisable<std::vector<ADL::test17::Pair<std::string,std::string> > >::serialiser(sf) )
            {}
        
        
        typename Serialiser<int32_t>::Ptr f1_s;
        typename Serialiser<ADL::test17::T1>::Ptr f2_s;
        typename Serialiser<ADL::test17::T2<std::string> >::Ptr f3_s;
        typename Serialiser<ADL::test17::T3<std::string,std::string> >::Ptr f4_s;
        typename Serialiser<std::vector<int32_t> >::Ptr f5_s;
        typename Serialiser<std::vector<ADL::test17::Pair<std::string,int32_t> > >::Ptr f6_s;
        typename Serialiser<std::vector<ADL::test17::Pair<std::string,std::string> > >::Ptr f7_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<int32_t>( json, f1_s, "f1", v.f1 );
            writeField<ADL::test17::T1>( json, f2_s, "f2", v.f2 );
            writeField<ADL::test17::T2<std::string> >( json, f3_s, "f3", v.f3 );
            writeField<ADL::test17::T3<std::string,std::string> >( json, f4_s, "f4", v.f4 );
            writeField<std::vector<int32_t> >( json, f5_s, "f5", v.f5 );
            writeField<std::vector<ADL::test17::Pair<std::string,int32_t> > >( json, f6_s, "f6", v.f6 );
            writeField<std::vector<ADL::test17::Pair<std::string,std::string> > >( json, f7_s, "f7", v.f7 );
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
                readField( f6_s, v.f6, "f6", json ) ||
                readField( f7_s, v.f7, "f7", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL