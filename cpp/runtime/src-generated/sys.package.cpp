// @generated from adl module sys.package
#include "adl/sys.package.h"

namespace ADL {
namespace sys {
namespace package {

AdlPackage::AdlPackage()
    : dependencies(mkvec<AdlPackageRef>())
{
}

AdlPackage::AdlPackage(
    const std::string & name_,
    const std::vector<AdlPackageRef>  & dependencies_
    )
    : name(name_)
    , dependencies(dependencies_)
{
}

bool
operator<( const AdlPackage &a, const AdlPackage &b )
{
    if( a.name < b.name ) return true;
    if( b.name < a.name ) return false;
    if( a.dependencies < b.dependencies ) return true;
    if( b.dependencies < a.dependencies ) return false;
    return false;
}

bool
operator==( const AdlPackage &a, const AdlPackage &b )
{
    return
        a.name == b.name &&
        a.dependencies == b.dependencies ;
}

AdlPackageRef::AdlPackageRef()
    : d_(STDLIB), p_(0)
{
}

AdlPackageRef AdlPackageRef::mk_stdlib()
{
    return AdlPackageRef( STDLIB, 0 );
}

AdlPackageRef AdlPackageRef::mk_localdir( const std::string & v )
{
    return AdlPackageRef( LOCALDIR, new std::string(v) );
}

AdlPackageRef::AdlPackageRef( const AdlPackageRef & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

AdlPackageRef::~AdlPackageRef()
{
    free(d_,p_);
}

AdlPackageRef & AdlPackageRef::operator=( const AdlPackageRef & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

void AdlPackageRef::set_stdlib()
{
    if( d_ != STDLIB )
    {
        free(d_,p_);
        d_ = STDLIB;
        p_ = 0;
    }
}

const std::string & AdlPackageRef::set_localdir(const std::string &v)
{
    if( d_ == LOCALDIR )
    {
        *(std::string *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = LOCALDIR;
        p_ = new std::string(v);
    }
    return *(std::string *)p_;
}

AdlPackageRef::AdlPackageRef(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void AdlPackageRef::free(DiscType d, void *p)
{
    switch( d )
    {
        case STDLIB: return;
        case LOCALDIR: delete (std::string *)p; return;
    }
}

void * AdlPackageRef::copy( DiscType d, void *p )
{
    switch( d )
    {
        case STDLIB: return 0;
        case LOCALDIR: return new std::string(*(std::string *)p);
    }
    return 0;
}

bool
operator<( const AdlPackageRef &a, const AdlPackageRef &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case AdlPackageRef::STDLIB: return false;
        case AdlPackageRef::LOCALDIR: return a.localdir() < b.localdir();
    }
    return false;
}

bool
operator==( const AdlPackageRef &a, const AdlPackageRef &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case AdlPackageRef::STDLIB: return true;
        case AdlPackageRef::LOCALDIR: return a.localdir() == b.localdir();
    }
    return false;
}

}}}; // ADL::sys::package

namespace ADL {

typename Serialiser<ADL::sys::package::AdlPackage>::Ptr
Serialisable<ADL::sys::package::AdlPackage>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::package::AdlPackage _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : name_s( Serialisable<std::string>::serialiser(sf) )
            , dependencies_s( Serialisable<std::vector<ADL::sys::package::AdlPackageRef> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<std::string>::Ptr name_s;
        typename Serialiser<std::vector<ADL::sys::package::AdlPackageRef> >::Ptr dependencies_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::string>( json, name_s, "name", v.name );
            writeField<std::vector<ADL::sys::package::AdlPackageRef> >( json, dependencies_s, "dependencies", v.dependencies );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( name_s, v.name, "name", json ) ||
                readField( dependencies_s, v.dependencies, "dependencies", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::sys::package::AdlPackageRef>::Ptr
Serialisable<ADL::sys::package::AdlPackageRef>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::sys::package::AdlPackageRef _T;
    
    struct U_ : public Serialiser<_T>
    {
        U_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<Void>::Ptr stdlib_;
        mutable typename Serialiser<std::string>::Ptr localdir_;
        
        typename Serialiser<Void>::Ptr stdlib_s() const
        {
            if( !stdlib_ )
                stdlib_ = Serialisable<Void>::serialiser(sf_);
            return stdlib_;
        }
        
        typename Serialiser<std::string>::Ptr localdir_s() const
        {
            if( !localdir_ )
                localdir_ = Serialisable<std::string>::serialiser(sf_);
            return localdir_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::sys::package::AdlPackageRef::STDLIB: json.stringV( "stdlib" ); break;
                case ADL::sys::package::AdlPackageRef::LOCALDIR: json.startObject(); writeField( json, localdir_s(), "localdir", v.localdir() ); json.endObject(); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::STRING )
            {
                if( json.stringV() == "stdlib" )
                    v.set_stdlib();
                else
                    throw json_parse_failure();
                json.next();
                return;
            }
            if( json.type() == JsonReader::START_OBJECT )
            {
                match( json, JsonReader::START_OBJECT );
                if( json.type() == JsonReader::END_OBJECT )
                    throw json_parse_failure();
                while( !match0( json, JsonReader::END_OBJECT ) )
                {
                    if( matchField0( "localdir", json ) )
                        v.set_localdir(localdir_s()->fromJson( json ));
                    else
                        throw json_parse_failure();
                }
                return;
            }
            throw json_parse_failure();
        }
    };
    
    return typename Serialiser<_T>::Ptr( new U_(sf) );
}

}; // ADL
