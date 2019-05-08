#include "test20.h"

namespace ADL {
namespace test20 {

Role::Role()
    : d_(UNDERLING), p_(0)
{
}

Role Role::mk_underling()
{
    return Role( UNDERLING, 0 );
}

Role Role::mk_boss()
{
    return Role( BOSS, 0 );
}

Role Role::mk_superBoss()
{
    return Role( SUPERBOSS, 0 );
}

Role::Role( const Role & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

Role::~Role()
{
    free(d_,p_);
}

Role & Role::operator=( const Role & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

void Role::set_underling()
{
    if( d_ != UNDERLING )
    {
        free(d_,p_);
        d_ = UNDERLING;
        p_ = 0;
    }
}

void Role::set_boss()
{
    if( d_ != BOSS )
    {
        free(d_,p_);
        d_ = BOSS;
        p_ = 0;
    }
}

void Role::set_superBoss()
{
    if( d_ != SUPERBOSS )
    {
        free(d_,p_);
        d_ = SUPERBOSS;
        p_ = 0;
    }
}

Role::Role(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void Role::free(DiscType d, void *p)
{
    switch( d )
    {
        case UNDERLING: return;
        case BOSS: return;
        case SUPERBOSS: return;
    }
}

void * Role::copy( DiscType d, void *p )
{
    switch( d )
    {
        case UNDERLING: return 0;
        case BOSS: return 0;
        case SUPERBOSS: return 0;
    }
    return 0;
}

bool
operator<( const Role &a, const Role &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case Role::UNDERLING: return false;
        case Role::BOSS: return false;
        case Role::SUPERBOSS: return false;
    }
    return false;
}

bool
operator==( const Role &a, const Role &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case Role::UNDERLING: return true;
        case Role::BOSS: return true;
        case Role::SUPERBOSS: return true;
    }
    return false;
}

Person::Person()
    : age(0)
{
}

Person::Person(
    const std::string & firstName_,
    const std::string & lastName_,
    const int16_t & age_,
    const Role & role_
    )
    : firstName(firstName_)
    , lastName(lastName_)
    , age(age_)
    , role(role_)
{
}

bool
operator<( const Person &a, const Person &b )
{
    if( a.firstName < b.firstName ) return true;
    if( b.firstName < a.firstName ) return false;
    if( a.lastName < b.lastName ) return true;
    if( b.lastName < a.lastName ) return false;
    if( a.age < b.age ) return true;
    if( b.age < a.age ) return false;
    if( a.role < b.role ) return true;
    if( b.role < a.role ) return false;
    return false;
}

bool
operator==( const Person &a, const Person &b )
{
    return
        a.firstName == b.firstName &&
        a.lastName == b.lastName &&
        a.age == b.age &&
        a.role == b.role ;
}

}}; // ADL::test20

namespace ADL {

typename Serialiser<ADL::test20::Role>::Ptr
Serialisable<ADL::test20::Role>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test20::Role _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<Void>::Ptr underling_;
        mutable typename Serialiser<Void>::Ptr boss_;
        mutable typename Serialiser<Void>::Ptr superBoss_;
        
        typename Serialiser<Void>::Ptr underling_s() const
        {
            if( !underling_ )
                underling_ = Serialisable<Void>::serialiser(sf_);
            return underling_;
        }
        
        typename Serialiser<Void>::Ptr boss_s() const
        {
            if( !boss_ )
                boss_ = Serialisable<Void>::serialiser(sf_);
            return boss_;
        }
        
        typename Serialiser<Void>::Ptr superBoss_s() const
        {
            if( !superBoss_ )
                superBoss_ = Serialisable<Void>::serialiser(sf_);
            return superBoss_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test20::Role::UNDERLING: json.stringV( "u" ); break;
                case ADL::test20::Role::BOSS: json.stringV( "b" ); break;
                case ADL::test20::Role::SUPERBOSS: json.stringV( "sb" ); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::STRING )
            {
                if( json.stringV() == "u" )
                    v.set_underling();
                else if( json.stringV() == "b" )
                    v.set_boss();
                else if( json.stringV() == "sb" )
                    v.set_superBoss();
                else
                    throw json_parse_failure();
                json.next();
                return;
            }
            throw json_parse_failure();
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
}

typename Serialiser<ADL::test20::Person>::Ptr
Serialisable<ADL::test20::Person>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test20::Person _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : firstName_s( Serialisable<std::string>::serialiser(sf) )
            , lastName_s( Serialisable<std::string>::serialiser(sf) )
            , age_s( Serialisable<int16_t>::serialiser(sf) )
            , role_s( Serialisable<ADL::test20::Role>::serialiser(sf) )
            {}
        
        
        typename Serialiser<std::string>::Ptr firstName_s;
        typename Serialiser<std::string>::Ptr lastName_s;
        typename Serialiser<int16_t>::Ptr age_s;
        typename Serialiser<ADL::test20::Role>::Ptr role_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<std::string>( json, firstName_s, "fn", v.firstName );
            writeField<std::string>( json, lastName_s, "ln", v.lastName );
            writeField<int16_t>( json, age_s, "age", v.age );
            writeField<ADL::test20::Role>( json, role_s, "role", v.role );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( firstName_s, v.firstName, "fn", json ) ||
                readField( lastName_s, v.lastName, "ln", json ) ||
                readField( age_s, v.age, "age", json ) ||
                readField( role_s, v.role, "role", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL