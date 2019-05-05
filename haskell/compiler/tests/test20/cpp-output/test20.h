// @generated from adl module test20
#ifndef TEST20_H
#define TEST20_H
#include <adl/adl.h>
#include <stdint.h>
#include <string>

namespace ADL {
namespace test20 {

class Role
{
public:
    Role();
    static Role mk_underling();
    static Role mk_boss();
    static Role mk_superBoss();
    
    Role( const Role & );
    ~Role();
    Role & operator=( const Role & );
    
    enum DiscType
    {
        UNDERLING,
        BOSS,
        SUPERBOSS
    };
    
    DiscType d() const;
    
    template<class Visitor>
    void visit(Visitor vis) const
    {
        switch (d())
        {
        case UNDERLING: { vis.underling();}
        case BOSS: { vis.boss();}
        case SUPERBOSS: { vis.superBoss();}
        }
    }
    
    bool is_underling() const { return d_ == UNDERLING; };
    bool is_boss() const { return d_ == BOSS; };
    bool is_superBoss() const { return d_ == SUPERBOSS; };
    
    
    void set_underling();
    void set_boss();
    void set_superBoss();
    
private:
    Role( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const Role &a, const Role &b );
bool operator==( const Role &a, const Role &b );

inline Role::DiscType Role::d() const
{
    return d_;
}

struct Person
{
    Person();
    
    Person(
        const std::string & firstName,
        const std::string & lastName,
        const int16_t & age,
        const Role & role
        );
    
    std::string firstName;
    std::string lastName;
    int16_t age;
    Role role;
};

bool operator<( const Person &a, const Person &b );
bool operator==( const Person &a, const Person &b );

}}; // ADL::test20

namespace ADL {

template <>
struct Serialisable<ADL::test20::Role>
{
    static Serialiser<ADL::test20::Role>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test20::Person>
{
    static Serialiser<ADL::test20::Person>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // TEST20_H
