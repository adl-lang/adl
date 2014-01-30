#ifndef TEST_H
#define TEST_H
#include <adl/adl.h>
#include <string>
#include <vector>

namespace ADL {
namespace test {

struct X1;

struct X2;

struct Y1;

struct Y2;

class X1
{
public:
    X1();
    static X1 mk_f1( const double & v );
    static X1 mk_f2( const Y1 & v );
    
    X1( const X1 & );
    ~X1();
    X1 & operator=( const X1 & );
    
    enum DiscType
    {
        F1,
        F2
    };
    
    DiscType d() const;
    double & f1() const;
    Y1 & f2() const;
    
    const double & set_f1(const double & );
    const Y1 & set_f2(const Y1 & );
    
private:
    X1( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const X1 &a, const X1 &b );
bool operator==( const X1 &a, const X1 &b );

inline X1::DiscType X1::d() const
{
    return d_;
}

inline double & X1::f1() const
{
    if( d_ == F1 )
    {
        return *(double *)p_;
    }
    throw invalid_union_access();
}

inline Y1 & X1::f2() const
{
    if( d_ == F2 )
    {
        return *(Y1 *)p_;
    }
    throw invalid_union_access();
}

struct X2
{
    X2();
    
    X2(
        const double & f1,
        const std::vector<Y2>  & f2
        );
    
    double f1;
    std::vector<Y2>  f2;
};

bool operator<( const X2 &a, const X2 &b );
bool operator==( const X2 &a, const X2 &b );

class Y1
{
public:
    Y1();
    static Y1 mk_f1( const std::string & v );
    static Y1 mk_f2( const X1 & v );
    
    Y1( const Y1 & );
    ~Y1();
    Y1 & operator=( const Y1 & );
    
    enum DiscType
    {
        F1,
        F2
    };
    
    DiscType d() const;
    std::string & f1() const;
    X1 & f2() const;
    
    const std::string & set_f1(const std::string & );
    const X1 & set_f2(const X1 & );
    
private:
    Y1( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const Y1 &a, const Y1 &b );
bool operator==( const Y1 &a, const Y1 &b );

inline Y1::DiscType Y1::d() const
{
    return d_;
}

inline std::string & Y1::f1() const
{
    if( d_ == F1 )
    {
        return *(std::string *)p_;
    }
    throw invalid_union_access();
}

inline X1 & Y1::f2() const
{
    if( d_ == F2 )
    {
        return *(X1 *)p_;
    }
    throw invalid_union_access();
}

struct Y2
{
    Y2();
    
    Y2(
        const std::string & f1,
        const std::vector<X2>  & f2
        );
    
    std::string f1;
    std::vector<X2>  f2;
};

bool operator<( const Y2 &a, const Y2 &b );
bool operator==( const Y2 &a, const Y2 &b );

}}; // ADL::test

namespace ADL {

template <>
struct Serialisable<ADL::test::X1>
{
    static Serialiser<ADL::test::X1>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test::X2>
{
    static Serialiser<ADL::test::X2>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test::Y1>
{
    static Serialiser<ADL::test::Y1>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test::Y2>
{
    static Serialiser<ADL::test::Y2>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // TEST_H