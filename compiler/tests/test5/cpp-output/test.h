#include "adl.h"
#include <stdint.h>

namespace ADL {
namespace test {

struct S1
{
    S1();
    
    S1(
        const int16_t & f
        );
    
    int16_t f;
};

bool operator<( const S1 &a, const S1 &b );


class U1
{
public:
    U1();
    static U1 mk_v();
    
    U1( const U1 & );
    ~U1();
    U1 operator=( const U1 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    Void & v() const;
    
private:
    DiscType d_;
    void *v_;
    void clear();
};
bool operator<( const U1 &a, const U1 &b );

class U2
{
public:
    U2();
    static U2 mk_v( const int16_t & v );
    
    U2( const U2 & );
    ~U2();
    U2 operator=( const U2 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    int16_t & v() const;
    
private:
    DiscType d_;
    void *v_;
    void clear();
};
bool operator<( const U2 &a, const U2 &b );

class U3
{
public:
    U3();
    static U3 mk_v( const int16_t & v );
    
    U3( const U3 & );
    ~U3();
    U3 operator=( const U3 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    int16_t & v() const;
    
private:
    DiscType d_;
    void *v_;
    void clear();
};
bool operator<( const U3 &a, const U3 &b );

class U4
{
public:
    U4();
    static U4 mk_v( const S1 & v );
    
    U4( const U4 & );
    ~U4();
    U4 operator=( const U4 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    S1 & v() const;
    
private:
    DiscType d_;
    void *v_;
    void clear();
};
bool operator<( const U4 &a, const U4 &b );

class U5
{
public:
    U5();
    static U5 mk_v( const S1 & v );
    
    U5( const U5 & );
    ~U5();
    U5 operator=( const U5 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    S1 & v() const;
    
private:
    DiscType d_;
    void *v_;
    void clear();
};
bool operator<( const U5 &a, const U5 &b );

class U6
{
public:
    U6();
    static U6 mk_v( const U3 & v );
    
    U6( const U6 & );
    ~U6();
    U6 operator=( const U6 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    U3 & v() const;
    
private:
    DiscType d_;
    void *v_;
    void clear();
};
bool operator<( const U6 &a, const U6 &b );

class U7
{
public:
    U7();
    static U7 mk_v( const U3 & v );
    
    U7( const U7 & );
    ~U7();
    U7 operator=( const U7 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    U3 & v() const;
    
private:
    DiscType d_;
    void *v_;
    void clear();
};
bool operator<( const U7 &a, const U7 &b );

class U8
{
public:
    U8();
    static U8 mk_v1( const S1 & v );
    static U8 mk_v2( const int16_t & v );
    
    U8( const U8 & );
    ~U8();
    U8 operator=( const U8 & );
    
    enum DiscType
    {
        V1,
        V2
    };
    
    DiscType d() const;
    S1 & v1() const;
    int16_t & v2() const;
    
private:
    DiscType d_;
    void *v_;
    void clear();
};
bool operator<( const U8 &a, const U8 &b );

}
}