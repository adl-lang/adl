#ifndef TEST5_H
#define TEST5_H
#include <adl/adl.h>
#include <stdint.h>

namespace ADL {
namespace test5 {

template <class T>
struct Cell;

struct S1;

struct U3;

template <class T>
class List
{
public:
    List();
    static List<T> mk_null_();
    static List<T> mk_cell( const Cell<T>  & v );
    
    List( const List<T> & );
    ~List();
    List<T> & operator=( const List<T> & );
    
    enum DiscType
    {
        NULL_,
        CELL
    };
    
    DiscType d() const;
    Cell<T>  & cell() const;
    
    void set_null_();
    const Cell<T>  & set_cell(const Cell<T>  & );
    
private:
    List( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

template <class T>
bool operator<( const List<T> &a, const List<T> &b );
template <class T>
bool operator==( const List<T> &a, const List<T> &b );

template <class T>
typename List<T>::DiscType List<T>::d() const
{
    return d_;
}

template <class T>
inline Cell<T>  & List<T>::cell() const
{
    if( d_ == CELL )
    {
        return *(Cell<T>  *)p_;
    }
    throw invalid_union_access();
}

template <class T>
List<T>::List()
    : d_(NULL_), p_(0)
{
}

template <class T>
List<T> List<T>::mk_null_()
{
    return List<T>( NULL_, 0 );
}

template <class T>
List<T> List<T>::mk_cell( const Cell<T>  & v )
{
    return List<T>( CELL, new Cell<T> (v) );
}

template <class T>
List<T>::List( const List<T> & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

template <class T>
List<T>::~List()
{
    free(d_,p_);
}

template <class T>
List<T> & List<T>::operator=( const List<T> & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

template <class T>
void List<T>::set_null_()
{
    if( d_ != NULL_ )
    {
        free(d_,p_);
        d_ = NULL_;
        p_ = 0;
    }
}

template <class T>
const Cell<T>  & List<T>::set_cell(const Cell<T>  &v)
{
    if( d_ == CELL )
    {
        *(Cell<T>  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = CELL;
        p_ = new Cell<T> (v);
    }
    return *(Cell<T>  *)p_;
}

template <class T>
List<T>::List(DiscType d, void *p)
    : d_(d), p_(p)
{
}

template <class T>
void List<T>::free(DiscType d, void *p)
{
    switch( d )
    {
        case NULL_: return;
        case CELL: delete (Cell<T>  *)p; return;
    }
}

template <class T>
void * List<T>::copy( DiscType d, void *p )
{
    switch( d )
    {
        case NULL_: return 0;
        case CELL: return new Cell<T> (*(Cell<T>  *)p);
    }
    return 0;
}

template <class T>
bool
operator<( const List<T> &a, const List<T> &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case List<T>::NULL_: return false;
        case List<T>::CELL: return a.cell() < b.cell();
    }
    return false;
}

template <class T>
bool
operator==( const List<T> &a, const List<T> &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case List<T>::NULL_: return true;
        case List<T>::CELL: return a.cell() == b.cell();
    }
    return false;
}

struct S1
{
    S1();
    
    S1(
        const int16_t & f
        );
    
    int16_t f;
};

bool operator<( const S1 &a, const S1 &b );
bool operator==( const S1 &a, const S1 &b );

class U1
{
public:
    U1();
    static U1 mk_v();
    
    U1( const U1 & );
    ~U1();
    U1 & operator=( const U1 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    
    void set_v();
    
private:
    U1( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U1 &a, const U1 &b );
bool operator==( const U1 &a, const U1 &b );

inline U1::DiscType U1::d() const
{
    return d_;
}

class U2
{
public:
    U2();
    static U2 mk_v( const int16_t & v );
    
    U2( const U2 & );
    ~U2();
    U2 & operator=( const U2 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    int16_t & v() const;
    
    const int16_t & set_v(const int16_t & );
    
private:
    U2( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U2 &a, const U2 &b );
bool operator==( const U2 &a, const U2 &b );

inline U2::DiscType U2::d() const
{
    return d_;
}

inline int16_t & U2::v() const
{
    if( d_ == V )
    {
        return *(int16_t *)p_;
    }
    throw invalid_union_access();
}

class U3
{
public:
    U3();
    static U3 mk_v( const int16_t & v );
    
    U3( const U3 & );
    ~U3();
    U3 & operator=( const U3 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    int16_t & v() const;
    
    const int16_t & set_v(const int16_t & );
    
private:
    U3( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U3 &a, const U3 &b );
bool operator==( const U3 &a, const U3 &b );

inline U3::DiscType U3::d() const
{
    return d_;
}

inline int16_t & U3::v() const
{
    if( d_ == V )
    {
        return *(int16_t *)p_;
    }
    throw invalid_union_access();
}

class U4
{
public:
    U4();
    static U4 mk_v( const S1 & v );
    
    U4( const U4 & );
    ~U4();
    U4 & operator=( const U4 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    S1 & v() const;
    
    const S1 & set_v(const S1 & );
    
private:
    U4( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U4 &a, const U4 &b );
bool operator==( const U4 &a, const U4 &b );

inline U4::DiscType U4::d() const
{
    return d_;
}

inline S1 & U4::v() const
{
    if( d_ == V )
    {
        return *(S1 *)p_;
    }
    throw invalid_union_access();
}

class U5
{
public:
    U5();
    static U5 mk_v( const S1 & v );
    
    U5( const U5 & );
    ~U5();
    U5 & operator=( const U5 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    S1 & v() const;
    
    const S1 & set_v(const S1 & );
    
private:
    U5( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U5 &a, const U5 &b );
bool operator==( const U5 &a, const U5 &b );

inline U5::DiscType U5::d() const
{
    return d_;
}

inline S1 & U5::v() const
{
    if( d_ == V )
    {
        return *(S1 *)p_;
    }
    throw invalid_union_access();
}

class U6
{
public:
    U6();
    static U6 mk_v( const U3 & v );
    
    U6( const U6 & );
    ~U6();
    U6 & operator=( const U6 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    U3 & v() const;
    
    const U3 & set_v(const U3 & );
    
private:
    U6( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U6 &a, const U6 &b );
bool operator==( const U6 &a, const U6 &b );

inline U6::DiscType U6::d() const
{
    return d_;
}

inline U3 & U6::v() const
{
    if( d_ == V )
    {
        return *(U3 *)p_;
    }
    throw invalid_union_access();
}

class U7
{
public:
    U7();
    static U7 mk_v( const U3 & v );
    
    U7( const U7 & );
    ~U7();
    U7 & operator=( const U7 & );
    
    enum DiscType
    {
        V
    };
    
    DiscType d() const;
    U3 & v() const;
    
    const U3 & set_v(const U3 & );
    
private:
    U7( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U7 &a, const U7 &b );
bool operator==( const U7 &a, const U7 &b );

inline U7::DiscType U7::d() const
{
    return d_;
}

inline U3 & U7::v() const
{
    if( d_ == V )
    {
        return *(U3 *)p_;
    }
    throw invalid_union_access();
}

class U8
{
public:
    U8();
    static U8 mk_v1( const S1 & v );
    static U8 mk_v2( const int16_t & v );
    
    U8( const U8 & );
    ~U8();
    U8 & operator=( const U8 & );
    
    enum DiscType
    {
        V1,
        V2
    };
    
    DiscType d() const;
    S1 & v1() const;
    int16_t & v2() const;
    
    const S1 & set_v1(const S1 & );
    const int16_t & set_v2(const int16_t & );
    
private:
    U8( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const U8 &a, const U8 &b );
bool operator==( const U8 &a, const U8 &b );

inline U8::DiscType U8::d() const
{
    return d_;
}

inline S1 & U8::v1() const
{
    if( d_ == V1 )
    {
        return *(S1 *)p_;
    }
    throw invalid_union_access();
}

inline int16_t & U8::v2() const
{
    if( d_ == V2 )
    {
        return *(int16_t *)p_;
    }
    throw invalid_union_access();
}

template <class T>
class U9
{
public:
    U9();
    static U9<T> mk_v1( const T & v );
    static U9<T> mk_v2( const int16_t & v );
    
    U9( const U9<T> & );
    ~U9();
    U9<T> & operator=( const U9<T> & );
    
    enum DiscType
    {
        V1,
        V2
    };
    
    DiscType d() const;
    T & v1() const;
    int16_t & v2() const;
    
    const T & set_v1(const T & );
    const int16_t & set_v2(const int16_t & );
    
private:
    U9( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

template <class T>
bool operator<( const U9<T> &a, const U9<T> &b );
template <class T>
bool operator==( const U9<T> &a, const U9<T> &b );

template <class T>
typename U9<T>::DiscType U9<T>::d() const
{
    return d_;
}

template <class T>
inline T & U9<T>::v1() const
{
    if( d_ == V1 )
    {
        return *(T *)p_;
    }
    throw invalid_union_access();
}

template <class T>
inline int16_t & U9<T>::v2() const
{
    if( d_ == V2 )
    {
        return *(int16_t *)p_;
    }
    throw invalid_union_access();
}

template <class T>
U9<T>::U9()
    : d_(V1), p_(new T())
{
}

template <class T>
U9<T> U9<T>::mk_v1( const T & v )
{
    return U9<T>( V1, new T(v) );
}

template <class T>
U9<T> U9<T>::mk_v2( const int16_t & v )
{
    return U9<T>( V2, new int16_t(v) );
}

template <class T>
U9<T>::U9( const U9<T> & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

template <class T>
U9<T>::~U9()
{
    free(d_,p_);
}

template <class T>
U9<T> & U9<T>::operator=( const U9<T> & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

template <class T>
const T & U9<T>::set_v1(const T &v)
{
    if( d_ == V1 )
    {
        *(T *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V1;
        p_ = new T(v);
    }
    return *(T *)p_;
}

template <class T>
const int16_t & U9<T>::set_v2(const int16_t &v)
{
    if( d_ == V2 )
    {
        *(int16_t *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = V2;
        p_ = new int16_t(v);
    }
    return *(int16_t *)p_;
}

template <class T>
U9<T>::U9(DiscType d, void *p)
    : d_(d), p_(p)
{
}

template <class T>
void U9<T>::free(DiscType d, void *p)
{
    switch( d )
    {
        case V1: delete (T *)p; return;
        case V2: delete (int16_t *)p; return;
    }
}

template <class T>
void * U9<T>::copy( DiscType d, void *p )
{
    switch( d )
    {
        case V1: return new T(*(T *)p);
        case V2: return new int16_t(*(int16_t *)p);
    }
    return 0;
}

template <class T>
bool
operator<( const U9<T> &a, const U9<T> &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case U9<T>::V1: return a.v1() < b.v1();
        case U9<T>::V2: return a.v2() < b.v2();
    }
    return false;
}

template <class T>
bool
operator==( const U9<T> &a, const U9<T> &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case U9<T>::V1: return a.v1() == b.v1();
        case U9<T>::V2: return a.v2() == b.v2();
    }
    return false;
}

template <class T>
struct Cell
{
    Cell();
    
    Cell(
        const T & head,
        const List<T>  & tail
        );
    
    T head;
    List<T>  tail;
};

template <class T>
bool operator<( const Cell<T> &a, const Cell<T> &b );
template <class T>
bool operator==( const Cell<T> &a, const Cell<T> &b );

template <class T>
Cell<T>::Cell()
{
}

template <class T>
Cell<T>::Cell(
    const T & head_,
    const List<T>  & tail_
    )
    : head(head_)
    , tail(tail_)
{
}

template <class T>
bool
operator<( const Cell<T> &a, const Cell<T> &b )
{
    if( a.head < b.head ) return true;
    if( b.head < a.head ) return false;
    if( a.tail < b.tail ) return true;
    if( b.tail < a.tail ) return false;
    return false;
}

template <class T>
bool
operator==( const Cell<T> &a, const Cell<T> &b )
{
    return
        a.head == b.head &&
        a.tail == b.tail ;
}

}}; // ADL::test5

namespace ADL {

template <class T>
struct Serialisable<ADL::test5::List<T>>
{
    static typename Serialiser<ADL::test5::List<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test5::List<T>>::Ptr
Serialisable<ADL::test5::List<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::List<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<Void>::Ptr null_;
        mutable typename Serialiser<ADL::test5::Cell<T> >::Ptr cell_;
        
        typename Serialiser<Void>::Ptr null_s() const
        {
            if( !null_ )
                null_ = Serialisable<Void>::serialiser(sf_);
            return null_;
        }
        
        typename Serialiser<ADL::test5::Cell<T> >::Ptr cell_s() const
        {
            if( !cell_ )
                cell_ = Serialisable<ADL::test5::Cell<T> >::serialiser(sf_);
            return cell_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::List<T>::NULL_: json.stringV( "null" ); break;
                case ADL::test5::List<T>::CELL: json.startObject(); writeField( json, cell_s(), "cell", v.cell() ); json.endObject(); break;
            }
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            if( json.type() == JsonReader::STRING )
            {
                if( json.stringV() == "null" )
                    v.set_null_();
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
                    if( matchField0( "cell", json ) )
                        v.set_cell(cell_s()->fromJson( json ));
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

template <>
struct Serialisable<ADL::test5::S1>
{
    static Serialiser<ADL::test5::S1>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test5::U1>
{
    static Serialiser<ADL::test5::U1>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test5::U2>
{
    static Serialiser<ADL::test5::U2>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test5::U3>
{
    static Serialiser<ADL::test5::U3>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test5::U4>
{
    static Serialiser<ADL::test5::U4>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test5::U5>
{
    static Serialiser<ADL::test5::U5>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test5::U6>
{
    static Serialiser<ADL::test5::U6>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test5::U7>
{
    static Serialiser<ADL::test5::U7>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::test5::U8>
{
    static Serialiser<ADL::test5::U8>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
struct Serialisable<ADL::test5::U9<T>>
{
    static typename Serialiser<ADL::test5::U9<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test5::U9<T>>::Ptr
Serialisable<ADL::test5::U9<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::U9<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<T>::Ptr v1_;
        mutable typename Serialiser<int16_t>::Ptr v2_;
        
        typename Serialiser<T>::Ptr v1_s() const
        {
            if( !v1_ )
                v1_ = Serialisable<T>::serialiser(sf_);
            return v1_;
        }
        
        typename Serialiser<int16_t>::Ptr v2_s() const
        {
            if( !v2_ )
                v2_ = Serialisable<int16_t>::serialiser(sf_);
            return v2_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::test5::U9<T>::V1: json.startObject(); writeField( json, v1_s(), "v1", v.v1() ); json.endObject(); break;
                case ADL::test5::U9<T>::V2: json.startObject(); writeField( json, v2_s(), "v2", v.v2() ); json.endObject(); break;
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
                    if( matchField0( "v1", json ) )
                        v.set_v1(v1_s()->fromJson( json ));
                    else if( matchField0( "v2", json ) )
                        v.set_v2(v2_s()->fromJson( json ));
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

template <class T>
struct Serialisable<ADL::test5::Cell<T>>
{
    static typename Serialiser<ADL::test5::Cell<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::test5::Cell<T>>::Ptr
Serialisable<ADL::test5::Cell<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::test5::Cell<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : head_s( Serialisable<T>::serialiser(sf) )
            , tail_s( Serialisable<ADL::test5::List<T> >::serialiser(sf) )
            {}
        
        
        typename Serialiser<T>::Ptr head_s;
        typename Serialiser<ADL::test5::List<T> >::Ptr tail_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<T>( json, head_s, "head", v.head );
            writeField<ADL::test5::List<T> >( json, tail_s, "tail", v.tail );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( head_s, v.head, "head", json ) ||
                readField( tail_s, v.tail, "tail", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL
#endif // TEST5_H