// @generated from adl module picture
#ifndef PICTURE_H
#define PICTURE_H
#include <adl/adl.h>
#include <vector>

namespace ADL {
namespace picture {

struct Circle;

struct Rectangle;

template <class T>
struct Translated;

struct Circle
{
    Circle();
    
    Circle(
        const double & radius
        );
    
    double radius;
};

bool operator<( const Circle &a, const Circle &b );
bool operator==( const Circle &a, const Circle &b );

class Picture
{
public:
    Picture();
    static Picture mk_circle( const Circle & v );
    static Picture mk_rectangle( const Rectangle & v );
    static Picture mk_composed( const std::vector<Picture>  & v );
    static Picture mk_translated( const Translated<Picture>  & v );
    
    Picture( const Picture & );
    ~Picture();
    Picture & operator=( const Picture & );
    
    enum DiscType
    {
        CIRCLE,
        RECTANGLE,
        COMPOSED,
        TRANSLATED
    };
    
    DiscType d() const;
    
    template<class Visitor>
    void visit(Visitor vis) const
    {
        switch (d())
        {
        case CIRCLE: { vis.circle(circle()); }
        case RECTANGLE: { vis.rectangle(rectangle()); }
        case COMPOSED: { vis.composed(composed()); }
        case TRANSLATED: { vis.translated(translated()); }
        }
    }
    
    bool is_circle() const { return d_ == CIRCLE; };
    bool is_rectangle() const { return d_ == RECTANGLE; };
    bool is_composed() const { return d_ == COMPOSED; };
    bool is_translated() const { return d_ == TRANSLATED; };
    
    Circle & circle() const;
    Rectangle & rectangle() const;
    std::vector<Picture>  & composed() const;
    Translated<Picture>  & translated() const;
    
    const Circle & set_circle(const Circle & );
    const Rectangle & set_rectangle(const Rectangle & );
    const std::vector<Picture>  & set_composed(const std::vector<Picture>  & );
    const Translated<Picture>  & set_translated(const Translated<Picture>  & );
    
private:
    Picture( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const Picture &a, const Picture &b );
bool operator==( const Picture &a, const Picture &b );

inline Picture::DiscType Picture::d() const
{
    return d_;
}

inline Circle & Picture::circle() const
{
    if( d_ == CIRCLE )
    {
        return *(Circle *)p_;
    }
    throw invalid_union_access();
}

inline Rectangle & Picture::rectangle() const
{
    if( d_ == RECTANGLE )
    {
        return *(Rectangle *)p_;
    }
    throw invalid_union_access();
}

inline std::vector<Picture>  & Picture::composed() const
{
    if( d_ == COMPOSED )
    {
        return *(std::vector<Picture>  *)p_;
    }
    throw invalid_union_access();
}

inline Translated<Picture>  & Picture::translated() const
{
    if( d_ == TRANSLATED )
    {
        return *(Translated<Picture>  *)p_;
    }
    throw invalid_union_access();
}

struct Rectangle
{
    Rectangle();
    
    Rectangle(
        const double & width,
        const double & height
        );
    
    double width;
    double height;
};

bool operator<( const Rectangle &a, const Rectangle &b );
bool operator==( const Rectangle &a, const Rectangle &b );

template <class T>
struct Translated
{
    typedef T TType;
    
    Translated();
    
    Translated(
        const double & xoffset,
        const double & yoffset,
        const T & object
        );
    
    double xoffset;
    double yoffset;
    T object;
};

template <class T>
bool operator<( const Translated<T> &a, const Translated<T> &b );
template <class T>
bool operator==( const Translated<T> &a, const Translated<T> &b );

template <class T>
Translated<T>::Translated()
    : xoffset(0)
    , yoffset(0)
{
}

template <class T>
Translated<T>::Translated(
    const double & xoffset_,
    const double & yoffset_,
    const T & object_
    )
    : xoffset(xoffset_)
    , yoffset(yoffset_)
    , object(object_)
{
}

template <class T>
bool
operator<( const Translated<T> &a, const Translated<T> &b )
{
    if( a.xoffset < b.xoffset ) return true;
    if( b.xoffset < a.xoffset ) return false;
    if( a.yoffset < b.yoffset ) return true;
    if( b.yoffset < a.yoffset ) return false;
    if( a.object < b.object ) return true;
    if( b.object < a.object ) return false;
    return false;
}

template <class T>
bool
operator==( const Translated<T> &a, const Translated<T> &b )
{
    return
        a.xoffset == b.xoffset &&
        a.yoffset == b.yoffset &&
        a.object == b.object ;
}

}}; // ADL::picture

namespace ADL {

template <>
struct Serialisable<ADL::picture::Circle>
{
    static Serialiser<ADL::picture::Circle>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::picture::Picture>
{
    static Serialiser<ADL::picture::Picture>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::picture::Rectangle>
{
    static Serialiser<ADL::picture::Rectangle>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
struct Serialisable<ADL::picture::Translated<T>>
{
    static typename Serialiser<ADL::picture::Translated<T>>::Ptr serialiser(const SerialiserFlags &);
};

template <class T>
typename Serialiser<ADL::picture::Translated<T>>::Ptr
Serialisable<ADL::picture::Translated<T>>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::picture::Translated<T> _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : xoffset_s( Serialisable<double>::serialiser(sf) )
            , yoffset_s( Serialisable<double>::serialiser(sf) )
            , object_s( Serialisable<T>::serialiser(sf) )
            {}
        
        
        typename Serialiser<double>::Ptr xoffset_s;
        typename Serialiser<double>::Ptr yoffset_s;
        typename Serialiser<T>::Ptr object_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<double>( json, xoffset_s, "xoffset", v.xoffset );
            writeField<double>( json, yoffset_s, "yoffset", v.yoffset );
            writeField<T>( json, object_s, "object", v.object );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( xoffset_s, v.xoffset, "xoffset", json ) ||
                readField( yoffset_s, v.yoffset, "yoffset", json ) ||
                readField( object_s, v.object, "object", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL
#endif // PICTURE_H
