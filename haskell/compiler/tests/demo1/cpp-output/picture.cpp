#include "picture.h"

namespace ADL {
namespace picture {

Circle::Circle()
    : radius(0.0)
{
}

Circle::Circle(
    const double & radius_
    )
    : radius(radius_)
{
}

bool
operator<( const Circle &a, const Circle &b )
{
    if( a.radius < b.radius ) return true;
    if( b.radius < a.radius ) return false;
    return false;
}

bool
operator==( const Circle &a, const Circle &b )
{
    return
        a.radius == b.radius ;
}

Picture::Picture()
    : d_(CIRCLE), p_(new Circle())
{
}

Picture Picture::mk_circle( const Circle & v )
{
    return Picture( CIRCLE, new Circle(v) );
}

Picture Picture::mk_rectangle( const Rectangle & v )
{
    return Picture( RECTANGLE, new Rectangle(v) );
}

Picture Picture::mk_composed( const std::vector<Picture>  & v )
{
    return Picture( COMPOSED, new std::vector<Picture> (v) );
}

Picture Picture::mk_translated( const Translated<Picture>  & v )
{
    return Picture( TRANSLATED, new Translated<Picture> (v) );
}

Picture::Picture( const Picture & v )
    : d_(v.d_), p_(copy(v.d_,v.p_))
{
}

Picture::~Picture()
{
    free(d_,p_);
}

Picture & Picture::operator=( const Picture & o )
{
    free(d_,p_);
    d_ = o.d_;
    p_ = copy( o.d_, o.p_ );
    return *this;
}

const Circle & Picture::set_circle(const Circle &v)
{
    if( d_ == CIRCLE )
    {
        *(Circle *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = CIRCLE;
        p_ = new Circle(v);
    }
    return *(Circle *)p_;
}

const Rectangle & Picture::set_rectangle(const Rectangle &v)
{
    if( d_ == RECTANGLE )
    {
        *(Rectangle *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = RECTANGLE;
        p_ = new Rectangle(v);
    }
    return *(Rectangle *)p_;
}

const std::vector<Picture>  & Picture::set_composed(const std::vector<Picture>  &v)
{
    if( d_ == COMPOSED )
    {
        *(std::vector<Picture>  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = COMPOSED;
        p_ = new std::vector<Picture> (v);
    }
    return *(std::vector<Picture>  *)p_;
}

const Translated<Picture>  & Picture::set_translated(const Translated<Picture>  &v)
{
    if( d_ == TRANSLATED )
    {
        *(Translated<Picture>  *)p_ = v;
    }
    else
    {
        free(d_,p_);
        d_ = TRANSLATED;
        p_ = new Translated<Picture> (v);
    }
    return *(Translated<Picture>  *)p_;
}

Picture::Picture(DiscType d, void *p)
    : d_(d), p_(p)
{
}

void Picture::free(DiscType d, void *p)
{
    switch( d )
    {
        case CIRCLE: delete (Circle *)p; return;
        case RECTANGLE: delete (Rectangle *)p; return;
        case COMPOSED: delete (std::vector<Picture>  *)p; return;
        case TRANSLATED: delete (Translated<Picture>  *)p; return;
    }
}

void * Picture::copy( DiscType d, void *p )
{
    switch( d )
    {
        case CIRCLE: return new Circle(*(Circle *)p);
        case RECTANGLE: return new Rectangle(*(Rectangle *)p);
        case COMPOSED: return new std::vector<Picture> (*(std::vector<Picture>  *)p);
        case TRANSLATED: return new Translated<Picture> (*(Translated<Picture>  *)p);
    }
    return 0;
}

bool
operator<( const Picture &a, const Picture &b )
{
    if( a.d() < b.d() ) return true;
    if( b.d() < a.d()) return false;
    switch( a.d() )
    {
        case Picture::CIRCLE: return a.circle() < b.circle();
        case Picture::RECTANGLE: return a.rectangle() < b.rectangle();
        case Picture::COMPOSED: return a.composed() < b.composed();
        case Picture::TRANSLATED: return a.translated() < b.translated();
    }
    return false;
}

bool
operator==( const Picture &a, const Picture &b )
{
    if( a.d() != b.d() ) return false;
    switch( a.d() )
    {
        case Picture::CIRCLE: return a.circle() == b.circle();
        case Picture::RECTANGLE: return a.rectangle() == b.rectangle();
        case Picture::COMPOSED: return a.composed() == b.composed();
        case Picture::TRANSLATED: return a.translated() == b.translated();
    }
    return false;
}

Rectangle::Rectangle()
    : width(0.0)
    , height(0.0)
{
}

Rectangle::Rectangle(
    const double & width_,
    const double & height_
    )
    : width(width_)
    , height(height_)
{
}

bool
operator<( const Rectangle &a, const Rectangle &b )
{
    if( a.width < b.width ) return true;
    if( b.width < a.width ) return false;
    if( a.height < b.height ) return true;
    if( b.height < a.height ) return false;
    return false;
}

bool
operator==( const Rectangle &a, const Rectangle &b )
{
    return
        a.width == b.width &&
        a.height == b.height ;
}

}}; // ADL::picture

namespace ADL {

typename Serialiser<ADL::picture::Circle>::Ptr
Serialisable<ADL::picture::Circle>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::picture::Circle _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : radius_s( Serialisable<double>::serialiser(sf) )
            {}
        
        
        typename Serialiser<double>::Ptr radius_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<double>( json, radius_s, "radius", v.radius );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( radius_s, v.radius, "radius", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

typename Serialiser<ADL::picture::Picture>::Ptr
Serialisable<ADL::picture::Picture>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::picture::Picture _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : sf_(sf)
            {}
        
        SerialiserFlags sf_;
        mutable typename Serialiser<ADL::picture::Circle>::Ptr circle_;
        mutable typename Serialiser<ADL::picture::Rectangle>::Ptr rectangle_;
        mutable typename Serialiser<std::vector<ADL::picture::Picture> >::Ptr composed_;
        mutable typename Serialiser<ADL::picture::Translated<ADL::picture::Picture> >::Ptr translated_;
        
        typename Serialiser<ADL::picture::Circle>::Ptr circle_s() const
        {
            if( !circle_ )
                circle_ = Serialisable<ADL::picture::Circle>::serialiser(sf_);
            return circle_;
        }
        
        typename Serialiser<ADL::picture::Rectangle>::Ptr rectangle_s() const
        {
            if( !rectangle_ )
                rectangle_ = Serialisable<ADL::picture::Rectangle>::serialiser(sf_);
            return rectangle_;
        }
        
        typename Serialiser<std::vector<ADL::picture::Picture> >::Ptr composed_s() const
        {
            if( !composed_ )
                composed_ = Serialisable<std::vector<ADL::picture::Picture> >::serialiser(sf_);
            return composed_;
        }
        
        typename Serialiser<ADL::picture::Translated<ADL::picture::Picture> >::Ptr translated_s() const
        {
            if( !translated_ )
                translated_ = Serialisable<ADL::picture::Translated<ADL::picture::Picture> >::serialiser(sf_);
            return translated_;
        }
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            switch( v.d() )
            {
                case ADL::picture::Picture::CIRCLE: json.startObject(); writeField( json, circle_s(), "circle", v.circle() ); json.endObject(); break;
                case ADL::picture::Picture::RECTANGLE: json.startObject(); writeField( json, rectangle_s(), "rectangle", v.rectangle() ); json.endObject(); break;
                case ADL::picture::Picture::COMPOSED: json.startObject(); writeField( json, composed_s(), "composed", v.composed() ); json.endObject(); break;
                case ADL::picture::Picture::TRANSLATED: json.startObject(); writeField( json, translated_s(), "translated", v.translated() ); json.endObject(); break;
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
                    if( matchField0( "circle", json ) )
                        v.set_circle(circle_s()->fromJson( json ));
                    else if( matchField0( "rectangle", json ) )
                        v.set_rectangle(rectangle_s()->fromJson( json ));
                    else if( matchField0( "composed", json ) )
                        v.set_composed(composed_s()->fromJson( json ));
                    else if( matchField0( "translated", json ) )
                        v.set_translated(translated_s()->fromJson( json ));
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

typename Serialiser<ADL::picture::Rectangle>::Ptr
Serialisable<ADL::picture::Rectangle>::serialiser( const SerialiserFlags &sf )
{
    typedef ADL::picture::Rectangle _T;
    
    struct S_ : public Serialiser<_T>
    {
        S_( const SerialiserFlags & sf )
            : width_s( Serialisable<double>::serialiser(sf) )
            , height_s( Serialisable<double>::serialiser(sf) )
            {}
        
        
        typename Serialiser<double>::Ptr width_s;
        typename Serialiser<double>::Ptr height_s;
        
        void toJson( JsonWriter &json, const _T & v ) const
        {
            json.startObject();
            writeField<double>( json, width_s, "width", v.width );
            writeField<double>( json, height_s, "height", v.height );
            json.endObject();
        }
        
        void fromJson( _T &v, JsonReader &json ) const
        {
            match( json, JsonReader::START_OBJECT );
            while( !match0( json, JsonReader::END_OBJECT ) )
            {
                readField( width_s, v.width, "width", json ) ||
                readField( height_s, v.height, "height", json ) ||
                ignoreField( json );
            }
        }
    };
    
    return typename Serialiser<_T>::Ptr( new S_(sf) );
};

}; // ADL