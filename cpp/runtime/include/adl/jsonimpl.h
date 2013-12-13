#ifndef ADL_JSONIMPL_H
#define ADL_JSONIMPL_H

#include <vector>
#include <iostream>
#include <adl/json.h>

namespace ADL {

class StreamJsonWriter : public JsonWriter
{
public:
    StreamJsonWriter( std::ostream &ostream, bool pretty );

    virtual void startObject();
    virtual void field( const std::string &f );
    virtual void endObject();

    virtual void startArray();
    virtual void endArray();

    virtual void nullV();
    virtual void boolV( bool v );
    virtual void stringV( const std::string &v );
    virtual void intV( int64_t v );
    virtual void uintV( uint64_t v );
    virtual void doubleV( double v );

private:
    std::ostream &ostream_;
    bool pretty_;
    std::string newline_;
    std::string space_;

    enum State {
        TOPLEVEL,
        FLABEL0,
        FVALUE0,
        FLABELN,
        FVALUEN,
        AVALUE0,
        AVALUEN,
    };

    std::vector<State> state_;

    void separator();
    void calculateIndent();
};

class StreamJsonReader : public JsonReader
{
public:
    StreamJsonReader( std::istream &s );
    Type type();
    void next();
    const std::string & fieldName();
    bool boolV();
    int64_t intV();
    uint64_t uintV();
    double doubleV();
    const std::string & stringV();

private:
    char speek();
    char snext();
    bool sdone();

    enum State {
        START,
        ARRAY1,
        ARRAY2,
        OBJECT1,
        OBJECT2,
        OBJECT3,
        DONE,
    };

    void skipWhitespace();

    bool match( const char *s );
    std::string parseString();
    std::string parseNumber();

    std::istream &s_;
    char c_;
    bool done_;

    std::vector<State> state_;
    Type type_;
    std::string sval_;
    bool bval_;
};

inline char
StreamJsonReader::speek()
{
    return c_;
}

inline char
StreamJsonReader::snext()
{
    if( s_.eof() )
        done_ = true;
    else
        s_ >> c_;
}

inline bool
StreamJsonReader::sdone()
{
    return done_;
}

inline void
StreamJsonReader::skipWhitespace()
{
    while( !sdone() && (speek() == ' ' || speek() == '\n' || speek() == '\t' ))
        snext();
}

};

#endif
