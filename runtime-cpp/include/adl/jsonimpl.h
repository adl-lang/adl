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

class StringJsonReader : public JsonReader
{
public:
    StringJsonReader( const std::string &s );
    Type type();
    void next();
    const std::string & fieldName();
    bool boolV();
    int64_t intV();
    uint64_t uintV();
    double doubleV();
    const std::string & stringV();

private:
    void next0();

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

    std::string s_;
    std::string::iterator c_;
    std::vector<State> state_;
    Type type_;
    std::string sval_;
    bool bval_;
};

inline void
StringJsonReader::skipWhitespace()
{
    while( c_ != s_.end() && (*c_ == ' ' || *c_ == '\n' || *c_ == '\t' ))
        c_++;
}

};

#endif
