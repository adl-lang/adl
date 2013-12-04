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

};

#endif
