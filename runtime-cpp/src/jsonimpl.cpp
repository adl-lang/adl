#include <iomanip>
#include <adl/jsonimpl.h>

namespace ADL {

StreamJsonWriter::StreamJsonWriter( std::ostream &ostream, bool pretty )
    : ostream_(ostream), pretty_(pretty)
{
    state_.push_back( TOPLEVEL );
    calculateIndent();
    if( pretty_ )
        space_ = " ";
}

void
StreamJsonWriter::startObject()
{
    separator();
    ostream_ << '{';
    state_.push_back( FLABEL0 );
    calculateIndent();
}

void
StreamJsonWriter::field( const std::string &f )
{
    separator();
    ostream_ << '"' << f << '"' << space_ << ':' << space_;
}

void
StreamJsonWriter::endObject()
{
    state_.pop_back();
    ostream_ << '}';
    calculateIndent();
}

void
StreamJsonWriter::startArray()
{
    separator();
    ostream_ << '[';
    state_.push_back( AVALUE0 );
    calculateIndent();
}

void
StreamJsonWriter::endArray()
{
    state_.pop_back( );
    ostream_ << ']';
    calculateIndent();
}

void
StreamJsonWriter::nullV()
{
    separator();
    ostream_ << "null";
}

void
StreamJsonWriter::boolV( bool v )
{
    separator();
    if( v )
        ostream_ << "true";
    else
        ostream_ << "false";
}

void
StreamJsonWriter::stringV( const std::string &v )
{
    // ADL strings are internally UTF8 in c++, so we don't
    // need to do any conversion to write a UTF8 JSON value

    separator();
    ostream_ << '"' << v << '"';
}

void
StreamJsonWriter::intV( int64_t v )
{
    separator();
    ostream_ << v;
}

void
StreamJsonWriter::uintV( uint64_t v )
{
    separator();
    ostream_ << v;
}

void
StreamJsonWriter::doubleV( double v )
{
    // 17 digits should roundtrip accurately, but won't generally
    // give a human friendly representation.

    separator();
    ostream_ << std::setprecision(17) << v;
}

void
StreamJsonWriter::separator()
{
    switch( state_.back() )
    {
    case TOPLEVEL:
        break;
    case FLABEL0:
        ostream_ << newline_;
        state_.back() = FVALUE0;
        break;
    case FVALUE0:
        state_.back() = FLABELN;
        break;
    case FLABELN:
        ostream_ << "," << newline_;
        state_.back() = FVALUEN;
        break;
    case FVALUEN:
        state_.back() = FLABELN;
        break;
    case AVALUE0:
        ostream_ << newline_;
        state_.back() = AVALUEN;
        break;
    case AVALUEN:
        ostream_ << "," << newline_;
        break;
    }
}

void
StreamJsonWriter::calculateIndent()
{
    if( pretty_ )
    {
        newline_ = "\n";
        for( size_t i = 0; i < state_.size(); i++ )
            newline_ += "\t";
    }
}

};
