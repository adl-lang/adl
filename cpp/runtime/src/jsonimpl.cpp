#include <iomanip>
#include <sstream>
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
    calculateIndent();
    ostream_ << newline_ << '}';
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
    calculateIndent();
    ostream_ << newline_ << ']';
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
        for( size_t i = 0; i < state_.size() -1; i++ )
            newline_ += "\t";
    }
}

StreamJsonReader::StreamJsonReader( std::istream & s )
    : s_(s), done_(false), type_(NULLV)
{
    snext();
    state_.push_back( START );
    next();
}

JsonReader::Type
StreamJsonReader::type()
{
    return type_;
}

/*


def parseJSON(s):                             START
   if match( s, "[" ):
       output( START_ARRAY )                  ARRAY_1
       if match( s, "]" ):
           output( END_ARRAY )                DONE
       else:
           while 1:
               parseJSON(s)                   ARRAY_2
               if match( s, "]" ):
                   output( END_OBJECT )       DONE
                   break
               if !match( "," ): error()

   if match( s, "{" ):
       output( START_OBJECT )                 OBJECT_1
       if match( s, "}" ):
           output( END_OBJECT )               DONE
       else:
           while 1:
               field <- parseString(s)
               if !match( ":" ): error()
               output( FIELD, field )         OBJECT_2
               parseJSON(s)                   OBJECT_3
               if match( s, "}" ):
                   output( END_OBJECT )       DONE
                   break
               if !match( "," ): error()

   if match( s, '"' ):
       sv <- parseString(s)
       output( STRING, sv )                   DONE

   if match( s, 'true' ):
       output( BOOLEAN, true )                DONE

   if match( s, 'false' ):
       output( BOOLEAN, false )               DONE

   if match( s, 'null' ):
       output( VOID, None )                   DONE

   v = parseNumber( s )
       output( NUMBER, n )                   DONE
       
*/

void
StreamJsonReader::next()
{
    for(;;)
    {
        skipWhitespace();
        switch( state_.back() )
        {
        case START:
            if( match( "[" ))
            {
                type_ = START_ARRAY;
                state_.back() = ARRAY1;
                return;
            }
            else if( match( "{" ) )
            {
                type_ = START_OBJECT;
                state_.back() = OBJECT1;
                return;
            }
            else if( match( "\"" ) )
            {
                type_ = STRING;
                sval_ = parseString();
                state_.back() = DONE;
                return;
            }
            else if( match( "null" ) )
            {
                type_ = NULLV;
                state_.back() = DONE;
                return;
            }
            else if( match( "true" ) )
            {
                type_ = BOOLEAN;
                bval_ = true;
                state_.back() = DONE;
                return;
            }
            else if( match( "false" ) )
            {
                type_ = BOOLEAN;
                bval_ = false;
                state_.back() = DONE;
                return;
            }
            else if( speek() == '-' || (speek() >= '0' && speek() <= '9')  )
            {
                type_ = NUMBER;
                sval_ = parseNumber();
                state_.back() = DONE;
                return;
            }
            else
                throw json_parse_failure("START");
            break;

        case ARRAY1:
            if( match( "]" ) )
            {
                type_ = END_ARRAY;
                state_.back() = DONE;
                return;
            }
            else
            {
                state_.back() = ARRAY2;
                state_.push_back( START );
                // loop for next value
            }
            break;
        
        case ARRAY2:
            if( match( "]" ) )
            {
                type_ = END_ARRAY;
                state_.back() = DONE;
                return;
            }
            else if( match( "," ) )
            {
                state_.back() = ARRAY2;
                state_.push_back( START );
                // loop for next value
            }
            else
                throw json_parse_failure("ARRAY2");
            break;

        case OBJECT1:
            if( match( "}" ) )
            {
                type_ = END_OBJECT;
                state_.back() = DONE;
                return;
            }
            else if( match( "\"" ) )
            {
                sval_ = parseString();
                type_ = FIELD;
                skipWhitespace();
                if ( !match( ":" ) )
                    throw json_parse_failure("OBJECT1/1");
                state_.back() = OBJECT2;
                return;
            }
            else
                throw json_parse_failure("OBJECT1/2");
            break;

        case OBJECT2:
            if( match( "}" ) )
            {
                type_ = END_OBJECT;
                state_.back() = DONE;
                return;
            }
            else
            {
                state_.back() = OBJECT3;
                state_.push_back( START );
                // loop for next value
            }
            break;

        case OBJECT3:
            if( match( "}" ) )
            {
                type_ = END_OBJECT;
                state_.back() = DONE;
                return;
            }
            else if( match( "," ) )
            {
                state_.back() = OBJECT1;
                // loop for next value
            }
            else
                throw json_parse_failure("OBJECT3");
            break;

        case DONE:
            if( state_.size() == 1 )
            {
                type_ = END_OF_STREAM;
                return;
            }
            else
            {
                state_.pop_back();
                // loop for next value
            }
        }
    }
}

bool
StreamJsonReader::match( const char* cstr )
{
    // Leave the stream iterator unmodified if the first
    // character doesn't match
    if( sdone() || (*cstr && speek() != *cstr) )
        return false;

    while( *cstr )
    {
        if( sdone() || speek() != *cstr )
            return false;
        snext();
        cstr++;
    }
    return true;
}

inline bool ishex( char c )
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

std::string
hexToUTF8( std::string hexDigits )
{
    throw json_parse_failure("hexToUTF8");
}

std::string
StreamJsonReader::parseString()
{
    std::string b;
    std::string u;

    enum {
        NORMAL,
        ESCAPE,
        U1,
        U2,
        U3,
        U4
    } state = NORMAL;

    for(;;)
    {
        if( sdone() )
            throw json_parse_failure("End of input in string literal");

        char c = speek();
        snext();
        switch( state )
        {
        case NORMAL:
            if( c == '"' )
                return b;
            else if( c == '\\' )
                state = ESCAPE;
            else
                b.push_back( c );
            break;
        case ESCAPE:
            if( c == 'u' )
                state = U1;
            else
            {
                if( c == '"' )
                    b.push_back( '"' );
                else if( c == '\\' )
                    b.push_back( '\\' );
                else if( c == '/' )
                    b.push_back( '/' );
                else if( c == 'b' )
                    b.push_back( '\b' );
                else if( c == 'f' )
                    b.push_back( '\f' );
                else if( c == 'n' )
                    b.push_back( '\n' );
                else if( c == 'r' )
                    b.push_back( '\r' );
                else if( c == 't' )
                    b.push_back( '\t' );
                else
                    b.push_back( c );
                state = NORMAL;
            }
            break;
        case U1:
            if( !ishex(c) ) throw json_parse_failure("U1");
            u.push_back( c );
            state = U2;
            break; 
        case U2:
            if( !ishex(c) ) throw json_parse_failure("U2");
            u.push_back( c );
            state = U3;
            break; 
        case U3:
            if( !ishex(c) ) throw json_parse_failure("U3");
            u.push_back( c );
            state = U4;
            break; 
        case U4:
            if( !ishex(c) ) throw json_parse_failure("U4");
            u.push_back( c );
            b += hexToUTF8( u );
            state = NORMAL;
            break; 
        }
    }
}

std::string
StreamJsonReader::parseNumber()
{
    std::string b;
    bool ok = false;

    // FIXME: this implementation is lazy. It lets things
    // through that are not actually numbers
    for(;;)
    {
        if( sdone() )
            throw json_parse_failure("end of input in parseNumber");

        char c = speek();
        if( isdigit(c) )
        {
            snext();
            b.push_back( c );
            ok = true;
        }
        else if( c == '-' || c == '.' || c == 'e' || c == 'E' )
        {
            snext();
            b.push_back( c );
        }
        else 
            break;
    }

    if( ok )
        return b;
    else
        throw json_parse_failure("Invalid number");
}

const std::string &
StreamJsonReader::fieldName()
{
    return sval_;
}

bool
StreamJsonReader::boolV()
{
    return bval_;
}

const std::string &
StreamJsonReader::stringV()
{
    return sval_;
}

int64_t
StreamJsonReader::intV()
{
    int64_t v;
    std::istringstream( sval_) >> v;
    return v;
}

uint64_t
StreamJsonReader::uintV()
{
    uint64_t v;
    std::istringstream( sval_) >> v;
    return v;
}

double
StreamJsonReader::doubleV()
{
    double v;
    std::istringstream( sval_) >> v;
    return v;
}

};
