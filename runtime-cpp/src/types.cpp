#include <adl/types.h>

#include <sstream>

extern "C" {
#include <b64/cencode.h>
#include <b64/cdecode.h>
};

namespace ADL
{

std::string
ByteVector::toLiteral( const ByteVector & v )
{

    std::ostringstream os;

    base64_encodestate state;
    base64_init_encodestate(&state);

    const int N = 4096;
    char* code = new char[2*N];
    size_t plainlength;
    size_t codelength;

    std::vector<uint8_t>::const_iterator bi = v.bytes.begin();

    do
    {
        plainlength = v.bytes.end() - bi;
        if( plainlength > N ) plainlength = N;
        codelength = base64_encode_block( (char* )&(*bi), plainlength, code, &state);
        bi += plainlength;
        os.write(code, codelength);
    }
    while (plainlength > 0);

    codelength = base64_encode_blockend(code, &state);
    os.write(code, codelength);

    delete [] code;

    return os.str();
}

ByteVector
ByteVector::fromLiteral( const std::string & v )
{
    ByteVector result;

    base64_decodestate state;
    base64_init_decodestate(&state);
    //
    const int N = 4096;
    char* plaintext = new char[N];
    size_t codelength;
    size_t plainlength;

    std::string::const_iterator ci = v.begin();

    do
    {
        codelength = v.end() - ci;
        if( codelength > N )
            codelength = N;
        plainlength = base64_decode_block( &*ci, codelength, plaintext, &state);
        ci += codelength;
        result.bytes.insert( result.bytes.end(), plaintext, plaintext + plainlength );
    }
    while (codelength > 0);

    delete [] plaintext;
    return result;
}

};
