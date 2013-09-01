#include "Date.h"
#include "adl.h"
#include <string>

namespace ADL {
namespace test {

// Date generated as DateO due to custom definition


struct DateO
{
    DateO();
    
    DateO(
        const std::string & date
        );
    
    std::string date;
};

bool operator<( const DateO &a, const DateO &b );
bool operator==( const DateO &a, const DateO &b );

struct S
{
    S();
    
    S(
        const Date & v1
        );
    
    Date v1;
};

bool operator<( const S &a, const S &b );
bool operator==( const S &a, const S &b );

}
}