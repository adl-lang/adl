#include "Date.h"
#include "adl.h"

namespace ADL {
namespace test {

// Date excluded due to custom definition

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