// @generated from adl module sys.package
#ifndef SYS_PACKAGE_H
#define SYS_PACKAGE_H
#include <adl/adl.h>
#include <string>
#include <vector>

namespace ADL {
namespace sys {
namespace package {

struct AdlPackageRef;

struct AdlPackage
{
    AdlPackage();
    
    AdlPackage(
        const std::string & name,
        const std::vector<AdlPackageRef>  & dependencies
        );
    
    std::string name;
    std::vector<AdlPackageRef>  dependencies;
};

bool operator<( const AdlPackage &a, const AdlPackage &b );
bool operator==( const AdlPackage &a, const AdlPackage &b );

class AdlPackageRef
{
public:
    AdlPackageRef();
    static AdlPackageRef mk_stdlib();
    static AdlPackageRef mk_localdir( const std::string & v );
    
    AdlPackageRef( const AdlPackageRef & );
    ~AdlPackageRef();
    AdlPackageRef & operator=( const AdlPackageRef & );
    
    enum DiscType
    {
        STDLIB,
        LOCALDIR
    };
    
    DiscType d() const;
    
    template<class Visitor>
    void visit(Visitor vis) const
    {
        switch (d())
        {
        case STDLIB: { vis.stdlib(); break; }
        case LOCALDIR: { vis.localdir(localdir()); break; }
        }
    }
    
    bool is_stdlib() const { return d_ == STDLIB; };
    bool is_localdir() const { return d_ == LOCALDIR; };
    
    std::string & localdir() const;
    
    void set_stdlib();
    const std::string & set_localdir(const std::string & );
    
private:
    AdlPackageRef( DiscType d, void * v);
    
    DiscType d_;
    void *p_;
    
    static void free( DiscType d, void *v );
    static void *copy( DiscType d, void *v );
};

bool operator<( const AdlPackageRef &a, const AdlPackageRef &b );
bool operator==( const AdlPackageRef &a, const AdlPackageRef &b );

inline AdlPackageRef::DiscType AdlPackageRef::d() const
{
    return d_;
}

inline std::string & AdlPackageRef::localdir() const
{
    if( d_ == LOCALDIR )
    {
        return *(std::string *)p_;
    }
    throw invalid_union_access();
}

}}}; // ADL::sys::package

namespace ADL {

template <>
struct Serialisable<ADL::sys::package::AdlPackage>
{
    static Serialiser<ADL::sys::package::AdlPackage>::Ptr serialiser(const SerialiserFlags &);
};

template <>
struct Serialisable<ADL::sys::package::AdlPackageRef>
{
    static Serialiser<ADL::sys::package::AdlPackageRef>::Ptr serialiser(const SerialiserFlags &);
};

}; // ADL
#endif // SYS_PACKAGE_H
