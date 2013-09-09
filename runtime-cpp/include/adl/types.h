#ifndef ADL_TYPES_H
#define ADL_TYPES_H

// Void type

struct Void
{
};

inline
bool operator<( const Void &a, const Void &b ) { return false; }

// Sink type

template <class T>
struct Sink {
};

#endif
