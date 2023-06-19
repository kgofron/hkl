#ifndef __XRAYS_MACROS_H__
#define __XRAYS_MACROS_H__

/* Guard C code in headers, while including them from C++ */
#ifdef __cplusplus
# define XRAYS_BEGIN_DECLS  extern "C" {
# define XRAYS_END_DECLS    }
#else
# define XRAYS_BEGIN_DECLS
# define XRAYS_END_DECLS
#endif

// add the win32 portability part
#if _MSC_VER && _MSC_VER <= 1200
# include <float.h>
# define INFINITY DBL_MAX
# define M_PI     3.14159265358979323846264338328
# define M_PI_2   1.57079632679489661923132169164
#endif

// common part
#ifdef __GNUC__
# define NORETURN __attribute__((__noreturn__))
#else
# define NORETURN
# ifndef __attribute__
#  define __attribute__(x)
# endif
#endif

#endif

XRAYS_BEGIN_DECLS

extern void die(const char *err, ...) NORETURN __attribute__((format (printf, 1, 2)));

XRAYS_END_DECLS
