#ifndef __HKL_BINOCULARS_CNPY_PRIVATE_H__
#define __HKL_BINOCULARS_CNPY_PRIVATE_H__

#include <stdint.h>

#include "hkl.h"

G_BEGIN_DECLS

enum HklBinocularsNpyDataType {
        HKL_BINOCULARS_NPY_BOOL,
};

extern void *npy_load(const char *filename,
                      enum HklBinocularsNpyDataType,
                      const darray_int *shape);

G_END_DECLS

#endif
