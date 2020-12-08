#ifndef __HKL_BINOCULARS_CNPY_PRIVATE_H__
#define __HKL_BINOCULARS_CNPY_PRIVATE_H__

#include <stdint.h>

#include "hkl.h"

G_BEGIN_DECLS

enum HklBinocularsNpyDataType {
        HKL_BINOCULARS_NPY_BOOL,
        HKL_BINOCULARS_NPY_DOUBLE,
};

extern void *npy_load(const char *filename,
                      enum HklBinocularsNpyDataType,
                      const darray_int *shape);

extern void npy_save(const char *fname,
                     const void *arr,
                     enum HklBinocularsNpyDataType type,
                     const darray_int *shape);

G_END_DECLS

#endif
