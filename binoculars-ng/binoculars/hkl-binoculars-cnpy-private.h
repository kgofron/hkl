#ifndef __HKL_BINOCULARS_CNPY_PRIVATE_H__
#define __HKL_BINOCULARS_CNPY_PRIVATE_H__

#include <stdint.h>

#include "datatype99.h"

#include "hkl.h"

G_BEGIN_DECLS

datatype(
        HklBinocularsNpyDataType,
        (HklBinocularsNpyBool),
        (HklBinocularsNpyDouble),
        (HklBinocularsNpyUInt16)
        );

extern int npy_data_type_cmp(const HklBinocularsNpyDataType t1,
                             const HklBinocularsNpyDataType t2);

extern int npy_data_type_element_size(const HklBinocularsNpyDataType t);

extern void *npy_load(const char *filename,
                      HklBinocularsNpyDataType type,
                      const darray_int *shape);

extern void npy_save(const char *fname,
                     const void *arr,
                     HklBinocularsNpyDataType type,
                     const darray_int *shape);

G_END_DECLS

#endif
