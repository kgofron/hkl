#ifndef __HKL_BINOCULARS_IO_PRIVATE_H__
#define __HKL_BINOCULARS_IO_PRIVATE_H__
/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2025 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include <stdint.h>

#include "hkl.h"
#include "hkl-binoculars-detectors-2d-private.h"

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
