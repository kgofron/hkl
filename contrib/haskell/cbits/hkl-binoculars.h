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
 * Copyright (C) 2003-2019 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include "stdint.h"

typedef struct _HklBinocularsSpace HklBinocularsSpace;
struct _HklBinocularsSpace
{
	int *indexes;
	int n_indexes;

	int *origin;
	int *dims;
	int ndim;
} space_t;

extern double *hkl_binoculars_project_q(const HklGeometry *geometry,
					double *inout, int n_inout,
					double k);

extern void hkl_binoculars_space_free(HklBinocularsSpace *self);

extern HklBinocularsSpace *hkl_binoculars_from_image(const double *resolutions ,
						     int resolutions_n,
						     const double *coordinates,
						     const int *coordinates_dims,
						     int coordinates_ndim,
						     const uint16_t *image, int n_pixels);
