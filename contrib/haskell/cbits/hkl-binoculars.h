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

	double *resolutions;
	int *origin;
	int *dims;
	int ndim;
} space_t;

extern void hkl_binoculars_space_free(HklBinocularsSpace *self);

HklBinocularsSpace *hkl_binoculars_space_q(const HklGeometry *geometry,
					   double k,
					   const uint16_t *image,
					   int32_t n_pixels,
					   const double *pixels_coordinates,
					   int32_t pixels_coordinates_ndim,
					   const int32_t *pixels_coordinates_dims,
					   const double *resolutions,
					   int32_t n_resolutions);
