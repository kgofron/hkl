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
 * Copyright (C) 2003-2020 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include "stdint.h"

typedef struct _HklBinocularsAxis HklBinocularsAxis;
struct _HklBinocularsAxis
{
	const char *name; /* the name of the axis */
	size_t index; /* the index of the axis of the projection used */
	double resolution; /* the resolution of the bins */
	ptrdiff_t imin; /* the minimum index of the axis min = imin * resolution */
	ptrdiff_t imax; /* the maximum index of the axis max = imax * resolution */
};

extern double *hkl_binoculars_axis_array(const HklBinocularsAxis *self);

typedef struct _HklBinocularsSpace HklBinocularsSpace;
struct _HklBinocularsSpace
{
	size_t n_indexes_0;
	ptrdiff_t *indexes_0;
	size_t n_axes;
	HklBinocularsAxis *axes;
};

extern void hkl_binoculars_space_free(HklBinocularsSpace *self);

extern HklBinocularsSpace *hkl_binoculars_space_q(const HklGeometry *geometry,
						  const uint16_t *image,
						  size_t n_pixels,
						  const double *pixels_coordinates,
						  size_t pixels_coordinates_ndim,
						  const size_t *pixels_coordinates_dims,
						  const double *resolutions,
						  size_t n_resolutions);

extern HklBinocularsSpace *hkl_binoculars_space_hkl(const HklGeometry *geometry,
                                                    const HklSample *sample,
                                                    const uint16_t *image,
                                                    size_t n_pixels,
                                                    const double *pixels_coordinates,
                                                    size_t pixels_coordinates_ndim,
                                                    const size_t *pixels_coordinates_dims,
                                                    const double *resolutions,
                                                    size_t n_resolutions);

typedef  struct _HklBinocularsCube HklBinocularsCube;
struct _HklBinocularsCube
{
	size_t n_axes;
	HklBinocularsAxis *axes;
	unsigned int *photons;
	unsigned int *contributions;
};

extern void hkl_binoculars_cube_free(HklBinocularsCube *self);

extern HklBinocularsCube *hkl_binoculars_cube_new(size_t n_spaces,
						  const HklBinocularsSpace *const *spaces,
						  size_t n_pixels, const uint16_t **imgs);

extern HklBinocularsCube *hkl_binoculars_cube_new_copy(const HklBinocularsCube *src);

extern HklBinocularsCube *hkl_binoculars_cube_new_from_space(const HklBinocularsSpace *space,
                                                             size_t n_pixels,
                                                             const uint16_t *img);

extern HklBinocularsCube *hkl_binoculars_cube_new_merge(const HklBinocularsCube *cube1,
                                                        const HklBinocularsCube *cube2);
