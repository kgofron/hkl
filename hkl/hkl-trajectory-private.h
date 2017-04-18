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
 * Copyright (C) 2003-2017 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"

/* HklTrajectoryResult */

typedef darray(HklGeometry *) darray_geometry;

typedef struct _HklTrajectoryResult HklTrajectoryResult;

struct _HklTrajectoryResult {
	darray_geometry geometries;
};

extern HklTrajectoryResult * hkl_trajectory_result_new(void);

extern void hkl_trajectory_result_free(HklTrajectoryResult *self);

extern void hkl_trajectory_add_geometry(HklTrajectoryResult *self, const HklGeometry *geometry);

/* HklTrajectoryStats */

typedef darray(double) darray_double;
typedef darray(size_t) darray_sizet;

typedef struct _HklTrajectoryStats HklTrajectoryStats;

struct _HklTrajectoryStats {
	size_t n;
	darray_sizet nb_solutions;
	darray_double axes_min;
	darray_double axes_max;
	darray_double axes_range;
};

extern HklTrajectoryStats *hkl_trajectory_stats_new(int n);

extern void hkl_trajectory_stats_free(HklTrajectoryStats *self);

extern void hkl_trajectory_stats_add(HklTrajectoryStats *self, const HklGeometryList *geometries);

extern void hkl_trajectory_stats_fprintf(FILE *f, const HklTrajectoryStats *self);
