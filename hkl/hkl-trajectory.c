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
 * Copyright (C) 2003-2019, 2022 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "hkl-macros-private.h"
#include "hkl-geometry-private.h"
#include "hkl-trajectory-private.h"

HklTrajectoryResult * hkl_trajectory_result_new(void)
{
	HklTrajectoryResult *self = g_new(HklTrajectoryResult, 1);

	darray_init(self->geometries);

	return self;
}

void hkl_trajectory_result_free(HklTrajectoryResult *self)
{
	HklGeometry **geometry;

	darray_foreach(geometry, self->geometries){
		hkl_geometry_free(*geometry);
	}
	darray_free(self->geometries);
}

void hkl_trajectory_add_geometry(HklTrajectoryResult *self, const HklGeometry *geometry)
{
	darray_append(self->geometries, hkl_geometry_new_copy(geometry));
}

/* HklTrajectoryStats */

HklTrajectoryStats *hkl_trajectory_stats_new(int n)
{
	HklTrajectoryStats *self = g_new(HklTrajectoryStats, 1);

	self->n = 0;
	darray_init(self->nb_solutions);
	darray_init(self->axes_min);
	darray_init(self->axes_max);
	darray_init(self->axes_range);
	darray_resize0(self->axes_range, n);

	return self;
}

void hkl_trajectory_stats_free(HklTrajectoryStats *self)
{
	darray_free(self->axes_range);
	darray_free(self->axes_max);
	darray_free(self->axes_min);
	darray_free(self->nb_solutions);
	free(self);
}

void hkl_trajectory_stats_add(HklTrajectoryStats *self, const HklGeometryList *geometries)
{
	size_t i;

	const HklGeometryListItem *item = hkl_geometry_list_items_first_get(geometries);
	const HklGeometry *geometry = hkl_geometry_list_item_geometry_get(item);
	size_t n = darray_size(*hkl_geometry_axis_names_get(geometry));

	darray_append(self->nb_solutions, hkl_geometry_list_n_items_get(geometries));

	if(self->n == 0){
		darray_resize(self->axes_min, n);
		darray_resize(self->axes_max, n);
		darray_resize(self->axes_range, n);

		hkl_geometry_axis_values_get(geometry,
					     &darray_item(self->axes_min, 0), n,
					     HKL_UNIT_USER);
		hkl_geometry_axis_values_get(geometry,
					     &darray_item(self->axes_max, 0), n,
					     HKL_UNIT_USER);
	}else{
		double values[n];

		hkl_geometry_axis_values_get(geometry, values, n, HKL_UNIT_USER);
		for(i=0; i<n; ++i){
			if (values[i] < darray_item(self->axes_min, i))
				darray_item(self->axes_min, i) = values[i];
			else if (values[i] > darray_item(self->axes_max, i))
				darray_item(self->axes_max, i) = values[i];
		}
	}
	for(i=0;i<n;++i)
		darray_item(self->axes_range, i) = darray_item(self->axes_max, i) - darray_item(self->axes_min, i);

	self->n += 1;
}

void hkl_trajectory_stats_fprintf(FILE *f, const HklTrajectoryStats *self)
{
	size_t *p;
	double *v;

	fprintf(f, "Number of points of the trajectory: %zd\n", self->n);
	fprintf(f, "Solutions per points:");
	darray_foreach(p, self->nb_solutions){
		fprintf(f, " %zd", *p);
	}
	fprintf(f, "\n");
	fprintf(f, "Axes minium:");
	darray_foreach(v, self->axes_min){
		fprintf(f, " %f", *v);
	}
	fprintf(f, "\n");
	fprintf(f, "Axes max:");
	darray_foreach(v, self->axes_max){
		fprintf(f, " %f", *v);
	}
	fprintf(f, "\n");
	fprintf(f, "Axes range:");
	darray_foreach(v, self->axes_range){
		fprintf(f, " %f", *v);
	}
	fprintf(f, "\n");
}
