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
 * Copyright (C) 2003-2019, 2021 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl/api2/hkl2.h"
#include "hkl/ccan/array_size/array_size.h"
#include "hkl/ccan/generator/generator.h"
#include "hkl-geometry-private.h"
#include "hkl-trajectory-private.h"

static void hkl_geometry_save_as_dat(FILE *f, const HklGeometry *self)
{
	HklParameter **axis;
	darray_foreach(axis, self->axes){
		double value = hkl_parameter_value_get(*axis, HKL_UNIT_USER);
		fprintf(f, " % 18.15f", value);
	}
}

static void GeometryList_save_as_dat(const char *filename,  const struct Trajectory trajectory, const HklGeometryList *geometries)
{
	HklParameter **axis;
	const HklGeometryListItem *item;
	const struct Engine *econfig;
	generator_t(struct Engine) gen = trajectory_gen(trajectory);
	FILE *f = fopen(filename, "w+");

	/* print the header */
	econfig = generator_next(gen);
	fprintf(f, "#");
	Engine_header(f, *econfig);

	item = hkl_geometry_list_items_first_get(geometries);
	darray_foreach(axis, item->geometry->axes){
		fprintf(f, "%19s", (*axis)->name);
	}

	/* save the first point */
	fprintf(f, "\n");
	Engine_save_as_dat(f, *econfig);
	hkl_geometry_save_as_dat(f, item->geometry);

	while((econfig = generator_next(gen)) != NULL){
		fprintf(f, "\n");
		Engine_save_as_dat(f, *econfig);
		item = hkl_geometry_list_items_next_get(geometries, item);
		hkl_geometry_save_as_dat(f, item->geometry);
	}
	fclose(f);

	generator_free(gen);
}

static double hkl_geometry_list_kphi_range(const HklGeometryList *self)
{
	uint idx = 0;
	double min, max;
	const HklGeometryListItem *item;
	HklParameter **axis;

	/* get the kphi index */
	item = hkl_geometry_list_items_first_get(self);
	darray_foreach(axis, item->geometry->axes){
		if (!strcmp((*axis)->name, "kphi"))
			break;
		idx++;
	}

	min = max = hkl_parameter_value_get(darray_item(item->geometry->axes, idx), HKL_UNIT_USER);
	for(;item;item = hkl_geometry_list_items_next_get(self, item)){
		double current = hkl_parameter_value_get(darray_item(item->geometry->axes, idx), HKL_UNIT_USER);
		min = current < min ? current : min;
		max = current >= max ? current : max;
	}

	return max - min;
}

typedef struct _XY XY;

struct _XY {
	darray(int) x;
	darray(double) y;
};

static void xy_init(XY *xy) {
	darray_init(xy->x);
	darray_init(xy->y);
}

static void xy_add(XY *xy, int x, double y)
{
	darray_append(xy->x, x);
	darray_append(xy->y, y);
}

static void xy_free(XY *xy)
{
	darray_free(xy->x);
	darray_free(xy->y);
};

static void xy_save_as_dat(XY *xy, const char *filename)
{
	uint i;
	FILE *f = fopen(filename, "w+");
	fprintf(f, "# x y");
	for(i=0; i<darray_size(xy->x); ++i)
		fprintf(f, "\n%d %f",
			darray_item(xy->x, i),
			darray_item(xy->y, i));
	fclose(f);
}

int main_1(void)
{
	uint i;
	HklGeometryList *solutions;
	XY plot;

	xy_init(&plot);

	const struct Sample gaas = {
		.name = "GaAs",
		.lattice = Cubic(5.6533),
		.ux = -90.003382 * HKL_DEGTORAD,
		.uy = 0.12907 * HKL_DEGTORAD,
		.uz = -159.91372 * HKL_DEGTORAD,
	};

	Geometry gconfig =               \
		SoleilSiriusKappa(1.458637,
                                  VALUES(-0.5193202, 64.7853160, 133.5621380, -80.9690000, -0.0223369, 30.0000299));

	Geometry gconfig2 =              \
		SoleilSiriusKappa(1.458637,
				  VALUES(-0.5193202, 64.7853160, 133.5621380, 124.9690000, -0.0223369, 30.0000299));

	/* Trajectory */
	Mode mode = ModeHklBissectorVertical();
	struct Trajectory tconfig1 = TrajectoryHklFromTo(0, 0, 1, 0, 0, 6, 11, mode);
	struct Trajectory tconfig2 = TrajectoryHklFromTo(0, 0, 1, 0, 0, 6, 101, mode);
	/* move between each step */
	solutions = Trajectory_solve(tconfig1, gconfig, gaas, TRUE);
	GeometryList_save_as_dat("m1-11.dat", tconfig1, solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig1, gconfig2, gaas, TRUE);
	GeometryList_save_as_dat("m2-11.dat", tconfig1, solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig2, gconfig, gaas, TRUE);
	GeometryList_save_as_dat("m1-101.dat", tconfig2, solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig2, gconfig2, gaas, TRUE);
	GeometryList_save_as_dat("m2-101.dat", tconfig2, solutions);
	hkl_geometry_list_free(solutions);

	/* do not move between each steps */
	solutions = Trajectory_solve(tconfig1, gconfig, gaas, FALSE);
	GeometryList_save_as_dat("s1-11.dat", tconfig1, solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig1, gconfig2, gaas, FALSE);
	GeometryList_save_as_dat("s2-11.dat", tconfig1, solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig2, gconfig, gaas, FALSE);
	GeometryList_save_as_dat("s1-101.dat", tconfig2, solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig2, gconfig2, gaas, FALSE);
	GeometryList_save_as_dat("s2-101.dat", tconfig2, solutions);
	hkl_geometry_list_free(solutions);

	for(i=1; i<102; ++i){
		double range;
		struct Trajectory tconfig3 = TrajectoryHklFromTo(0, 0, 1, 0, 0, 6, i, mode);

		solutions = Trajectory_solve(tconfig3, gconfig2, gaas, TRUE);
		range = hkl_geometry_list_kphi_range(solutions);
		xy_add(&plot, i, range);
		hkl_geometry_list_free(solutions);
	}

	xy_save_as_dat(&plot, "traj_n.dat");
	xy_free(&plot);

	return 0;
}

int main_2(void)
{
	HklGeometryList *solutions;
	XY plot;

	xy_init(&plot);

	const struct Sample gaas = {
		.name = "GaAs",
		.lattice = Cubic(5.6533),
		.ux = -90.1 * HKL_DEGTORAD,
		.uy = -0.33 * HKL_DEGTORAD,
		.uz = 11.1 * HKL_DEGTORAD,
	};

	Geometry gconfig =	\
		SoleilSiriusKappa(1.553,
				  VALUES(0.00892, 65.84862, 135.42159, 100.249, -0.26559, 66.64474));

	/* Trajectory */
	Mode mode = ModeHklBissectorVertical();
	struct Trajectory tconfig1 = TrajectoryHklFromTo(0, 0, 4, 0, 0, 2, 100, mode);
	/* move between each step */
	solutions = Trajectory_solve(tconfig1, gconfig, gaas, TRUE);
	GeometryList_save_as_dat("m3-100.dat", tconfig1, solutions);
	hkl_geometry_list_free(solutions);

	xy_free(&plot);

	return 0;
}


int main(void)
{
	main_1();
	main_2();

	return 0;
}
