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
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include "hkl-axis-private.h" /* temporary */
#include "hkl-geometry-private.h" /* temporary */
/* Mode */

enum mode_e {
	MODE_HKL_BISSECTOR_VERTICAL,
};

struct Mode {
	enum mode_e tag;
};

#define ModeHklBissectorVertical {.tag=MODE_HKL_BISSECTOR_VERTICAL}

static const char *getModeName(struct Mode mode)
{
	const char *name;

	switch(mode.tag){
	case MODE_HKL_BISSECTOR_VERTICAL: name = "bissector_vertical";
	}

	return name;
}

/* Engine */

enum engine_e {
	ENGINE_HKL,
};

struct Engine {
	enum engine_e tag;
	union {
		struct {double h; double k; double l; struct Mode mode;} hkl;
	};
};

#define EngineHkl(h_, k_, l_, mode_) {.tag=ENGINE_HKL, .hkl={h_, k_, l_, mode_}}

static HklGeometryList *solve(HklEngineList *engines, struct Engine econfig)
{
	HklGeometryList *geometries;

	switch(econfig.tag) {
	case ENGINE_HKL:
	{
		double values[3] = {econfig.hkl.h, econfig.hkl.k, econfig.hkl.l};
		HklEngine *engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
		const char *mode_name = getModeName(econfig.hkl.mode);
		DIAG(hkl_engine_current_mode_set(engine, mode_name, NULL));
		geometries = hkl_engine_pseudo_axis_values_set(engine,
							       values, ARRAY_SIZE(values),
							       HKL_UNIT_DEFAULT, NULL);
	}
	break;
	}

	return geometries;
}

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

static HklTrajectoryStats *hkl_trajectory_stats_new(int n)
{
	HklTrajectoryStats *self = HKL_MALLOC(HklTrajectoryStats);

	self->n = 0;
	darray_init(self->nb_solutions);
	darray_init(self->axes_min);
	darray_init(self->axes_max);
	darray_init(self->axes_range);
	darray_resize0(self->axes_range, n);

	return self;
}

static void hkl_trajectory_stats_free(HklTrajectoryStats *self)
{
	darray_free(self->axes_range);
	darray_free(self->axes_max);
	darray_free(self->axes_min);
	darray_free(self->nb_solutions);
	free(self);
}

static void hkl_trajectory_stats_add(HklTrajectoryStats *self, const HklGeometryList *geometries)
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

	fprintf(f, "Number of points of the trajectory: %d\n", self->n);
	fprintf(f, "Solutions per points:");
	darray_foreach(p, self->nb_solutions){
		fprintf(f, " %d", *p);
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

/* tests */

static void stability(void)
{
	int i;
	int res = TRUE;
	HklEngineList *engines;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklTrajectoryStats *stats;
	static double from[] = {0, 0, 1};
	static double to[] = {0, 0, 6};
	static int n=10;

	static struct Sample gaas = {
		.name = "GaAs",
		.lattice = Cubic(5.6533),
		.ux = -90.003382 * HKL_DEGTORAD,
		.uy = 0.12907 * HKL_DEGTORAD,
		.uz = -159.91372 * HKL_DEGTORAD,
	};

	static struct Geometry gconfig = \
		SoleilSiriusKappa(1.458637,
				  -0.5193202, 64.7853160, 133.5621380, -80.9690000, -0.0223369, 30.0000299);

	geometry = newGeometry(gconfig);
	engines = newEngines(gconfig);
	sample = newSample(gaas);
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	stats = hkl_trajectory_stats_new(n);

	hkl_engine_list_init(engines, geometry, detector, sample);

	for(i=0; i<n; ++i){
		double h = (to[0] - from[0]) / (n + 1) * i + from[0];
		double k = (to[1] - from[1]) / (n + 1) * i + from[1];
		double l = (to[2] - from[2]) / (n + 1) * i + from[2];

		struct Engine econfig = EngineHkl(h, k, l, ModeHklBissectorVertical);

		geometries = solve(engines, econfig);
		hkl_trajectory_stats_add(stats, geometries);

		res &= DIAG((geometries != NULL));

		hkl_geometry_list_fprintf(stdout, geometries);
		hkl_geometry_list_free(geometries);
	}

	hkl_trajectory_stats_fprintf(stdout, stats);

	ok(res == TRUE, __func__);

	hkl_trajectory_stats_free(stats);
	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

int main(void)
{
	plan(1);

	stability();

	return 0;
}
