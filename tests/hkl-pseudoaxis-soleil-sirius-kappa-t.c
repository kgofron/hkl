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

static void stability(void)
{
	int i;
	int res = TRUE;
	HklEngineList *engines;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
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

	hkl_engine_list_init(engines, geometry, detector, sample);

	for(i=0; i<n; ++i){
		double h = (to[0] - from[0]) / (n + 1) * i + from[0];
		double k = (to[1] - from[1]) / (n + 1) * i + from[1];
		double l = (to[2] - from[2]) / (n + 1) * i + from[2];

		struct Engine econfig = EngineHkl(h, k, l, ModeHklBissectorVertical);

		geometries = solve(engines, econfig);

		res &= DIAG((geometries != NULL));

		hkl_geometry_list_fprintf(stdout, geometries);
		hkl_geometry_list_free(geometries);
	}

	ok(res == TRUE, __func__);

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
