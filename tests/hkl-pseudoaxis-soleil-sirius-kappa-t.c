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
#include "hkl/ccan/generator/generator.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include "hkl-geometry-private.h"
#include "hkl-trajectory-private.h"

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

static void Engine_fprintf(FILE *f, struct Engine engine)
{
	switch(engine.tag){
	case ENGINE_HKL:
	{
		fprintf(f, "hkl: %f %f %f\n", engine.hkl.h, engine.hkl.k, engine.hkl.l);
	}
	break;
	}
}

static HklGeometryList *Engine_solve(HklEngineList *engines, struct Engine econfig)
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

/* HklTrajectory */
enum trajectory_e {
	TRAJECTORY_HKL_FROM_TO,
};

struct Trajectory {
	enum trajectory_e tag;
	union {
		struct {double h0; double k0; double l0; double h1; double k1; double l1; uint n;} hklfromto;
	};
};

#define TrajectoryHklFromTo(h0_, k0_, l0_, h1_, k1_, l1_, n_) {.tag=TRAJECTORY_HKL_FROM_TO, .hklfromto={h0_, k0_, l0_, h1_, k1_, l1_, n_}}

generator_def_static(trajectory_gen, struct Engine, struct Trajectory, tconfig)
{
	switch(tconfig.tag){
	case TRAJECTORY_HKL_FROM_TO:
	{
		uint i;
		double dh = (tconfig.hklfromto.h1 - tconfig.hklfromto.h0) / (tconfig.hklfromto.n - 1);
		double dk = (tconfig.hklfromto.k1 - tconfig.hklfromto.k0) / (tconfig.hklfromto.n - 1);
		double dl = (tconfig.hklfromto.l1 - tconfig.hklfromto.l0) / (tconfig.hklfromto.n - 1);
		for(i=0; i<tconfig.hklfromto.n; ++i){
			double h = i * dh + tconfig.hklfromto.h0;
			double k = i * dk + tconfig.hklfromto.k0;
			double l = i * dl + tconfig.hklfromto.l0;

			struct Engine econfig = EngineHkl(h, k, l, ModeHklBissectorVertical);
			generator_yield(econfig);
		}
	}
	break;
	}
}

static uint Trajectory_len(struct Trajectory tconfig)
{
	uint n = 0;
	switch(tconfig.tag){
	case TRAJECTORY_HKL_FROM_TO:
	{
		n = tconfig.hklfromto.n;
	}
	break;
	}
}

static HklGeometryList *Trajectory_solve(struct Trajectory tconfig, struct Geometry gconfig, struct Sample sconfig)
{
	const struct Engine *econfig;
	HklGeometryList *solutions = hkl_geometry_list_new();
	generator_t(struct Engine) gen = trajectory_gen(tconfig);

	HklGeometry *geometry = newGeometry(gconfig);
	HklEngineList *engines = newEngines(gconfig);
	HklSample *sample = newSample(sconfig);
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HklTrajectoryStats *stats = hkl_trajectory_stats_new(Trajectory_len(tconfig));

	hkl_engine_list_init(engines, geometry, detector, sample);

	while((econfig = generator_next(gen)) != NULL){
		const HklGeometryListItem *solution;

		Engine_fprintf(stdout, *econfig);

		HklGeometryList *geometries = Engine_solve(engines, *econfig);
		hkl_trajectory_stats_add(stats, geometries);
		solution = hkl_geometry_list_items_first_get(geometries);
		hkl_engine_list_select_solution(engines, solution);

		hkl_geometry_list_add(solutions,
				      hkl_geometry_list_item_geometry_get(solution));
		/* hkl_geometry_list_fprintf(stdout, geometries); */
		hkl_geometry_list_free(geometries);
	}

	hkl_trajectory_stats_fprintf(stdout, stats);

	hkl_trajectory_stats_free(stats);
	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);

	return solutions;
}

/* tests */

static void stability(void)
{
	int i;
	int res = TRUE;
	HklGeometryList *solutions;

	static struct Sample gaas = {
		.name = "GaAs",
		.lattice = Cubic(5.6533),
		.ux = -90.003382 * HKL_DEGTORAD,
		.uy = 0.12907 * HKL_DEGTORAD,
		.uz = -159.91372 * HKL_DEGTORAD,
	};

	static struct Geometry gconfig =	\
		SoleilSiriusKappa(1.458637,
				  -0.5193202, 64.7853160, 133.5621380, -80.9690000, -0.0223369, 30.0000299);

	static struct Trajectory tconfig = TrajectoryHklFromTo(0, 0, 1, 0, 0, 6, 101);

	solutions = Trajectory_solve(tconfig, gconfig, gaas);
	res &= DIAG(NULL != solutions);

	ok(res == TRUE, __func__);

	hkl_geometry_list_free(solutions);
}

int main(void)
{
	plan(1);

	stability();

	return 0;
}
