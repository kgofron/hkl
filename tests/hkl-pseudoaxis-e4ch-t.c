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
 *          Jens Krüger <jens.krueger@frm.tum.de>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

#include "hkl/ccan/generator/generator.h"
#include "hkl-geometry-private.h"
#include "hkl-trajectory-private.h"

static void getter(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	struct Geometry gconf = E4ch(1.54, 0., 0., 0., 0.);

	geometry = newGeometry(gconf);
	engines = newEngines(gconf);
	sample = newSample(cu);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);

	/* geometry -> pseudo */
	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 0., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, 0., 1., 0.));

	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 90., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, 1., 0., 0.));

	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., -90., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, -1., 0., 0.));

	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 180., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, 0., -1., 0.));

	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 45., 0., 135., 90.));
	res &= DIAG(check_pseudoaxes_v(engine, 1., -1., 0.));

	ok(res == TRUE, "getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void degenerated(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const char **mode;
	const darray_string *modes;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	struct Geometry gconf = E4ch(1.54, 0., 0., 0., 0.);

	geometry = newGeometry(gconf);
	engines = newEngines(gconf);
	sample = newSample(cu);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	hkl_engine_list_init(engines, geometry, detector, sample);
	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
	modes = hkl_engine_modes_names_get(engine);

	darray_foreach(mode, *modes){
		static double values[] = {0, 0, 1};
		const darray_string *parameters;
		HklGeometryList *geometries;
		size_t n_params;

		res &= DIAG(hkl_engine_current_mode_set(engine, *mode, NULL));
		parameters = hkl_engine_parameters_names_get(engine);
		n_params = darray_size(*parameters);
		if (n_params){
			double params[n_params];

			hkl_engine_parameters_values_get(engine, params, n_params, HKL_UNIT_DEFAULT);
			values[0] = 0;
			res &= DIAG(hkl_engine_parameters_values_set(engine, params, n_params, HKL_UNIT_DEFAULT, NULL));
		}

		/* studdy this degenerated case */
		geometries = hkl_engine_pseudo_axis_values_set(engine, values, ARRAY_SIZE(values),
							       HKL_UNIT_DEFAULT, NULL);
		if(geometries){
			const HklGeometryListItem *item;

			HKL_GEOMETRY_LIST_FOREACH(item, geometries){
				hkl_geometry_set(geometry,
						 hkl_geometry_list_item_geometry_get(item));
				res &= DIAG(check_pseudoaxes(engine, values, 3));
			}
			hkl_geometry_list_free(geometries);
		}
	}

	ok(res == TRUE, "degenerated");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void psi_getter(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	double hkl[3];
	struct Geometry gconf = E4ch(1.54, 30., 0., 0., 60.);

	geometry = newGeometry(gconf);
	engines = newEngines(gconf);
	sample = newSample(cu);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "psi", NULL);

	/* the getter part */
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	hkl[0] = 1, hkl[1] = 0, hkl[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl),
						     HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(check_pseudoaxes_v(engine, 0.));

	/* here Q and <h, k, l>_ref are colinear must FAIL */
	hkl[0] = 0, hkl[1] = 1, hkl[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl),
						     HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(!check_pseudoaxes_v(engine, 0.));

	hkl[0] = -1, hkl[1] = 0, hkl[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl),
						     HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(check_pseudoaxes_v(engine, 180. * HKL_DEGTORAD));

	hkl[0] = 0, hkl[1] = 0, hkl[2] = -1;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl),
						     HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(check_pseudoaxes_v(engine, 90. * HKL_DEGTORAD));

	/* Q and <h, k, l>_ref are colinear so must FAIL */
	hkl[0] = 0, hkl[1] = -1, hkl[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl),
						     HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(!check_pseudoaxes_v(engine, 0.));

	ok(res == TRUE, "psi getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void psi_setter(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const darray_string *modes;
	const char **mode;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {1, 0, 0};
	struct Geometry gconf = E4ch(1.54, 30., 0., 0., 60.);

	geometry = newGeometry(gconf);
	engines = newEngines(gconf);
	sample = newSample(cu);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "psi", NULL);
	modes = hkl_engine_modes_names_get(engine);

	/* the init part */
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl),
						     HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	darray_foreach(mode, *modes){
		double psi;

		res &= DIAG(hkl_engine_current_mode_set(engine, *mode, NULL));
		for(psi=-180 * HKL_DEGTORAD;psi<180 * HKL_DEGTORAD;psi += HKL_DEGTORAD){
			HklGeometryList *geometries;

			geometries = hkl_engine_pseudo_axis_values_set(engine, &psi, 1,
								       HKL_UNIT_DEFAULT, NULL);
			if(geometries){
				const HklGeometryListItem *item;

				HKL_GEOMETRY_LIST_FOREACH(item, geometries){
					hkl_geometry_set(geometry,
							 hkl_geometry_list_item_geometry_get(item));
					res &= DIAG(check_pseudoaxes_v(engine, psi));
				}
				hkl_geometry_list_free(geometries);
			}
		}
	}

	ok(res == TRUE, "psi setter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}


static void q(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const darray_string *modes;
	const char **mode;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	struct Geometry gconf = E4ch(1.54, 30., 0., 0., 60.);

	geometry = newGeometry(gconf);
	engines = newEngines(gconf);
	sample = newSample(cu);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "q", NULL);
	modes = hkl_engine_modes_names_get(engine);

	/* the init part */
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	darray_foreach(mode, *modes){
		double q;

		res &= DIAG(hkl_engine_current_mode_set(engine, *mode, NULL));
		for(q=-1.; q<1.; q += 0.1){
			HklGeometryList *geometries;

			geometries = hkl_engine_pseudo_axis_values_set(engine, &q, 1,
								       HKL_UNIT_DEFAULT, NULL);

			if(geometries){
				const HklGeometryListItem *item;

				HKL_GEOMETRY_LIST_FOREACH(item, geometries){
					hkl_geometry_set(geometry,
							 hkl_geometry_list_item_geometry_get(item));
					res &= DIAG(check_pseudoaxes(engine, &q, 1));
				}
				hkl_geometry_list_free(geometries);
			}
		}
	}

	ok(res == TRUE, "q");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void hkl_psi_constant_horizontal(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {1, 0, 1};
	static double hkl2[] = {1, 1, 0};
	struct Geometry gconf = E4ch(1.54, 30., 0., 0., 60.);

	geometry = newGeometry(gconf);
	engines = newEngines(gconf);
	sample = newSample(cu);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);

	res &= DIAG(hkl_engine_current_mode_set(engine, "psi_constant", NULL));

	/* the init part */
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl2, ARRAY_SIZE(hkl2),
						     HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	geometries = hkl_engine_pseudo_axis_values_set(engine,
						       hkl, ARRAY_SIZE(hkl),
						       HKL_UNIT_DEFAULT, NULL);
	if(geometries){
		const HklGeometryListItem *item;

		HKL_GEOMETRY_LIST_FOREACH(item, geometries){
			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(item));
			res &= DIAG(check_pseudoaxes(engine, hkl, ARRAY_SIZE(hkl)));
		}
		hkl_geometry_list_free(geometries);
	}

	ok(res == TRUE, "psi constant horizontal");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void petra3_p01(void)
{
	int res = TRUE;
	HklGeometryList *solutions;

	struct Sample sample = {
		.name = "Sample",
		.lattice = Tetragonal(5.4, 11.9),
		.ux = -90 * HKL_DEGTORAD,
		.uy = 9 * HKL_DEGTORAD,
		.uz = -45 * HKL_DEGTORAD,
	};

	struct Geometry gconfig = E4ch(4.3687,
				       0., 0., 6., 0.);

	struct Mode mode = ModeHklE4CHConstantPhi;

	/* move between each step */
	struct Trajectory tconfig1 = TrajectoryHklFromTo(0, 0, 4, 1, 1, 4, 30, mode);
	solutions = Trajectory_solve(tconfig1, gconfig, sample, TRUE);
	res &= DIAG(NULL != solutions);
	hkl_geometry_list_free(solutions);

	ok(res == TRUE, __func__);
}

int main(void)
{
	plan(7);

	getter();
	degenerated();
	psi_getter();
	psi_setter();
	q();
	hkl_psi_constant_horizontal();
	petra3_p01();

	return 0;
}
