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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
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

#define GET_AXIS(_geometries, _name) hkl_parameter_value_get(		\
		hkl_geometry_axis_get(					\
			hkl_geometry_list_item_geometry_get(		\
				hkl_geometry_list_items_first_get((geometries))), \
			#_name, NULL), HKL_UNIT_USER)

#define GET_GAMMA(geometries) hkl_parameter_value_get(			\
		hkl_geometry_axis_get(					\
			hkl_geometry_list_item_geometry_get(		\
				hkl_geometry_list_items_first_get((geometries))), \
			"gamma", NULL), HKL_UNIT_USER)

/* tests */

static void qper_qpar(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	double qper_qpar[2];
	double gamma;
	HklGeometryList *geometries;

	static struct Sample gaas = {
		.name = "test",
		.lattice=Cubic(1.54),
		.ux = -90.0 * HKL_DEGTORAD,
		.uy = 0.0 * HKL_DEGTORAD,
		.uz = 0.0 * HKL_DEGTORAD,
	};

	struct Geometry gconfig = SoleilSixsMed2_3(1.54, 0., 0.1, 0., 0., 90., 0.);

	geometry = newGeometry(gconfig);
	engines = newEngines(gconfig);
	sample = newSample(gaas);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "qper_qpar", NULL);

	/* the init part */
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	/* gamma must be positif */
	qper_qpar[0] = 0.1;
	qper_qpar[1] = 4.;
	geometries = hkl_engine_pseudo_axis_values_set(engine, qper_qpar, ARRAY_SIZE(qper_qpar),
						       HKL_UNIT_DEFAULT, NULL);
	if(geometries){
		gamma = GET_GAMMA(geometries);
		is_double(2.61077, gamma, HKL_EPSILON * 10, __func__);
		hkl_geometry_list_free(geometries);
	}

	/* gamma must be negatif */
	qper_qpar[0] = -0.1;
	qper_qpar[1] = 4.;
	geometries = hkl_engine_pseudo_axis_values_set(engine, qper_qpar, ARRAY_SIZE(qper_qpar),
						       HKL_UNIT_DEFAULT, NULL);
	if(geometries){
		gamma = GET_GAMMA(geometries);
		is_double(-2.7956354, gamma, HKL_EPSILON * 10, __func__);
		hkl_geometry_list_free(geometries);
	}

	ok(res, __func__);

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void med_2_3(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *hkl;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double hkl_p[] = {1.95, 2, 6};

	static struct Sample gaas = {
		.name = "GaAs",
		.lattice = Hexagonal(4.759, 12.992),
		.ux = -90.59 * HKL_DEGTORAD,
		.uy = -9.97 * HKL_DEGTORAD,
		.uz = 176.35 * HKL_DEGTORAD,
	};

	static struct Geometry gconfig = SoleilSixsMed2_3(1.54980,
							  0, 1, -14.27, 99.62, 60.98, 0);

	/* Wavelength 1.54980 */
	/* Mode       mu_fixed */
	/* Ux -90.59 Uy -9.97 Uz 176.35  */
	/* A 4.759 B 4.759 C 12.992  */
	/* Alpha 90 Beta 90 Gamma 120  */

	geometry = newGeometry(gconfig);
	engines = newEngines(gconfig);
	sample = newSample(gaas);
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	hkl_engine_list_init(engines, geometry, detector, sample);

	hkl = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
	res &= DIAG(hkl_engine_current_mode_set(hkl, "mu_fixed", NULL));

	/* hkl 1.95, 2, 6 (should not fail) */
	geometries = hkl_engine_pseudo_axis_values_set(hkl,
						       hkl_p, ARRAY_SIZE(hkl_p),
						       HKL_UNIT_DEFAULT, NULL);
	res &= DIAG((geometries != NULL));
	hkl_geometry_list_free(geometries);

	ok(res == TRUE, __func__);

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void med_2_3_v2(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *hkl;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double hkl_p[] = {1.95, 2, 6};
	static double values[] = {1};

	static struct Sample gaas = {
		.name = "GaAs",
		.lattice = Hexagonal(4.759, 12.992),
		.ux = -90.59 * HKL_DEGTORAD,
		.uy = -9.97 * HKL_DEGTORAD,
		.uz = 176.35 * HKL_DEGTORAD,
	};

	static struct Geometry gconfig = SoleilSixsMed2_3_v2(1.54980,
							     1, -14.27, 99.62, 60.98, 10);

	/* Wavelength 1.54980 */
	/* Mode       mu_fixed */
	/* Ux -90.59 Uy -9.97 Uz 176.35  */
	/* A 4.759 B 4.759 C 12.992  */
	/* Alpha 90 Beta 90 Gamma 120  */

	geometry = newGeometry(gconfig);
	engines = newEngines(gconfig);
	sample = newSample(gaas);
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	hkl_engine_list_init(engines, geometry, detector, sample);

	hkl = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);

	/* hkl 1.95, 2, 6 (should not fail) */
	geometries = hkl_engine_pseudo_axis_values_set(hkl,
						       hkl_p, ARRAY_SIZE(hkl_p),
						       HKL_UNIT_DEFAULT, NULL);
	res &= DIAG((geometries != NULL));

	/* check that eta_a did not moved */
	is_double(10.0, GET_AXIS(geometries, eta_a), HKL_EPSILON * 10, __func__);
	hkl_geometry_list_free(geometries);


	/* now make it move */
	res &= DIAG(hkl_engine_list_parameters_values_set(engines,
							  values, ARRAY_SIZE(values),
							  HKL_UNIT_USER, NULL));
	geometries = hkl_engine_pseudo_axis_values_set(hkl,
						       hkl_p, ARRAY_SIZE(hkl_p),
						       HKL_UNIT_DEFAULT, NULL);
	is_double(-80.079231, GET_AXIS(geometries, eta_a), HKL_EPSILON * 10, __func__);
	hkl_geometry_list_free(geometries);

	ok(res == TRUE, __func__);

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}


int main(void)
{
	plan(7);

	qper_qpar();
	med_2_3();
	med_2_3_v2();

	return 0;
}
