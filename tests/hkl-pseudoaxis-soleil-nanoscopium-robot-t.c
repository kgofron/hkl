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
 * Copyright (C) 2003-2021 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

static void solution(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {1, 0, 1};
	struct Sample sconf = {
		.name = "test",
		.lattice = Cubic(5.432),
		.ux = -90.0 * HKL_DEGTORAD,
		.uy = 0.0  * HKL_DEGTORAD,
		.uz = 0.0  * HKL_DEGTORAD,
	};
	struct Geometry gconf = SoleilNanoscopiumRobot(0.842, 0., 0., 0., 0., 0., 0.);

	geometry = newGeometry(gconf);
	engines = newEngines(gconf);
	sample = newSample(sconf);

	/* use a 0D detector */
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	/* select the hkl pseudo axis */
	hkl_engine_list_init(engines, geometry, detector, sample);
	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);

	/* compute the 1 1 0 */
	geometries = hkl_engine_pseudo_axis_values_set(engine, hkl, ARRAY_SIZE(hkl),
						       HKL_UNIT_DEFAULT, NULL);

	if (geometries){
		const HklGeometryListItem *item;

		HKL_GEOMETRY_LIST_FOREACH(item, geometries){
			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(item));
			res &= DIAG(check_pseudoaxes(engine, hkl, 3));
		}
		hkl_geometry_list_free(geometries);
	}else
		res = FALSE;

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);

	ok(res == TRUE, "solution");
}

int main(void)
{
	plan(1);

	solution();

	return 0;
}
