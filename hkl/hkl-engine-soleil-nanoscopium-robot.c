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
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-common-hkl-private.h"

#define RZ "rz"
#define RS "rs"
#define RX "rx"
#define R "r"
#define DELTA "delta"
#define GAMMA "gamma"

/********/
/* mode */
/********/

static HklMode *lifting_detector_rz()
{
	static const char *axes_r[] = {RZ, RS, RX, R, DELTA, GAMMA};
	static const char *axes_w[] = {RZ, DELTA, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector rz", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *lifting_detector_rs()
{
	static const char *axes_r[] = {RZ, RS, RX, R, DELTA, GAMMA};
	static const char *axes_w[] = {RS, DELTA, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector rs", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *lifting_detector_rx()
{
	static const char *axes_r[] = {RZ, RS, RX, R, DELTA, GAMMA};
	static const char *axes_w[] = {RX, DELTA, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector rx", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

/**********************/
/* pseudo axis engine */
/**********************/

static HklEngine *hkl_engine_soleil_nanoscopium_robot_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = lifting_detector_rz();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	hkl_engine_add_mode(self, lifting_detector_rs());
	hkl_engine_add_mode(self, lifting_detector_rx());

	return self;
}

/****************************/
/* SOLEIL NANOSCOPIUM ROBOT */
/****************************/

#define HKL_GEOMETRY_TYPE_SOLEIL_NANOSCOPIUM_ROBOT_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **" RZ "** : rotation around the :math:`-\\vec{z}` direction (0, 0, -1)\n" \
	"  + **" RS "** : rotation around the :math:`\\vec{y}` direction (0, 1, 0)\n" \
	"  + **" RX "** : rotating around the :math:`-\\vec{x}` direction (-1, 0, 0)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **" R "** : radius of the sphere\n"			\
	"  + **" DELTA "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **" GAMMA "** : rotation around the :math:`-\\vec{z}` direction (0, 0, -1)\n"

        static const char* hkl_geometry_soleil_nanoscopium_robot_axes[] = {RZ, RS, RX, R, DELTA, GAMMA};

static HklGeometry *hkl_geometry_new_soleil_nanoscopium_robot(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory, &hkl_geometry_operations_defaults);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, RZ, 0, 0, -1, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, RS, 0, 1, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, RX, -1, 0, 0, &hkl_unit_angle_deg);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_parameter(h, R, &hkl_unit_length_meter);
	hkl_holder_add_rotation(h, DELTA, 0, -1, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, GAMMA, 0, 0, -1, &hkl_unit_angle_deg);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_nanoscopium_robot(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_soleil_nanoscopium_robot_hkl_new(self);

	return self;
}

REGISTER_DIFFRACTOMETER(soleil_nanoscopium_robot, "SOLEIL NANOSCOPIUM ROBOT", HKL_GEOMETRY_TYPE_SOLEIL_NANOSCOPIUM_ROBOT_DESCRIPTION);
