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
 * Copyright (C)      2018 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_mode_operations, etc
#include "hkl-pseudoaxis-common-psi-private.h"  // for hkl_engine_psi_new, etc
#include "hkl-pseudoaxis-common-q-private.h"  // for hkl_engine_q2_new, etc
#include "hkl-pseudoaxis-common-tth-private.h"  // for hkl_engine_tth2_new, etc
#include "hkl-pseudoaxis-common-readonly-private.h"  // for hkl_engine_tth2_new, etc

#define OMEGA_T "omega_t"
#define MU "mu"
#define GAMMA "gamma"
#define DELTA "delta"

/************/
/* mode hkl */
/************/

static HklMode *bissector_vertical(void)
{
	static const char* axes_r[] = {OMEGA_T, MU, GAMMA, DELTA};
	static const char* axes_w[] = {OMEGA_T, MU, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *lifting_detector_omega_t(void)
{
	static const char* axes_r[] = {OMEGA_T, MU, GAMMA, DELTA};
	static const char* axes_w[] = {OMEGA_T, GAMMA, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *lifting_detector_mu(void)
{
	static const char* axes_r[] = {OMEGA_T, MU, GAMMA, DELTA};
	static const char* axes_w[] = {MU, GAMMA, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *bissector_horizontal(void)
{
	static const char* axes_r[] = {OMEGA_T, MU, GAMMA, DELTA};
	static const char* axes_w[] = {OMEGA_T, MU, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *psi_constant(void)
{
	static const char* axes_r[] = {OMEGA_T, MU, GAMMA, DELTA};
	static const char* axes_w[] = {OMEGA_T, MU, GAMMA, DELTA};
	static const HklFunction *functions[] = {&psi_constant_vertical_func};
	static const HklParameter parameters[] = { PSI_CONSTANT_PARAMETERS(1, 0, 0, 0) };
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes_r, axes_w, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &psi_constant_vertical_mode_operations,
				 TRUE);
}

static HklEngine *hkl_engine_petra3_p23_4c_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = bissector_vertical();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	hkl_engine_add_mode(self, lifting_detector_omega_t());
	hkl_engine_add_mode(self, lifting_detector_mu());
	hkl_engine_add_mode(self, bissector_horizontal());
	hkl_engine_add_mode(self, psi_constant());

	return self;
}

/*****************/
/* mode readonly */
/*****************/

REGISTER_READONLY_INCIDENCE(hkl_engine_petra3_p23_4c_incidence_new,
			    P99_PROTECT({OMEGA_T, MU}),
			    surface_parameters_z);

REGISTER_READONLY_EMERGENCE(hkl_engine_petra3_p23_4c_emergence_new,
			    P99_PROTECT({OMEGA_T, MU, GAMMA, DELTA}),
			    surface_parameters_z);

/*******/
/* E6C */
/*******/

#define HKL_GEOMETRY_PETRA3_P23_4C_DESCRIPTION				\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 2 axes for the sample\n"					\
	"\n"								\
	"  + **" OMEGA_T "** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **" MU "** : rotating around the :math:`-\\vec{z}` direction (0, 0, -1)\n" \
	"\n"								\
	"+ 2 axes for the detector\n"					\
	"\n"								\
	"  + **" GAMMA "** : rotation around the :math:`-\\vec{z}` direction (0, 0, -1)\n" \
	"  + **" DELTA "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_petra3_p23_4c_axes[] = {OMEGA_T, MU, GAMMA, DELTA};

static HklGeometry *hkl_geometry_new_petra3_p23_4c(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory, &hkl_geometry_operations_defaults);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, OMEGA_T, 0, -1, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, MU, 0, 0, -1, &hkl_unit_angle_deg);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, GAMMA, 0, 0, -1, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, DELTA, 0, -1, 0, &hkl_unit_angle_deg);

	return self;
}

static HklEngineList *hkl_engine_list_new_petra3_p23_4c(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_petra3_p23_4c_hkl_new(self);
	hkl_engine_q2_new(self);
	hkl_engine_qper_qpar_new(self);
	hkl_engine_tth2_new(self);
	hkl_engine_petra3_p23_4c_incidence_new(self);
	hkl_engine_petra3_p23_4c_emergence_new(self);

	return self;
}

REGISTER_DIFFRACTOMETER(petra3_p23_4c, "PETRA3 P23 4C", HKL_GEOMETRY_PETRA3_P23_4C_DESCRIPTION);
