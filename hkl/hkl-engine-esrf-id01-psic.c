/* COPY THIS FILE INTO hkl-engine-xxx.c and fill the XXX */
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
 * Copyright (C) 2023      ESRF - The European Synchrotron
 *						   71 Avenue des Martyrs
 *                         Grenoble
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Steven leake <leake@esrf.fr>
 *          Sebastien Petitdemange <sebastien.petitdemange@esrf.fr>
 */
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_mode_operations, etc
#include "hkl-pseudoaxis-common-psi-private.h"  // for hkl_engine_psi_new, etc
#include "hkl-pseudoaxis-common-q-private.h"  // for hkl_engine_q2_new, etc
#include "hkl-pseudoaxis-common-tth-private.h"  // for hkl_engine_tth2_new, etc
#include "hkl-pseudoaxis-common-readonly-private.h"

/**************/
/* Axes names */
/**************/

#define MU "mu"
#define ETA "eta"
#define PHI "phi"
#define NU "nu"
#define DELTA "delta"

/************/
/* Geometry */
/************/

#define HKL_GEOMETRY_PSIC_DESCRIPTION					\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **" MU "** : rotating around the :math:`-\\vec{z}` direction (0, 0, -1)\n" \
	"  + **" ETA "** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **" PHI "** : rotating around the :math:`-\\vec{z}` direction (0, 0, -1)\n" \
	"\n"								\
	"+ 2 axes for the detector\n"					\
	"\n"								\
	"  + **" NU "** : rotation around the :math:`-\\vec{z}` direction (0, 0, -1)\n" \
	"  + **" DELTA "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_PSIC_axes[] = {MU, ETA, PHI, NU, DELTA};

static HklGeometry *hkl_geometry_new_PSIC(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory, &hkl_geometry_operations_defaults);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, MU, 0, 0, -1, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, ETA, 0, -1, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, PHI, 0, 0, -1, &hkl_unit_angle_deg);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, NU, 0, 0, -1, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, DELTA, 0, -1, 0, &hkl_unit_angle_deg);

	return self;
}

/*********/
/* Modes */
/*********/


static HklMode *constant_nu_coplanar(void)
{
	static const char* axes_r[] = {MU, ETA, PHI, NU, DELTA};
	static const char* axes_w[] = {ETA, PHI, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *constant_delta_coplanar(void)
{
	static const char* axes_r[] = {MU, ETA, PHI, NU, DELTA};
	static const char* axes_w[] = {ETA, PHI, NU};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *constant_eta_noncoplanar(void)
{
	static const char* axes_r[] = {MU, ETA, PHI, NU, DELTA};
	static const char* axes_w[] = {PHI, NU, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}


/*****************/
/* mode readonly */
/*****************/

REGISTER_READONLY_INCIDENCE(hkl_engine_template_incidence_new,
			    P99_PROTECT({MU, ETA, PHI}),
			    surface_parameters_y);

REGISTER_READONLY_EMERGENCE(hkl_engine_template_emergence_new,
			    P99_PROTECT({MU, ETA, PHI, NU, DELTA}),
			    surface_parameters_y);

/***********/
/* Engines */
/***********/

static HklEngine *hkl_engine_PSIC_hkl(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = constant_nu_coplanar();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	hkl_engine_add_mode(self, constant_delta_coplanar());
	hkl_engine_add_mode(self, constant_eta_noncoplanar());

	return self;
}

/***************/
/* Engine list */
/***************/

static HklEngineList *hkl_engine_list_new_PSIC(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_PSIC_hkl(self);

	return self;
}

/* Register the diffractometer into the factory */
REGISTER_DIFFRACTOMETER(PSIC, "ESRF ID01 PSIC", HKL_GEOMETRY_PSIC_DESCRIPTION);
