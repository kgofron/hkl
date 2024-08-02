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
 * Copyright (C) 2023-2024  UChicago Argonne, LLC
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Pete R. Jemian <jemian@anl.gov>
 */

#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_mode_operations, etc
#include "hkl-pseudoaxis-common-psi-private.h"  // for hkl_engine_psi_new, etc

/**************/
/* Axes names */
/**************/

#define TAU "tau"
#define MU "mu"
#define CHI "chi"
#define PHI "phi"
#define GAMMA "gamma"
#define DELTA "delta"

/************/
/* Geometry */
/************/

#define HKL_GEOMETRY_TYPE_APS_POLAR_DESCRIPTION                                      \
	"+ Incoming X-rays source along the :math:`\\vec{x}` direction (1, 0, 0)\n"      \
	"+ 4 axes for the sample\n"                                                      \
	"\n"                                                                             \
	"  + **" TAU "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **" MU "** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n"    \
	"  + **" CHI "** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n"   \
	"  + **" PHI "** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n"   \
	"\n"                                                                             \
	"+ 3 axis for the detector\n"                                                    \
	"\n"                                                                             \
	"  + **" TAU "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **" GAMMA "** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **" DELTA "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char *hkl_geometry_aps_polar_axes[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};

static HklGeometry *hkl_geometry_new_aps_polar(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory, &hkl_geometry_operations_defaults);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, TAU, 0, -1, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, MU, 0, 0, 1, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, CHI, 1, 0, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, PHI, 0, 0, 1, &hkl_unit_angle_deg);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, TAU, 0, -1, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, GAMMA, 0, 0, 1, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, DELTA, 0, -1, 0, &hkl_unit_angle_deg);

	return self;
}

/**********************/
/* Modes (hkl engine) */
/**********************/

static HklMode *zaxis_alpha_fixed()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {MU, GAMMA, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("zaxis + alpha-fixed", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

static HklMode *zaxis_beta_fixed()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {TAU, GAMMA, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("zaxis + beta-fixed", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

/* zaxis + alpha=beta */

static int _reflectivity(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double tau = x->data[0];
	const double delta = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = tau - delta;

	return GSL_SUCCESS;
}

static const HklFunction reflectivity = {
	.function = _reflectivity,
	.size = 4,
};

static HklMode *zaxis_alpha_eq_beta()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {TAU, MU, GAMMA, DELTA};
	static const HklFunction *functions[] = {&reflectivity};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("zaxis + alpha=beta", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

/* 4-circles bissecting horizontal */

static int _bissector_horizontal(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double mu = x->data[0];
	const double gamma = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = gamma - 2 * fmod(mu, M_PI);

	return GSL_SUCCESS;
}

static const HklFunction bissector_horizontal = {
	.function = _bissector_horizontal,
	.size = 4,
};

static HklMode *fourc_bissector_horizontal()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {MU, CHI, PHI, GAMMA};
	static const HklFunction *functions[] = {&bissector_horizontal};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("4-circles bissecting horizontal", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

static HklMode *fourc_constant_mu_horizontal()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {CHI, PHI, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("4-circles constant mu horizontal", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

static HklMode *fourc_constant_chi_horizontal()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {MU, PHI, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("4-circles constant chi horizontal", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

static HklMode *fourc_constant_phi_horizontal()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {MU, CHI, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("4-circles constant phi horizontal", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

static HklMode *lifting_detector_tau()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {TAU, GAMMA, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector mu", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

static HklMode *lifting_detector_mu()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {MU, GAMMA, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector mu", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

static HklMode *lifting_detector_chi()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {CHI, GAMMA, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector chi", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

static HklMode *lifting_detector_phi()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {PHI, GAMMA, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector phi", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
							 &hkl_full_mode_operations,
							 TRUE);
}

static HklMode *psi_constant_horizontal(void)
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {MU, CHI, PHI, GAMMA};
	static const HklFunction *functions[] = {&psi_constant_vertical_func};
	static const HklParameter parameters[] = {PSI_CONSTANT_PARAMETERS(1, 0, 0, 0)};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(
			"psi constant horizontal", axes_r, axes_w,
			functions, psi_constant_parameters),
	};

	return hkl_mode_auto_new(&info,
							 &psi_constant_vertical_mode_operations,
							 TRUE);
}

static HklMode *psi_constant_vertical(void)
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {TAU, CHI, PHI, DELTA};
	static const HklFunction *functions[] = {&psi_constant_vertical_func};
	static const HklParameter parameters[] = {PSI_CONSTANT_PARAMETERS(1, 0, 0, 0)};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(
			"psi constant vertical", axes_r, axes_w,
			functions, psi_constant_parameters),
	};

	return hkl_mode_auto_new(
		&info,
		&psi_constant_vertical_mode_operations,
		TRUE);
}

/**********************/
/* Modes (psi engine) */
/**********************/

static HklMode* psi_vertical()
{
	static const char *axes_r[] = {TAU, MU, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {MU, CHI, PHI, DELTA};
	static const HklFunction *functions[] = {&psi_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes_r, axes_w,
					       functions, psi_parameters),
	};

	return hkl_mode_psi_new(&info);
}

/*****************/
/* mode readonly */
/*****************/

// none

/***********/
/* Engines */
/***********/

static HklEngine *hkl_engine_aps_polar_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = fourc_constant_phi_horizontal();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	hkl_engine_add_mode(self, zaxis_alpha_fixed());
	hkl_engine_add_mode(self, zaxis_beta_fixed());
	hkl_engine_add_mode(self, zaxis_alpha_eq_beta());
	hkl_engine_add_mode(self, fourc_bissector_horizontal());
	hkl_engine_add_mode(self, fourc_constant_mu_horizontal());
	hkl_engine_add_mode(self, fourc_constant_chi_horizontal());

	hkl_engine_add_mode(self, lifting_detector_tau());
	hkl_engine_add_mode(self, lifting_detector_mu());
	hkl_engine_add_mode(self, lifting_detector_chi());
	hkl_engine_add_mode(self, lifting_detector_phi());

	hkl_engine_add_mode(self, psi_constant_horizontal());
	hkl_engine_add_mode(self, psi_constant_vertical());

	return self;
}

static HklEngine *hkl_engine_aps_polar_psi_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_psi_new(engines);

	default_mode = psi_vertical();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}

/***************/
/* Engine list */
/***************/

static HklEngineList *hkl_engine_list_new_aps_polar(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_aps_polar_hkl_new(self);
	hkl_engine_aps_polar_psi_new(self);

	return self;
}

REGISTER_DIFFRACTOMETER(aps_polar, "APS POLAR", HKL_GEOMETRY_TYPE_APS_POLAR_DESCRIPTION);
