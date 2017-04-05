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
 * Copyright (C) 2017      DESY
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Teresa Núñez <tnunez@mail.desy.de>
 */
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_mode_operations, etc
#include "hkl-pseudoaxis-common-psi-private.h"  // for hkl_engine_psi_new, etc
#include "hkl-pseudoaxis-common-q-private.h"  // for hkl_engine_q2_new, etc
#include "hkl-pseudoaxis-common-tth-private.h"  // for hkl_engine_tth2_new, etc
#include "hkl-pseudoaxis-common-readonly-private.h"

/***********************

    Description
         
There are 5 real motors:

mchi  (monochromator rotation)  -> rotation y axis +1
sphi  (sample rotation)         -> rotation z axis +1
dtth  (detector table rotation) -> rotation z axis -1
dh    (detector height)         -> traslation z axis +1
drot  (detector rotation)       -> rotation x axis +1

There are 4 pseudo motors related to them:

alpha   (incident angle - kin with xy plane)
beta    (outgoing angle - kout with xy plane)
sth      (angle between the kin projection in the xy plane and the y axis)
stth     (angle between kin and kout projections in the xy plane)

sth and stth has to be independently as pseudomotors.

For a given energy, E, one can compute mchi, sphi, dtth, dh and drot from
alpha, beta, th and tth and viceversa.
The solutions are not unique, for fixing them one has to set:

 - Detector rotation tracking -> the detector table rotation, dtth, follows the monochromator
 rotation, mchi, so that the position of stth stays constant. For stth = 0 the detector is in
 diffraction plane.

 - mchi rotation direction -> if set to '+' the mchi angle goes from 0 to pi, set to '-' it goes from
 0 to -pi.

 - allow rotation sample -> if 'yes' the sample will be rotated with the monochromator, so that
   sth will stay constant. If 'no' the sample will not be rotated.

The following geometry constants have to be set:

  - distance sample center to detector
  - distance first crystal to sample center
  - distance first crystal to second one.
  - first crystal diffraction parameter
  - second crystal diffraction parameter

For a alpha, beta, th and tth set there are different possibilities of choosing the position
of the real motors for getting a given h,k,l. Four possible geometry modes have to be implemented,
each mode gives an unique solution for the real computers computed from the angles.
These modes are:

  - mode 0 -> K axis from reciprocal space parallel to incident beam.
  - mode 1 -> alpha = beta
  - mode 2 -> th = tth/2
  - mode 3 -> K axis from reciprocal space parallel to outgoing beam.

There are some corrections (motor movements) that have to be done if the energy is changed,
in order to correct the shifts in the position of the beam on the sample.

*******************/


/**************/
/* Axes names */
/**************/

#define SPHI "sphi"
#define DTTH "dtth"
#define DH   "dh"
#define DROT "drot"
#define MCHI "mchi"

/************/
/* Geometry */
/************/

#define HKL_GEOMETRY_PETRA3_P08_LISA_DESCRIPTION \
  "+ xrays source fix along the :math:`\\vec{x}` direction (1, 0, 0)\n"   \		  
  "+ 1 axes for the monochromator\n"					\
  "\n"									\
  "  + **" MCHI "** : rotation around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
  "\n"									\
  "+ 1 axis for the sample\n"						\
  "\n"									\
  "  + **" SPHI "** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
  "\n"									\
  "+ 2 axes for the detector\n"						\
  "\n"									\
  "  + **" DTTH "** : rotation around the :math:`-\\vec{z}` direction (0, 0, -1)\n" \
  "  + **" DROT "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
  "  + **" DH   "** : traslation along the :math:`\\vec{z}` direction (0, 0, 1)\n" \
  "\n"

static const char* hkl_geometry_petra3_p08_lisa_axes[] = {MCHI, SPHI, DTTH, DH, DROT};

static HklGeometry *hkl_geometry_new_petra3_p08_lisa(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;
	
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, MCHI, 1, 0, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, SPHI, 0, 0, 1);
	
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, DTTH, 0, 0, -1);
	hkl_holder_add_rotation_axis(h, DROT, 0, -1, 0);
	//hkl_holder_add_translation_axis(h, DH, 0, 0, 1)

	return self;
}

/*********/
/* Modes */
/********/



static HklMode *k_parallel_incident(void)
{
  /* TODO: check if the axes are correct, add the functions and implement them */
  
  static const char* axes_r[] = {MCHI, SPHI, DTTH, DROT}; 

  static const char* axes_w[] = {MCHI, SPHI, DTTH, DROT};
  
  static const HklFunction *functions[] = {};

  /* here just the description of the mode: name, axes_r, axes_w, functions */
  static const HklModeAutoInfo info = {
    HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
  };

  /* instantiate a new mode */
  return hkl_mode_auto_new(&info,
			   &hkl_mode_operations,
			   TRUE);
  
}

static HklMode *alpha_eq_beta(void)
{
  /* TODO: check if the axes are correct, add the functions and implement them */
  
  static const char* axes_r[] = {MCHI, SPHI, DTTH, DROT};

  static const char* axes_w[] = {MCHI, SPHI, DTTH, DROT};
  
  static const HklFunction *functions[] = {};

  /* here just the description of the mode: name, axes_r, axes_w, functions */
  static const HklModeAutoInfo info = {
    HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
  };

  /* instantiate a new mode */
  return hkl_mode_auto_new(&info,
			   &hkl_mode_operations,
			   TRUE);
}
static HklMode *th_eq_tth2(void)
{
  /* TODO: check if the axes are correct, add the functions and implement them */
  
  static const char* axes_r[] = {MCHI, SPHI, DTTH, DROT};

  static const char* axes_w[] = {MCHI, SPHI, DTTH, DROT};
  
  static const HklFunction *functions[] = {};

  /* here just the description of the mode: name, axes_r, axes_w, functions */
  static const HklModeAutoInfo info = {
    HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
  };

  /* instantiate a new mode */
  return hkl_mode_auto_new(&info,
			   &hkl_mode_operations,
			   TRUE);
}
static HklMode *k_parallel_outgoing(void)
{
  /* TODO: check if the axes are correct, add the functions and implement them */
  
  static const char* axes_r[] = {MCHI, SPHI, DTTH, DROT};

  static const char* axes_w[] = {MCHI, SPHI, DTTH, DROT};
  
  static const HklFunction *functions[] = {};

  /* here just the description of the mode: name, axes_r, axes_w, functions */
  static const HklModeAutoInfo info = {
    HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
  };

  /* instantiate a new mode */
  return hkl_mode_auto_new(&info,
			   &hkl_mode_operations,
			   TRUE);
}

/* mode_lisa_pm for LisaPM Engine */


static const HklParameter mode_lisa_pm_parameters[] = {
	{
		HKL_PARAMETER_DEFAULTS, .name = "det_rot_tracking", ._value = 0,
		.description = "if 1, the detector table will be rotated with the monochromator, so that stth stays constant",
		.range = { .min=0, .max=1 },
	},
	{
		HKL_PARAMETER_DEFAULTS, .name = "mchi_rot_dir", ._value = 1,
		.description = "if 1 mchi angle goes from 0 to pi, if -1 from 0 to -pi",
		.range = { .min=-1, .max=1 },
	},
	{
		HKL_PARAMETER_DEFAULTS, .name = "sample_rot", ._value = 0,
		.description = "if 1, the sample will be rotated with the monochromator, so that sth stays constant.",
		.range = { .min=0, .max=1 },
	},
};

static int _mode_lisa_pm_m_to_pm_func(const gsl_vector *x, void *params, gsl_vector *f)
{
  /* TODO: implement it */

	return  GSL_SUCCESS;
}

static const HklFunction mode_lisa_pm_m_to_pm_func = {
	.function = _mode_lisa_pm_m_to_pm_func,
	.size = 3,
};

static int _mode_lisa_pm_pm_to_m_func(const gsl_vector *x, void *params, gsl_vector *f)
{
  /* TODO: implement it */

	return  GSL_SUCCESS;
}

static const HklFunction mode_lisa_pm_pm_to_m_func = {
	.function = _mode_lisa_pm_pm_to_m_func,
	.size = 3,
};

static HklMode *mode_lisa_pm(void)
{
  
  static const char* axes_r[] = {MCHI, SPHI, DTTH}; /* QUESTION: these are the two motors I need for computing sth and stth positions, is this right ??? */

  static const char* axes_w[] = {MCHI, SPHI, DTTH}; /* QUESTION: these are the motors I move if I write to the pseudomotor sth or stth, is this right ??? */

  /* here a list of functions use to solve the mode */ /* QUESTION: which functions should be here ???, I need one for computing motor positions from pseudomotor and one for computing pseudonmotor from motor positions. Why in the other diffractometers there is only one function here ??? */
  static const HklFunction *functions[] = {&mode_lisa_pm_m_to_pm_func, &mode_lisa_pm_pm_to_m_func};

  /* here just the description of the mode: name, axes_r, axes_w, functions */
  static const HklModeAutoInfo info = {
    HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes_r, axes_w, functions, mode_lisa_pm_parameters),
  };

  /* instantiate a new mode */
  return hkl_mode_auto_new(&info,
			   &hkl_mode_operations,
			   TRUE);
}


/*****************/
/* mode readonly */
/*****************/

REGISTER_READONLY_INCIDENCE(hkl_engine_template_incidence_new,
			    P99_PROTECT({MCHI}),
			    surface_parameters_y);

REGISTER_READONLY_EMERGENCE(hkl_engine_template_emergence_new,
			    P99_PROTECT({DROT}),
			    surface_parameters_y);



/*************/
/* LisaSth Engine */
/*************/

struct _HklEngineLisaPM
{
	HklEngine engine;
	HklParameter *sth;
	HklParameter *stth;
};

typedef struct _HklEngineLisaPM HklEngineLisaPM;

static void hkl_engine_lisapm_free_real(HklEngine *base)
{
	HklEngineLisaPM *self = container_of(base, HklEngineLisaPM, engine);
	hkl_engine_release(&self->engine);
	free(self);
}

HklEngine *hkl_engine_lisa_pm_new(HklEngineList *engines)
{
	HklEngineLisaPM *self;

	static const HklParameter sth = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name="sth",
		.description = "angle of the projection of $\\vec{ki}$ in the $xy$ and the sample table rotation",
	};
	
	static const HklParameter stth = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name="stth",
		.description = "angle of the projection of $\\vec{ki}$ in the $xy$ and the detector arm",
	};
	
	static const HklParameter *pseudo_axes[] = {&sth, &stth};
	static const HklEngineInfo info = {
		HKL_ENGINE_INFO("LisaPM",
				pseudo_axes,
				HKL_ENGINE_DEPENDENCIES_AXES),
	};
	static const HklEngineOperations operations = {
		HKL_ENGINE_OPERATIONS_DEFAULTS,
		.free=hkl_engine_lisapm_free_real,
	};

	self = HKL_MALLOC(HklEngineLisaPM);

	hkl_engine_init(&self->engine, &info, &operations, engines);
	self->sth = register_pseudo_axis(&self->engine, engines, &sth);
	self->stth = register_pseudo_axis(&self->engine, engines, &stth);

	return &self->engine;

}



/***********/
/* Engines */
/***********/

static HklEngine *hkl_engine_petra3_p08_lisa_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = k_parallel_incident();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	hkl_engine_add_mode(self, alpha_eq_beta());
	hkl_engine_add_mode(self, th_eq_tth2());
	hkl_engine_add_mode(self, k_parallel_outgoing());

	return self;
}

static HklEngine *hkl_engine_petra3_p08_lisa_pm_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_lisa_pm_new(engines);

	default_mode = mode_lisa_pm();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}


/***************/
/* Engine list */
/***************/

static HklEngineList *hkl_engine_list_new_petra3_p08_lisa(const HklFactory *factory)
{

	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_petra3_p08_lisa_pm_new(self);
  
	hkl_engine_petra3_p08_lisa_hkl_new(self);

	return self;
}

/* Register the diffractometer into the factory */
REGISTER_DIFFRACTOMETER(petra3_p08_lisa, "PETRA3 P08 LISA", HKL_GEOMETRY_PETRA3_P08_LISA_DESCRIPTION);
