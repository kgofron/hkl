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
 * Copyright (C) 2003-2019, 2022 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 * Copyright (C) 2017      DESY
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Teresa Núñez <tnunez@mail.desy.de>
 */
#include "hkl.h"
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-axis-private.h"
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-matrix-private.h"
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_mode_operations, etc
#include "hkl-pseudoaxis-common-psi-private.h"  // for hkl_engine_psi_new, etc
#include "hkl-pseudoaxis-common-q-private.h"  // for hkl_engine_q2_new, etc
#include "hkl-pseudoaxis-common-tth-private.h"  // for hkl_engine_tth2_new, etc
#include "hkl-pseudoaxis-common-readonly-private.h"
#include "hkl-sample-private.h"

/***********************

    Description

There are 5 real motors:

mchi  (monochromator rotation)  -> rotation y axis +1
sphi  (sample rotation)         -> rotation z axis +1
dtth  (detector table rotation) -> rotation z axis -1
dh    (detector height)         -> translation z axis +1
drot  (detector rotation)       -> rotation x axis +1

There are 4 pseudo motors related to them:

alpha   (incident angle - kin with xy plane)
beta    (outgoing angle - kout with xy plane)
sth      (angle between the kin projection and the vector defined via sphi)
stth     (angle between kin and kout projections in the xy plane)

sth and stth has to be independently as pseudomotors.

For a given energy, E, one can compute mchi, sphi, dtth, dh and drot
from alpha, beta, th and tth and viceversa. The solutions are not
unique, for fixing them one has to set:

FRED: reading this and looking at the schema, it seems that you need a
pseudo motor engine with the four pseudo motors. something which takes
alpha, beta, sth and stth in order to compute the 5 real motors
positions. So maybe the right things to do is to create a pm with all
the pseudo motors and not only stth and sth.

Teresa: I agree. The users told me only about stth and sth, as the manual
also says, but it is better to have the four ones.

Maybe the right things to do is to create for now, only pseudo motors
with the read part.  So once we have this setup it will be easier for
us to understand what is going on.

 - Detector rotation tracking -> the detector table rotation, dtth,
   follows the monochromator rotation, mchi, so that the position of
   stth stays constant. For stth = 0 the detector is in diffraction
   plane.

 - mchi rotation direction -> if set to '+' the mchi angle goes from 0
   to pi, set to '-' it goes from 0 to -pi.

 - allow rotation sample -> if 'yes' the sample will be rotated with
   the monochromator, so that sth will stay constant. If 'no' the
   sample will not be rotated.

The following geometry constants have to be set:

  - distance sample center to detector = 500
  - distance first crystal (mono1) to sample center = 650
  - distance first crystal (mono1) to second one (mono2) = 25
  - first crystal diffraction parameter
  - second crystal diffraction parameter

lambda = hc_over_e/Energy
bragg1 = g_tau1 * lambda * 1/(4*pi)
bragg2 = g_tau2 * lambda * 1/(4*pi)

where

the crystals used are Si111 and Si220.

g_tau1 = 2*PI/d111
g_tau2 = 2*PI/d220

where d111 and d220 are the lattice plane distances (in Amstrong) of the
crystals.

d111 = 3.136 A
d220 = 1.920 A

FRED: Do you want to set these parameters in the code or do you want
to add these parameters to the geometry dynamiquely ?  So the user can
change this during the experimenta ? I would prefer a static
declaration for now in the code.

For a alpha, beta, th and tth set there are different possibilities of
choosing the position of the real motors for getting a given
h,k,l. Four possible geometry modes have to be implemented, each mode
gives an unique solution for the real computers computed from the
angles. These modes are:

  - mode 0 -> K axis from reciprocal space parallel to incident beam.
  - mode 1 -> alpha = beta
  - mode 2 -> th = tth/2
  - mode 3 -> K axis from reciprocal space parallel to outgoing beam.

There are some corrections (motor movements) that have to be done if
the energy is changed, in order to correct the shifts in the position
of the beam on the sample.

*******************/


/**************/
/* Axes names */
/**************/

#define SPHI "sphi"
#define DTTH "dtth"
#define DH   "dh"
#define DROT "drot"
#define MCHI "mchi"

#define D_SD 0.5 /* meter */
#define D_Si111 3.136 /* Si111 of mono1 Angstöm */
#define D_Si220 1.920 /* Si220 ok mono2 Angstöm */

#define PETRA3_P08_LISA_SOURCE_HOLDER_IDX 0
#define PETRA3_P08_LISA_SAMPLE_HOLDER_IDX 1
#define PETRA3_P08_LISA_DETECTOR_HOLDER_IDX 2

#define HKL_GEOMETRY_PETRA3_P08_LISA_DESCRIPTION			\
	"+ xrays source fix along the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 1 axes for the monochromator\n"				\
	"\n"								\
	"  + **" MCHI "** : rotation around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"\n"								\
	"+ 1 axis for the sample\n"					\
	"\n"								\
	"  + **" SPHI "** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 2 axes for the detector\n"					\
	"\n"								\
	"  + **" DTTH "** : rotation around the :math:`-\\vec{z}` direction (0, 0, -1)\n" \
	"  + **" DH   "** : translation along the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **" DROT "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static double _alpha_max(double wavelength, double d1, double d2)
{
	double theta1 = asin(wavelength / (2 * d1));
	double theta2 = asin(wavelength / (2 * d2));
	return 2 * (theta2 - theta1);
}

static int hkl_mode_hkl_petra3_p08_lisa_get_real(HklMode *self,
						 HklEngine *engine,
						 HklGeometry *geometry,
						 HklDetector *detector,
						 HklSample *sample,
						 GError **error)
{
	HklHolder *holder;
	HklMatrix RUB;
	HklVector hkl;
	HklVector ki;
	HklVector Q;
	HklEngineHkl *engine_hkl = container_of(engine, HklEngineHkl, engine);

	/* update the geometry internals */
	hkl_geometry_update(geometry);

	/* R * UB */
	/* for the lisa geometry the holder 1 is the sample holder. */
	holder = hkl_geometry_sample_holder_get(geometry, sample);
	hkl_quaternion_to_matrix(&holder->q, &RUB);
	hkl_matrix_times_matrix(&RUB, &sample->UB);

	/* kf - ki = Q */
	ki = hkl_geometry_ki_get(geometry);

	Q = hkl_geometry_kf_get(geometry, detector);

	hkl_vector_minus_vector(&Q, &ki);

	/* compute hkl */

	hkl_matrix_solve(&RUB, &hkl, &Q);

	engine_hkl->h->_value = hkl.data[0];
	engine_hkl->k->_value = hkl.data[1];
	engine_hkl->l->_value = hkl.data[2];

	return TRUE;
}

int hkl_mode_hkl_petra3_p08_lisa_set_real(HklMode *self,
					  HklEngine *engine,
					  HklGeometry *geometry,
					  HklDetector *detector,
					  HklSample *sample,
					  GError **error)
{
/* FRED: to implement */
/*	int last_axis; */

/*	hkl_error (error == NULL || *error == NULL); */

/*	/\* check the input parameters *\/ */
/*	if(!hkl_is_reachable(engine, geometry->source.wave_length, */
/*			     error)){ */
/*		hkl_assert(error == NULL || *error != NULL); */
/*		return FALSE; */
/*	} */
/*	hkl_assert(error == NULL || *error == NULL); */

/*	/\* compute the mode *\/ */
/*	if(!hkl_mode_auto_set_real(self, engine, */
/*				   geometry, detector, sample, */
/*				   error)){ */
/*		hkl_assert(error == NULL || *error != NULL); */
/*		//fprintf(stdout, "message :%s\n", (*error)->message); */
/*		return FALSE; */
/*	} */
/*	hkl_assert(error == NULL || *error == NULL); */

/*	/\* check that the mode allow to move a sample axis *\/ */
/*	/\* FIXME for now the sample holder is the first one *\/ */
/*	last_axis = get_last_axis_idx(geometry, 0, &self->info->axes_w); */
/*	if(last_axis >= 0){ */
/*		uint i; */
/*		const HklGeometryListItem *item; */
/*		uint len = engine->engines->geometries->n_items; */

/*		/\* For each solution already found we will generate another one *\/ */
/*		/\* using the Ewalds construction by rotating Q around the last sample *\/ */
/*		/\* axis of the mode until it intersect again the Ewald sphere. *\/ */
/*		/\* FIXME do not work if ki is colinear with the axis. *\/ */

/*		/\* for this we needs : *\/ */
/*		/\* - the coordinates of the end of the Q vector (q) *\/ */
/*		/\* - the last sample axis orientation of the mode (axis_v) *\/ */
/*		/\* - the coordinates of the center of the ewalds sphere (c) *\/ */
/*		/\* - the coordinates of the center of rotation of the sample (o = 0, 0, 0) *\/ */

/*		/\* then we can : *\/ */
/*		/\* - project the origin in plane of normal axis_v containing q (o') *\/ */
/*		/\* - project the center of the ewalds sphere into the same plan (c') *\/ */
/*		/\* - rotate q around this (o', c') line of 180° to find the (q2) solution *\/ */
/*		/\* - compute the (kf2) corresponding to this q2 solution *\/ */
/*		/\* at the end we just need to solve numerically the position of the detector *\/ */

/*		/\* we will add solution to the geometries so save its length before *\/ */
/*		for(i=0, item=list_top(&engine->engines->geometries->items, HklGeometryListItem, list); */
/*		    i<len; */
/*		    ++i, item=list_next(&engine->engines->geometries->items, item, list)){ */
/*			int j; */
/*			HklVector ki; */
/*			HklVector kf2; */
/*			HklVector q; */
/*			HklVector axis_v; */
/*			HklQuaternion qr; */
/*			HklAxis *axis; */
/*			HklVector cp = {{0}}; */
/*			HklVector op = {{0}}; */
/*			double angle; */
/*			HklGeometry *geom; */

/*			geom = hkl_geometry_new_copy(item->geometry); */

/*			/\* get the Q vector kf - ki *\/ */
/*			hkl_detector_compute_kf(detector, geom, &q); */
/*			hkl_source_compute_ki(&geom->source, &ki); */
/*			hkl_vector_minus_vector(&q, &ki); */

/*			/\* compute the current orientation of the last axis *\/ */
/*			axis = container_of(darray_item(geom->axes, */
/*							darray_item(geom->holders, 0)->config->idx[last_axis]), */
/*					    HklAxis, parameter); */
/*			axis_v = axis->axis_v; */
/*			hkl_quaternion_init(&qr, 1, 0, 0, 0); */
/*			for(j=0; j<last_axis; ++j) */
/*				hkl_quaternion_times_quaternion( */
/*					&qr, */
/*					&container_of(darray_item(geom->axes, */
/*								  darray_item(geom->holders, 0)->config->idx[j]), */
/*						      HklAxis, parameter)->q); */
/*			hkl_vector_rotated_quaternion(&axis_v, &qr); */

/*			/\* - project the center of the ewalds sphere into the same plan (c') *\/ */
/*			hkl_vector_minus_vector(&cp, &ki); */
/*			hkl_vector_project_on_plan_with_point(&cp, &axis_v, &q); */
/*			hkl_vector_project_on_plan_with_point(&op, &axis_v, &q); */

/*			/\* - rotate q around this (o', c') line of 180° to find the (q2) solution *\/ */
/*			kf2 = q; */
/*			hkl_vector_rotated_around_line(&kf2, M_PI, &cp, &op); */
/*			angle = hkl_vector_oriented_angle_points(&q, &op, &kf2, &axis_v); */
/*			/\* TODO parameter list for geometry *\/ */
/*			if(!hkl_parameter_value_set(&axis->parameter, */
/*						    hkl_parameter_value_get(&axis->parameter, HKL_UNIT_DEFAULT) + angle, */
/*						    HKL_UNIT_DEFAULT, error)) */
/*				return FALSE; */
/*			hkl_geometry_update(geom); */
/* #ifdef DEBUG */
/*			fprintf(stdout, "\n- try to add a solution by rotating Q <%f, %f, %f> around the \"%s\" axis <%f, %f, %f> of %f radian", */
/*				q.data[0], q.data[1], q.data[2], */
/*				((HklParameter *)axis)->name, */
/*				axis_v.data[0], axis_v.data[1], axis_v.data[2], */
/*				angle); */
/*			fprintf(stdout, "\n   op: <%f, %f, %f>", op.data[0], op.data[1], op.data[2]); */
/*			fprintf(stdout, "\n   q2: <%f, %f, %f>", kf2.data[0], kf2.data[1], kf2.data[2]); */
/* #endif */
/*			hkl_vector_add_vector(&kf2, &ki); */

/*			/\* at the end we just need to solve numerically the position of the detector *\/ */
/*			if(fit_detector_position(self, geom, detector, &kf2)) */
/*				hkl_geometry_list_add(engine->engines->geometries, */
/*						      geom); */

/*			hkl_geometry_free(geom); */
/*		} */
/*	} */
	return TRUE;
}

#define HKL_MODE_HKL_PETRA3_P08_LISA_OPERATIONS_DEFAULTS	\
	HKL_MODE_OPERATIONS_AUTO_DEFAULTS,			\
		.get = hkl_mode_hkl_petra3_p08_lisa_get_real,	\
		.set = hkl_mode_hkl_petra3_p08_lisa_set_real

static const HklModeOperations hkl_mode_hkl_petra3_p08_lisa_operations = {
	HKL_MODE_HKL_PETRA3_P08_LISA_OPERATIONS_DEFAULTS,
};

static const char* hkl_geometry_petra3_p08_lisa_axes[] = {MCHI, SPHI, DTTH, DH, DROT};

static HklHolder *hkl_geometry_petra3_p08_lisa_sample_holder_get_real(const HklGeometry *geometry,
								      const HklSample *sample)
{
	return darray_item(geometry->holders, PETRA3_P08_LISA_SAMPLE_HOLDER_IDX);
}

static HklHolder *hkl_geometry_petra3_p08_lisa_detector_holder_get_real(const HklGeometry *geometry,
									const HklDetector *detector)
{
	return darray_item(geometry->holders, PETRA3_P08_LISA_DETECTOR_HOLDER_IDX);
}

static HklVector hkl_geometry_petra3_p08_lisa_ki_get_real(const HklGeometry *geometry)
{
	double alpha_max = _alpha_max(geometry->source.wave_length, D_Si111, D_Si220);
	HklHolder *holder;
	HklVector ki = {{cos(alpha_max), 0, -sin(alpha_max)}};

	holder = darray_item(geometry->holders, PETRA3_P08_LISA_SOURCE_HOLDER_IDX);
	ki = hkl_holder_transformation_apply(holder, &ki);
	hkl_vector_times_double(&ki, HKL_TAU / geometry->source.wave_length);

	return ki;
}

static HklVector hkl_geometry_petra3_p08_lisa_kf_get_real(const HklGeometry *geometry,
							  const HklDetector *detector)
{
	HklHolder *holder;
	HklVector kf = {{D_SD, 0, 0}};

	holder = hkl_geometry_detector_holder_get(geometry, detector);
	kf = hkl_holder_transformation_apply(holder, &kf);
	hkl_vector_normalize(&kf);
	hkl_vector_times_double(&kf, HKL_TAU / geometry->source.wave_length);

	return kf;
}

#define HKL_GEOMETRY_PETRA3_P08_LISA_OPERATIONS_DEFAULTS		\
	HKL_GEOMETRY_OPERATIONS_DEFAULTS,				\
		.sample_holder_get = hkl_geometry_petra3_p08_lisa_sample_holder_get_real, \
		.detector_holder_get = hkl_geometry_petra3_p08_lisa_detector_holder_get_real, \
		.ki_get = hkl_geometry_petra3_p08_lisa_ki_get_real,	\
		.kf_get = hkl_geometry_petra3_p08_lisa_kf_get_real

static HklGeometry *hkl_geometry_new_petra3_p08_lisa(const HklFactory *factory)
{
	static HklGeometryOperations ops = {HKL_GEOMETRY_PETRA3_P08_LISA_OPERATIONS_DEFAULTS};
	HklGeometry *self = hkl_geometry_new(factory, &ops);
	HklHolder *h;

	/* source */
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, MCHI, 1, 0, 0, &hkl_unit_angle_deg);

	/* sample */
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, SPHI, 0, 0, 1, &hkl_unit_angle_deg);

	/* detector */
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, DTTH, 0, 0, -1, &hkl_unit_angle_deg);
	hkl_holder_add_translation(h, DH, 0, 0, 1, &hkl_unit_length_mm);
	hkl_holder_add_rotation_with_origin(h, DROT, 0, -1, 0, D_SD, 0, 0, &hkl_unit_angle_deg);

	return self;
}

/*********/
/* Modes */
/********/

/* hkl modes */

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
				 &hkl_mode_hkl_petra3_p08_lisa_operations,
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
				 &hkl_mode_hkl_petra3_p08_lisa_operations,
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
				 &hkl_mode_hkl_petra3_p08_lisa_operations,
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
				 &hkl_mode_hkl_petra3_p08_lisa_operations,
				 TRUE);
}

/* lisapm modes */


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
	/* FRED this are not the functions in order to do get and set but only to do the set. the get/set is done in the hkl_mode_operations. */
	static const HklFunction *functions[] = {&mode_lisa_pm_m_to_pm_func, &mode_lisa_pm_pm_to_m_func};

	/* here just the description of the mode: name, axes_r, axes_w, functions */
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes_r, axes_w, functions, mode_lisa_pm_parameters),
	};

	/* instantiate a new mode */
	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations, /* FRED these operation must be specifique to the pseudo motors look at the read_only emergence pseudo in hkl-pseudoaxis-common-readonly code. */
				 TRUE);
}

/******************/
/* LisaPM Engine */
/*****************/

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

	self = g_new(HklEngineLisaPM, 1);

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

	hkl_engine_petra3_p08_lisa_hkl_new(self);
	/* hkl_engine_petra3_p08_lisa_pm_new(self); */

	return self;
}

/* Register the diffractometer into the factory */
/* REGISTER_DIFFRACTOMETER(petra3_p08_lisa, "PETRA3 P08 LISA", HKL_GEOMETRY_PETRA3_P08_LISA_DESCRIPTION); */
