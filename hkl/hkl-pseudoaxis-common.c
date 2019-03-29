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
 *          Maria-Teresa Nunez-Pardo-de-Verra <tnunez@mail.desy.de>
 */
#include "hkl-detector-private.h"       // for hkl_detector_compute_kf
#include "hkl-geometry-private.h"       // for HklHolder, _HklGeometry, etc
#include "hkl-matrix-private.h"         // for hkl_matrix_times_vector, etc
#include "hkl-pseudoaxis-common-readonly-private.h"  // for HklEngineHkl
#include "hkl-sample-private.h"         // for _HklSample
#include "hkl-vector-private.h"         // for HklVector, etc

/*******/
/* hkl */
/*******/

struct HklHklRead
{
	HklVector ki;
	HklVector kf;
	HklVector Q;
	HklVector hkl;
};

struct HklHklRead hkl_hkl_read(const HklGeometry *geometry,
			       const HklDetector *detector,
			       const HklSample *sample)
{
	struct HklHklRead result;
	HklMatrix RUB;
	HklHolder *sample_holder = hkl_geometry_sample_holder_get(geometry, sample);

	result.ki = hkl_geometry_ki_get(geometry);

	result.kf  = hkl_geometry_kf_get(geometry, detector);

	result.Q = result.kf;
	hkl_vector_minus_vector(&result.Q, &result.ki);

	/* R * UB */
	hkl_quaternion_to_matrix(&sample_holder->q, &RUB);
	hkl_matrix_times_matrix(&RUB, &sample->UB);
	hkl_matrix_solve(&RUB, &result.hkl, &result.Q);

	return result;
}

struct HklHklWrite
{
	HklVector ki;
	HklVector kf;
	HklVector Q;
	HklVector hkl;
	HklVector dQ;
};

struct HklHklWrite hkl_hkl_write(const HklGeometry *geometry,
				 const HklDetector *detector,
				 const HklSample *sample,
				 const HklVector *hkl)
{
	struct HklHklWrite result;
	HklHolder *sample_holder = hkl_geometry_sample_holder_get(geometry,
								  sample);

	result.ki = hkl_geometry_ki_get(geometry);

	result.kf = hkl_geometry_kf_get(geometry, detector);

	result.Q = result.kf;
	hkl_vector_minus_vector(&result.Q, &result.ki);

	result.hkl = *hkl;
	hkl_matrix_times_vector(&sample->UB, &result.hkl);
	/* Hkl = hkl_holder_transformation_apply(sample_holder, &Hkl); */
	hkl_vector_rotated_quaternion(&result.hkl, &sample_holder->q);

	result.dQ = result.Q;
	hkl_vector_minus_vector(&result.dQ, &result.hkl);

	return result;
}


/**************************/
/* hkl double_diffraction */
/**************************/

struct HklDoubleDiffractionWrite {
	struct HklHklWrite hkl1W;
	struct HklHklWrite hkl2W;
	HklVector kf2;
};

struct HklDoubleDiffractionWrite hkl_double_diffraction_write(const HklGeometry *geometry,
							      const HklDetector *detector,
							      const HklSample *sample,
							      const HklVector *hkl1,
							      const HklVector *hkl2)
{
	struct HklDoubleDiffractionWrite result;

	result.hkl1W = hkl_hkl_write(geometry, detector, sample, hkl1);
	result.hkl2W = hkl_hkl_write(geometry, detector, sample, hkl2);

	result.kf2 = result.hkl2W.hkl;
	hkl_vector_add_vector(&result.kf2, &result.hkl2W.ki);

	return result;
}

/******************************************/
/* the psi_constant_vertical get set part */
/******************************************/

struct HklPsiWrite {
	struct HklHklWrite hklW;
	struct HklHklWrite hkl2W;
	HklVector Qn; /* Q normalized */
	HklVector hkl2; /* projection of hkl2 on plan Q */
	HklVector n; /* compute n the intersection of the plan P(kf, ki) and PQn (normal Qn) */
	double psi; /* computed psi */
	int status;
};

struct HklPsiWrite hkl_psi_write(const HklGeometry *geometry,
				 const HklDetector *detector,
				 const HklSample *sample,
				 const HklVector *hkl,
				 const HklVector *hkl2)
{
	struct HklPsiWrite result;

	result.hklW = hkl_hkl_write(geometry, detector, sample, hkl);
	result.Qn = result.hklW.Q;
	result.status = hkl_vector_normalize(&result.Qn);
	/* if |Q| > epsilon ok */
	if (result.status){
		result.hkl2W = hkl_hkl_write(geometry, detector, sample, hkl2);

		/* project hkl on the plan of normal Q */
		result.hkl2 = result.hkl2W.hkl;
		hkl_vector_project_on_plan(&result.hkl2, &result.Qn);

		/* compute n the intersection of the plan P(kf, ki) and PQ (normal Q) */
		result.n = result.hklW.kf;
		hkl_vector_vectorial_product(&result.n, &result.hklW.ki);
		hkl_vector_vectorial_product(&result.n, &result.Qn);

		/* compute psi */
		result.psi = hkl_vector_oriented_angle(&result.n, &result.hkl2, &result.Qn);
	}

	return result;
}

/***********************/
/* hkl emergence fixed */
/***********************/

struct HklEmergenceFixedWrite {
	struct HklHklWrite hklW;
	HklVector n; /* the surface orientation */
	double emergence; /* the computed emergence */
};

struct HklEmergenceFixedWrite hkl_emergence_fixed_write(const HklGeometry *geometry,
							const HklDetector *detector,
							const HklSample *sample,
							const HklVector *hkl,
							const HklVector *n)
{
	struct HklEmergenceFixedWrite result;
	const HklHolder *sample_holder = hkl_geometry_sample_holder_get(geometry, sample);

	result.hklW = hkl_hkl_write(geometry, detector, sample, hkl);

	/* compute the orientation of the surface */
	result.n = *n;
	hkl_vector_rotated_quaternion(&result.n, &sample_holder->q);

	result.emergence = _emergence(&result.n, &result.hklW.kf);

	return result;
}
