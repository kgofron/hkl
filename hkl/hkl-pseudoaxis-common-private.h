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
#ifndef __HKL_PSEUDOAXIS_COMMON_PRIVATE__
#define __HKL_PSEUDOAXIS_COMMON_PRIVATE__

#include <gsl/gsl_vector_double.h>      // for gsl_vector
#include "hkl-pseudoaxis-auto-private.h"
#include "hkl-pseudoaxis-private.h"     // for HklModeOperations, etc
#include "hkl.h"                        // for HklEngine, HklDetector, etc
#include "hkl-pseudoaxis-common-readonly-private.h"


struct HklHklRead
{
	HklVector ki;
	HklVector kf;
	HklVector Q;
	HklVector hkl;
};

extern struct HklHklRead hkl_hkl_read(const HklGeometry *geometry,
				      const HklDetector *detector,
				      const HklSample *sample);

struct HklHklWrite
{
	HklVector ki;
	HklVector kf;
	HklVector Q;
	HklVector hkl;
	HklVector dQ;
};

extern struct HklHklWrite hkl_hkl_write(const HklGeometry *geometry,
					const HklDetector *detector,
					const HklSample *sample,
					const HklVector *hkl);

/**************************/
/* hkl double_diffraction */
/**************************/

struct HklDoubleDiffractionWrite {
	struct HklHklWrite hkl1W;
	struct HklHklWrite hkl2W;
	HklVector kf2;
};

extern struct HklDoubleDiffractionWrite hkl_double_diffraction_write(const HklGeometry *geometry,
								     const HklDetector *detector,
								     const HklSample *sample,
								     const HklVector *hkl1,
								     const HklVector *hkl2);

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

extern struct HklPsiWrite hkl_psi_write(const HklGeometry *geometry,
					const HklDetector *detector,
					const HklSample *sample,
					const HklVector *hkl,
					const HklVector *hkl2);

/***********************/
/* hkl emergence fixed */
/***********************/

struct HklEmergenceFixedWrite {
	struct HklHklWrite hklW;
	HklVector n; /* the surface orientation */
	double emergence; /* the computed emergence */
};

extern struct HklEmergenceFixedWrite hkl_emergence_fixed_write(const HklGeometry *geometry,
							       const HklDetector *detector,
							       const HklSample *sample,
							       const HklVector *hkl,
							       const HklVector *n);

#endif
