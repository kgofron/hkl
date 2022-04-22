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
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <gsl/gsl_nan.h>                // for GSL_NAN
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include <stdlib.h>                     // for free, NULL
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-unit-private.h"           // for HklUnit, etc
#include "hkl.h"                        // for FALSE, HKL_DEGTORAD, etc

/**
 * hkl_unit_dup: (skip)
 * @self:
 *
 * copy an #Hklunit
 *
 * Returns: the copied #HklUnit (memory must be release with
 * hkl_unit_free)
 **/
HklUnit* hkl_unit_dup(const HklUnit *self)
{
	if (!self)
		return NULL;

	HklUnit *dup = g_new(HklUnit, 1);
	*dup = *self;

	return dup;
}

/**
 * hkl_unit_free: (skip)
 * @self:
 *
 * release the memory of an #HklUnit
 **/
void hkl_unit_free(HklUnit *self)
{
	if (self)
		free(self);
}

/**
 * hkl_unit_compatible: (skip)
 * @self: the first @HklUnit
 * @unit: the second @HklUnit to check
 *
 * check if two units are compatible.
 *
 * Returns: TRUE or FALSE
 **/
int hkl_unit_compatible(const HklUnit *unit1, const HklUnit *unit2)
{
	int res = FALSE;

	if (unit1 == NULL) {
		if (unit2 == NULL) {
			res = TRUE;
		}else{
			res = FALSE;
		}
	}else{
		if (unit2 == NULL){
			res = FALSE;
		}else{
			res = ( (unit1->dimension.l == unit2->dimension.l)
				&& (unit1->dimension.m == unit2->dimension.m)
				&& (unit1->dimension.t == unit2->dimension.t)
				&& (unit1->dimension.i == unit2->dimension.i)
				&& (unit1->dimension.th == unit2->dimension.th)
				&& (unit1->dimension.n == unit2->dimension.n)
				&& (unit1->dimension.j == unit2->dimension.j));
		}
	}
	return res;
}
