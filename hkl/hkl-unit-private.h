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
 * Copyright (C) 2003-2019, 2023 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_UNIT_PRIVATE_H__
#define __HKL_UNIT_PRIVATE_H__

#include "hkl.h"                        // for G_BEGIN_DECLS, etc

G_BEGIN_DECLS

extern HklUnit *hkl_unit_dup(const HklUnit *self);
extern void hkl_unit_free(HklUnit *self);

extern int hkl_unit_compatible(const HklUnit *unit1, const HklUnit *unit2);

/**
 * hkl_unit_factor:
 * @self:
 * @unit:
 *
 * compute the factor to convert from one @Hklunit to another one.
 * @self * factor =  @unit
 *
 * Returns: the factor of the conversion.
 **/
static inline double hkl_unit_factor(const HklUnit *from, const HklUnit *to)
{
	double res = 1.0;

	if (from == NULL) {
		if (to == NULL) {
			res = 1.0;
		}else{
			res = 1.0 / to->factor;
		}
	}else{
		if (to == NULL){
			res = from->factor;
		}else{
			res = from->factor / to->factor;
		}
	}

	return res;
}

G_END_DECLS

#endif /* __HKL_UNIT_PRIVATE_H__ */
