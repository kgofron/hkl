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
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_UNIT_PRIVATE_H__
#define __HKL_UNIT_PRIVATE_H__

#include "hkl.h"                        // for G_BEGIN_DECLS, etc

G_BEGIN_DECLS

typedef struct _HklUnitDimension HklUnitDimension;

struct _HklUnitDimension
{
	int l; /* Length */
	int m; /* Mass */
	int t; /* Time */
	int i; /* Electric current */
	int th; /* Thermodynamic temperature */
	int n; /* Amount of substance */
	int j; /* Luminous intensity */
};

#define HklDPlaneAngle {0, 0, 0, 0, 0, 0, 0}
#define HklDLength {1, 0, 0, 0, 0, 0, 0}

typedef struct _HklUnit HklUnit;

struct _HklUnit
{
	HklUnitDimension dimension;
	double factor;
	char const *name;
	char const *repr;
};

static HklUnit const hkl_unit_angle_deg = {HklDPlaneAngle, M_PI/180., "Degree", "°"};
static HklUnit const hkl_unit_angle_rad = {HklDPlaneAngle, 1., "Radian", "rad"};
static HklUnit const hkl_unit_length_nm = {HklDLength, 1e-9, "Nano Meter", "nm"};
static HklUnit const hkl_unit_angle_mrad = {HklDPlaneAngle, 1e-3, "Milli Radian", "mrad"};
static HklUnit const hkl_unit_length_mm = {HklDLength, 1e-3, "Milli Meter", "mm"};
static HklUnit const hkl_unit_length_meter = {HklDLength, 1., "Meter", "m"};

extern HklUnit *hkl_unit_dup(const HklUnit *self);
extern void hkl_unit_free(HklUnit *self);

extern int hkl_unit_compatible(const HklUnit *unit1, const HklUnit *unit2);

extern double hkl_unit_factor(const HklUnit *from, const HklUnit *to);

G_END_DECLS

#endif /* __HKL_UNIT_PRIVATE_H__ */
