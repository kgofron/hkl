#pragma once
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
 * Copyright (C) 2003-2024 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include <stdio.h>

#include "datatype99.h"
#include "hkl.h"

datatype(
	InputRange,
	(InputRange_FromTo, int, int),
	(InputRange_At, int)
	);

void input_range_fprintf(FILE*, const InputRange *);

typedef darray(InputRange) darray_input_range;

darray_input_range *parse_input_ranges(const char*);

void darray_input_range_fprintf(FILE *, const darray_input_range*);


datatype(
	ProjectionType,
	(ProjectionType_Angles),
	(ProjectionType_Angles2),
 	(ProjectionType_Hkl),
	(ProjectionType_QCustom),
	(ProjectionType_QIndex),
	(ProjectionType_QparQper),
	(ProjectionType_QxQyQz),
	(ProjectionType_RealSpace),
	(ProjectionType_Pixels),
	(ProjectionType_Test)
	);


void projection_type_fprintf(FILE *, const ProjectionType*);

HKLAPI int hkl_binoculars_config();
