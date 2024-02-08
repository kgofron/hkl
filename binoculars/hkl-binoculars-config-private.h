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

#include "datatype99.h"
#include "hkl.h"

datatype(
	FilePath,
	(FilePath_FileAbs, char *)
	);

datatype(
	InputRange,
	(InputRange_FromTo, int, int),
	(InputRange_At, int)
	);

datatype(
	ConfigRange,
	(ConfigRange_List, InputRange*)
	);


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

HKLAPI int hkl_binoculars_config();
