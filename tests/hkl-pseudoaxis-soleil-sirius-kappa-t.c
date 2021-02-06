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
 * Copyright (C) 2003-2019, 2021 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include "hkl/ccan/generator/generator.h"
#include "hkl-geometry-private.h"
#include "hkl-trajectory-private.h"

/* tests */

static void stability(void)
{
	int res = TRUE;
	HklGeometryList *solutions;

	struct Sample gaas = {
		.name = "GaAs",
		.lattice = Cubic(5.6533),
		.ux = -90.003382 * HKL_DEGTORAD,
		.uy = 0.12907 * HKL_DEGTORAD,
		.uz = -159.91372 * HKL_DEGTORAD,
	};

	struct Geometry gconfig =		\
		SoleilSiriusKappa(1.458637,
				  -0.5193202, 64.7853160, 133.5621380, -80.9690000, -0.0223369, 30.0000299);

	struct Geometry gconfig2 =		\
		SoleilSiriusKappa(1.458637,
				  -0.5193202, 64.7853160, 133.5621380, 124.9690000, -0.0223369, 30.0000299);

	Mode mode = ModeHklBissectorVertical();

	/* move between each step */
	struct Trajectory tconfig1 = TrajectoryHklFromTo(0, 0, 1, 0, 0, 6, 11, mode);
	solutions = Trajectory_solve(tconfig1, gconfig, gaas, TRUE);
	res &= DIAG(NULL != solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig1, gconfig, gaas, TRUE);
	res &= DIAG(NULL != solutions);
	hkl_geometry_list_free(solutions);

	struct Trajectory tconfig2 = TrajectoryHklFromTo(0, 0, 1, 0, 0, 6, 101, mode);
	solutions = Trajectory_solve(tconfig2, gconfig, gaas, TRUE);
	res &= DIAG(NULL != solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig2, gconfig2, gaas, TRUE);
	res &= DIAG(NULL != solutions);
	hkl_geometry_list_free(solutions);

	/* do not move between each steps */
	solutions = Trajectory_solve(tconfig1, gconfig, gaas, FALSE);
	res &= DIAG(NULL != solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig1, gconfig, gaas, FALSE);
	res &= DIAG(NULL != solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig2, gconfig, gaas, FALSE);
	res &= DIAG(NULL != solutions);
	hkl_geometry_list_free(solutions);

	solutions = Trajectory_solve(tconfig2, gconfig2, gaas, FALSE);
	res &= DIAG(NULL != solutions);
	hkl_geometry_list_free(solutions);

	ok(res == TRUE, __func__);

}

int main(void)
{
	plan(1);

	stability();

	return 0;
}
