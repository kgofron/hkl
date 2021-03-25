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
 * Copyright (C) 2003-2021 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_HKL2_H__
#define __HKL_HKL2_H__

#include "hkl.h"
#include "hkl/ccan/generator/generator.h"
#include "datatype99.h"

G_BEGIN_DECLS

/* Geometry */

typedef struct { double data[4]; } double4;
typedef struct { double data[5]; } double5;
typedef struct { double data[6]; } double6;

#define VALUES(...)                                                                                \
        ((ML99_CAT(double, ML99_VARIADICS_COUNT(__VA_ARGS__))){.data = {__VA_ARGS__}})

datatype(
        Geometry,
        (E4ch, double, double4),
        (E4cv, double, double4),
        (E6c, double, double6),
        (K4ch, double, double4),
        (K4cv, double, double4),
        (K6c, double, double6),
        (SoleilNanoscopiumRobot, double, double6),
        (SoleilSiriusKappa, double, double6),
        (SoleilSixsMed2_3, double, double6),
        (SoleilSixsMed2_3_v2, double, double5),
        (Zaxis, double, double4)
        );

extern HklGeometry *newGeometry(struct Geometry geometry);

/* Engines */

extern HklEngineList *newEngines(struct Geometry geometry);

/* Lattice */

datatype(
        Lattice,
        (Cubic, double),
        (Hexagonal, double, double),
        (Tetragonal, double, double)
        );

extern HklLattice *newLattice(const Lattice lattice);

/* Sample */

struct Sample {
	const char *name;
	Lattice lattice;
	double ux;
	double uy;
	double uz;
};

#define CU (struct Sample) {                                          \
                .name = "default",                                    \
                        .lattice = Cubic(1.54),                       \
                        .ux = 0.0 * HKL_DEGTORAD,                     \
                        .uy = 0.0 * HKL_DEGTORAD,                     \
                        .uz = 0.0 * HKL_DEGTORAD                      \
                        }


extern HklSample *newSample(struct Sample sample);

/* Mode */

datatype (
        Mode,
        (ModeHklBissectorVertical),
        (ModeHklE4CHConstantPhi)
        );

extern const char *getModeName(struct Mode mode);

/* Engine */

datatype(
        Engine,
        (EngineHkl, double, double, double, Mode)
        );

extern void Engine_fprintf(FILE *f, const Engine engine);

extern void Engine_header(FILE *f, const Engine engine);

extern void Engine_save_as_dat(FILE *f, const Engine engine);

extern HklGeometryList *Engine_solve(HklEngineList *engines,
				     const Engine econfig);

/* HklTrajectory */

datatype(
        Trajectory,
        (TrajectoryHklFromTo, double, double, double, double, double, double, uint, Mode)
        );

extern generator_declare(trajectory_gen, Engine, Trajectory, tconfig);

extern uint Trajectory_len(const Trajectory tconfig);

extern HklGeometryList *Trajectory_solve(const Trajectory tconfig,
					 struct Geometry gconfig,
					 struct Sample sconfig,
					 uint move);

G_END_DECLS

#endif /* __HKL_TAP_H__ */
