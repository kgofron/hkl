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

enum geometry_e {
	GEOMETRY_E4CH,
	GEOMETRY_E4CV,
	GEOMETRY_E6C,
	GEOMETRY_K4CH,
	GEOMETRY_K4CV,
	GEOMETRY_K6C,
        GEOMETRY_SOLEIL_NANOSCOPIUM_ROBOT,
	GEOMETRY_SOLEIL_SIRIUS_KAPPA,
	GEOMETRY_SOLEIL_SIXS_MED_2_3,
	GEOMETRY_SOLEIL_SIXS_MED_2_3_v2,
	GEOMETRY_ZAXIS,
};

struct Geometry {
	enum geometry_e tag;
	union {
		struct {
			double wavelength;
			union {
				double positions[4];
				struct {
					double omega;
					double chi;
					double phi;
					double tth;
				};
			};
		} e4ch;
		struct {
			double wavelength;
			union {
				double positions[4];
				struct {
					double omega;
					double chi;
					double phi;
					double tth;
				};
			};
		} e4cv;
		struct {
			double wavelength;
			union {
				double positions[6];
				struct {
					double mu;
					double komega;
					double kappa;
					double kphi;
					double gamma;
					double delta;
				};
			};
		} e6c;
		struct {
			double wavelength;
			union {
				double positions[4];
				struct {
					double komega;
					double kappa;
					double kphi;
					double tth;
				};
			};
		} k4ch;
		struct {
			double wavelength;
			union {
				double positions[4];
				struct {
					double komega;
					double kappa;
					double kphi;
					double tth;
				};
			};
		} k4cv;
		struct {
			double wavelength;
			union {
				double positions[6];
				struct {
					double mu;
					double komega;
					double kappa;
					double kphi;
					double gamma;
					double delta;
				};
			};
		} k6c;
		struct {
			double wavelength;
			union {
				double positions[6];
				struct {
					double rz;
					double rs;
					double rx;
					double r;
					double delta;
					double gamma;
				};
			};
		} soleil_nanoscopium_robot;
		struct {
			double wavelength;
			union {
				double positions[6];
				struct {
					double mu;
					double komega;
					double kappa;
					double kphi;
					double delta;
					double gamma;
				};
			};
		} soleil_sirius_kappa;
		struct {
			double wavelength;
			union {
				double positions[6];
				struct {
					double beta;
					double mu;
					double omega;
					double gamma;
					double delta;
					double eta_a;
				};
			};
		} soleil_sixs_med_2_3;
		struct {
			double wavelength;
			union {
				double positions[5];
				struct {
					double mu;
					double omega;
					double gamma;
					double delta;
					double eta_a;
				};
			};
		} soleil_sixs_med_2_3_v2;
		struct {
			double wavelength;
			union {
				double positions[4];
				struct {
					double mu;
					double omega;
					double delta;
					double gamma;
				};
			};
		} zaxis;
	};
};

#define E4ch(_w, _o, _c, _p, _t)			\
	{.tag=GEOMETRY_E4CH,				\
			.e4ch={_w, {{_o, _c, _p, _t}}}}
#define E4cv(_w, _o, _c, _p, _t)			\
	{.tag=GEOMETRY_E4CV,				\
			.e4cv={_w, {{_o, _c, _p, _t}}}}
#define E6c(_w, _m, _ko, _ka, _kp, _g, _d)				\
	{.tag=GEOMETRY_E6C,						\
			.e6c={_w, {{_m, _ko, _ka, _kp, _g, _d}}}}
#define K4ch(_w, _o, _c, _p, _t)			\
	{.tag=GEOMETRY_K4CH,				\
			.k4ch={_w, {{_o, _c, _p, _t}}}}
#define K4cv(_w, _o, _c, _p, _t)			\
	{.tag=GEOMETRY_K4CV,				\
			.k4cv={_w, {{_o, _c, _p, _t}}}}
#define K6c(_w, _m, _ko, _ka, _kp, _g, _d)				\
	{.tag=GEOMETRY_K6C,						\
			.k6c={_w, {{_m, _ko, _ka, _kp, _g, _d}}}}
#define SoleilNanoscopiumRobot(_w,  _rz, _rs, _rx, _r, _d, _g)		\
	{.tag=GEOMETRY_SOLEIL_NANOSCOPIUM_ROBOT,                                      \
			.soleil_nanoscopium_robot={_w, {{_rz, _rs, _rx, _r, _d, _g}}}}
#define SoleilSiriusKappa(_w,  _m, _ko, _ka, _kp, _d, _g)		\
	{.tag=GEOMETRY_SOLEIL_SIRIUS_KAPPA,				\
			.soleil_sirius_kappa={_w, {{_m, _ko, _ka, _kp, _d, _g}}}}
#define SoleilSixsMed2_3(_w, _b, _m, _o, _g, _d, _e)			\
	{.tag=GEOMETRY_SOLEIL_SIXS_MED_2_3,				\
			.soleil_sixs_med_2_3={_w, {{_b, _m, _o, _g, _d, _e}}}}
#define SoleilSixsMed2_3_v2(_w, _m, _o, _g, _d, _e)			\
	{.tag=GEOMETRY_SOLEIL_SIXS_MED_2_3_v2,				\
			.soleil_sixs_med_2_3_v2={_w, {{_m, _o, _g, _d, _e}}}}
#define Zaxis(_w, _m, _o, _d, _g)				\
	{.tag=GEOMETRY_ZAXIS,					\
			.zaxis={_w, {{_m, _o, _d, _g}}}}

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

extern void Engine_fprintf(FILE *f, struct Engine engine);

extern void Engine_header(FILE *f, const struct Engine engine);

extern void Engine_save_as_dat(FILE *f, const struct Engine engine);

extern HklGeometryList *Engine_solve(HklEngineList *engines,
				     struct Engine econfig);

/* HklTrajectory */

enum trajectory_e {
	TRAJECTORY_HKL_FROM_TO,
};

struct Trajectory {
	enum trajectory_e tag;
	union {
		struct {double h0; double k0; double l0;
			double h1; double k1; double l1;
			uint n; struct Mode mode;} hklfromto;
	};
};

#define TrajectoryHklFromTo(h0_, k0_, l0_, h1_, k1_, l1_, n_, mode_) {.tag=TRAJECTORY_HKL_FROM_TO, .hklfromto={h0_, k0_, l0_, h1_, k1_, l1_, n_, .mode=mode_}}

extern generator_declare(trajectory_gen, struct Engine, struct Trajectory, tconfig);

extern uint Trajectory_len(struct Trajectory tconfig);

extern HklGeometryList *Trajectory_solve(struct Trajectory tconfig,
					 struct Geometry gconfig,
					 struct Sample sconfig,
					 uint move);

G_END_DECLS

#endif /* __HKL_TAP_H__ */
