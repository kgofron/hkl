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
 * Copyright (C) 2003-2012, 2020, 2021, 2022 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdarg.h>

#include "hkl/hkl-matrix-private.h"
#include "hkl/hkl-macros-private.h"
#include "hkl/hkl-pseudoaxis-private.h"
#include "hkl/hkl-trajectory-private.h"
#include "hkl/api2/hkl2.h"

static const HklFactory *getFactory(struct Geometry geometry)
{
        HklFactory *res = NULL;

        match(geometry){
                of(E4ch) res = hkl_factory_get_by_name("E4CH", NULL);
                of(E4cv) res = hkl_factory_get_by_name("E4CV", NULL);
                of(E6c)  res = hkl_factory_get_by_name("E6C", NULL);
                of(K4ch) res = hkl_factory_get_by_name("K4CH", NULL);
                of(K4cv) res = hkl_factory_get_by_name("K4CV", NULL);
                of(K6c)  res = hkl_factory_get_by_name("K6C", NULL);
                of(SoleilNanoscopiumRobot) res = hkl_factory_get_by_name("SOLEIL NANOSCOPIUM ROBOT", NULL);
                of(SoleilSiriusKappa) res = hkl_factory_get_by_name("SOLEIL SIRIUS KAPPA", NULL);
                of(SoleilSixsMed2_3) res = hkl_factory_get_by_name("SOLEIL SIXS MED2+3", NULL);
                of(SoleilSixsMed2_3_v2) res = hkl_factory_get_by_name("SOLEIL SIXS MED2+3 v2", NULL);
                of(Zaxis) res = hkl_factory_get_by_name("ZAXIS", NULL);
	}
        return res;
}

HklEngineList *newEngines(struct Geometry geometry)
{
	return hkl_factory_create_new_engine_list(getFactory(geometry));
}

HklGeometry *newGeometry(struct Geometry geometry)
{
	HklGeometry *self = hkl_factory_create_new_geometry(getFactory(geometry));

#define NEW_GEOMETRY(_w, _values) do{                                   \
		if(!hkl_geometry_axis_values_set(self,			\
						 _values->data,         \
						 ARRAY_SIZE(_values->data), \
						 HKL_UNIT_USER, NULL)){	\
			goto failed;					\
		}							\
		if(!hkl_geometry_wavelength_set(self, _w,               \
						HKL_UNIT_DEFAULT, NULL)){ \
			goto failed;					\
		}							\
	}while(0)

        match(geometry){
                of(E4ch, w, values){ NEW_GEOMETRY(*w, values); }
                of(E4cv, w, values){ NEW_GEOMETRY(*w, values); }
                of(E6c, w, values){ NEW_GEOMETRY(*w, values); }
                of(K4ch, w, values){ NEW_GEOMETRY(*w, values); }
                of(K4cv, w, values){ NEW_GEOMETRY(*w, values); }
                of(K6c, w, values){ NEW_GEOMETRY(*w, values); }
                of(SoleilNanoscopiumRobot, w, values){ NEW_GEOMETRY(*w, values); }
                of(SoleilSiriusKappa, w, values){ NEW_GEOMETRY(*w, values); }
		of(SoleilSixsMed2_3, w, values){ NEW_GEOMETRY(*w, values); }
		of(SoleilSixsMed2_3_v2, w, values){ NEW_GEOMETRY(*w, values); }
                of(Zaxis, w, values){ NEW_GEOMETRY(*w, values); }
	}
	return self;
failed:
	hkl_geometry_free(self);
	return NULL;
#undef NEW_GEOMETRY
}

/* Lattice */

HklLattice *newLattice(const Lattice lattice)
{
	HklLattice *self = NULL;

        match(lattice){
                of(Cubic, a){
                        self = hkl_lattice_new(*a,
                                               *a,
                                               *a,
                                               90*HKL_DEGTORAD,
                                               90*HKL_DEGTORAD,
                                               90*HKL_DEGTORAD,
                                               NULL);
                }
                of(Hexagonal, a, c){
                        self = hkl_lattice_new(*a,
                                               *a,
                                               *c,
                                               90*HKL_DEGTORAD,
                                               90*HKL_DEGTORAD,
                                               120*HKL_DEGTORAD,
                                               NULL);
                }
                of(Tetragonal, a, c){
                        self = hkl_lattice_new(*a,
                                               *a,
                                               *c,
                                               90*HKL_DEGTORAD,
                                               90*HKL_DEGTORAD,
                                               90*HKL_DEGTORAD,
                                               NULL);
                }
        };

	return self;
}

/* Sample */

HklSample *newSample(struct Sample sample)
{
	HklSample *self;
	HklLattice *lattice;
	HklMatrix *U;

	self = hkl_sample_new(sample.name);
	lattice = newLattice(sample.lattice);
	hkl_sample_lattice_set(self, lattice);
	hkl_lattice_free(lattice);
	U = hkl_matrix_new_euler(sample.ux, sample.uy, sample.uz);
	hkl_sample_U_set(self, U, NULL);
	hkl_matrix_free(U);

	return self;
}

/* Mode */

const char *getModeName(struct Mode mode)
{
        const char *res = NULL;

        match(mode){
                of(ModeHklBissectorVertical){
                        res = "bissector_vertical";
                }
                of(ModeHklE4CHConstantPhi){
                        res = "constant_phi";
                }
	};
        return res;
}

/* Engine */

void Engine_fprintf(FILE *f, struct Engine engine)
{
        match(engine){
                of(EngineHkl, h, k, l, _){
                        fprintf(f, "hkl: %f %f %f\n", *h, *k, *l);
                }
	}
}

void Engine_header(FILE *f, const struct Engine engine)
{
        match(engine){
                of(EngineHkl){
                        fprintf(f, "h k l");
                }
	}
}

void Engine_save_as_dat(FILE *f, const struct Engine engine)
{
        match(engine){
                of(EngineHkl, h, k, l, _){
                        fprintf(f, "%f %f %f", *h, *k, *l);
                }
	}
}

HklGeometryList *Engine_solve(HklEngineList *engines, struct Engine econfig)
{
	HklGeometryList *geometries = NULL;

        match(econfig){
                of(EngineHkl, h, k, l, mode){
                        double values[3] = {*h, *k, *l};
                        HklEngine *engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
                        const char *mode_name = getModeName(*mode);
                        if(hkl_engine_current_mode_set(engine, mode_name, NULL)){

                                geometries = hkl_engine_pseudo_axis_values_set(engine,
                                                                               values, ARRAY_SIZE(values),
                                                                               HKL_UNIT_DEFAULT, NULL);
                        }
                }
	}

	return geometries;
}

/* HklTrajectory */

generator_def(trajectory_gen, struct Engine, struct Trajectory, tconfig)
{
        match(tconfig){
                of(TrajectoryHklFromTo, h0, k0, l0, h1, k1, l1, n, mode){
                        uint i;
                        double dh = (*h1 - *h0) / *n;
                        double dk = (*k1 - *k0) / *n;
                        double dl = (*l1 - *l0) / *n;
                        for(i=0; i<*n + 1; ++i){
                                double h = i * dh + *h0;
                                double k = i * dk + *k0;
                                double l = i * dl + *l0;

                                struct Engine econfig = EngineHkl(h, k, l, *mode);
                                generator_yield(econfig);
                        }
                }
	}
}

uint Trajectory_len(Trajectory tconfig)
{
        uint res = 0;

        match(tconfig){
                of(TrajectoryHklFromTo, _, _, _, _, _, _, n, _){
                        res = *n + 1;
                }
        }

        return res;
}

HklGeometryList *Trajectory_solve(struct Trajectory tconfig,
				  struct Geometry gconfig,
				  struct Sample sconfig,
				  uint move)
{
	const struct Engine *econfig;
	HklGeometryList *solutions = hkl_geometry_list_new();
	generator_t(struct Engine) gen = trajectory_gen(tconfig);

	HklGeometry *geometry = newGeometry(gconfig);
	HklEngineList *engines = newEngines(gconfig);
	HklSample *sample = newSample(sconfig);
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HklTrajectoryStats *stats = hkl_trajectory_stats_new(Trajectory_len(tconfig));

	hkl_engine_list_init(engines, geometry, detector, sample);

	while((econfig = generator_next(gen)) != NULL){
		/* Engine_fprintf(stdout, *econfig); */
		HklGeometryList *geometries = Engine_solve(engines, *econfig);
		if(NULL != geometries){
			const HklGeometryListItem *solution;

			hkl_trajectory_stats_add(stats, geometries);
			solution = hkl_geometry_list_items_first_get(geometries);
			if(move)
				hkl_engine_list_select_solution(engines, solution);

			hkl_geometry_list_add(solutions,
					      hkl_geometry_list_item_geometry_get(solution));
			/* hkl_geometry_list_fprintf(stdout, geometries); */
			hkl_geometry_list_free(geometries);
		}
	}

	/* hkl_trajectory_stats_fprintf(stdout, stats); */

	hkl_trajectory_stats_free(stats);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_engine_list_free(engines);
	hkl_geometry_free(geometry);
	generator_free(gen);

	return solutions;
}
