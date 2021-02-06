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
 * Copyright (C) 2003-2012, 2020, 2021 Synchrotron SOLEIL
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
	const HklFactory *self = NULL;

	switch(geometry.tag) {
	case GEOMETRY_E4CH:
		self = hkl_factory_get_by_name("E4CH", NULL);
		break;
	case GEOMETRY_E4CV:
		self = hkl_factory_get_by_name("E4CV", NULL);
		break;
	case GEOMETRY_E6C:
		self = hkl_factory_get_by_name("E6C", NULL);
		break;
	case GEOMETRY_K4CH:
		self = hkl_factory_get_by_name("K4CH", NULL);
		break;
	case GEOMETRY_K4CV:
		self = hkl_factory_get_by_name("K4CV", NULL);
		break;
	case GEOMETRY_K6C:
		self = hkl_factory_get_by_name("K6C", NULL);
		break;
	case GEOMETRY_SOLEIL_NANOSCOPIUM_ROBOT:
		self = hkl_factory_get_by_name("SOLEIL NANOSCOPIUM ROBOT", NULL);
		break;
	case GEOMETRY_SOLEIL_SIRIUS_KAPPA:
		self = hkl_factory_get_by_name("SOLEIL SIRIUS KAPPA", NULL);
		break;
	case GEOMETRY_SOLEIL_SIXS_MED_2_3:
		self = hkl_factory_get_by_name("SOLEIL SIXS MED2+3", NULL);
		break;
	case GEOMETRY_SOLEIL_SIXS_MED_2_3_v2:
		self = hkl_factory_get_by_name("SOLEIL SIXS MED2+3 v2", NULL);
		break;
	case GEOMETRY_ZAXIS:
		self = hkl_factory_get_by_name("ZAXIS", NULL);
		break;
	}
	return self;
}

HklEngineList *newEngines(struct Geometry geometry)
{
	return hkl_factory_create_new_engine_list(getFactory(geometry));
}

HklGeometry *newGeometry(struct Geometry geometry)
{
	HklGeometry *self = hkl_factory_create_new_geometry(getFactory(geometry));

#define NEW_GEOMETRY(type_) do{						\
		if(!hkl_geometry_axis_values_set(self,			\
						 geometry.type_.positions, \
						 ARRAY_SIZE(geometry.type_.positions), \
						 HKL_UNIT_USER, NULL)){	\
			goto failed;					\
		}							\
		if(!hkl_geometry_wavelength_set(self, geometry.type_.wavelength, \
						HKL_UNIT_DEFAULT, NULL)){ \
			goto failed;					\
		}							\
	}while(0)

	switch (geometry.tag) {
	case GEOMETRY_E4CH: NEW_GEOMETRY(e4ch); break;
	case GEOMETRY_E4CV: NEW_GEOMETRY(e4cv); break;
	case GEOMETRY_E6C: NEW_GEOMETRY(e6c); break;
	case GEOMETRY_K4CH: NEW_GEOMETRY(k4ch); break;
	case GEOMETRY_K4CV: NEW_GEOMETRY(k4cv); break;
	case GEOMETRY_K6C: NEW_GEOMETRY(k6c); break;
	case GEOMETRY_SOLEIL_NANOSCOPIUM_ROBOT: NEW_GEOMETRY(soleil_nanoscopium_robot); break;
	case GEOMETRY_SOLEIL_SIRIUS_KAPPA: NEW_GEOMETRY(soleil_sirius_kappa); break;
	case GEOMETRY_SOLEIL_SIXS_MED_2_3: NEW_GEOMETRY(soleil_sixs_med_2_3); break;
	case GEOMETRY_SOLEIL_SIXS_MED_2_3_v2: NEW_GEOMETRY(soleil_sixs_med_2_3_v2); break;
	case GEOMETRY_ZAXIS: NEW_GEOMETRY(zaxis); break;
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
        match(mode){
                of(ModeHklBissectorVertical){
                        return "bissector_vertical";
                }
                of(ModeHklE4CHConstantPhi){
                        return "constant_phi";
                }
	};
}

/* Engine */

void Engine_fprintf(FILE *f, struct Engine engine)
{
	switch(engine.tag){
	case ENGINE_HKL:
	{
		fprintf(f, "hkl: %f %f %f\n", engine.hkl.h, engine.hkl.k, engine.hkl.l);
	}
	break;
	}
}

void Engine_header(FILE *f, const struct Engine engine)
{
	switch(engine.tag){
	case ENGINE_HKL:
	{
		fprintf(f, "h k l");
	}
	break;
	}
}

void Engine_save_as_dat(FILE *f, const struct Engine engine)
{
	switch(engine.tag){
	case ENGINE_HKL:
	{
		fprintf(f, "%f %f %f", engine.hkl.h, engine.hkl.k, engine.hkl.l);
	}
	break;
	}
}

HklGeometryList *Engine_solve(HklEngineList *engines, struct Engine econfig)
{
	HklGeometryList *geometries = NULL;

	switch(econfig.tag) {
	case ENGINE_HKL:
	{
		double values[3] = {econfig.hkl.h, econfig.hkl.k, econfig.hkl.l};
		HklEngine *engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
		const char *mode_name = getModeName(econfig.hkl.mode);
		if(hkl_engine_current_mode_set(engine, mode_name, NULL)){

			geometries = hkl_engine_pseudo_axis_values_set(engine,
								       values, ARRAY_SIZE(values),
								       HKL_UNIT_DEFAULT, NULL);
		}
	}
	break;
	}

	return geometries;
}

/* HklTrajectory */

generator_def(trajectory_gen, struct Engine, struct Trajectory, tconfig)
{
	switch(tconfig.tag){
	case TRAJECTORY_HKL_FROM_TO:
	{
		uint i;
		double dh = (tconfig.hklfromto.h1 - tconfig.hklfromto.h0) / (tconfig.hklfromto.n);
		double dk = (tconfig.hklfromto.k1 - tconfig.hklfromto.k0) / (tconfig.hklfromto.n);
		double dl = (tconfig.hklfromto.l1 - tconfig.hklfromto.l0) / (tconfig.hklfromto.n);
		for(i=0; i<tconfig.hklfromto.n + 1; ++i){
			double h = i * dh + tconfig.hklfromto.h0;
			double k = i * dk + tconfig.hklfromto.k0;
			double l = i * dl + tconfig.hklfromto.l0;

			struct Engine econfig = EngineHkl(h, k, l, tconfig.hklfromto.mode);
			generator_yield(econfig);
		}
	}
	break;
	}
}

uint Trajectory_len(struct Trajectory tconfig)
{
	uint n = 0;
	switch(tconfig.tag){
	case TRAJECTORY_HKL_FROM_TO:
	{
		n = tconfig.hklfromto.n + 1;
	}
	break;
	}
	return n;
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
