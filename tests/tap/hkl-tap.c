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
 * Copyright (C) 2003-2012 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdarg.h>

#include "basic.h"
#include "hkl-tap.h"
#include "hkl/hkl-matrix-private.h"
#include "hkl/hkl-macros-private.h"
#include "hkl/hkl-pseudoaxis-private.h"

void is_quaternion(const HklQuaternion *wanted, const HklQuaternion *seen, const char *format, ...)
{
	va_list args;

	va_start(args, format);
	fflush(stderr);
	if(TRUE == hkl_quaternion_cmp(wanted, seen))
		okv(1, format, args);
	else{
		printf("# wanted: %g %g %g %g\n",
		       wanted->data[0], wanted->data[1], wanted->data[2], wanted->data[3]);
		printf("#   seen: %g %g %g %g\n",
		       seen->data[0], seen->data[1], seen->data[2], wanted->data[3]);
		okv(0, format, args);
	}
}

void is_matrix(const HklMatrix *wanted, const HklMatrix *seen, const char *format, ...)
{
	va_list args;

	va_start(args, format);
	fflush(stderr);
	if(TRUE == hkl_matrix_cmp(wanted, seen))
		okv(1, format, args);
	else{
		printf("# wanted: %g %g %g\n#       : %g %g %g\n#       : %g %g %g\n",
		       wanted->data[0][0], wanted->data[0][1], wanted->data[0][2],
		       wanted->data[1][0], wanted->data[1][1], wanted->data[1][2],
		       wanted->data[2][0], wanted->data[2][1], wanted->data[2][2]);
		printf("#   seen: %g %g %g\n#       : %g %g %g\n#       : %g %g %g\n",
		       seen->data[0][0], seen->data[0][1], seen->data[0][2],
		       seen->data[1][0], seen->data[1][1], seen->data[1][2],
		       seen->data[2][0], seen->data[2][1], seen->data[2][2]);
		okv(0, format, args);
	}
}

int check_pseudoaxes_v(HklEngine *engine, ...)
{
	uint i;
	va_list ap;
	unsigned int len = hkl_engine_len(engine);
	double values[len];

	/* extract the variable part of the method */
	va_start(ap, engine);
	for(i=0; i<len; ++i)
		values[i] = va_arg(ap, double);
	va_end(ap);

	return check_pseudoaxes(engine, values, len);
}

int check_pseudoaxes(HklEngine *engine,
		     double expected[], uint len)
{
	int res = TRUE;
	unsigned int i = 0;
	double currents[len];

	hkl_assert(hkl_engine_len(engine) == len);

	if(hkl_engine_pseudo_axis_values_get(engine, currents, len, HKL_UNIT_DEFAULT, NULL)){
		for(i=0; i<len; ++i){
			res &= fabs(currents[i] - expected[i]) <= HKL_EPSILON;
			if (!res){
				fprintf(stderr, "current: %f, expected: %f, epsilon: %f\n",
					currents[i], expected[i], HKL_EPSILON);
			}
		}
	}else
		res = FALSE;

	return res;
}

/**
 * hkl_engine_set_values_v: (skip)
 * @self: the Engine
 * @values: the values to set
 *
 * set the values of the PseudoAxes with the given values. This method
 * is only available for test as it is sort of brittle.
 **/
HklGeometryList *hkl_engine_set_values_v(HklEngine *self, ...)
{
	uint i;
	va_list ap;
	unsigned int len = hkl_engine_len(self);
	double values[len];

	va_start(ap, self);
	for(i=0; i<len; ++i)
		values[i] = va_arg(ap, double);

	va_end(ap);
	return hkl_engine_pseudo_axis_values_set(self, values, len,
						 HKL_UNIT_DEFAULT, NULL);
}

/**
 * hkl_tap_engine_pseudo_axes_randomize: (skip)
 * @self: the this ptr
 *
 * randomize all the parameters of the #HklEngine
 **/
void hkl_tap_engine_pseudo_axes_randomize(HklEngine *self,
					  double values[], size_t n_values,
					  HklUnitEnum unit_type)
{
	size_t i;

	for(i=0; i<n_values; ++i){
		HklParameter *parameter = darray_item(self->pseudo_axes, i);
		hkl_parameter_randomize(parameter);
		values[i] = hkl_parameter_value_get(parameter, unit_type);
	}
}

/**
 * hkl_tap_engine_parameters_randomize: (skip)
 * @self: the this ptr
 *
 * randomize all the parameters of the #HklEngine
 **/
void hkl_tap_engine_parameters_randomize(HklEngine *self)
{
	HklParameter **parameter;

	darray_foreach(parameter, self->mode->parameters){
		hkl_parameter_randomize(*parameter);
	}
}



/* API 2 */

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
	case GEOMETRY_SOLEIL_SIXS_MED_2_3:
		self = hkl_factory_get_by_name("SOLEIL SIXS MED2+3", NULL);
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
		DIAG(hkl_geometry_axis_values_set(self,			\
						  geometry.type_.positions, \
						  ARRAY_SIZE(geometry.type_.positions), \
						  HKL_UNIT_USER, NULL)); \
		DIAG(hkl_geometry_wavelength_set(self, geometry.type_.wavelength, \
						 HKL_UNIT_DEFAULT, NULL)); \
	}while(0)

	switch (geometry.tag) {
	case GEOMETRY_E4CH: NEW_GEOMETRY(e4ch);	break;
	case GEOMETRY_E4CV: NEW_GEOMETRY(e4cv);	break;
	case GEOMETRY_E6C: NEW_GEOMETRY(e6c);	break;
	case GEOMETRY_K4CH: NEW_GEOMETRY(k4ch);	break;
	case GEOMETRY_K4CV: NEW_GEOMETRY(k4cv);	break;
	case GEOMETRY_SOLEIL_SIXS_MED_2_3: NEW_GEOMETRY(soleil_sixs_med_2_3); break;
	}

	return self;
#undef NEW_GEOMETRY
}

HklLattice *newLattice(struct Lattice lattice)
{
	HklLattice *self = NULL;

	switch (lattice.tag) {
	case LATTICE_CUBIC:
		self = hkl_lattice_new(lattice.cubic.a,
				       lattice.cubic.a,
				       lattice.cubic.a,
				       90*HKL_DEGTORAD,
				       90*HKL_DEGTORAD,
				       90*HKL_DEGTORAD,
				       NULL);
		break;
	case LATTICE_HEXAGONAL:
		self = hkl_lattice_new(lattice.hexagonal.a,
				       lattice.hexagonal.a,
				       lattice.hexagonal.c,
				       90*HKL_DEGTORAD,
				       90*HKL_DEGTORAD,
				       120*HKL_DEGTORAD,
				       NULL);
		break;
	}

	return self;
}

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

const struct Sample cu = {
	.name = "default",
	.lattice = Cubic(1.54),
	.ux = 0.0 * HKL_DEGTORAD,
	.uy = 0.0 * HKL_DEGTORAD,
	.uz = 0.0 * HKL_DEGTORAD,
};
