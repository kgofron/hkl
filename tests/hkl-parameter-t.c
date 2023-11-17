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
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include "hkl-parameter-private.h"

static void new(void)
{
	HklParameter *p;
	GError *error;
        int res = TRUE;

	res &= DIAG(NULL == hkl_parameter_new("", "", 2, 1, 3,
                                              FALSE, TRUE,
                                              &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                                              &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	res &= DIAG(NULL == hkl_parameter_new("", "", 2, 1, 3,
                                              FALSE, TRUE,
                                              &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                                              &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	res &= DIAG(NULL == hkl_parameter_new("", "", 2, 1, 3,
                                              FALSE, TRUE,
                                              &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                                              &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	res &= DIAG(NULL == hkl_parameter_new("toto", "", 2, 1, 3,
                                              FALSE, TRUE,
                                              &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                                              &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	res &= DIAG(NULL == hkl_parameter_new("toto", "", 1, 2, 3,
                                              FALSE, TRUE,
                                              &hkl_unit_angle_rad, &hkl_unit_length_nm,
                                              &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
                              FALSE, TRUE,
                              &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                              NULL);
        res &= DIAG(0 == !p);
        res &= DIAG(is_double(1., p->range.min, HKL_EPSILON, __func__));
        res &= DIAG(is_double(2., p->_value, HKL_EPSILON, __func__));
        res &= DIAG(is_double(3., p->range.max, HKL_EPSILON, __func__));
	res &= DIAG(FALSE == p->fit);
	res &= DIAG(TRUE == p->changed);
	res &= DIAG(&hkl_unit_angle_rad == p->unit);
	res &= DIAG(&hkl_unit_angle_deg == p->punit);

	hkl_parameter_free(p);

	ok(res == TRUE, __func__);
}

static void new_copy(void)
{
	HklParameter *copy, *p;
        int res = TRUE;

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                              NULL);

	copy = hkl_parameter_new_copy(p);

	res &= DIAG(copy->name == p->name);
	res &= DIAG(copy->description == p->description);
	res &= DIAG(is_double(copy->range.min, p->range.min, HKL_EPSILON, __func__));
	res &= DIAG(is_double(copy->_value, p->_value, HKL_EPSILON, __func__));
	res &= DIAG(is_double(copy->range.max, p->range.max, HKL_EPSILON, __func__));
	res &= DIAG(copy->fit == p->fit);
	res &= DIAG(copy->changed == p->changed);
	res &= DIAG(&hkl_unit_angle_rad == copy->unit);
	res &= DIAG(&hkl_unit_angle_deg == copy->punit);

	hkl_parameter_free(copy);
	hkl_parameter_free(p);

	ok(res == TRUE, __func__);
}

static void init(void)
{
	HklParameter *p;
	int res = TRUE;

	res &= DIAG(NULL == hkl_parameter_new("", "no description", 2, NAN, 3,
					      FALSE, TRUE,
					      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                                              NULL));
	res &= DIAG(NULL == hkl_parameter_new("", "no description", 2, 1, 3,
					      FALSE, TRUE,
					      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                                              NULL));
	res &= DIAG(NULL == hkl_parameter_new("", "no description", 2, 1, 3,
					      FALSE, TRUE,
					      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                                              NULL));
	res &= DIAG(NULL == hkl_parameter_new("toto", "no description", 2, 1, 3,
					      FALSE, TRUE,
					      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                                              NULL));
	res &= DIAG(NULL == hkl_parameter_new("toto", "no description", 1, 2, 3,
					      FALSE, TRUE,
					      &hkl_unit_angle_rad, &hkl_unit_length_nm,
                                              NULL));
	res &= DIAG(NULL == hkl_parameter_new("toto", "no description", 1, 2, 3,
					      FALSE, TRUE,
					      &hkl_unit_angle_rad, &hkl_unit_length_nm,
                                              NULL));
	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                              NULL);
	res &= DIAG(NULL != p);

	hkl_parameter_free(p);

	ok(res == TRUE, __func__);
}

static void set(void)
{
	HklParameter *p;
	GError *error;
	int res = TRUE;

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                              NULL);

	/* can not set a parameter with a NaN value */
	error = NULL;
	res &= DIAG(FALSE == hkl_parameter_value_set(p, NAN, HKL_UNIT_DEFAULT, &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	/* set a normal value is ok */
	error = NULL;
	res &= DIAG(TRUE == hkl_parameter_value_set(p, 10, HKL_UNIT_DEFAULT, &error));
	res &= DIAG(error == NULL);

	hkl_parameter_free(p);

	ok(res == TRUE, __func__);
}

static void is_valid(void)
{
	HklParameter *p;
	GError *error;
	int res = TRUE;

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                              NULL);
	res &= DIAG(TRUE == hkl_parameter_is_valid(p));

	error = NULL;
	res &= DIAG(TRUE == hkl_parameter_value_set(p, 10, HKL_UNIT_DEFAULT, &error));
	res &= DIAG(error == NULL);
	res &= DIAG(FALSE == hkl_parameter_is_valid(p));

	hkl_parameter_free(p);

	ok(res == TRUE, __func__);
}

static void min_max(void)
{
	HklParameter *p;
	double min, max;
	GError *error;
	int res = TRUE;

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                              NULL);
	hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_DEFAULT);
	is_double(1, min, HKL_EPSILON, __func__);
	is_double(3, max, HKL_EPSILON, __func__);

	res &= DIAG(TRUE == hkl_parameter_min_max_set(p, 1.1, 4, HKL_UNIT_DEFAULT, NULL));
	hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_DEFAULT);
	is_double(1.1, min, HKL_EPSILON, __func__);
	is_double(4, max, HKL_EPSILON, __func__);

	error = NULL;
	res &= DIAG(FALSE == hkl_parameter_min_max_set(p, 4, 1, HKL_UNIT_DEFAULT, &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	/* nothing should have changed */
	hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_DEFAULT);
	is_double(1.1, min, HKL_EPSILON, __func__);
	is_double(4, max, HKL_EPSILON, __func__);

	error = NULL;
	res &= DIAG(FALSE == hkl_parameter_min_max_set(p, NAN, 1, HKL_UNIT_DEFAULT, &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	/* nothing should have changed */
	hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_DEFAULT);
	is_double(1.1, min, HKL_EPSILON, __func__);
	is_double(4, max, HKL_EPSILON, __func__);

	error = NULL;
	res &= DIAG(FALSE == hkl_parameter_min_max_set(p, 1, NAN, HKL_UNIT_DEFAULT, &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	/* nothing should have changed */
	hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_DEFAULT);
	is_double(1.1, min, HKL_EPSILON, __func__);
	is_double(4, max, HKL_EPSILON, __func__);

	error = NULL;
	res &= DIAG(FALSE == hkl_parameter_min_max_set(p, NAN, NAN, HKL_UNIT_DEFAULT, &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	/* nothing should have changed */
	hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_DEFAULT);
	is_double(1.1, min, HKL_EPSILON, __func__);
	is_double(4, max, HKL_EPSILON, __func__);

	hkl_parameter_free(p);
}

static void getter(void)
{
	HklParameter *p;

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg,
                              NULL);

	ok(NULL == hkl_parameter_axis_v_get(p), __func__);
	ok(NULL == hkl_parameter_quaternion_get(p), __func__);
	ok(NULL != hkl_parameter_description_get(p), __func__);

	hkl_parameter_free(p);
}

int main(void)
{
	plan(26);

	new();
	new_copy();
	init();
	set();
	is_valid();
	min_max();
	getter();
	return 0;
}
