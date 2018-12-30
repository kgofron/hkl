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
 * Copyright (C) 2003-2018 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_GUI_3D_H__
#define __HKL_GUI_3D_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

/* HklGui3D */
#define HKL_GUI_TYPE_3D (hkl_gui_3d_get_type ())
G_DECLARE_FINAL_TYPE (HklGui3D, hkl_gui_3d, HKL_GUI, 3D, GObject)

HklGui3D *hkl_gui_3d_new (const char *filename, HklGeometry *geometry);

void hkl_gui_3d_is_colliding(HklGui3D *self);

void hkl_gui_3d_invalidate(HklGui3D *self);

GtkFrame *hkl_gui_3d_frame_get(HklGui3D *self);

#endif // __HKL_GUI_3D_H__
