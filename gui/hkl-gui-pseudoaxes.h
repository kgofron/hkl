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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#ifndef __HKL_GUI_ENGINE_H__
#define __HKL_GUI_ENGINE_H__

#include <glib.h>
#include <glib-object.h>
#include <gtk/gtk.h>
#include <hkl.h>

G_BEGIN_DECLS

typedef enum  {
	MODE_COL_NAME = 0,
	MODE_COL_NUM_COLS
} ModeCol;

typedef enum  {
	PSEUDO_COL_NAME = 0,
	PSEUDO_COL_IDX,
	PSEUDO_COL_VALUE,
	PSEUDO_COL_NUM_COLS
} PseudoCol;

#define HKL_GUI_TYPE_ENGINE (hkl_gui_engine_get_type ())
G_DECLARE_FINAL_TYPE (HklGuiEngine, hkl_gui_engine, HKL_GUI, ENGINE, GObject)

HklGuiEngine* hkl_gui_engine_new (HklEngine* engine);

void hkl_gui_engine_set_engine (HklGuiEngine *gui_engine,
				HklEngine *engine);

HklEngine* hkl_gui_engine_get_engine (HklGuiEngine *gui_engine);

GtkListStore* hkl_gui_engine_get_liststore (HklGuiEngine *gui_engine);

GtkFrame *hkl_gui_engine_get_frame(HklGuiEngine *self);

void hkl_gui_engine_update (HklGuiEngine* self);

G_END_DECLS

#endif /* __HKL_GUI_ENGINE_H__ */
