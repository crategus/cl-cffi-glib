;;; ----------------------------------------------------------------------------
;;; gio.emblemed-icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.74 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2014 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GEmblemedIcon
;;;
;;;     Icon with emblems
;;;
;;; Types and Values
;;;
;;;     GEmblemedIcon
;;;
;;; Functions
;;;
;;;     g_emblemed_icon_new
;;;     g_emblemed_icon_get_icon
;;;     g_emblemed_icon_get_emblems
;;;     g_emblemed_icon_add_emblem
;;;     g_emblemed_icon_clear_emblems
;;;
;;; Properties
;;;
;;;     gicon
;;;
;;; Implemented Interfaces
;;;
;;;     GEmblemedIcon implements GIcon.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; struct GEmblemedIcon
;;; ----------------------------------------------------------------------------

(define-g-object-class "GEmblemedIcon" emblemed-icon
  (:superclass gobject:object
   :export t
   :interfaces ("GIcon")
   :type-initializer "g_emblemed_icon_get_type")
  ((gicon
    emblemed-icon-gicon
    "gicon" "GIcon" t t)))

#+liber-documentation
(setf (documentation 'emblemed-icon 'type)
 "@version{#2022-12-29}
  @begin{short}
    The @sym{g:emblemed-icon} class is an implementation of the @class{g:icon}
    interface that supports adding an emblem to an icon.
  @end{short}
  Adding multiple emblems to an icon is ensured via the
  @fun{g:emblemed-icon-add-emblem} function.

  Note that the @sym{g:emblemed-icon} class allows no control over the position
  of the emblems. See also the @class{g:emblem} class for more information.
  @see-constructor{g:emblemed-icon-new}
  @see-slot{g:emblemed-icon-gicon}
  @see-class{g:icon}
  @see-class{g:emblem}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gicon" 'emblemed-icon) t)
 "The @code{gicon} property of type @class{g:icon}
  (Read / Write / Construct Only) @br{}
  The icon to attach emblems to.")

#+liber-documentation
(setf (liber:alias-for-function 'emblemed-icon-gicon)
      "Accessor"
      (documentation 'emblemed-icon-gicon 'function)
 "@version{#2022-12-29}
  @syntax[]{(g:emblemed-icon-gicon object) => gicon}
  @syntax[]{(setf (g:emblemend-icon-gicon object) gicon)}
  @argument[object]{a @class{g:emblemed-icon} object}
  @argument[gicon]{a @class{g:icon} object to attach emblems to}
  @begin{short}
    Accessor of the @slot[g:emblemed-icon]{gicon} slot of the
    @class{g:emblemed-icon} class.
  @end{short}
  @see-class{g:emblemed-icon}
  @see-function{g:emblemed-icon-icon}")

(export 'emblemed-icon-gicon)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblemed_icon_new" emblemed-icon-new) (gobject:object icon)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[icon]{a @class{g:icon} object}
  @argument[emblem]{a @class{g:emblem} object, or @code{nil}}
  @begin{short}
    Creates a new emblemed icon for @arg{icon} with the emblem @arg{emblem}.
  @end{short}
  @see-class{g:emblemed-icon}
  @see-class{g:emblem}"
  (icon (gobject:object icon))
  (emblem (gobject:object emblem)))

(export 'emblemed-icon-new)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_get_icon () -> emblemed-icon-icon
;;; ----------------------------------------------------------------------------

(declaim (inline emblemed-icon-icon))

(defun emblemed-icon-icon (emblemed)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[emblemed]{a @class{g:emblemed-icon} object}
  @return{A @class{g:icon} object that is owned by @arg{emblemed}.}
  @short{Gets the main icon for @arg{emblemed}.}
  @see-class{g:emblemed-icon}
  @see-class{g:icon}"
  (emblemed-icon-gicon emblemed))

(export 'emblemed-icon-icon)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_get_emblems () -> emblemed-icon-emblems
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblemed_icon_get_emblems" emblemed-icon-emblems)
    (glib:list-t (gobject:object emblem) :free-from-foreign nil)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[emblemed]{a @class{g:emblemed-icon} object}
  @return{A list of @class{g:emblem} objects that is owned by @arg{emblemed}.}
  @begin{short}
    Gets the list of emblems for the icon.
  @end{short}
  @see-class{g:emblemed-icon}
  @see-class{g:emblem}"
  (emblemed (gobject:object emblemed-icon)))

(export 'emblemed-icon-emblems)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_add_emblem ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblemed_icon_add_emblem" emblemed-icon-add-emblem) :void
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[emblemed]{a @class{g:emblemed-icon} object}
  @argument[emblem]{a @class{g:emblem} object}
  @short{Adds @arg{emblem} to the list of @class{g:emblem} objects.}
  @see-class{g:emblemed-icon}
  @see-class{g:emblem}"
  (emblemed (gobject:object emblemed-icon))
  (emblem (gobject:object emblem)))

(export 'emblemed-icon-add-emblem)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_clear_emblems ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblemed_icon_clear_emblems" emblemed-icon-clear-emblems) :void
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[emblemed]{a @class{g:emblemed-icon} object}
  @begin{short}
    Removes all the emblems from icon.
  @end{short}
  @see-class{g:emblemed-icon}"
  (emblemed (gobject:object emblemed-icon)))

(export 'emblemed-icon-clear-emblems)

;;; --- End of file gio.emblemed-icon.lisp -------------------------------------
