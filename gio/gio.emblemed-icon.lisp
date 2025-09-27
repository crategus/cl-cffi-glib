;;; ----------------------------------------------------------------------------
;;; gio.emblemed-icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2014 - 2024 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
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
;;;     GEmblemedIcon implements GIcon
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GEmblemedIcon
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GEmblemedIcon" emblemed-icon
  (:superclass gobject:object
   :export t
   :interfaces ("GIcon")
   :type-initializer "g_emblemed_icon_get_type")
  ((gicon
    emblemed-icon-gicon
    "gicon" "GIcon" t t)))

#+liber-documentation
(setf (documentation 'emblemed-icon 'type)
 "@version{2024-10-23}
  @begin{short}
    The @class{g:emblemed-icon} class is an implementation of the @class{g:icon}
    interface that supports adding an emblem to an icon.
  @end{short}
  The addition of more than one emblem to an icon is possible using the
  @fun{g:emblemed-icon-add-emblem} function.

  Note that the @class{g:emblemed-icon} class allows no control over the
  position of the emblems. See also the @class{g:emblem} class for more
  information.
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
 "@version{2025-09-27}
  @syntax{(g:emblemed-icon-gicon object) => gicon}
  @argument[object]{a @class{g:emblemed-icon} object}
  @argument[gicon]{a @class{g:icon} object to attach emblems to}
  @begin{short}
    The accessor for the @slot[g:emblemed-icon]{gicon} slot of the
    @class{g:emblemed-icon} class returns the icon to attach emblems to.
  @end{short}
  @see-class{g:emblemed-icon}
  @see-class{g:icon}
  @see-function{g:emblemed-icon-icon}")

(export 'emblemed-icon-gicon)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_emblemed_icon_new" emblemed-icon-new)
    (gobject:object icon :return)
 #+liber-documentation
 "@version{2024-10-23}
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
;;; g_emblemed_icon_get_icon
;;; ----------------------------------------------------------------------------

(declaim (inline emblemed-icon-icon))

(defun emblemed-icon-icon (emblemed)
 #+liber-documentation
 "@version{2024-10-23}
  @argument[emblemed]{a @class{g:emblemed-icon} object}
  @return{The @class{g:icon} object that is owned by @arg{emblemed}.}
  @short{Gets the main icon for the emblemed icon.}
  @see-class{g:emblemed-icon}
  @see-class{g:icon}"
  (emblemed-icon-gicon emblemed))

(export 'emblemed-icon-icon)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_get_emblems
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_emblemed_icon_get_emblems" emblemed-icon-emblems)
    (glib:list-t (gobject:object emblem) :free-from-foreign nil)
 #+liber-documentation
 "@version{2024-10-23}
  @argument[emblemed]{a @class{g:emblemed-icon} object}
  @return{The list of @class{g:emblem} objects that is owned by @arg{emblemed}.}
  @begin{short}
    Gets the list of emblems for the emblemed icon.
  @end{short}
  @see-class{g:emblemed-icon}
  @see-class{g:emblem}"
  (emblemed (gobject:object emblemed-icon)))

(export 'emblemed-icon-emblems)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_add_emblem
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_emblemed_icon_add_emblem" emblemed-icon-add-emblem) :void
 #+liber-documentation
 "@version{2024-10-23}
  @argument[emblemed]{a @class{g:emblemed-icon} object}
  @argument[emblem]{a @class{g:emblem} object}
  @short{Adds @arg{emblem} to the list emblems for the emblemed icon.}
  @see-class{g:emblemed-icon}
  @see-class{g:emblem}"
  (emblemed (gobject:object emblemed-icon))
  (emblem (gobject:object emblem)))

(export 'emblemed-icon-add-emblem)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_clear_emblems
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_emblemed_icon_clear_emblems" emblemed-icon-clear-emblems)
    :void
 #+liber-documentation
 "@version{2024-10-23}
  @argument[emblemed]{a @class{g:emblemed-icon} object}
  @begin{short}
    Removes all the emblems from the emblemed icon.
  @end{short}
  @see-class{g:emblemed-icon}"
  (emblemed (gobject:object emblemed-icon)))

(export 'emblemed-icon-clear-emblems)

;;; --- End of file gio.emblemed-icon.lisp -------------------------------------
