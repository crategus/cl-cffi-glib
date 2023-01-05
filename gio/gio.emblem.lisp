;;; ----------------------------------------------------------------------------
;;; gio.emblem.lisp
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
;;; GEmblem
;;;
;;;     An object for emblems
;;;
;;; Types and Values
;;;
;;;     GEmblem
;;;     GEmblemOrigin
;;;
;;; Functions
;;;
;;;     g_emblem_new
;;;     g_emblem_new_with_origin
;;;     g_emblem_get_icon
;;;     g_emblem_get_origin
;;;
;;; Object Hierarchy
;;;
;;;     GEnum
;;;     ╰── GEmblemOrigin
;;;
;;;     GObject
;;;     ╰── GEmblem
;;;
;;; Implemented Interfaces
;;;
;;;     GEmblem implements GIcon.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; enum GEmblemOrigin
;;; ----------------------------------------------------------------------------

(define-g-enum "GEmblemOrigin" emblem-origin
  (:export t
   :type-initializer "g_emblem_origin_get_type")
  :unknown
  :device
  :livemetadata
  :tag)

#+liber-documentation
(setf (liber:alias-for-symbol 'emblem-origin)
      "GEnum"
      (liber:symbol-documentation 'emblem-origin)
 "@version{2022-12-29}
  @begin{short}
    The @sym{g:emblem-origin} enumeration is used to add information about the
    origin of the emblem to a @class{g:emblem} object.
  @end{short}
  @begin{pre}
(define-g-enum \"GEmblemOrigin\" emblem-origin
  (:export t
   :type-initializer \"g_emblem_origin_get_type\")
  :unknown
  :device
  :livemetadata
  :tag)
  @end{pre}
  @begin[code]{table}
    @entry[:unkown]{Emblem of unknown origin.}
    @entry[:device]{Emblem adds device-specific information.}
    @entry[:livemetadata]{Emblem depicts live metadata, such as \"readonly\".}
    @entry[:tag]{Emblem comes from a user-defined tag, e.g. set by nautilus
      (in the future).}
  @end{table}
  @see-class{g:emblem}")

;;; ----------------------------------------------------------------------------
;;; GEmblem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GEmblem" emblem
  (:superclass gobject:object
   :export t
   :interfaces ("GIcon")
   :type-initializer "g_emblem_get_type")
  ((icon
    emblem-icon
    "icon" "GIcon" t t)
   (origin
    emblem-origin
    "orign" "GEmblemOrigin" t t)))

#+liber-documentation
(setf (documentation 'emblem 'type)
 "@version{2022-12-29}
  @begin{short}
    The @sym{g:emblem} class is an implementation of the @class{g:icon} class
    that supports having an emblem, which is an icon with additional properties.
  @end{short}
  It can than be added to a @class{g:emblemed-icon} object.

  Currently, only metainformation about the emblem's origin is supported. More
  may be added in the future.
  @see-constructor{g:emblem-new}
  @see-constructor{g:emblem-new-with-origin}
  @see-slot{g:emblem-icon}
  @see-slot{g:emblem-origin}
  @see-class{g:icon}
  @see-class{g:emblemed-icon}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- emblem-icon ------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon" 'emblem) t)
 "The @code{icon} property of tpye @class{g:object}
  (Read / Write / Construct Only) @br{}
  The actual icon of the emblem.")

#+liber-documentation
(setf (liber:alias-for-function 'emblem-icon)
      "Accessor"
      (documentation 'emblem-icon 'function)
 "@version{#2022-12-29}
  @argument[object]{a @class{g:emblem} object from which the icon should be
    extracted}
  @begin{short}
    Accessor of the @slot[G:emblem]{icon} slot of the @class{g:emblem} class.
  @end{short}
  The @sym{g:emblem-icon} function gives back the icon from the emblem.
  @see-class{g:emblem}")

;;; --- emblem-origin ----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "origin" 'emblem) t)
 "The @code{origin} property of type @symbol{g:emblem-origin}
  (Read / Write / Construct Only) @br{}
  Tells which origin the emblem is derived from. @br{}
  Default value: @code{:unkown}")

#+liber-documentation
(setf (liber:alias-for-function 'emblem-origin)
      "Accessor"
      (documentation 'emblem-origin 'function)
 "@version{#2022-12-29}
  @argument[object]{a @class{g:emblem} object}
  @begin{short}
    Accessor of the @slot[g:emblem]{origin} slot of the @class{g:emblem} class.
  @end{short}
  The @sym{g:emblem-origin} function gets the origin of the emblem.
  @see-class{g:emblem}")

;;; ----------------------------------------------------------------------------
;;; g_emblem_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblem_new" emblem-new) (gobject:object emblem)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[icon]{a @class{g:icon} object containing the icon}
  @return{A new @class{g:emblem} object.}
  @begin{short}
    Creates a new emblem for icon.
  @end{short}
  @see-class{g:emblem}
  @see-class{g:icon}"
  (icon (gobject:object icon)))

(export 'emblem-new)

;;; ----------------------------------------------------------------------------
;;; g_emblem_new_with_origin ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblem_new_with_origin" emblem-new-with-origin)
    (gobject:object emblem)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[icon]{a @class{g:icon} object containing the icon}
  @argument[origin]{a @symbol{g:emblem-origin} value defining the emblem's
    origin}
  @return{A new @class{g:emblem} object.}
  @begin{short}
    Creates a new emblem for icon.
  @end{short}
  @see-class{g:emblem}
  @see-class{g:icon}
  @see-symbol{g:emblem-origin}"
  (icon (gobject:object icon))
  (origin emblem-origin))

(export 'emblem-new-with-origin)

;;; --- End of file gio.emblem.lisp --------------------------------------------
