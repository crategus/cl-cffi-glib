;;; ----------------------------------------------------------------------------
;;; gio.emblem.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2014 - 2025 Dieter Kaiser
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
;;; GEmblemOrigin
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GEmblemOrigin" emblem-origin
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
 "@version{2025-08-23}
  @begin{declaration}
(gobject:define-genum \"GEmblemOrigin\" emblem-origin
  (:export t
   :type-initializer \"g_emblem_origin_get_type\")
  :unknown
  :device
  :livemetadata
  :tag)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:unkown]{Emblem of unknown origin.}
      @entry[:device]{Emblem adds device-specific information.}
      @entry[:livemetadata]{Emblem depicts live metadata, such as \"readonly\".}
      @entry[:tag]{Emblem comes from a user-defined tag, for example, set by
        nautilus (in the future).}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{g:emblem-origin} enumeration is used to add information about
    the origin of the emblem to a @class{g:emblem} object.
  @end{short}
  @see-class{g:emblem}")

;;; ----------------------------------------------------------------------------
;;; GEmblem
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GEmblem" emblem
  (:superclass gobject:object
   :export t
   :interfaces ("GIcon")
   :type-initializer "g_emblem_get_type")
  ((icon
    emblem-icon
    "icon" "GIcon" t t)
   (origin
    emblem-origin
    "origin" "GEmblemOrigin" t t)))

#+liber-documentation
(setf (documentation 'emblem 'type)
 "@version{2024-10-23}
  @begin{short}
    The @symbol{g:emblem} class is an implementation of the @class{g:icon} class
    that supports having an emblem, which is an icon with additional properties.
  @end{short}
  It can than be added to a @class{g:emblemed-icon} object.

  Currently, only metainformation about the origin of the emblem is supported.
  More may be added in the future.
  @see-constructor{g:emblem-new}
  @see-constructor{g:emblem-new-with-origin}
  @see-slot{g:emblem-icon}
  @see-slot{g:emblem-origin}
  @see-class{g:icon}
  @see-class{g:emblemed-icon}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:emblem-icon ----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon" 'emblem) t)
 "The @code{icon} property of type @class{g:object}
  (Read / Write / Construct Only) @br{}
  The actual icon of the emblem.")

#+liber-documentation
(setf (liber:alias-for-function 'emblem-icon)
      "Accessor"
      (documentation 'emblem-icon 'function)
 "@version{2024-10-23}
  @argument[object]{a @class{g:emblem} object from which the icon should be
    extracted}
  @begin{short}
    Accessor of the @slot[G:emblem]{icon} slot of the @class{g:emblem} class.
  @end{short}
  The @fun{g:emblem-icon} function gives back the icon from the emblem.
  @see-class{g:emblem}")

;;; --- g:emblem-origin --------------------------------------------------------

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
 "@version{204-10-23}
  @argument[object]{a @class{g:emblem} object}
  @begin{short}
    Accessor of the @slot[g:emblem]{origin} slot of the @class{g:emblem} class.
  @end{short}
  The @fun{g:emblem-origin} function gets the origin of the emblem.
  @see-class{g:emblem}")

;;; ----------------------------------------------------------------------------
;;; g_emblem_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_emblem_new" emblem-new) (gobject:object emblem :return)
 #+liber-documentation
 "@version{2024-10-23}
  @argument[icon]{a @class{g:icon} object containing the icon}
  @return{The new @class{g:emblem} object for @arg{icon}.}
  @begin{short}
    Creates a new emblem for an icon.
  @end{short}
  @see-class{g:emblem}
  @see-class{g:icon}"
  (icon (gobject:object icon)))

(export 'emblem-new)

;;; ----------------------------------------------------------------------------
;;; g_emblem_new_with_origin
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_emblem_new_with_origin" emblem-new-with-origin)
    (gobject:object emblem :return)
 #+liber-documentation
 "@version{2024-10-23}
  @argument[icon]{a @class{g:icon} object containing the icon}
  @argument[origin]{a @symbol{g:emblem-origin} value defining the origin of
   the emblem}
  @return{The new @class{g:emblem} object for @arg{icon}.}
  @begin{short}
    Creates a new emblem for an icon.
  @end{short}
  @see-class{g:emblem}
  @see-class{g:icon}
  @see-symbol{g:emblem-origin}"
  (icon (gobject:object icon))
  (origin emblem-origin))

(export 'emblem-new-with-origin)

;;; --- End of file gio.emblem.lisp --------------------------------------------
