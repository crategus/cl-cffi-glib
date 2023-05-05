;;; ----------------------------------------------------------------------------
;;; gio.simple-permission.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;; GSimplePermission
;;;
;;;     GPermission that does not change value
;;;
;;; Types and Values
;;;
;;;     GSimplePermission
;;;
;;; Functions
;;;
;;;     g_simple_permission_new
;;;
;;; Object Hierarchy
;;;
;;;    GObject
;;;    ╰── GPermission
;;;        ╰── GSimplePermission
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GSimplePermission
;;; ----------------------------------------------------------------------------

(define-g-object-class "GSimplePermission" simple-permission
  (:superclass permission
   :export t
   :interfaces ()
   :type-initializer "g_simple_permission_get_type")
  nil)

#+liber-documentation
(setf (documentation 'simple-permission 'type)
 "@version{2023-5-5}
  @begin{short}
    The @sym{g:simple-permission} class is a trivial implementation of
    the @class{g:permission} class that represents a permission that is either
    always or never allowed.
  @end{short}
  The value is given at construction and does not change. Calling request or
  release will result in errors.
  @see-constructor{g:simple-permission-new}
  @see-class{g:permission}")

;;; ----------------------------------------------------------------------------
;;; g_simple_permission_new
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_permission_new" simple-permission-new)
    (gobject:object permission)
 #+liber-documentation
 "@version{2023-5-5}
  @argument[allowed]{a boolean whether the action is allowed}
  @return{A @class{g:simple-permission} instance.}
  @begin{short}
    Creates a new @class{g:permission} instance that represents an action that
    is either always or never allowed.
  @end{short}
  @see-class{g:simple-permission}
  @see-class{g:permission}"
  (allowed :boolean))

(export 'simple-permission-new)

;;; --- End of file gio.simple-permission.lisp ---------------------------------
