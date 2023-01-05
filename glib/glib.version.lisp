;;; ----------------------------------------------------------------------------
;;; glib.version.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.72 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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
;;; Version Information
;;;
;;;     Variables and functions to check the GLib version
;;;
;;; Types and Values
;;;
;;;     glib_major_version
;;;     glib_minor_version
;;;     glib_micro_version
;;;     glib_binary_age                                    not implemented
;;;     glib_interface_age                                 not implemented
;;;
;;; Functions
;;;
;;;     glib_check_version
;;; ----------------------------------------------------------------------------

(in-package :glib)

(defparameter *cl-cffi-glib-build-time*
              (multiple-value-list (get-decoded-time)))

;;; ----------------------------------------------------------------------------
;;; glib_major_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_major_version" +glib-major-version+ :read-only t) :uint)

#+liber-documentation
(setf (liber:alias-for-symbol '+glib-major-version+)
      "Constant"
      (liber:symbol-documentation '+glib-major-version+)
 "@version{2022-11-21}
  @begin{short}
    The major version number of the GLib C library the Lisp binding is running
    against.
  @end{short}
  @see-function{glib:check-version}")

(export '+glib-major-version+)

;;; ----------------------------------------------------------------------------
;;; glib_minor_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_minor_version" +glib-minor-version+ :read-only t) :uint)

#+liber-documentation
(setf (liber:alias-for-symbol '+glib-minor-version+)
      "Constant"
      (liber:symbol-documentation '+glib-minor-version+)
 "@version{2022-11-21}
  @begin{short}
    The minor version number of the GLib C library the Lisp binding is running
    against.
  @end{short}
  @see-function{glib:check-version}")

(export '+glib-minor-version+)

;;; ----------------------------------------------------------------------------
;;; glib_micro_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_micro_version" +glib-micro-version+ :read-only t) :uint)

#+liber-documentation
(setf (liber:alias-for-symbol '+glib-micro-version+)
      "Constant"
      (liber:symbol-documentation '+glib-micro-version+)
 "@version{2022-11-21}
  @begin{short}
    The micro version number of the GLib C library the Lisp binding is running
    against.
  @end{short}
  @see-function{glib:check-version}")

(export '+glib-micro-version+)

;;; ----------------------------------------------------------------------------
;;; glib_binary_age                                        not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; glib_interface_age                                     not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; glib_check_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("glib_check_version" check-version) :string
 #+liber-documentation
 "@version{2022-11-21}
  @argument[major]{an unsigned integer with the required major version}
  @argument[minor]{an unsigned integer with the required minor version}
  @argument[micro]{an unsigned integer with the required micro version}
  @begin{return}
    @code{Nil} if the GLib C library against which the Lisp binding is running
    is compatible with the given version, or a string describing the version
    mismatch.
  @end{return}
  @begin{short}
    Checks that the GLib C library in use is compatible with the given
    Lisp binding.
  @end{short}
  @begin[Examples]{dictionary}
    Suppose the Glib library version 2.72.1 is installed. Then the following
    results are returned:
    @begin{pre}
(glib:check-version 2 72 0) => NIL
(glib:check-version 2 99 0) => \"GLib version too old (micro mismatch)\"
    @end{pre}
  @end{dictionary}
  @see-function{cl-cffi-glib-build-info}"
  (major :uint)
  (minor :uint)
  (micro :uint))

(export 'check-version)

;;; ----------------------------------------------------------------------------

(defun cl-cffi-glib-build-info (&optional (out *standard-output*))
 #+liber-documentation
 "@version{2022-11-21}
  @argument[out]{an optional stream, the default is @code{*standard-output*}}
  @begin{short}
    Provides information about the version of the loaded GLIB library against
    which the Lisp binding is running.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
* (cl-cffi-glib-build-info)
cl-cffi-glib build date: 22:17 10/23/2022
GLIB version: 2.72.1
Machine type: X86-64
Machine version: Intel(R) Core(TM) i5-4210U CPU @@ 1.70GHz
Software type: Linux
Software version: 5.15.0-52-generic
Lisp implementation type: SBCL
Lisp implementation version: 2.1.11.debian
NIL
    @end{pre}
  @end{dictionary}
  @see-symbol{+glib-major-version+}
  @see-symbol{+glib-minor-version+}
  @see-symbol{+glib-micro-version+}
  @see-function{glib:check-version}"
  (format out "cl-cffi-glib build date: ~a:~a ~a/~a/~a~%"
          (third *cl-cffi-glib-build-time*)
          (second *cl-cffi-glib-build-time*)
          (fifth *cl-cffi-glib-build-time*)
          (fourth *cl-cffi-glib-build-time*)
          (sixth *cl-cffi-glib-build-time*))
  (format out "GLIB version: ~a.~a.~a~%"
          +glib-major-version+
          +glib-minor-version+
          +glib-micro-version+)
  (format out "Machine type: ~a~%" (machine-type))
  (format out "Machine version: ~a~%" (machine-version))
  (format out "Software type: ~a~%" (software-type))
  (format out "Software version: ~A~%" (software-version))
  (format out "Lisp implementation type: ~a~%" (lisp-implementation-type))
  (format out "Lisp implementation version: ~a~%" (lisp-implementation-version))
  nil)

(export 'cl-cffi-glib-build-info)

;;; --- End of file glib.version.lisp ------------------------------------------
