;;; ----------------------------------------------------------------------------
;;; glib.error.lisp
;;;
;;; The documentation in this file is taken from the GLib Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GLib library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; Error Reporting
;;;
;;;      A system for reporting errors
;;;
;;; Types and Values
;;;
;;;     GError
;;;
;;; Macros
;;;
;;;     with-error
;;;     with-ignore-error
;;;     with-catching-error
;;; ----------------------------------------------------------------------------

(in-package :glib)

;; TODO: Consider to export the macros for handling GError instances.

(define-gboxed-opaque error "GError"
  :export t
  :type-initializer "g_error_get_type"
  :alloc (cl:error "GError cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'error)
      "GBoxed"
      (documentation 'error 'type)
 "@version{2025-05-18}
  @begin{declaration}
(define-gboxed-opaque error \"GError\"
  :export t
  :type-initializer \"g_error_get_type\"
  :alloc (cl:error \"GError cannot be created from the Lisp side.\"))
  @end{declaration}
  @begin{short}
    The @class{glib:error} structure contains information about an error that
    has occurred.
  @end{short}
  This boxed type is exported for use in signal handlers or callback functions,
  that take a @class{g:error} instance as an argument.
  @begin[Lisp implementation]{dictionary}
    There are no exported functions for @class{g:error} instances. Errors from
    C functions either create and signal an error condition, or the error is
    ignored and a suitable value is returned from the Lisp function.
  @end{dictionary}")

(export 'error)

;;; ----------------------------------------------------------------------------
;;; GError                                                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %error
  (%domain quark-as-string)
  (%code :int)
  (%message :string))

;;; ----------------------------------------------------------------------------
;;; g_clear_error                                           not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_clear_error" %clear-error) :void
  (err :pointer))

;;; ----------------------------------------------------------------------------
;;; g_set_error_error_literal                               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_set_error_literal" %set-error-literal) :void
  (err :pointer)
  (domain quark-as-string)
  (code :int)
  (message :string))

;;; ----------------------------------------------------------------------------

;; The Lisp implementation for GError replaces the C implementation. Only the
;; WITH-ERROR, WITH-IGNORE-ERROR, and WITH-CATCHING-TO-ERROR macros are
;; exported for use.

(define-condition gerror-condition (cl:error)
  ((domain  :initarg :domain
            :initform nil
            :reader gerror-condition-domain)
   (code    :initarg :code
            :initform nil
            :reader gerror-condition-code)
   (message :initarg :message
            :initform nil
            :reader gerror-condition-message))
  (:report (lambda (err stream)
             (format stream
                     "GError: Domain: ~s, Code: ~s, Message: ~a"
                     (gerror-condition-domain err)
                     (gerror-condition-code err)
                     (gerror-condition-message err)))))

(defun maybe-raise-gerror-condition (err)
  (unless (cffi:null-pointer-p err)
    (cffi:with-foreign-slots ((%domain %code %message) err (:struct %error))
      (cl:error 'gerror-condition
                :domain %domain
                :code %code
                :message %message))))

(defmacro with-error ((err) &body body)
  `(cffi:with-foreign-object (,err :pointer)
     (setf (cffi:mem-ref ,err :pointer) (cffi:null-pointer))
     (unwind-protect
       (progn ,@body)
       (maybe-raise-gerror-condition (cffi:mem-ref ,err :pointer))
       (%clear-error ,err))))

(defmacro with-ignore-error ((err) &body body)
  `(cffi:with-foreign-object (,err :pointer)
     (setf (cffi:mem-ref ,err :pointer) (cffi:null-pointer))
     (unwind-protect
       (progn ,@body)
       (%clear-error ,err))))

;;; ----------------------------------------------------------------------------
;; This macro is used for handling errors in callback functions. An example
;; is the gtk:print-job-complete-func callback function.

(defmacro with-catching-to-error ((err) &body body)
  `(handler-case
     (progn ,@body)
     (gerror-condition (e)
       (%set-error-literal ,err
                           (gerror-condition-domain e)
                           (gerror-condition-code e)
                           (gerror-condition-message e)))))

;;; --- End of file glib.error.lisp --------------------------------------------
