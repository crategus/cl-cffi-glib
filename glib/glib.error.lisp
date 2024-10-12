;;; ----------------------------------------------------------------------------
;;; glib.error.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.80 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;;     with-g-error
;;;     with-ignore-g-error
;;;     with-catching-g-error
;;;
;;;     with-error
;;;     with-ignore-error
;;;     with-catching-error
;;; ----------------------------------------------------------------------------

(in-package :glib)

;; GError is a boxed type. We export the type for usage in signals handlers,
;; when getting a GError as argument.

(define-g-boxed-opaque error "GError"
  :export t
  :type-initializer "g_error_get_type"
  :alloc (cl:error "GError cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'error)
      "GBoxed"
      (documentation 'error 'type)
 "@version{2023-6-1}
  @begin{short}
    The @class{g:error} structure contains information about an error that has
    occurred.
  @end{short}")

(export 'error)

;;; ----------------------------------------------------------------------------
;;; GError                                                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %g-error
  (%domain quark-as-string)
  (%code :int)
  (%message :string))

;;; ----------------------------------------------------------------------------
;;; g_clear_error                                           not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_clear_error" %g-clear-error) :void
  (err :pointer))

;;; ----------------------------------------------------------------------------
;;; g_set_error_error_literal                               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_set_error_literal" %g-set-error-literal) :void
  (err :pointer)
  (domain quark-as-string)
  (code :int)
  (message :string))

;;; ----------------------------------------------------------------------------

;; The Lisp implementation for GError replaces the C implementation. Only the
;; WITH-G-ERROR, WITH-IGNORE-G-ERROR, and WITH-CATCHING-TO-G-ERROR macros are
;; exported for use.

(define-condition g-error-condition (cl:error)
  ((domain  :initarg :domain
            :initform nil
            :reader g-error-condition-domain)
   (code    :initarg :code
            :initform nil
            :reader g-error-condition-code)
   (message :initarg :message
            :initform nil
            :reader g-error-condition-message))
  (:report (lambda (err stream)
             (format stream
                     "GError: Domain: ~S, Code: ~S, Message: ~A"
                     (g-error-condition-domain err)
                     (g-error-condition-code err)
                     (g-error-condition-message err)))))

(defun maybe-raise-g-error-condition (err)
  (unless (cffi:null-pointer-p err)
    (cffi:with-foreign-slots ((%domain %code %message) err (:struct %g-error))
      (cl:error 'g-error-condition
                :domain %domain
                :code %code
                :message %message))))

(defmacro with-g-error ((err) &body body)
  `(cffi:with-foreign-object (,err :pointer)
     (setf (cffi:mem-ref ,err :pointer) (cffi:null-pointer))
     (unwind-protect
       (progn ,@body)
       (maybe-raise-g-error-condition (cffi:mem-ref ,err :pointer))
       (%g-clear-error ,err))))

(defmacro with-error ((err) &body body)
  `(cffi:with-foreign-object (,err :pointer)
     (setf (cffi:mem-ref ,err :pointer) (cffi:null-pointer))
     (unwind-protect
       (progn ,@body)
       (maybe-raise-g-error-condition (cffi:mem-ref ,err :pointer))
       (%g-clear-error ,err))))

(defmacro with-ignore-g-error ((err) &body body)
  `(cffi:with-foreign-object (,err :pointer)
     (setf (cffi:mem-ref ,err :pointer) (cffi:null-pointer))
     (unwind-protect
       (progn ,@body)
       (%g-clear-error ,err))))

(defmacro with-ignore-error ((err) &body body)
  `(cffi:with-foreign-object (,err :pointer)
     (setf (cffi:mem-ref ,err :pointer) (cffi:null-pointer))
     (unwind-protect
       (progn ,@body)
       (%g-clear-error ,err))))

;;; ----------------------------------------------------------------------------
;; TODO: There is only the gtk-text-buffer-deserialize-func callback function
;; which uses this macro. This is a code fragement for the usage of the macro:
;;
;; (defcallback callback-func :boolean
;;     ((...
;;      (err :pointer))
;;   (with-catching-to-g-error (err)
;;     (restart-case
;;       ...
;;       (return-from-callback-func
;;           ()
;;           (error 'g-error-condition
;;                  :domain "cl-cffi-glib"
;;                  :code 0
;;                  :message "Error in CALLBACK-FUNC"))))))
;;; ----------------------------------------------------------------------------

(defmacro with-catching-to-g-error ((err) &body body)
  `(handler-case
     (progn ,@body)
     (g-error-condition (e)
       (%g-set-error-literal ,err
                             (g-error-condition-domain e)
                             (g-error-condition-code e)
                             (g-error-condition-message e)))))

(defmacro with-catching-to-error ((err) &body body)
  `(handler-case
     (progn ,@body)
     (g-error-condition (e)
       (%g-set-error-literal ,err
                             (g-error-condition-domain e)
                             (g-error-condition-code e)
                             (g-error-condition-message e)))))

;;; --- End of file glib.error.lisp --------------------------------------------
