;;; ----------------------------------------------------------------------------
;;; glib.error.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.76 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; Macros
;;;
;;;     with-g-error
;;;     with-ignore-g-error
;;;     with-catching-g-error
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; struct GError                                          not exported
;;; ----------------------------------------------------------------------------

(defcstruct %g-error
  (:domain quark-as-string)
  (:code :int)
  (:message :string))

;;; ----------------------------------------------------------------------------
;;; g_clear_error                                          not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_clear_error" %g-clear-error) :void
  (err :pointer))

;;; ----------------------------------------------------------------------------
;;; g_set_error_error_literal                              not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_set_error_literal" %g-set-error-literal) :void
  (err :pointer)
  (domain quark-as-string)
  (code :int)
  (message :string))

;;; ----------------------------------------------------------------------------

;; The Lisp implemenation for GError replaces the C implementation. Only the
;; WITH-G-ERROR, WITH-IGNORE-G-ERROr, and WITH-CATCHING-TO-G-ERROR macros are
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
    (cl:error 'g-error-condition
              :domain (cffi:foreign-slot-value err '(:struct %g-error) :domain)
              :code (cffi:foreign-slot-value err '(:struct %g-error) :code)
              :message (cffi:foreign-slot-value err
                                                '(:struct %g-error) :message))))

(defmacro with-g-error ((err) &body body)
  `(with-foreign-object (,err :pointer)
     (setf (cffi:mem-ref ,err :pointer) (cffi:null-pointer))
     (unwind-protect
       (progn ,@body)
       (maybe-raise-g-error-condition (cffi:mem-ref ,err :pointer))
       (%g-clear-error ,err))))

(defmacro with-ignore-g-error ((err) &body body)
  `(with-foreign-object (,err :pointer)
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

;;; --- End of file glib.error.lisp --------------------------------------------
