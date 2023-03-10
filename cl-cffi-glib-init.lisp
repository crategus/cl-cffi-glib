;;; ----------------------------------------------------------------------------
;;; glib.init.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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

(defpackage :glib-init
  (:use :common-lisp)
  (:export #:at-init
           #:at-finalize
           #:push-library-version-features
           #:require-library-version))

(in-package :glib-init)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *initializers-table* (make-hash-table :test 'equalp))
  (defvar *initializers* nil)

  (defun register-initializer (key fn)
    (unless (gethash key *initializers-table*)
      (setf (gethash key *initializers-table*) t
            *initializers* (nconc *initializers* (list fn)))))

  (defvar *finalizers-table* (make-hash-table :test 'equalp))
  (defvar *finalizers* nil)

  (defun register-finalizer (key fn)
    (unless (gethash key *finalizers-table*)
      (setf (gethash key *finalizers-table*) t
            *finalizers* (nconc *finalizers* (list fn))))))

(defun run-initializers ()
  (dolist (fn *initializers*)
    (funcall fn)))

(defun run-finalizers ()
  (dolist (fn *finalizers*)
    (funcall fn)))

#+sbcl
(pushnew 'run-initializers sb-ext:*init-hooks*)
#+openmcl
(pushnew 'run-initializers ccl:*restore-lisp-functions*)

#+sbcl
(pushnew 'run-finalizers sb-ext:*save-hooks*)
#+openmcl
(pushnew 'run-finalizers ccl:*save-exit-functions*)

;;; ----------------------------------------------------------------------------
;;; at-init (keys body)
;;;
;;; Runs the code normally but also schedules the code to be run at image load
;;; time. It is used to reinitialize the libraries when the dumped image is
;;; loaded. (Works only on SBCL for now).
;;;
;;; At-init form may be called multiple times. The same code from should not be
;;; run multiple times at initialization time (in best case, this will only
;;; slow down initialization, in worst case, the code may crash). To ensure
;;; this, every at-init expression is added to hash-table with the body and
;;; keys as a composite key. This ensures that the same code is only executed
;;; once (once on the same set of parameters).
;;;
;;; Example:
;;;
;;; (defmethod initialize-instance :after ((class gobject-class)
;;;                                        &key &allow-other-keys)
;;;   (setf (gobject:symbol-for-gtype (gobject-class-g-type-name class)
;;;                                   (class-name class))
;;;   (at-init (class) (initialize-gobject-class-g-type class)))
;;;
;;; In this example, for every class, (initialize-gobject-class-g-type class)
;;; will be called only once.
;;; ----------------------------------------------------------------------------

(defmacro at-init ((&rest keys) &body body)
  `(progn (register-initializer (list ,@keys ',body) (lambda () ,@body))
          ,@body))

(defmacro at-finalize ((&rest keys) &body body)
  `(register-finalizer (list ,@keys ',body) (lambda () ,@body)))

;;; ----------------------------------------------------------------------------

;;; Load the foreign libraries Glib and GThread

(at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:define-foreign-library glib
      ((:and :unix (:not :darwin))
       (:or "libglib-2.0.so.0" "libglib-2.0.so"))
      (:darwin (:or "libglib-2.0.0.dylib" "libglib-2.0.dylib"))
      (:windows "libglib-2.0-0.dll")
      (t (:default "libglib-2.0"))))
  (cffi:use-foreign-library glib))

(at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:define-foreign-library gthread
      ((:and :unix (:not :darwin))
       (:or "libgthread-2.0.so.0"  "libgthread-2.0.so"))
      (:darwin (:or "libgthread-2.0.0.dylib"  "libgthread-2.0.dylib"))
      (:windows "libgthread-2.0-0.dll")
      (t "libgthread-2.0")))
  (cffi:use-foreign-library gthread))

;;; Lisp support to check the library version

(defmacro push-library-version-features (library-name
                                         major-version-var
                                         minor-version-var
                                         &body versions)
  `(eval-when (:load-toplevel :execute)
     ,@(loop for (major minor) on versions by #'cddr
             collect `(when (or (and (= ,major-version-var ,major)
                                     (>= ,minor-version-var ,minor))
                                (> ,major-version-var ,major))
                        (pushnew ,(intern (format nil "~A-~A-~A"
                                                  (string library-name)
                                                  major minor)
                                          (find-package :keyword))
                                 *features*)))))

(define-condition foreign-library-minimum-version-mismatch (error)
  ((library :initarg :library :reader .library)
   (minimum-version :initarg :minimum-version :reader .minimum-version)
   (actual-version :initarg :actual-version :reader .actual-version))
  (:report (lambda (c s)
             (format s
                     "Library ~A has too old version: it is ~A but required ~
                      to be at least ~A"
                     (.library c)
                     (.actual-version c)
                     (.minimum-version c)))))

(defun require-library-version (library min-major-version
                                        min-minor-version
                                        major-version
                                        minor-version)
  (unless (or (> major-version min-major-version)
              (and (= major-version min-major-version)
                   (>= minor-version min-minor-version)))
    (restart-case
        (error 'foreign-library-minimum-version-mismatch
               :library library
               :minimum-version (format nil "~A.~A"
                                        min-major-version min-minor-version)
               :actual-version (format nil "~A.~A"
                                       major-version minor-version))
      (ignore () :report "Ignore version requirement" nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :glib *features*))

(cffi:defcvar 
  ("glib_major_version" +glib-init-major-version+ :read-only t) :uint)
(cffi:defcvar 
  ("glib_minor_version" +glib-init-minor-version+ :read-only t) :uint)

(push-library-version-features glib
                               +glib-init-major-version+
                               +glib-init-minor-version+
  2 56    ; Since 12.03.2018
  2 58    ; Since 03.09.2018
  2 60    ; Since 04.03.2019
  2 62    ; Since 05.09.2019
  2 64    ; Since 27.02.2020
  2 66    ; Since 10.09.2020
  2 68    ; Since 18.03.2021
  2 70    ; Since 17.09.2021
  2 72    ; Since 17.03.2022
  2 74    ; Since 17.09.2022
)

(require-library-version "GLib" 2 56
                         +glib-init-major-version+
                         +glib-init-minor-version+)

;;; --- End of file glib.init.lisp ---------------------------------------------
