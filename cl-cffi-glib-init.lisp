;;; ----------------------------------------------------------------------------
;;; glib.init.lisp
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
(progn
  (pushnew 'run-initializers sb-ext:*init-hooks*)
  (pushnew 'run-finalizers sb-ext:*save-hooks*))

#+openmcl
(progn
  (pushnew 'run-initializers ccl:*restore-lisp-functions*)
  (pushnew 'run-finalizers ccl:*save-exit-functions*))

;;; ----------------------------------------------------------------------------
;;; at-init (keys body)
;;;
;;; Runs the code normally but also schedules the code to be run at image load
;;; time. It is used to reinitialize the libraries when the dumped image is
;;; loaded. Works only on SBCL for now.
;;;
;;; The AT-INIT form may be called multiple times. The same code should not be
;;; run multiple times at initialization time. In best case, this will only
;;; slow down initialization, in worst case, the code may crash. To ensure
;;; this, every AT-INIT expression is added to a hash-table with the BODY and
;;; KEYS as a composite key. This ensures that the same code is only executed
;;; once on the same set of parameters.
;;;
;;; Example:
;;;
;;; (defmethod initialize-instance :after ((class gobject-class)
;;;                                        &key &allow-other-keys)
;;;   (setf (glib:symbol-for-gtype (gobject-class-g-type-name class)
;;;                                (class-name class))
;;;   (at-init (class) (initialize-gobject-class-g-type class)))
;;;
;;; In this example, for every class, (initialize-gobject-class-g-type class)
;;; will be called only once.
;;; ----------------------------------------------------------------------------

(defmacro at-init ((&rest keys) &body body)
  `(progn
     (register-initializer (list ,@keys ',body) (lambda () ,@body))
     ,@body))

(defmacro at-finalize ((&rest keys) &body body)
  `(register-finalizer (list ,@keys ',body) (lambda () ,@body)))

;;; ----------------------------------------------------------------------------

;;; Load the foreign libraries Glib, GObject, GIO and GThread

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
   (cffi:define-foreign-library gobject
     ((:and :unix (:not :darwin))
      (:or "libgobject-2.0.so.0" "libgobject-2.0.so"))
     (:darwin (:or "libgobject-2.0.0.dylib" "libgobject-2.0.dylib"))
     (:windows "libgobject-2.0-0.dll")
     (t "libgobject-2.0")))
 (cffi:use-foreign-library gobject))

(glib-init:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:define-foreign-library gio
      ((:and :unix (:not :darwin))
       (:or "libgio-2.0.so.0" "libgio-2.0.so"))
      (:darwin "libgio-2.0.dylib")
      (:windows "libgio-2.0-0.dll")
      (t (:default "libgio-2.0"))))
  (cffi:use-foreign-library gio))

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

(define-condition foreign-library-version-mismatch (error)
  ((library :initarg :library :reader .library)
   (minimum-version :initarg :minimum-version :reader .minimum-version)
   (actual-version :initarg :actual-version :reader .actual-version))
  (:report (lambda (c s)
             (format s "Library ~A has too old version: it is ~A but required ~
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
      (error 'foreign-library-version-mismatch
             :library library
             :minimum-version (format nil "~A.~A"
                                      min-major-version min-minor-version)
             :actual-version (format nil "~A.~A"
                                     major-version minor-version))
      (ignore () :report "Ignore version requirement" nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; push the hostname on *features*
  (pushnew (intern (string-upcase (machine-instance)) :keyword) *features*)
  (pushnew :glib *features*))

(cffi:defcvar
  ("glib_major_version" +glib-init-major-version+ :read-only t) :uint)
(cffi:defcvar
  ("glib_minor_version" +glib-init-minor-version+ :read-only t) :uint)

(push-library-version-features glib
                               +glib-init-major-version+
                               +glib-init-minor-version+
  2 60    ; Since 2019-03-04
  2 62    ; Since 2019-09-05
  2 64    ; Since 2020-02-27
  2 66    ; Since 2020-09-10
  2 68    ; Since 2021-03-18
  2 70    ; Since 2021-09-17
  2 72    ; Since 2022-03-17
  2 74    ; Since 2022-09-17
  2 76    ; Since 2023-03-10
  2 78    ; Since 2023-09-08
  2 80    ; Since 2024-03-07
  2 82    ; Since 2024-08-26
)

(require-library-version "GLib" 2 60
                         +glib-init-major-version+
                         +glib-init-minor-version+)

;;; --- End of file glib.init.lisp ---------------------------------------------
