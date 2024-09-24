(defpackage :glib-test
  (:use :fiveam :iterate :common-lisp)
  (:import-from :cffi)
  (:import-from :glib)
  (:import-from :gobject)
  (:import-from :gio)
  (:import-from :glib-sys)
  (:export #:run!
           #:list-children
           #:list-interfaces
           #:list-properties
           #:list-interface-prerequisites
           #:list-interface-properties
           #:list-signals
           #:list-enum-item-names
           #:list-enum-item-values
           #:list-enum-item-nicks
           #:list-flags-item-names
           #:list-flags-item-values
           #:list-flags-item-nicks
           #:profile #:unprofile #:report #:reset))

(in-package :glib-test)

(defvar *first-run-glib-test* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Set the current package for the testsuite
  (setf (glib-sys:get-current-package) "cl-cffi-glib")
  ;; Set the package and the prefix for this testsuite
;  (setf gobject::*lisp-name-package* "G")
;  (setf gobject::*strip-prefix* "G")
  ;; Set a PRGNAME to avoid side effects when running the tests a second time
  (setf (glib:prgname) "glib-test")
  ;; Ensure directory for the output of test results
  (ensure-directories-exist
      (asdf:system-relative-pathname :cl-cffi-glib "test/out/")))

(def-suite glib-test)
(def-suite glib-suite :in glib-test)
(def-suite gobject-suite :in glib-test)
(def-suite gio-suite :in glib-test)

;;; ----------------------------------------------------------------------------

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(defun approx-equal (x y &optional (eps 1.0d-5))
  (or (< (abs (- x y)) eps)
      (< (abs (- x y)) (* eps (max (abs x) (abs y))))))

;;; ----------------------------------------------------------------------------

#+sbcl
(defun profile (&rest args)
  (let ((symbols (glib-sys:flatten args)))
    (if symbols
        (dolist (sym symbols)
          (eval `(sb-profile:profile ,sym)))
        (sb-profile:profile))))

#+sbcl
(defun report ()
  (sb-profile:report))

#+sbcl
(defun unprofile (&rest args)
  (let ((symbols (glib-sys:flatten args)))
    (if symbols
        (dolist (sym symbols)
          (eval `(sb-profile:unprofile ,sym)))
        (sb-profile:unprofile))))

#+sbcl
(defun reset ()
  (sb-profile:reset))

;;; ----------------------------------------------------------------------------

(defun list-children (gtype)
  (sort (mapcar #'g:type-name (g:type-children gtype)) #'string<))

(defun list-interfaces (gtype)
  (mapcar #'g:type-name (g:type-interfaces gtype)))

;; A sorted list of the class property names without inherited properties
(defun list-properties (gtype)
  (sort (set-difference (mapcar #'g:param-spec-name
                                (g:object-class-list-properties gtype))
                        (mapcar #'g:param-spec-name
                                (g:object-class-list-properties
                                  (g:type-parent gtype)))
                        :test #'string=)
        #'string<))

(defun list-interface-prerequisites (gtype)
  (mapcar #'g:type-name
          (g:type-interface-prerequisites gtype)))

(defun list-interface-properties (gtype)
  (mapcar #'g:param-spec-name
          (g:object-interface-list-properties gtype)))

;; A sorted list of the signal names of a class
(defun list-signals (gtype)
  (sort (mapcar #'g:signal-name
                (g:signal-list-ids gtype)) #'string<))

;;; ----------------------------------------------------------------------------

(defun list-flags-item-values (gtype)
  (mapcar #'gobject:flags-item-value
          (gobject:get-flags-items gtype)))

(defun list-flags-item-names (gtype)
  (mapcar #'gobject:flags-item-name
          (gobject:get-flags-items gtype)))

(defun list-flags-item-nicks (gtype)
  (mapcar #'gobject:flags-item-nick
          (gobject:get-flags-items gtype)))

(defun list-enum-item-values (gtype)
  (mapcar #'gobject:enum-item-value
          (gobject:get-enum-items gtype)))

(defun list-enum-item-names (gtype)
  (mapcar #'gobject:enum-item-name
          (gobject:get-enum-items gtype)))

(defun list-enum-item-nicks (gtype)
  (mapcar #'gobject:enum-item-nick
          (gobject:get-enum-items gtype)))

;;; 2024-9-17
