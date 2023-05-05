(defpackage :glib-test
  (:use :fiveam :common-lisp)
  (:import-from :cffi    #:with-foreign-object
                         #:with-foreign-objects
                         #:with-foreign-slots
                         #:defcenum
                         #:defbitfield)
  (:import-from :glib    ;; Symbols from glib.stable-pointer.lisp
                         #:with-stable-pointer

                         ;; Import from glib.version.lisp
                         #:+glib-major-version+
                         #:+glib-minor-version+
                         #:+glib-micro-version+
                         #:cl-cffi-glib-build-info

                         ;; Import from glib.error.lisp
                         #:with-g-error
                         #:with-ignore-g-error
                         #:with-catching-to-g-error

                         ;; Symbols from glib.main-loop.lisp
                         #:+g-priority-high+
                         #:+g-priority-default+
                         #:+g-priority-high-idle+
                         #:+g-priority-default-idle+
                         #:+g-priority-low+
                         #:+g-source-continue+
                         #:+g-source-remove+

                         ;; Import from glib.option.lisp
                         #:with-g-option-context
                         #:with-g-option-group

                         ;; Import from glib.key-file.lisp
                         #:with-g-key-file)
  (:import-from :gobject #:+g-type-invalid+
                         #:+g-type-none+
                         #:+g-type-interface+
                         #:+g-type-char+
                         #:+g-type-uchar+
                         #:+g-type-boolean+
                         #:+g-type-int+
                         #:+g-type-uint+
                         #:+g-type-long+
                         #:+g-type-ulong+
                         #:+g-type-int64+
                         #:+g-type-uint64+
                         #:+g-type-enum+
                         #:+g-type-flags+
                         #:+g-type-float+
                         #:+g-type-double+
                         #:+g-type-string+
                         #:+g-type-pointer+
                         #:+g-type-boxed+
                         #:+g-type-param+
                         #:+g-type-object+
                         #:+g-type-gtype+
                         #:+g-type-variant+
                         #:+g-type-checksum+

                         #:parse-g-value
                         #:set-g-value

                         #:define-g-boxed-opaque

                         #:define-g-enum
                         #:define-g-flags
                         #:define-g-object-class
                         #:define-g-interface
                         #:get-enum-items
                         #:get-flags-items
                         #:enum-item-name
                         #:enum-item-nick
                         #:enum-item-value
                         #:flags-item-name
                         #:flags-item-nick
                         #:flags-item-value
                         #:get-g-type-definition
                         #:get-boxed-info)
  (:import-from :gio     #:define-g-enum
                         #:define-g-flags
                         #:define-g-object-class
                         #:with-g-resources)
  (:export #:run!))

(in-package :glib-test)

(def-suite glib-test)
(def-suite glib-suite :in glib-test)
(def-suite gobject-suite :in glib-test)
(def-suite gio-suite :in glib-test)

;; We set a PRGNAME to avoid side effects when running the tests a second time.
(setf (glib:prgname) "glib-test")

;; Ensure directory for the output of test results
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-directories-exist
      (asdf:system-relative-pathname :cl-cffi-glib "test/out/")))

;; Get the pathname for a file in the testsuite
(defun sys-path (filename &optional (system :cl-cffi-glib))
  (asdf:system-relative-pathname system
                                 (concatenate 'string "test/" filename)))

(defun list-children (gtype)
  (sort (mapcar #'g:type-name (g:type-children gtype))
        #'string<))

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

(defun list-interface-properties (gtype)
  (mapcar #'g:param-spec-name
          (g:object-interface-list-properties gtype)))

;; A sorted list of the signal names of a class
(defun list-signals (gtype)
  (sort (mapcar #'g:signal-name
                (g:signal-list-ids gtype)) #'string<))

(defun list-flags-item-name (gtype)
  (mapcar #'gobject:flags-item-name
          (gobject:get-flags-items gtype)))

(defun list-flags-item-nick (gtype)
  (mapcar #'gobject:flags-item-nick
          (gobject:get-flags-items gtype)))

(defun list-flags-item-value (gtype)
  (mapcar #'gobject:flags-item-value
          (gobject:get-flags-items gtype)))

(defun list-enum-item-name (gtype)
  (mapcar #'gobject:enum-item-name
          (gobject:get-enum-items gtype)))

(defun list-enum-item-nick (gtype)
  (mapcar #'gobject:enum-item-nick
          (gobject:get-enum-items gtype)))

(defun list-enum-item-value (gtype)
  (mapcar #'gobject:enum-item-value
          (gobject:get-enum-items gtype)))

;;; --- 2023-5-4 ---------------------------------------------------------------
