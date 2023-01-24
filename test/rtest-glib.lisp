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
                         #:with-g-option-group)
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
                         #:with-g-resource)
  (:export #:run!))

(in-package :glib-test)

(def-suite glib-test)

(def-suite glib-suite :in glib-test)
(in-suite glib-suite)

;; We set a PRGNAME to avoid side effects when running the tests a second time.
(setf (glib:prgname) "glib-test")

;;; --- 2023-1-3 ---------------------------------------------------------------
