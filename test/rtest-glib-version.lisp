(in-package :glib-test)

(def-suite glib-version :in glib-suite)
(in-suite glib-version)

(test push-library-version-features
  (is (member :glib      *features*))
  #+glib-2-64
  (is (member :glib-2-64 *features*)))

#+crategus
(test check-glib-version
  (is (= 2 glib:+major-version+))
  (is (= 84 glib:+minor-version+)))

(test g-check-version
  (is-true (integerp glib:+major-version+))
  (is-true (integerp glib:+minor-version+))
  (is-true (integerp glib:+micro-version+))
  (is-false (glib:check-version 2 64 0))
  (is (string= "GLib version too old (micro mismatch)"
               (glib:check-version 2 99 0))))

(test cl-cffi-glib-build-info
  (let ((result (make-array '(0) :element-type 'base-char
                                 :fill-pointer 0
                                 :adjustable t)))
    (with-output-to-string (s result)
      (is-false (glib:cl-cffi-glib-build-info s))
      (is (stringp result)))))

;;; 2025-05-18
