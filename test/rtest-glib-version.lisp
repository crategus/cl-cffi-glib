(in-package :glib-test)

(def-suite glib-version :in glib-suite)
(in-suite glib-version)

(test push-library-version-features
  (is (member :glib      *features*))
  #+glib-2-64
  (is (member :glib-2-64 *features*))
  #+glib-2-66
  (is (member :glib-2-66 *features*))
  #+glib-2-68
  (is (member :glib-2-68 *features*))
  #+glib-2-70
  (is (member :glib-2-70 *features*))
  #+glib-2-72
  (is (member :glib-2-72 *features*))
  #+glib-2-74
  (is (member :glib-2-74 *features*))
  #+glib-2-76
  (is (member :glib-2-76 *features*))
  #+glib-2-78
  (is (member :glib-2-78 *features*))
  #+glib-2-80
  (is (member :glib-2-80 *features*))
  #+glib-2-82
  (is (member :glib-2-82 *features*)))

#-windows
(test check-glib-version
  (is (= 2 glib:+major-version+))
  (is (= 82 glib:+minor-version+)))

#+windows
(test check-glib-version
  (is (= 2 glib:+major-version+))
  (is (= 84 glib:+minor-version+)))

(test g-check-version
  (is-true (integerp glib:+major-version+))
  (is-true (integerp glib:+minor-version+))
  (is-true (integerp glib:+micro-version+))
  (is-false (glib:check-version 2 72 0))
  (is (string= "GLib version too old (micro mismatch)"
               (glib:check-version 2 99 0))))

(test cl-cffi-glib-build-info
  (let ((result (make-array '(0) :element-type 'base-char
                                 :fill-pointer 0
                                 :adjustable t)))
    (with-output-to-string (s result)
      (is-false (glib:cl-cffi-glib-build-info s))
      (is (stringp result)))))

;;; 2025-05-01
