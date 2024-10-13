(in-package :glib-test)

(def-suite glib-version :in glib-suite)
(in-suite glib-version)

(test push-library-version-features
  (is (member :glib      *features*))
  (is (member :glib-2-60 *features*))
  (is (member :glib-2-62 *features*))
  (is (member :glib-2-64 *features*))
  (is (member :glib-2-66 *features*))
  (is (member :glib-2-68 *features*))
  (is (member :glib-2-70 *features*))
  (is (member :glib-2-72 *features*))
  (is (member :glib-2-74 *features*))
  (is (member :glib-2-76 *features*))
  (is (member :glib-2-78 *features*))
  (is (member :glib-2-80 *features*))
  (is (member :glib-2-82 *features*)))

(test check-glib-version
  (is (= 2 glib:+major-version+))
  (is (= 82 glib:+minor-version+)))

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

;;; 2024-10-13
