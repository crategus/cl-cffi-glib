(in-package :glib-test)

(in-suite glib-test)

(test glib-test-finished
  (cond (*first-run-testsuite*
         (setf *first-run-testsuite* nil)
         (when *test-dribble*
           (format t "~%First run of the glib-test suite finished.~%")))
        (t
         (when *test-dribble*
           (format t "~%Second or more run of the glib-test suite finished.~%")))))

;;; 2024-12-23
