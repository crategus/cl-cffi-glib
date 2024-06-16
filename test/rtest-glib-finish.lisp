(in-package :glib-test)

(in-suite glib-test)

(test glib-test-finished
  (cond (*first-run-glib-test*
         (setf *first-run-glib-test* nil)
         (format t "~%First run of the glib-test suite finished.~%"))
        (t
         (format t "~%Second or more run of the glib-test suite finished.~%"))))

;;; 2024-6-14
