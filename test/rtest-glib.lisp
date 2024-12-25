(defpackage :glib-test
  (:use :fiveam :iterate :common-lisp)
  (:import-from :cffi)
  (:import-from :glib)
  (:import-from :gobject)
  (:import-from :gio)
  (:import-from :glib-sys)
  (:export #:*first-run-testsuite*
           #:run!
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
           #:profile #:unprofile #:report #:reset
           #:with-ref-count-object
           #:with-ref-count-objects
           #:with-check-memory
           #:with-check-memory-new))

(in-package :glib-test)

(defvar *first-run-testsuite* t)

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

(defun run-repeat (tests &key (count 1) (on-error nil) (linecount 50))
  (let ((fiveam:*on-error* on-error)
        (count (if (> count 0) (1- count) 0)))
    (format t "~&Run tests ~a times:~%" (1+ count))
    (format t "~6d " linecount)
    (let ((*test-dribble* nil)
          (*first-run-testsuite* nil))  ; Do not repeat tests for single run
      (dotimes (i count)
        (if (= 0 (mod (1+ i) linecount))
            (progn
              (format t ".~%")
              (format t "~6d " (+ 1 i linecount)))
            (format t "."))
        (fiveam:run tests)))
    (format t ".~%")
    ;; Explain the last run
    (let ((*first-run-testsuite* t)) ; Do all tests
      (fiveam:explain! (fiveam:run tests)))))

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

;;; ----------------------------------------------------------------------------

(defmacro with-ref-count-object ((var &optional (refcount 1)) &body body)
  `(let (,var)
     (progn ,@body)
     (is (= ,refcount (g:object-ref-count ,var))
         "~a holds ~a reference(s) (expected ~a)"
         ,var
         (g:object-ref-count ,var)
         ,refcount)))

(defmacro with-ref-count-objects (vars &body body)
    (if vars
        (let ((var (glib-sys:mklist (first vars))))
          `(with-ref-count-object ,var
             (with-ref-count-objects ,(rest vars)
               ,@body)))
        `(progn ,@body)))

#+nil
(defmacro with-check-memory ((vars &key (strong 0)) &body body)
  (setf vars (glib-sys:mklist vars))
  (let ((nptr1 (gensym))
        (lptr1 (gensym))
        (nptr2 (gensym))
        (lptr2 (gensym)))
    `(let* ((,lptr1 (gobject::list-gobject-for-pointer-strong))
            (,nptr1 (length ,lptr1)))
       (with-ref-count-objects ,vars
          ,@body)
       (let* ((,lptr2 (gobject::list-gobject-for-pointer-strong))
              (,nptr2 (length ,lptr2)))
         (is (= ,strong (- ,nptr2 ,nptr1))
             "The test added ~a (expected ~a) strong references for~%     ~a"
             (- ,nptr2 ,nptr1)
             ,strong
             (set-difference ,lptr2 ,lptr1 :test #'eq))))))

(defmacro with-check-memory (args &body body)
  (let* ((keys (member-if #'keywordp args))
         (vars (ldiff args keys))
         (strong (if keys (second keys) 0)))
  (let ((nptr1 (gensym))
        (lptr1 (gensym))
        (nptr2 (gensym))
        (lptr2 (gensym)))
    (if vars
        `(let* ((,lptr1 (gobject::list-gobject-for-pointer-strong))
                (,nptr1 (length ,lptr1)))
           (with-ref-count-objects ,vars
              ,@body)
           (let* ((,lptr2 (gobject::list-gobject-for-pointer-strong))
                  (,nptr2 (length ,lptr2)))
             (is (>= ,strong (- ,nptr2 ,nptr1))
                 "The test added ~a (expected ~a) strong references:~%     ~a"
                 (- ,nptr2 ,nptr1)
                 ,strong
                 (set-difference ,lptr2 ,lptr1 :test #'eq))))
        `(let* ((,lptr1 (gobject::list-gobject-for-pointer-strong))
                (,nptr1 (length ,lptr1)))
           (progn ,@body)
           (let* ((,lptr2 (gobject::list-gobject-for-pointer-strong))
                  (,nptr2 (length ,lptr2)))
             (is (>= ,strong (- ,nptr2 ,nptr1))
                 "The test added ~a (expected ~a) strong references:~%     ~a"
                 (- ,nptr2 ,nptr1)
                 ,strong
                 (set-difference ,lptr2 ,lptr1 :test #'eq))))))))

;;; 2024-12-16
