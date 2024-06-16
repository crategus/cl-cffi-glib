(in-package :glib-test)

(def-suite glib-stable-pointer :in glib-suite)
(in-suite glib-stable-pointer)

(defparameter glib-stable-pointer
              '(glib::allocate-stable-pointer
                glib::free-stable-pointer
                glib::get-stable-pointer-value
                glib::get-stable-pointers-length
                glib::get-stable-pointers-counter
                glib::get-stable-pointers
                glib::get-stable-pointers-array))

(export 'glib-stable-pointer)

;;; ----------------------------------------------------------------------------

(test glib-stable-pointer-value
  (let* ((length (glib::get-stable-pointers-length))
         (counter (glib::get-stable-pointers-counter))
         (func (lambda () 88))
         (ptr (glib:allocate-stable-pointer func)))
    (is (<= counter length))
    (is (= (1+ counter) (glib::get-stable-pointers-counter)))
    (is (every (lambda (x) (or (null x)
                               (cffi:pointerp x)
                               (listp x)
                               (functionp x)))
                 (glib::get-stable-pointers)))
    (is (= 88 (funcall (glib:get-stable-pointer-value ptr))))
    (is-false (glib:free-stable-pointer ptr))
    (is-false (glib:get-stable-pointer-value ptr))
    (is (<= counter length))
    (is (= counter (glib::get-stable-pointers-counter)))
    (is (every (lambda (x) (or (null x)
                               (cffi:pointerp x)
                               (listp x)
                               (functionp x)))
                 (glib::get-stable-pointers)))))

(test glib-stable-poiner-array
  (let ((pointers (glib::get-stable-pointers)))
    (iter (for ptr in pointers)
          (for i from 0)
          (is (equal ptr
                     (aref (glib::get-stable-pointers-array) i))))))

(test glib-with-stable-pointer
  (flet ((func () 888))
    (let ((length (glib::get-stable-pointers-length))
          (counter (glib::get-stable-pointers-counter)))
      (glib:with-stable-pointer (ptr #'func)
        (is (<= counter length))
        (is (= (1+ counter) (glib::get-stable-pointers-counter)))
        (is (= 888 (funcall (glib:get-stable-pointer-value ptr)))))
      (is (<= counter length))
      (is (= counter (glib::get-stable-pointers-counter))))))

;;; 2024-6-15
