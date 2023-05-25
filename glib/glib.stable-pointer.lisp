;;; ----------------------------------------------------------------------------
;;; glib.stable-pointer.lisp
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;; Allocates the stable pointer for thing. Stable pointer is an integer that
;; can be dereferenced with get-stable-pointer-value and freed with
;; free-stable-pointer. Stable pointers are used to pass references to lisp
;; objects to foreign code. thing is any object. The return value is an integer.

(let ((stable-pointers (make-array 0 :adjustable t :fill-pointer t))
      (stable-pointers-length 0)
      (stable-pointers-counter 0))

  (defun allocate-stable-pointer (thing)
    (flet ((find-fresh-id ()
             (or ;; Search a free place for the pointer
                 (position nil stable-pointers)
                 ;; Add a place for the pointer and increment the array length.
                 (progn
                   (vector-push-extend nil stable-pointers)
                   (1- (incf stable-pointers-length))))))
      (let ((id (find-fresh-id)))
        (incf stable-pointers-counter)
        (setf (aref stable-pointers id) thing)
        (cffi:make-pointer id))))

  ;; This is a workaround: We store a NULL-POINTER for the 0-index of the array.
  ;; This way we do not use the NULL-POINTER to address the 0-indexed slot of
  ;; the array. This resolves a bug in the GOBJECT:TYPE-QDATA function.
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (allocate-stable-pointer (cffi:null-pointer)))

  ;; Frees the stable pointer previously allocated by allocate-stable-pointer

  (defun free-stable-pointer (stable-pointer)
    (decf stable-pointers-counter)
    (setf (aref stable-pointers (cffi:pointer-address stable-pointer)) nil))

  ;; Returns the objects that is referenced by stable pointer previously
  ;; allocated by allocate-stable-pointer. May be called any number of times.

  (defun get-stable-pointer-value (pointer)
    (let ((ptrid (cffi:pointer-address pointer)))
      (when (<= 0 ptrid stable-pointers-length)
        (aref stable-pointers ptrid))))

  ;; Replacement for the SET-STABLE-POINTER-VALUE function
  (defun (setf get-stable-pointer-value) (data pointer)
    (let ((ptrid (cffi:pointer-address pointer)))
      (when (<= 0 ptrid stable-pointers-length)
        (setf (aref stable-pointers ptrid) data))))

  (defun set-stable-pointer-value (pointer data)
    (let ((ptrid (cffi:pointer-address pointer)))
      (when (<= 0 ptrid stable-pointers-length)
        (setf (aref stable-pointers ptrid) data))))

  ;; The following functions are for debugging and inspecting.

  (defun get-stable-pointers-length ()
    stable-pointers-length)

  (defun get-stable-pointers-counter ()
    stable-pointers-counter)

  (defun get-stable-pointers ()
    (iter (for i from 0 below stable-pointers-length)
          (collect (aref stable-pointers i))))

  (defun get-stable-pointers-array ()
    stable-pointers)
)

;; Executes body with ptr bound to the stable pointer to result of evaluating
;; expr. ptr is a symbol naming the variable which will hold the stable pointer
;; value and expr is an expression

(defmacro with-stable-pointer ((ptr expr) &body body)
  `(let ((,ptr (allocate-stable-pointer ,expr)))
     (unwind-protect
       (progn ,@body)
       (free-stable-pointer ,ptr))))

;; Callback function to free a pointer

(defcallback stable-pointer-destroy-notify :void
    ((data :pointer))
  (free-stable-pointer data))

;;; --- End of file glib.stable-pointer.lisp -----------------------------------
