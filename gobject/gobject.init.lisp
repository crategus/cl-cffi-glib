;;; ----------------------------------------------------------------------------
;;; gobject.init.lisp
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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

(in-package :gobject)

(defvar *generated-types* nil)

(defvar *gobject-debug* nil)
(defvar *debug-gc* nil)
(defvar *debug-subclass* nil)
(defvar *debug-stream* t)

(export '*gobject-debug*)
(export '*debug-gc*)
(export '*debug-subclass*)

(defmacro log-for (categories control-string &rest args)
  (let ((vars (iter (for sym in (if (listp categories)
                                    categories
                                    (list categories)))
                    (collect (intern (format nil "*DEBUG-~A*"
                                             (symbol-name sym))
                                     (find-package :gobject))))))
    `(progn
       (when (or ,@vars)
         (format *debug-stream* ,control-string ,@args))
       nil)))

(defmacro ev-case (keyform &body clauses)
  "Macro that is an analogue of CASE except that it evaluates keyforms"
  (let ((value (gensym)))
    `(let ((,value ,keyform))
       (cond
         ,@(loop
              for (key . forms) in clauses
              collect
                (if (eq key t)
                    `(t ,@forms)
                    `((equalp ,key ,value) ,@forms)))))))

#+nil ; not needed
(defmacro with-unwind ((var expr unwind-function) &body body)
  `(let ((,var ,expr))
     (unwind-protect
       (progn ,@body)
       (,unwind-function ,var))))

;;; ----------------------------------------------------------------------------

;; Manually frees the Lisp reference to the object. Probably should not be
;; called. object is an instance of g:object

(defgeneric release (object))

(defmethod release ((object null)))

;; Calls release on all objects in objects.
;; objects is a list of instances of g:object

(defun release* (&rest objects)
  (declare (dynamic-extent objects))
  (iter (for object in objects)
        (release object)))

(defmacro using ((var &optional (expr var)) &body body)
  `(let ((,var ,expr))
     (unwind-protect
       (progn ,@body)
       (release ,var))))

(defun ensure-list (thing)
  (if (listp thing)
      thing
      (list thing)))

(defun using-expand (bindings body)
  (if bindings
      (destructuring-bind (var &optional (expr var))
          (ensure-list (first bindings))
       `(let ((,var ,expr))
          (unwind-protect
            ,(using-expand (rest bindings) body)
            (release ,var))))
      `(progn ,@body)))

(defmacro using* ((&rest bindings) &body body)
  (using-expand bindings body))

;;; --- End of file gobject.init.lisp ------------------------------------------
