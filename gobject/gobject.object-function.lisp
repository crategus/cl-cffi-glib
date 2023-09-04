;;; ----------------------------------------------------------------------------
;;; gobject.object-function.lisp
;;;
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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

(cffi:defcstruct object-func-ref
  (:object :pointer)
  (:fn-id :int))

(defmacro define-cb-methods (name return-type (&rest args))
  (flet ((make-name (control-string)
           (intern (format nil control-string (symbol-name name))
                   (symbol-package name))))
    (let ((call-cb (make-name "~A"))
          (destroy-cb (make-name "~A-DESTROY-NOTIFY"))
          (object (gensym "OBJECT"))
          (fn-id (gensym "FN-ID"))
          (fn (gensym "FN"))
          (data (gensym "DATA"))
          (arg-names (mapcar #'first args)))
      `(progn
         (cffi:defcallback ,call-cb ,return-type (,@args (,data :pointer))
           (let* ((,object (cffi:convert-from-foreign
                               (cffi:foreign-slot-value ,data
                                                        '(:struct object-func-ref)
                                                        :object)
                               'object))
                  (,fn-id (cffi:foreign-slot-value ,data
                                                   '(:struct object-func-ref)
                                                   :fn-id))
                  (,fn (retrieve-handler-from-object ,object ,fn-id)))
             (funcall ,fn ,@arg-names)))
         (cffi:defcallback ,destroy-cb :void ((,data :pointer))
           (let* ((,object (cffi:convert-from-foreign
                               (cffi:foreign-slot-value ,data
                                                        '(:struct object-func-ref)
                                                        :object)
                               'object))
                  (,fn-id (cffi:foreign-slot-value ,data
                                                   '(:struct object-func-ref)
                                                   :fn-id)))
             (delete-handler-from-object ,object ,fn-id))
           (cffi:foreign-free ,data))))))

(defun create-fn-ref (object function)
  (let ((ref (cffi:foreign-alloc '(:struct object-func-ref)))
        (fn-id (save-handler-to-object object function)))
    (setf (cffi:foreign-slot-value ref '(:struct object-func-ref) :object)
          (object-pointer object)
          (cffi:foreign-slot-value ref '(:struct object-func-ref) :fn-id)
          fn-id)
    ref))

;;; --- End of file gobject.object-function.lisp -------------------------------
