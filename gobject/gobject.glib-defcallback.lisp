;;; ----------------------------------------------------------------------------
;;; gobject.glib-defcallback.lisp
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

(in-package :gobject)

(defun wrap-body-with-boxed-translations (args body)
  (if (null args)
      body
      (let ((arg (first args)))
        (destructuring-bind (arg-name arg-type) arg
          (if (and (listp arg-type) (eq 'boxed (first arg-type)))
              (let ((var (gensym))
                    (cffi-type (cffi::parse-type arg-type)))
                `((let ((,var ,arg-name)
                        (,arg-name (cffi:translate-from-foreign ,arg-name
                                                                ,cffi-type)))
                    (unwind-protect
                      (progn
                        ,@(wrap-body-with-boxed-translations (rest args) body))
                      (cleanup-translated-object-for-callback ,cffi-type
                                                              ,arg-name
                                                              ,var)))))
              (wrap-body-with-boxed-translations (rest args) body))))))

(defmacro glib-defcallback (name-and-options return-type args &body body)
  (let* ((c-args (iter (for arg in args)
                       (for (name type) = arg)
                       (if (and (listp type) (eq 'boxed (first type)))
                           (collect `(,name :pointer))
                           (collect arg))))
         (c-body (wrap-body-with-boxed-translations args body)))
    `(cffi:defcallback ,name-and-options ,return-type ,c-args ,@c-body)))

;;; --- End of file gobject.glib-defcallback.lisp ------------------------------
