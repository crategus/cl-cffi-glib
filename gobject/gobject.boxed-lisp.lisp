;;; ----------------------------------------------------------------------------
;;; gobject.boxed-lisp.lisp
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

(glib-init:at-init () (cffi:foreign-funcall "g_strv_get_type" :size))
(glib-init:at-init () (cffi:foreign-funcall "g_value_get_type" :size))

;;; ----------------------------------------------------------------------------

(defmethod get-gvalue-for-type
           (gvalue (gtype (eql (glib:gtype "GBoxed"))))
  (cond (;; Handle a GStrv type
         (eq (value-type gvalue) (glib:gtype "GStrv"))
         (cffi:convert-from-foreign (value-boxed gvalue)
                                    '(glib:strv-t :free-from-foreign nil)))
        ;; Handle a GValue type
        ((eq (value-type gvalue) (glib:gtype "GValue"))
         (value-boxed gvalue))
        (t
         (let ((info (glib:get-boxed-info (value-type gvalue))))
           (get-gvalue-boxed gvalue info)))))

(defgeneric get-gvalue-boxed (gvalue info))

(defmethod get-gvalue-boxed (gvalue (info glib:boxed-opaque-info))
  (cffi:translate-from-foreign (glib:boxed-copy-fn info (value-boxed gvalue))
                               (glib:make-boxed-type info :returnp nil)))

(defmethod get-gvalue-boxed (gvalue (info glib:boxed-cstruct-info))
  (cffi:translate-from-foreign (value-boxed gvalue)
                               (glib:make-boxed-type info :returnp nil)))

(defmethod get-gvalue-boxed (gvalue (info glib:boxed-variant-info))
  (cffi:translate-from-foreign (value-boxed gvalue)
                               (glib:make-boxed-type info :returnp nil)))

;;; ----------------------------------------------------------------------------

(defmethod set-gvalue-for-type
           (gvalue (gtype (eql (glib:gtype "GBoxed"))) value)
  (cond (;; Handle a GStrv type
         (eq (value-type gvalue) (glib:gtype "GStrv"))
         (setf (value-boxed gvalue)
               (cffi:convert-to-foreign value
                                        '(glib:strv-t :free-from-foreign nil))))
        ;; Handle a GValue type
        ((eq (value-type gvalue) (glib:gtype "GValue"))
         (setf (value-boxed gvalue) value))
        (t
         (let ((info (glib:get-boxed-info (value-type gvalue))))
           (set-gvalue-boxed gvalue info value)))))

(defgeneric set-gvalue-boxed (gvalue info value))

(defmethod set-gvalue-boxed (gvalue
                             (info glib:boxed-opaque-info)
                             value)
  (setf (value-boxed gvalue) ; must be value-boxed and not value-take-boxed
        (cffi:translate-to-foreign value
                                   (glib:make-boxed-type info :returnp nil))))

(defmethod set-gvalue-boxed (gvalue
                             (info glib:boxed-cstruct-info)
                             value)
  (value-take-boxed gvalue
                    (cffi:translate-to-foreign value
                                               (glib:make-boxed-type info
                                                                :returnp nil))))

(defmethod set-gvalue-boxed (gvalue
                             (info glib:boxed-variant-info)
                             value)
  (value-take-boxed gvalue
                    (cffi:translate-to-foreign value
                                               (glib:make-boxed-type info
                                                                :returnp nil))))

;;; --- End of file gobject.boxed-lisp.lisp ------------------------------------
