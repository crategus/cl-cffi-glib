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

(defmethod parse-g-value-for-type (gvalue
                                   (gtype (eql (glib:gtype "GBoxed")))
                                   kind)
  (declare (ignore kind))
  (if (gtype= (value-type gvalue) (type-strv))
      ;; Handle the special case for the GStrv type
      (cffi:convert-from-foreign (value-boxed gvalue)
                                 '(glib:strv-t :free-from-foreign nil))
      (let ((info (glib:get-boxed-info (value-type gvalue))))
        (boxed-parse-g-value gvalue info))))

(defgeneric boxed-parse-g-value (gvalue info))

(defmethod boxed-parse-g-value (gvalue (info glib:boxed-opaque-info))
  (cffi:translate-from-foreign (glib:boxed-copy-fn info (value-boxed gvalue))
                               (glib:make-boxed-type info :returnp nil)))

(defmethod boxed-parse-g-value (gvalue (info glib:boxed-cstruct-info))
  (cffi:translate-from-foreign (value-boxed gvalue)
                               (glib:make-boxed-type info :returnp nil)))

(defmethod boxed-parse-g-value (gvalue (info glib:boxed-variant-info))
  (cffi:translate-from-foreign (value-boxed gvalue)
                               (glib:make-boxed-type info :returnp nil)))

;;; ----------------------------------------------------------------------------

(defmethod set-g-value-for-type (gvalue
                                 (gtype (eql (glib:gtype "GBoxed")))
                                 value)
  (if (gtype= (value-type gvalue) (type-strv))
      ;; Handle the special case for the GStrv type
      (setf (value-boxed gvalue)
            (cffi:convert-to-foreign value
                                     '(glib:strv-t :free-from-foreign nil)))
      (let ((info (glib:get-boxed-info (value-type gvalue))))
        (boxed-set-g-value gvalue info value))))

(defgeneric boxed-set-g-value (gvalue info value))

(defmethod boxed-set-g-value (gvalue
                              (info glib:boxed-opaque-info)
                              value)
  (setf (value-boxed gvalue) ; must be value-boxed and not value-take-boxed
        (cffi:translate-to-foreign value
                                   (glib:make-boxed-type info :returnp nil))))

(defmethod boxed-set-g-value (gvalue
                              (info glib:boxed-cstruct-info)
                              value)
  (value-take-boxed gvalue
                    (cffi:translate-to-foreign value
                                               (glib:make-boxed-type info
                                                                :returnp nil))))

(defmethod boxed-set-g-value (gvalue
                              (info glib:boxed-variant-info)
                              value)
  (value-take-boxed gvalue
                    (cffi:translate-to-foreign value
                                               (glib:make-boxed-type info
                                                                :returnp nil))))

;;; --- End of file gobject.boxed-lisp.lisp ------------------------------------
