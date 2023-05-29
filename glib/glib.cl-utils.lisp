;;; ----------------------------------------------------------------------------
;;; glib.cl-utils.lisp
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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

(defvar *debug-stream* t)

;;; ----------------------------------------------------------------------------
;;; log-for
;;;
;;;      Print debug information
;;;
;;; Usage is as following. Define a global variable, e.g.;
;;;
;;;     (defvar *debug-gboxed-gc* nil)
;;;
;;; Use the corresponding keyword in the code. By default no debug information
;;; is shown. Set the global *debug-gboxed-gc* variable to T to get debug
;;; information.
;;;
;;;     (log-for :debug-gboxed-gc
;;;              "Activate gc hooks for boxed opaque: ~A~%" 
;;;              *gboxed-gc-hooks*)
;;; ----------------------------------------------------------------------------

(defmacro log-for (categories control-string &rest args)
  (let ((vars (iter (for sym in (if (listp categories)
                                    categories
                                    (list categories)))
                    (collect (intern (format nil "*DEBUG-~A*"
                                             (symbol-name sym))
                                     (find-package :glib))))))
    `(progn
       (when (or ,@vars)
         (format *debug-stream* ,control-string ,@args))
       nil)))

;;; --- End of file glib.cl-utils.lisp -----------------------------------------
