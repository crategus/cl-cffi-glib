;;; ----------------------------------------------------------------------------
;;; cl-cffi-glib-sys.lisp
;;;
;;; Copyright (C) 2024 Dieter Kaiser
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

(defpackage :glib-sys
  (:use :common-lisp)
  (:export #:mklist
           #:get-current-package
           #:sys-path
           #:check-and-create-resources))

(in-package :glib-sys)

;; Ensure OBJ is a list
(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(let ((current-package nil))

  ;; Get pathname for FILENAME in PACKAGE
  (defun sys-path (filename &optional (package current-package))
    (asdf:system-relative-pathname package filename))

  (defun get-current-package ()
    current-package)

  (defun (setf get-current-package) (value)
    (setf current-package value))

  ;; Check for RESOURCE in PACKAGE, create or update it when neccessary and
  ;; return the pathname of TARGET
  (defun check-and-create-resources (resource &key (package current-package)
                                                   (sourcedir nil)
                                                   (verbose nil))
    (let* ((source (sys-path resource package))
           (sourcedir (directory-namestring (sys-path sourcedir package)))
           (target (sys-path (make-pathname :name (pathname-name resource)
                                            :directory
                                            (pathname-directory resource)
                                            :type "gresource")
                             package)))
      (unless (and (probe-file target)
                   (> (file-write-date target)
                      (file-write-date source)))
        (when verbose
          (format t "Run GLIB-COMPILE-RESOUCRES~%")
          (format t "    source : ~a~%" source)
          (format t " sourcedir : ~a~%" sourcedir)
          (format t "    target : ~a~%" target))
        (uiop:run-program (list "glib-compile-resources"
                                "--sourcedir"
                                sourcedir
                                (namestring source))))
      target))
)

;;; --- End of file c-cffi-glib-sys.lisp ---------------------------------------
