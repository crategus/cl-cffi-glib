;;; ----------------------------------------------------------------------------
;;; liber.lisp
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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

#-liber-documentation
(push :liber-documentation *features*)

(asdf:load-system :liber)
(asdf:load-system :cl-cffi-glib :force t)

(defpackage :liber-glib
  (:use :common-lisp)
  (:import-from :liber)
  (:export :generate-html
           :generate-html-single-page))

(in-package :liber-glib)

(unexport 'glib:allocate-stable-pointer :glib)
(unexport 'glib:free-stable-pointer :glib)
(unexport 'glib:get-stable-pointer-value :glib)
(unexport 'glib:stable-pointer-destroy-notify :glib)
(unexport 'glib:with-stable-pointer :glib)
(unexport 'glib:with-error :glib)
(unexport 'glib:with-ignore-error :glib)
(unexport 'glib:with-catching-to-error :glib)
(unexport 'glib:with-gboxed-array :glib)
(unexport 'glib:boxed-copy-fn :glib)
(unexport 'glib:boxed-opaque-pointer :glib)
(unexport 'glib:cleanup-translated-object-for-callback :glib)
(unexport 'glib:define-gboxed-cstruct :glib)
(unexport 'glib:define-gboxed-opaque :glib)
(unexport 'glib:define-gboxed-variant-cstruct :glib)
(unexport 'glib:get-boxed-info :glib)
(unexport 'glib:make-boxed-type :glib)
(unexport 'glib:pointer :glib)
(unexport 'glib:type-initializer-call :glib)
(unexport 'glib:*warn-unknown-gtype* :glib)
(unexport 'glib:boxed-cstruct-info :glib)
(unexport 'glib:boxed-opaque-info :glib)
(unexport 'glib:boxed-variant-info :glib)

(unexport 'gobject:initially-unowned :gobject)
(unexport 'gobject:gobject-class :gobject)
(unexport 'gobject:create-closure-for-instance :gobject)
(unexport 'gobject:create-fn-ref :gobject)
(unexport 'gobject:define-cb-methods :gobject)
(unexport 'gobject:define-genum :gobject)
(unexport 'gobject:define-gflags :gobject)
(unexport 'gobject:define-ginterface :gobject)
(unexport 'gobject:define-gobject :gobject)
(unexport 'gobject:define-gobject-subclass :gobject)
(unexport 'gobject:define-vtable :gobject)
(unexport 'gobject:enum-item-name :gobject)
(unexport 'gobject:enum-item-nick :gobject)
(unexport 'gobject:enum-item-value :gobject)
(unexport 'gobject:flags-item-name :gobject)
(unexport 'gobject:flags-item-nick :gobject)
(unexport 'gobject:flags-item-value :gobject)
(unexport 'gobject:get-enum-items :gobject)
(unexport 'gobject:get-flags-items :gobject)
(unexport 'gobject:get-genum-definition :gobject)
(unexport 'gobject:get-gflags-definition :gobject)
(unexport 'gobject:get-gtype-definition :gobject)
(unexport 'gobject:get-gvalue :gobject)
(unexport 'gobject:get-gvalue-for-type :gobject)
(unexport 'gobject:get-lisp-name-exception :gobject)
(unexport 'gobject:set-gvalue :gobject)
(unexport 'gobject:set-gvalue-for-type :gobject)
(unexport 'gobject:create-closure :gobject)
(unexport 'gobject:*debug-gc* :gobject)
(unexport 'gobject:*debug-subclass* :gobject)
(unexport 'gobject:*gobject-debug* :gobject)

;;; ---------------------------------------------------------------------------

(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-glib)))
         (output-directory (merge-pathnames "../books/cl-cffi-glib/" base)))
    (format t "Generate HTML to ~a~%" output-directory)
    (liber:generate-html-documentation
      '(:glib :gobject :gio)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-glib API documentation"
      :heading "cl-cffi-glib"
      :css "crategus.css"
      :single-page-p nil
      :paginate-section-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-glib)))
         (output-directory
             (merge-pathnames "../books/cl-cffi-glib/single-page/" base)))
    (format t "Generate Single PAGE HTML to ~a~%" output-directory)
    (liber:generate-html-documentation
      '(:glib :gobject :gio)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-glib API documentation (single page)"
      :heading "cl-cffi-glib"
      :css "crategus.css"
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

;;; --- End of file liber.lisp -------------------------------------------------
