;;; ----------------------------------------------------------------------------
;;; gio.file-icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2021 - 2024 Dieter Kaiser
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
;;;
;;; GFileIcon
;;;
;;;     Icons pointing to an image file
;;;
;;; Types and Values
;;;
;;;     GFileIcon
;;;
;;; Functions
;;;
;;;     g_file_icon_new
;;;     g_file_icon_get_file
;;;
;;; Properties
;;;
;;;     file
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GFileIcon
;;;
;;; Implemented Interfaces
;;;
;;;     GFileIcon implements GIcon and GLoadableIcon
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GFileIcon
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GFileIcon" file-icon
  (:superclass gobject:object
   :export t
   :interfaces ("GIcon"
                "GLoadableIcon")
   :type-initializer "g_file_icon_get_type")
  ((file
    file-icon-file
    "file" "GFile" t nil)))

#+liber-documentation
(setf (documentation 'file-icon 'type)
 "@version{2024-12-18}
  @begin{short}
    The @class{g:file-icon} class specifies an icon by pointing to an image
    file to be used as icon.
  @end{short}
  @see-constructor{g:file-icon-new}
  @see-slot{g:file-icon-file}
  @see-class{g:icon}
  @see-class{g:loadable-icon}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:file-icon-file -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'file-icon) t)
 "The @code{file} property of type @class{g:file}
  (Read / Write / Construct Only) @br{}
  The file containing the icon.")

#+liber-documentation
(setf (liber:alias-for-function 'file-icon-file)
      "Accessor"
      (documentation 'file-icon-file 'function)
 "@version{2024-10-23}
  @syntax{(g:file-icon-file object) => file}
  @argument[object]{a @class{g:file-icon} object}
  @argument[file]{a @class{g:file} object}
  @begin{short}
    Accessor of the @slot[g:file-icon]{file} slot of the @class{g:file-icon}
    class.
  @end{short}
  The @fun{g:file-icon-file} function gets the @class{g:file} object associated
  with the given icon.
  @see-class{g:file-icon}
  @see-class{g:file}")

;;; ----------------------------------------------------------------------------
;;; g_file_icon_new
;;; ----------------------------------------------------------------------------

;; TODO: Extend the implementation and support pathnames and namestrings

(cffi:defcfun ("g_file_icon_new" file-icon-new) (gobject:object icon :return)
 #+liber-documentation
 "@version{2024-10-23}
  @argument[file]{a @class{g:file} object}
  @return{The @class{g:icon} object for the given file, or @code{nil} on error.}
  @begin{short}
    Creates a new icon for a file.
  @end{short}
  @see-class{g:file-icon}
  @see-class{g:file}
  @see-class{g:icon}"
  (file gobject:object))

(export 'file-icon-new)

;;; --- End of file gio.file-icon.lisp -----------------------------------------
