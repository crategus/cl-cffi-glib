;;; ----------------------------------------------------------------------------
;;; gio.content-type.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; GContentType
;;;
;;;     Platform-specific content typing
;;;
;;; Functions
;;;
;;;     g_content_type_equals
;;;     g_content_type_is_a
;;;     g_content_type_is_mime_type
;;;     g_content_type_is_unknown
;;;     g_content_type_get_description
;;;     g_content_type_get_mime_type
;;;     g_content_type_set_mime_dirs
;;;     g_content_type_get_mime_dirs
;;;     g_content_type_get_icon
;;;     g_content_type_get_symbolic_icon
;;;     g_content_type_get_generic_icon_name
;;;     g_content_type_can_be_executable
;;;     g_content_type_from_mime_type
;;;     g_content_type_guess                                not implemented
;;;     g_content_type_guess_for_tree                       not implemented
;;;     g_content_types_get_registered
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; g_content_type_equals
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_equals" content-type-equals) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype1]{a content type string}
  @argument[ctype2]{a content type string}
  @begin{return}
    @em{True} if the two content types are identical or equivalent, @em{false}
    otherwise.
  @end{return}
  @begin{short}
    Compares two content types for equality.
  @end{short}
  @see-function{g:content-type-is-a}"
  (ctype1 :string)
  (ctype2 :string))

(export 'content-type-equals)

;;; ----------------------------------------------------------------------------
;;; g_content_type_is_a
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_is_a" content-type-is-a) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a content type string}
  @argument[supertype]{a content type string}
  @begin{return}
    @em{True} if @arg{ctype} is a kind of @arg{supertype}, @em{false} otherwise.
  @end{return}
  @begin{short}
    Determines if @arg{ctype} is a subset of @arg{supertype}.
  @end{short}
  @see-function{g:content-type-is-mime-type}"
  (ctype :string)
  (supertype :string))

(export 'content-type-is-a)

;;; ----------------------------------------------------------------------------
;;; g_content_type_is_mime_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_is_mime_type" content-type-is-mime-type) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a content type string}
  @argument[mimetype]{a MIME type string}
  @begin{return}
    @em{True} if @arg{ctype} is a kind of @arg{mimetype}, @em{false} otherwise.
  @end{return}
  @begin{short}
    Determines if @arg{ctype} is a subset of @arg{mimetype}.
  @end{short}
  Convenience wrapper around the @fun{g:content-type-is-a} function.
  @see-function{g:content-type-is-a}"
  (ctype :string)
  (mimetype :string))

(export 'content-type-is-mime-type)

;;; ----------------------------------------------------------------------------
;;; g_content_type_is_unknown
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_is_unknown" content-type-is-unknown) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a content type string}
  @return{@em{True} if @arg{ctype} is the unknown content type.}
  @begin{short}
    Checks if the content type is the generic \"unknown\" content type.
  @end{short}
  On UNIX this is the @code{\"application/octet-stream\"} MIME type, while on
  Win32 it is @code{\"*\"}.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:content-type-is-unknown \"application/octet-stream\") => T
    @end{pre}
  @end{dictionary}"
  (ctype :string))

(export 'content-type-is-unknown)

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_description
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_get_description" content-type-description)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[ctype]{a content type string}
  @begin{return}
    The string for a short description of the content type.
  @end{return}
  @begin{short}
    Gets the human readable description of the content type.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:content-type-description \"text/plain\")
=> \"Einfaches Textdokument\"
    @end{pre}
  @end{dictionary}
  @see-function{g:content-types-registered}"
  (ctype :string))

(export 'content-type-description)

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_mime_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_get_mime_type" content-type-mime-type) :string
 #+liber-documentation
 "@version{2025-09-27}
  @argument[ctype]{a content type string}
  @begin{return}
    The string for the registered MIME type for the given @arg{ctype}, or
    @code{nil} if unknown.
  @end{return}
  @begin{short}
    Gets the MIME type for the content type, if one is registered.
  @end{short}
  @see-function{g:content-type-is-mime-type}"
  (ctype :string))

(export 'content-type-mime-type)

;;; ----------------------------------------------------------------------------
;;; g_content_type_set_mime_dirs
;;; g_content_type_get_mime_dirs                            not implemented
;;; ---------------------------------------------------------------------------

;; FIXME: This implementation does not work. The type conversion glib:strv-t
;; is not the correct implementation to pass in the arguments.

#+nil
(defun (setf content-type-mime-dirs) (dirs)
  (cffi:foreign-funcall "g_content_type_set_mime_dirs"
                        (glib:strv-t :free-to-foreign nil) dirs
                        :void)
  dirs)

(cffi:defcfun ("g_content_type_get_mime_dirs" content-type-mime-dirs)
    (glib:strv-t :free-from-foreign nil)
 #+liber-documentation
 "@version{2025-05-01}
  @syntax{(g:content-type-mime-dirs) => dirs}
  @argument[dirs]{a list of directories to load MIME data from, including any
    @file{file/} subdirectory, and with the first directory to try listed first}
  @begin{short}
    The @fun{g:content-type-mime-type} function gets the list of directories
    which MIME data is loaded from.
  @end{short}
  This function is intended to be used when writing tests that depend on
  information stored in the MIME database, in order to control the data.
  @begin[Notes]{dictionary}
    The corresponding @code{g_content_type_set_mime_dirs()} setter function is
    not implemented for the Lisp binding.
  @end{dictionary}")

(export 'content-type-mime-dirs)

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_get_icon" content-type-icon)
    (gobject:object icon :return)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a content type string}
  @begin{return}
    The @class{g:icon} object corresponding to the content type.
  @end{return}
  @begin{short}
    Gets the icon for a content type.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:content-type-icon \"text/plain\")
=> #<GIO:THEMED-ICON {10089505F3@}>
    @end{pre}
  @end{dictionary}
  @see-class{g:icon}
  @see-function{g:content-type-symbolic-icon}
  @see-function{g:content-type-generic-icon-name}"
  (ctype :string))

(export 'content-type-icon)

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_symbolic_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_get_symbolic_icon" content-type-symbolic-icon)
    (gobject:object icon :return)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a content type string}
  @begin{return}
    The symbolic @class{g:icon} object corresponding to the content type.
  @end{return}
  @short{Gets the symbolic icon for a content type.}
  @see-class{g:icon}
  @see-function{g:content-type-icon}
  @see-function{g:content-type-generic-icon-name}"
  (ctype :string))

(export 'content-type-symbolic-icon)

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_generic_icon_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_get_generic_icon_name"
                content-type-generic-icon-name) (:string :free-from-foreign t)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[ctype]{a content type string}
  @begin{return}
    The string for the registered generic icon name for the given @arg{ctype},
    or @code{nil} if unknown.
  @end{return}
  @begin{short}
    Gets the generic icon name for a content type.
  @end{short}
  See the
  @url[https://www.freedesktop.org/wiki/Specifications/shared-mime-info-spec/]{shared-mime-info specification}
  for more on the generic icon name.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:content-type-generic-icon-name \"text/plain\")
=> \"text-x-generic\"
    @end{pre}
  @end{dictionary}
  @see-function{g:content-type-icon}
  @see-function{g:content-type-symbolic-icon}"
  (ctype :string))

(export 'content-type-generic-icon-name)

;;; ----------------------------------------------------------------------------
;;; g_content_type_can_be_executable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_can_be_executable"
                content-type-can-be-executable) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a content type string}
  @begin{return}
    @em{True} if the file type corresponds to a content type that can be
    executable, @em{false} otherwise.
  @end{return}
  @begin{short}
    Checks if a content type can be executable.
  @end{short}
  Note that for instance things like text files can be executables, that is
  scripts and batch files."
  (ctype :string))

(export 'content-type-can-be-executable)

;;; ----------------------------------------------------------------------------
;;; g_content_type_from_mime_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_type_from_mime_type" content-type-from-mime-type)
    :string
 #+liber-documentation
 "@version{2025-09-27}
  @argument[mimetype]{a MIME type string}
  @return{The string for the content type or @code{nil}.}
  @begin{short}
    Tries to find a content type based on the MIME type name.
  @end{short}"
  (mimetype :string))

(export 'content-type-from-mime-type)

;;; ----------------------------------------------------------------------------
;;; g_content_type_guess ()
;;;
;;; Guesses the content type based on example data. If the function is
;;; uncertain, result_uncertain will be set to TRUE. Either filename or data may
;;; be NULL, in which case the guess will be based solely on the other argument.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_content_type_guess_for_tree ()
;;;
;;; Tries to guess the type of the tree with root root, by looking at the files
;;; it contains. The result is an array of content types, with the best guess
;;; coming first.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_content_types_get_registered
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_content_types_get_registered" content-types-registered)
    (glib:list-t (:string :free-from-foreign t) :free-from-foreign t)
 #+liber-documentation
 "@version{2025-05-01}
  @return{The list of strings with the registered content types.}
  @begin{short}
    Gets a list of strings containing all the registered content types known to
    the system.
  @end{short}
  @see-function{g:content-type-description}")

(export 'content-types-registered)

;;; --- End of file gio.content-type.lisp --------------------------------------
