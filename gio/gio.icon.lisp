;;; ----------------------------------------------------------------------------
;;; gio.icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;; GIcon
;;;
;;;     Interface for icons
;;;
;;; Types and Values
;;;
;;;     GIcon
;;;
;;; Functions
;;;
;;;     g_icon_hash
;;;     g_icon_equal
;;;     g_icon_to_string
;;;     g_icon_new_for_string
;;;     g_icon_serialize
;;;     g_icon_deserialize
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GIcon
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GIcon
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GIcon" icon
  (:export t
   :type-initializer "g_icon_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'icon)
      "Interface"
      (documentation 'icon 'type)
 "@version{2023-3-12}
  @begin{short}
    The @sym{g:icon} interface is a very minimal interface for icons.
  @end{short}
  It provides functions for checking the equality of two icons, hashing of
  icons and serializing an icon to and from strings.

  The @sym{g:icon} interface does not provide the actual pixmap for the icon
  as this is out of GIO's scope, however implementations of the @sym{g:icon}
  interface may contain the name of an icon, see the @class{g:themed-icon}
  class, or the path to an icon, see the @class{g:loadable-icon} class.

  To obtain a hash of a @sym{g:icon} object, see the @fun{g:icon-hash} function.
  To check if two @sym{g:icon} objects are equal, see the @fun{g:icon-equal}
  function.

  For serializing a @sym{g:icon} object, use the @fun{g:icon-serialize} and
  @fun{g:icon-deserialize} functions.

  If you want to consume the @sym{g:icon} interface, for example, in a
  toolkit, you must be prepared to handle at least the three following cases:
  @class{g:loadable-icon}, @class{g:themed-icon} and @class{g:emblemed-icon}
  classes. It may also make sense to have fast-paths for other cases, like
  handling the @class{gdk-pixbuf:pixbuf} object directly, for example, but all
  compliant @sym{g:icon} implementations outside of GIO must implement the
  @class{g:loadable-icon} class.

  If your application or library provides one or more @class{g:icon}
  implementations you need to ensure that your new implementation also
  implements the @class{g:loadable-icon} class. Additionally, you must provide
  an implementation of the @fun{g:icon-serialize} function that gives a result
  that is understood by the @fun{g:icon-deserialize} function, yielding one of
  the built-in icon types.
  @see-class{g:themed-icon}
  @see-class{g:emblemed-icon}
  @see-class{g:loadable-icon}")

;;; ----------------------------------------------------------------------------
;;; g_icon_hash ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_icon_hash" icon-hash) :uint
 #+liber-documentation
 "@version{2022-12-27}
  @argument[icon]{a @class{g:icon} object}
  @begin{return}
    An unsigned integer containing a hash for the icon, suitable for use in a
    Hash table or similar data structure.
  @end{return}
  @begin{short}
    Gets a hash for an icon.
  @end{short}
  @see-class{g:icon}"
  (icon (gobject:object icon)))

(export 'icon-hash)

;;; ----------------------------------------------------------------------------
;;; g_icon_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_icon_equal" icon-equal) :boolean
 #+liber-documentation
 "@version{2022-12-27}
  @argument[icon1]{a first @class{g:icon} object}
  @argument[icon2]{a second @class{g:icon} object}
  @begin{return}
    @em{True} if @arg{icon1} is equal to @arg{icon2}, @em{false} otherwise.
  @end{return}
  @begin{short}
    Checks if two icons are equal.
  @end{short}
  @see-class{g:icon}"
  (icon1 (gobject:object icon))
  (icon2 (gobject:object icon)))

(export 'icon-equal)

;;; ----------------------------------------------------------------------------
;;; g_icon_to_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_icon_to_string" icon-to-string) (:string :free-from-foreign t)
 #+liber-documentation
 "@version{2022-12-27}
  @argument[icon]{a @class{g:icon} object}
  @begin{return}
    An UTF-8 string or @code{nil} if @arg{icon} cannot be serialized.
  @end{return}
  @begin{short}
    Generates a textual representation of the icon that can be used for
    serialization such as when passing the icon to a different process or
    saving it to persistent storage.
  @end{short}
  Use the @fun{g:icon-new-for-string} function to get the icon back from
  the returned string.

  The encoding of the returned string is proprietary to a @class{g:icon} object
  except in the following two cases
  @begin{itemize}
    @begin{item}
      If @arg{icon} is a @class{g:file-icon} object, the returned string is a
      native path, such as @file{/path/to/my icon.png}, without escaping if the
      @class{g:file} object for icon is a native file. If the file is not
      native, the returned string is the result of the @fun{g:file-uri}
      function, such as @file{sftp://path/to/my%20icon.png}.
    @end{item}
    @begin{item}
      If @arg{icon} is a @class{g:themed-icon} object with exactly one name,
      the encoding is simply the name, such as @code{network-server}.
    @end{item}
  @end{itemize}
  @see-class{g:icon}
  @see-class{g:file-icon}
  @see-class{g:themed-icon}
  @see-function{g:icon-new-for-string}
  @see-function{g:file-uri}"
  (icon (gobject:object icon)))

(export 'icon-to-string)

;;; ----------------------------------------------------------------------------
;;; g_icon_new_for_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_icon_new_for_string" %icon-new-for-string)
    (gobject:object icon)
  (str :string)
  (err :pointer))

(defun icon-new-for-string (str)
 #+liber-documentation
 "@version{2022-12-27}
  @argument[str]{a string obtained via the @fun{g:icon-to-string} function}
  @begin{return}
    An object implementing the @class{g:icon} interface or @code{nil} if an
    error is set.
  @end{return}
  @begin{short}
    Generate a @class{g:icon} object from the @arg{str} argument.
  @end{short}
  This function can fail if the @arg{str} argument is not valid - see the
  @fun{g:icon-to-string} function for discussion.

  If your application or library provides one or more @class{g:icon}
  implementations you need to ensure that each type is registered with the type
  system prior to calling this function.
  @see-class{g:icon}
  @see-function{g:icon-to-string}"
  (glib:with-g-error (err)
    (%icon-new-for-string str err)))

(export 'icon-new-for-string)

;;; ----------------------------------------------------------------------------
;;; g_icon_serialize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_icon_serialize" icon-serialize)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{2022-12-27}
  @argument[icon]{a @class{g:icon} object}
  @return{A @type{g:variant} value, or @code{NULL} when serialization fails.
    The @type{g:variant} value will not be floating.}
  @begin{short}
    Serializes a @class{g:icon} object into a @type{g:variant} value.
  @end{short}
  An equivalent @class{g:icon} object can be retrieved back by calling the
  @fun{g:icon-deserialize} function on the returned value. As serialization will
  avoid using raw icon data when possible, it only makes sense to transfer the
  @type{g:variant} value between processes on the same machine, as opposed to
  over the network, and within the same file system namespace.
  @see-class{g:icon}
  @see-type{g:variant}
  @see-function{g:icon-deserialize}"
  (icon (gobject:object icon)))

(export 'icon-serialize)

;;; ----------------------------------------------------------------------------
;;; g_icon_deserialize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_icon_deserialize" icon-deserialize) (gobject:object icon)
 #+liber-documentation
 "@version{2022-12-27}
  @argument[value]{a @type{g:variant} value created with the
    @fun{g:icon-serialize} function}
  @return{A @class{g:icon} object, or @code{nil} when deserialization fails.}
  @begin{short}
    Deserializes a @class{g:icon} object previously serialized using
    the @fun{g:icon-serialize} function.
  @end{short}
  @see-class{g:icon}
  @see-function{g:icon-serialize}"
  (value (:pointer (:struct glib:variant))))

(export 'icon-deserialize)

;;; --- End of file gio.icon.lisp ----------------------------------------------
