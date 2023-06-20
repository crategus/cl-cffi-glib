;;; ----------------------------------------------------------------------------
;;; glib.quark.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.76 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;
;;; Quarks
;;;
;;;     A 2-way association between a string and a unique integer identifier
;;;
;;; Types and Values
;;;
;;;     GQuark
;;;
;;; Functions
;;;
;;;     G_DEFINE_QUARK
;;;     g_quark_from_string
;;;     g_quark_from_static_string
;;;     g_quark_to_string
;;;     g_quark_try_string
;;;     g_intern_string
;;;     g_intern_static_string
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GQuark
;;; ----------------------------------------------------------------------------

;; A GQuark is implemented in the C Library as guint32.

(cffi:define-foreign-type quark-type ()
  ()
  (:actual-type :uint32)
  (:simple-parser quark-as-string))

(defmethod cffi:translate-to-foreign (value (type quark-type))
  (cffi:foreign-funcall "g_quark_from_string"
                        :string (if value value (cffi:null-pointer))
                        :uint32))

(defmethod cffi:translate-from-foreign (value (type quark-type))
  (cffi:foreign-funcall "g_quark_to_string"
                        :uint32 value
                        :string))

#+liber-documentation
(setf (documentation 'quark-as-string 'type)
 "@version{2022-11-21}
  @begin{short}
    Quarks are associations between strings and integer identifiers.
  @end{short}
  Given either the string or the @code{GQuark} identifier it is possible to
  retrieve the other.
  @begin[Lisp binding]{dictionary}
    In the Lisp binding the @sym{g:quark-as-string} type translates a string
    argument to the corresponding @code{GQuark} identifier and a @code{GQuark}
    return value is translated to the corresponding Lisp string. No further
    functions are implemented for the @sym{g:quark-as-string} type.

    If the Lisp string does not currently have an associated @code{GQuark}, a
    new @code{GQuark} is created. A @code{GQuark} value of zero is associated
    to @code{nil} in Lisp.

    See the @fun{g:type-qdata} function for attaching a @code{GQuark}
    identifier to a @class{g:type-t} type.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Translate a Lisp String to a @code{GQuark} identifier:
    @begin{pre}
(cff:convert-to-foreign \"GtkWidget\" 'g:quark-as-string) => 91
(cffi:convert-to-foreign \"gboolean\" 'g:quark-as-string) => 9
(cffi:convert-to-foreign nil 'g:quark-as-string) => 0
     @end{pre}
     Translate a @code{GQuark} identifier to a Lisp string:
     @begin{pre}
(cffi:convert-from-foreign 91 'g:quark-as-string) => \"GtkWidget\"
(cffi:convert-from-foreign 9 'g:quark-as-string) => \"gboolean\"
(cffi:convert-from-foreign 0 'g:quark-as-string) => NIL
     @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-qdata}")

(export 'quark-as-string)

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_QUARK()
;;;
;;; #define G_DEFINE_QUARK(QN, q_n)
;;;
;;; A convenience macro which defines a function returning the GQuark for the
;;; name QN. The function will be named q_n_quark(). Note that the quark name
;;; will be stringified automatically in the macro, so you shouldn't use double
;;; quotes.
;;;
;;; QN :
;;;     the name to return a GQuark for
;;;
;;; q_n :
;;;     prefix for the function name
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_quark_from_string ()
;;; ----------------------------------------------------------------------------

;(defcfun ("g_quark_from_string" %g-quark-from-string) %g-quark
;  (str :string))

;(defun g-quark-from-string (str)
;  (%g-quark-from-string (if str str (cffi:null-pointer))))

;#+liber-documentation
;(setf (documentation 'g-quark-from-string 'function)
; "@version{#2020-10-3}
;  @argument[str]{a string}
;  @return{The @type{g-quark} identifying @arg{str}, or 0 if @arg{str}
;    is @code{nil}.}
;  @begin{short}
;    Gets the @type{g-quark} identifying the given @arg{str}.
;  @end{short}
;  If @arg{str} does not currently have an associated @type{g-quark}, a
;  new @type{g-quark} is created, using a copy of the @arg{str}.
;  @see-type{g-quark}
;  @see-function{g-quark-from-string}")

;(export 'g-quark-from-string)

;;; ----------------------------------------------------------------------------
;;; g_quark_from_static_string ()
;;;
;;; GQuark g_quark_from_static_string (const gchar *string);
;;;
;;; Gets the GQuark identifying the given (static) string. If the string does
;;; not currently have an associated GQuark, a new GQuark is created, linked to
;;; the given string.
;;;
;;; Note that this function is identical to g_quark_from_string() except that if
;;; a new GQuark is created the string itself is used rather than a copy. This
;;; saves memory, but can only be used if the string will always exist. It can
;;; be used with statically allocated strings in the main program, but not with
;;; statically allocated memory in dynamically loaded modules, if you expect to
;;; ever unload the module again (e.g. do not use this function in GTK+ theme
;;; engines).
;;;
;;; string :
;;;     a string
;;;
;;; Returns :
;;;     the GQuark identifying the string, or 0 if string is NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_quark_to_string ()
;;; ----------------------------------------------------------------------------

;(defcfun ("g_quark_to_string" g-quark-to-string) :string
; "@version{#2013-6-1}
;  @argument[quark]{a @type{g-quark}}
;  @return{The string associated with the @type{g-quark}.}
;  Gets the string associated with the given @type{g-quark}."
;  (quark %g-quark))

;(export 'g-quark-to-string)

;;; ----------------------------------------------------------------------------
;;; g_quark_try_string ()
;;;
;;; GQuark g_quark_try_string (const gchar *string);
;;;
;;; Gets the GQuark associated with the given string, or 0 if string is NULL or
;;; it has no associated GQuark.
;;;
;;; If you want the GQuark to be created if it does not already exist, use
;;; g_quark_from_string() or g_quark_from_static_string().
;;;
;;; string :
;;;     a string
;;;
;;; Returns :
;;;     the GQuark associated with the string, or 0 if string is NULL or there
;;;     is no GQuark associated with it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_intern_string ()
;;;
;;; const gchar * g_intern_string (const gchar *string);
;;;
;;; Returns a canonical representation for string. Interned strings can be
;;; compared for equality by comparing the pointers, instead of using strcmp().
;;;
;;; string :
;;;     a string
;;;
;;; Returns :
;;;     a canonical representation for the string
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_intern_static_string ()
;;;
;;; const gchar * g_intern_static_string (const gchar *string);
;;;
;;; Returns a canonical representation for string. Interned strings can be
;;; compared for equality by comparing the pointers, instead of using strcmp().
;;; g_intern_static_string() does not copy the string, therefore string must not
;;; be freed or modified.
;;;
;;; string :
;;;     a static string
;;;
;;; Returns :
;;;     a canonical representation for the string
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.quark.lisp --------------------------------------------
