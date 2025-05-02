;;; ----------------------------------------------------------------------------
;;; gobject.boxed.lisp
;;;
;;; The documentation in this file is taken from the GObject Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GObject
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; Boxed Types
;;;
;;;     A mechanism to wrap opaque C structures registered by the type system.
;;;
;;; Types and Values
;;;
;;;     G_TYPE_HASH_TABLE
;;;     G_TYPE_DATE
;;;     G_TYPE_GSTRING
;;;     G_TYPE_STRV
;;;     G_TYPE_REGEX
;;;     G_TYPE_MATCH_INFO
;;;     G_TYPE_ARRAY
;;;     G_TYPE_BYTE_ARRAY
;;;     G_TYPE_PTR_ARRAY
;;;     G_TYPE_BYTES
;;;     G_TYPE_VARIANT_TYPE
;;;     G_TYPE_ERROR
;;;     G_TYPE_DATE_TIME
;;;     G_Type_TIME_ZONE
;;;     G_TYPE_IO_CHANNEL
;;;     G_TYPE_IO_CONDITION
;;;     G_TYPE_VARIANT_BUILDER
;;;     G_TYPE_VARIANT_DICT
;;;     G_TYPE_KEY_FILE
;;;     G_TYPE_MAIN_CONTEXT
;;;     G_TYPE_MAIN_LOOP
;;;     G_TYPE_MAPPED_FILE
;;;     G_TYPE_MARKUP_PARSE_CONTEXT
;;;     G_TYPE_SOURCE
;;;     G_TYPE_POLLED
;;;     G_TYPE_THREAD
;;;     G_TYPE_OPTION_GROUP
;;;     G_TYPE_URI                                          Since 2.66
;;;
;;; Functions
;;;
;;;     GBoxedCopyFunc
;;;     GBoxedFreeFunc
;;;
;;;     g_boxed_copy
;;;     g_boxed_free
;;;     g_boxed_type_register_static
;;;     g_pointer_type_register_static
;;;
;;;
;;; Description
;;;
;;; GBoxed is a generic wrapper mechanism for arbitrary C structures. The only
;;; thing the type system needs to know about the structures is how to copy and
;;; free them, beyond that they are treated as opaque chunks of memory.
;;;
;;; Boxed types are useful for simple value-holder structures like rectangles or
;;; points. They can also be used for wrapping structures defined in non-GObject
;;; based libraries. They allow arbitrary structures to be handled in a uniform
;;; way, allowing uniform copying (or referencing) and freeing
;;; (or unreferencing) of them, and uniform representation of the type of the
;;; contained structure. In turn, this allows any type which can be boxed to be
;;; set as the data in a GValue, which allows for polymorphic handling of a much
;;; wider range of data types, and hence usage of such types as GObject property
;;; values.
;;;
;;; GBoxed is designed so that reference counted types can be boxed. Use the
;;; type’s ‘ref’ function as the GBoxedCopyFunc, and its ‘unref’ function as
;;; the GBoxedFreeFunc. For example, for GBytes, the GBoxedCopyFunc is
;;; g_bytes_ref(), and the GBoxedFreeFunc is g_bytes_unref().
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------

(declaim (inline type-is-boxed))

(defun type-is-boxed (gtype)
 #+liber-documentation
 "@version{2024-4-6}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a \"GBoxed\" type.}
  @begin{short}
    Checks whether @arg{gtype} is a \"GBoxed\" type.
  @end{short}
  @see-class{g:type-t}"
  (eq (type-fundamental gtype) (glib:gtype "GBoxed")))

(export 'type-is-boxed)

;;; ----------------------------------------------------------------------------
;;; GBoxedCopyFunc ()
;;;
;;; gpointer (*GBoxedCopyFunc) (gpointer boxed);
;;;
;;; This function is provided by the user and should produce a copy of the
;;; passed in boxed structure.
;;;
;;; boxed :
;;;     The boxed structure to be copied.
;;;
;;; Returns :
;;;     The newly created copy of the boxed structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GBoxedFreeFunc ()
;;;
;;; void (*GBoxedFreeFunc) (gpointer boxed);
;;;
;;; This function is provided by the user and should free the boxed structure
;;; passed.
;;;
;;; boxed :
;;;     The boxed structure to be freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_boxed_copy ()
;;; ----------------------------------------------------------------------------

;; Used internally for the implementation of a Lisp boxed type. This function
;; is not exported.

(cffi:defcfun ("g_boxed_copy" %boxed-copy) :pointer
  (gtype type-t)
  (boxed :pointer))

(defun boxed-copy (gtype boxed)
 #+liber-documentation
 "@version{#2013-6-10}
  @argument[boxed-type]{the type of @arg{src-boxed}}
  @argument[src-boxed]{the boxed structure to be copied}
  @return{The newly created copy of the boxed structure.}
  Provide a copy of a boxed structure @arg{src-boxed} which is of type
  @arg{boxed-type}."
  ;; We check for a null-pointer and return nil.
  (unless (cffi:null-pointer-p boxed)
    (%boxed-copy gtype boxed)))

;;; ----------------------------------------------------------------------------
;;; g_boxed_free ()
;;; ----------------------------------------------------------------------------

;; Used internally for the implementation of a Lisp boxed type. This function
;; is not exported.

(cffi:defcfun ("g_boxed_free" boxed-free) :void
 #+liber-documentation
 "@version{#2013-6-10}
  @argument[boxed-type]{the type of boxed}
  @argument[boxed]{the boxed structure to be freed}
  Free the boxed structure @arg{boxed} which is of type @arg{boxed-type}."
  (gtype type-t)
  (boxed :pointer))

;;; ----------------------------------------------------------------------------
;;; g_boxed_type_register_static ()
;;; ----------------------------------------------------------------------------

;; This function is not exported.

(cffi:defcfun ("g_boxed_type_register_static" g-boxed-type-register-static)
    type-t
 #+liber-documentation
 "@version{#2013-6-10}
  @argument[name]{name of the new boxed type}
  @argument[copy-fn]{boxed structure copy function}
  @argument[free-fn]{boxed structure free function}
  @return{The new @code{\"GBoxed\"} derived type ID for @arg{name}.}
  @begin{short}
    This function creates a new @code{\"GBoxed\"} derived type ID for a new
    boxed type with name @arg{name}.
  @end{short}
  Boxed type handling functions have to be provided to copy and free opaque
  boxed structures of this type."
  (name :string)
  (copy-fn :pointer)
  (free-fn :pointer))

;;; ----------------------------------------------------------------------------
;;; g_pointer_type_register_static ()
;;; ----------------------------------------------------------------------------

;; This function is not exported.

(cffi:defcfun ("g_pointer_type_register_static" g-pointer-type-register-static)
    type-t
 #+liber-documentation
 "@version{#2013-6-10}
  @argument[name]{the name of the new pointer type}
  @return{The new @code{\"gpointer\"} derived type ID for @arg{name}.}
  @begin{short}
    Creates a new @code{\"gpointer\"} derived type ID for a new pointer type
    with name @arg{name}.
  @end{short}"
  (name :string))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_HASH_TABLE
;;;
;;; #define G_TYPE_HASH_TABLE (g_hash_table_get_type ())
;;;
;;; The GType for a boxed type holding a GHashTable reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_DATE
;;;
;;; #define G_TYPE_DATE (g_date_get_type ())
;;;
;;; The GType for GDate.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_GSTRING
;;;
;;; #define G_TYPE_GSTRING (g_gstring_get_type ())
;;;
;;; The GType for GString.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_STRV
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_strv_get_type" type-strv) type-t
 #+liber-documentation
 "@version{#2022-12-17}
  @begin{short}
    The @class{g:type-t} type ID for a boxed type holding an array of strings.
  @end{short}
  The @type{g:strv-t} type represents and performs automatic conversion between
  a list of Lisp strings and an array of C strings.
  @see-class{g:type-t}
  @see-type{g:strv-t}")

#+nil
(export 'type-strv)

#+nil
(glib-init:at-init () (type-strv))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_REGEX
;;;
;;; #define G_TYPE_REGEX (g_regex_get_type ())
;;;
;;; The GType for a boxed type holding a GRegex reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MATCH_INFO
;;;
;;; #define G_TYPE_MATCH_INFO (g_match_info_get_type ())
;;;
;;; The GType for a boxed type holding a GMatchInfo reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_ARRAY
;;;
;;; #define G_TYPE_ARRAY (g_array_get_type ())
;;;
;;; The GType for a boxed type holding a GArray reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_BYTE_ARRAY
;;;
;;; #define G_TYPE_BYTE_ARRAY (g_byte_array_get_type ())
;;;
;;; The GType for a boxed type holding a GByteArray reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PTR_ARRAY
;;;
;;; #define G_TYPE_PTR_ARRAY (g_ptr_array_get_type ())
;;;
;;; The GType for a boxed type holding a GPtrArray reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_BYTES
;;;
;;; #define G_TYPE_BYTES (g_bytes_get_type ())
;;;
;;; The GType for GBytes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VARIANT_TYPE
;;;
;;; #define G_TYPE_VARIANT_TYPE (g_variant_type_get_gtype ())
;;;
;;; The GType for a boxed type holding a GVariantType.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_ERROR
;;;
;;; #define G_TYPE_ERROR (g_error_get_type ())
;;;
;;; The GType for a boxed type holding a GError.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_DATE_TIME
;;;
;;; #define G_TYPE_DATE_TIME (g_date_time_get_type ())
;;;
;;; The GType for a boxed type holding a GDateTime.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_TIME_ZONE
;;;
;;; #define G_TYPE_TIME_ZONE (g_time_zone_get_type ())
;;;
;;; The GType for a boxed type holding a GTimeZone.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IO_CHANNEL
;;;
;;; #define G_TYPE_IO_CHANNEL (g_io_channel_get_type ())
;;;
;;; The GType for GIOChannel.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IO_CONDITION
;;;
;;; #define G_TYPE_IO_CONDITION (g_io_condition_get_type ())
;;;
;;; The GType for GIOCondition.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VARIANT_BUILDER
;;;
;;; #define G_TYPE_VARIANT_BUILDER (g_variant_builder_get_type ())
;;;
;;; The GType for a boxed type holding a GVariantBuilder.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VARIANT_DICT
;;;
;;; #define G_TYPE_VARIANT_DICT (g_variant_dict_get_type ())
;;;
;;; The GType for a boxed type holding a GVariantDict.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_KEY_FILE
;;;
;;; #define G_TYPE_KEY_FILE (g_key_file_get_type ())
;;;
;;; The GType for a boxed type holding a GKeyFile.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MAIN_CONTEXT
;;;
;;; #define G_TYPE_MAIN_CONTEXT (g_main_context_get_type ())
;;;
;;; The GType for a boxed type holding a GMainContext.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MAIN_LOOP
;;;
;;; #define G_TYPE_MAIN_LOOP (g_main_loop_get_type ())
;;;
;;; The GType for a boxed type holding a GMainLoop.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MAPPED_FILE
;;;
;;; #define G_TYPE_MAPPED_FILE (g_mapped_file_get_type ())
;;;
;;; The GType for a boxed type holding a GMappedFile.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MARKUP_PARSE_CONTEXT
;;;
;;; #define G_TYPE_MARKUP_PARSE_CONTEXT (g_markup_parse_context_get_type ())
;;;
;;; The GType for a boxed type holding a GMarkupParseContext.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_SOURCE
;;;
;;; #define G_TYPE_SOURCE (g_source_get_type ())
;;;
;;; The GType for a boxed type holding a GSource.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_POLLFD
;;;
;;; #define G_TYPE_POLLFD (g_pollfd_get_type ())
;;;
;;; The GType for a boxed type holding a GPollFD.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_THREAD
;;;
;;; #define G_TYPE_THREAD (g_thread_get_type ())
;;;
;;; The GType for a boxed type holding a GThread.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_OPTION_GROUP
;;;
;;; #define G_TYPE_OPTION_GROUP (g_option_group_get_type ())
;;;
;;; The GType for a boxed type holding a GOptionGroup.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_URI
;;;
;;; #define G_TYPE_URI (g_uri_get_type ())
;;;
;;; The GType for a boxed type holding a GUri.
;;;
;;; Since 2.66
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.boxed.lisp -----------------------------------------
