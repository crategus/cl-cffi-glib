;;; ----------------------------------------------------------------------------
;;; gobject.param.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; Parameters and Values
;;;
;;;     Standard Parameter and Value Types
;;;
;;; Synopsis
;;;
;;;     GParamSpecBoolean
;;;     g_param_spec_boolean
;;;     g_value_set_boolean
;;;     g_value_get_boolean
;;;
;;;     GParamSpecChar
;;;     g_param_spec_char
;;;     g_value_set_char
;;;     g_value_get_char
;;;     g_value_get_schar
;;;     g_value_set_schar
;;;
;;;     GParamSpecUChar
;;;     g_param_spec_uchar
;;;     g_value_set_uchar
;;;     g_value_get_uchar
;;;
;;;     GParamSpecInt
;;;     g_param_spec_int
;;;     g_value_set_int
;;;     g_value_get_int
;;;
;;;     GParamSpecUInt
;;;     g_param_spec_uint
;;;     g_value_set_uint
;;;     g_value_get_uint
;;;
;;;     GParamSpecLong
;;;     g_param_spec_long
;;;     g_value_set_long
;;;     g_value_get_long
;;;
;;;     GParamSpecULong
;;;     g_param_spec_ulong
;;;     g_value_set_ulong
;;;     g_value_get_ulong
;;;
;;;     GParamSpecInt64
;;;     g_param_spec_int64
;;;     g_value_set_int64
;;;     g_value_get_int64
;;;
;;;     GParamSpecUInt64
;;;     g_param_spec_uint64
;;;     g_value_set_uint64
;;;     g_value_get_uint64
;;;
;;;     GParamSpecFloat
;;;     g_param_spec_float
;;;     g_value_set_float
;;;     g_value_get_float
;;;
;;;     GParamSpecDouble
;;;     g_param_spec_double
;;;     g_value_set_double
;;;     g_value_get_double
;;;
;;;     GParamSpecEnum
;;;     g_param_spec_enum
;;;     g_value_set_enum
;;;     g_value_get_enum
;;;
;;;     GParamSpecFlags
;;;     g_param_spec_flags
;;;     g_value_set_flags
;;;     g_value_get_flags
;;;
;;;     GParamSpecString
;;;     gchararray
;;;     g_param_spec_string
;;;     g_value_set_string
;;;     g_value_set_static_string
;;;     g_value_take_string
;;;     g_value_set_string_take_ownership
;;;     g_value_get_string
;;;     g_value_dup_string
;;;     g_value_set_interned_string
;;;
;;;     GParamSpecParam
;;;     g_param_spec_param
;;;     g_value_set_param
;;;     g_value_take_param
;;;     g_value_set_param_take_ownership
;;;     g_value_get_param
;;;     g_value_dup_param
;;;
;;;     GParamSpecBoxed
;;;     g_param_spec_boxed
;;;     g_value_set_boxed
;;;     g_value_set_static_boxed
;;;     g_value_take_boxed
;;;     g_value_set_boxed_take_ownership
;;;     g_value_get_boxed
;;;     g_value_dup_boxed
;;;
;;;     GParamSpecPointer
;;;     g_param_spec_pointer
;;;     g_value_set_pointer
;;;     g_value_get_pointer
;;;
;;;     GParamSpecObject
;;;     g_param_spec_object
;;;     g_value_set_object
;;;     g_value_take_object
;;;     g_value_set_object_take_ownership
;;;     g_value_get_object
;;;     g_value_dup_object
;;;
;;;     GParamSpecUnichar                                   not implemented
;;;     g_param_spec_unichar                                not implemented
;;;
;;;     GParamSpecValueArray                                not exported
;;;     g_param_spec_value_array                            not exported
;;;
;;;     GParamSpecOverride                                  not implemented
;;;     g_param_spec_override                               not implemented
;;;
;;;     GParamSpecGType
;;;     g_param_spec_gtype
;;;     g_value_get_gtype
;;;     g_value_set_gtype
;;;
;;;     GParamSpecVariant                                   not implemented
;;;     g_param_spec_variant                                not implemented
;;;     g_value_get_variant
;;;     g_value_dup_variant                                 not implemented
;;;     g_value_set_variant
;;;     g_value_take_variant                                not implemented
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; GParamSpecBoolean
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-boolean
  (:parent-instance (:pointer (:struct param-spec)))
  (:default-value :boolean))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-boolean)
      "CStruct"
      (liber:symbol-documentation 'param-spec-boolean)
 "@version{2024-9-13}
  @begin{declaration}
(cffi:defcstruct param-spec-boolean
  (:parent-instance (:pointer (:struct param-spec)))
  (:default-value :boolean))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{The private @symbol{g:param-spec} portion.}
      @entry[:default-value]{The boolean default value.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{g:param-spec} derived structure that contains the meta data for
    boolean properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-boolean}")

(export 'param-spec-boolean)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boolean
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_boolean" param-spec-boolean)
    (:pointer (:struct param-spec-boolean))
 #+liber-documentation
 "@version{2024-9-13}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[default]{a boolean with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-boolean} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    @code{\"gboolean\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-boolean}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (default :boolean)
  (flags param-flags))

(export 'param-spec-boolean)

;;; ----------------------------------------------------------------------------
;;; g_value_set_boolean
;;; g_value_get_boolean
;;; ----------------------------------------------------------------------------

(defun (setf value-boolean) (value gvalue)
  (cffi:foreign-funcall "g_value_set_boolean"
                        (:pointer (:struct value)) gvalue
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("g_value_get_boolean" value-boolean) :boolean
 #+liber-documentation
 "@version{2024-9-13}
  @syntax{(g:value-boolean gvalue) => value}
  @syntax{(setf (g:value-boolan gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"gboolean\"} type}
  @argument[value]{a boolean value}
  @begin{short}
    The @fun{g:value-boolean} function gets the contents of a @symbol{g:value}
    instance of @code{\"gboolean\"} type.
  @end{short}
  The @setf{g:value-boolean} function sets the contents of the @symbol{g:value}
  instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-boolean)

;;; ----------------------------------------------------------------------------
;;; GParamSpecChar
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-char
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :int8)
  (:maximum :int8)
  (:default-value :int8))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-char)
      "CStruct"
      (liber:symbol-documentation 'param-spec-char)
 "@version{2024-9-13}
  @begin{declaration}
(cffi:defcstruct param-spec-char
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :int8)
  (:maximum :int8)
  (:default-value :int8))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{The private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The 8-bit integer with the minimum value.}
      @entry[:maximum]{The 8-bit integer with the maximum value.}
      @entry[:default-value]{The 8-bit integer with the default value.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{g:param-spec} derived structure that contains the meta data for
    character properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-char}")

(export 'param-spec-char)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_char
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_char" param-spec-char)
    (:pointer (:struct param-spec-char))
 #+liber-documentation
 "@version{2024-9-13}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an 8-bit integer with the minimum value}
  @argument[maximum]{an 8-bit integer with the maximum value}
  @argument[default]{an 8-bit integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-char} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    @code{\"gchar\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-char}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int8)
  (maximum :int8)
  (default :int8)
  (flags param-flags))

(export 'param-spec-char)

;;; ----------------------------------------------------------------------------
;;; g_value_set_char
;;; g_value_get_char
;;; ----------------------------------------------------------------------------

(defun (setf value-char) (value gvalue)
  (cffi:foreign-funcall "g_value_set_char"
                        (:pointer (:struct value)) gvalue
                        :int8 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_char" value-char) :int8
 #+liber-documentation
 "@version{2024-9-13}
  @syntax{(g:value-char gvalue) => value}
  @syntax{(setf (g:value-char gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"gchar\"} type}
  @argument[value]{an 8-bit integer with the character value}
  @begin{short}
    The @fun{g:value-char} function gets the contents of a @symbol{g:value}
    instance of @code{\"gchar\"} type.
  @end{short}
  The @setf{g:value-char} function sets the contents of a @symbol{g:value}
  instance to @arg{value}.
  @begin[Warning]{dictionary}
    The @fun{g:value-char} function has been deprecated since version 2.32 and
    should not be used in newly written code. The function return type is
    broken, see the @fun{g:value-schar} function.
  @end{dictionary}
  @see-symbol{g:value}
  @see-function{g:value-schar}"
  (gvalue (:pointer (:struct value))))

(export 'value-char)

;;; ----------------------------------------------------------------------------
;;; g_value_get_schar
;;; g_value_set_schar
;;; ----------------------------------------------------------------------------

(defun (setf value-schar) (value gvalue)
  (cffi:foreign-funcall "g_value_set_schar"
                        (:pointer (:struct value)) gvalue
                        :int8 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_schar" value-schar) :int8
 #+liber-documentation
 "@version{2024-9-13}
  @syntax{(g:value-schar gvalue) => value}
  @syntax{(setf (g:value-schar gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"gchar\"} type}
  @argument[value]{an integer with the character value}
  @begin{short}
    The @fun{g:value-schar} function gets the contents of a @symbol{g:value}
    of @code{\"gchar\"} type.
  @end{short}
  The @setf{g:value-schar} function sets the contents of a @symbol{g:value}
  instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-schar)

;;; ----------------------------------------------------------------------------
;;; GParamSpecUChar
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-uchar
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :uint8)
  (:maximum :uint8)
  (:default-value :uint8))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-uchar)
      "CStruct"
      (liber:symbol-documentation 'param-spec-uchar)
 "@version{2024-9-13}
  @begin{declaration}
(cffi:defcstruct param-spec-uchar
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :uint8)
  (:maximum :uint8)
  (:default-value :uint8))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{The private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The unsigned 8-bit integer with the minimum value.}
      @entry[:maximum]{The unsigned 8-bit integer with the maximum value.}
      @entry[:default-value]{The unsigned 8-bit integer with the default value.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{g:param-spec} derived structure that contains the meta data for
    unsigned character properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-uchar}")

(export 'param-spec-uchar)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uchar
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_uchar" param-spec-uchar)
    (:pointer (:struct param-spec-uchar))
 #+liber-documentation
 "@version{2024-9-13}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an unsigned 8-bit integer with the minimum value}
  @argument[maximum]{an unsigned 8-bit integer with the maximum value}
  @argument[default]{an unsigned 8-bit integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-uchar} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    @code{\"guchar\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-uchar}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint8)
  (maximum :uint8)
  (default :uint8)
  (flags param-flags))

(export 'param-spec-uchar)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uchar
;;; g_value_set_uchar
;;; ----------------------------------------------------------------------------

(defun (setf value-uchar) (value gvalue)
  (cffi:foreign-funcall "g_value_set_uchar"
                        (:pointer (:struct value)) gvalue
                        :uint8 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_uchar" value-uchar) :uint8
 #+liber-documentation
 "@version{2024-9-13}
  @syntax{(g:value-uchar gvalue) => value}
  @syntax{(setf (g:value-uchar gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"guchar\"} type}
  @argument[value]{an unsigned 8-bit integer with the unsigned character value}
  @begin{short}
    The @fun{g:value-uchar} function gets the contents of a @symol{g:value}
    instance of @code{\"guchar\"} type.
  @end{short}
  The @setf{g:value-uchar} function sets the contents of a @symbol{g:value}
  instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-uchar)

;;; ----------------------------------------------------------------------------
;;; GParamSpecInt
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-int
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :int)
  (:maximum :int)
  (:default-value :int))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-int)
      "CStruct"
      (liber:symbol-documentation 'param-spec-int)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-int
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :int)
  (:maximum :int)
  (:default-value :int))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The integer with the minimum value.}
      @entry[:maximum]{The integer with the maximum value.}
      @entry[:default-value]{The integer with the default value.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    integer properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-int}")

(export 'param-spec-int)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_int" param-spec-int)
    (:pointer (:struct param-spec-int))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an integer with the minimum value}
  @argument[maximum]{an integer with the maximum value}
  @argument[default]{an integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-int} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"gint\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-int}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int)
  (maximum :int)
  (default :int)
  (flags param-flags))

(export 'param-spec-int)

;;; ----------------------------------------------------------------------------
;;; g_value_get_int
;;; g_value_set_int
;;; ----------------------------------------------------------------------------

(defun (setf value-int) (value gvalue)
  (cffi:foreign-funcall "g_value_set_int"
                        (:pointer (:struct value)) gvalue
                        :int value
                        :void)
  value)

(cffi:defcfun ("g_value_get_int" value-int) :int
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-int gvalue) => value}
  @syntax{(setf (g:value-int gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"gint\"} type}
  @argument[value]{an integer value}
  @begin{short}
    Integer contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-int} function gets the contents of a @symbol{g:value}
  instance of @code{\"gint\"} type. The @setf{value-int} function sets the
  contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-int)

;;; ----------------------------------------------------------------------------
;;; GParamSpecUInt
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-uint
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :uint)
  (:maximum :uint)
  (:default-value :uint))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-uint)
      "CStruct"
      (liber:symbol-documentation 'param-spec-uint)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-uint
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :uint)
  (:maximum :uint)
  (:default-value :uint))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The unsigned integer with the minimum value.}
      @entry[:maximum]{The unsigned integer with the maximum value.}
      @entry[:default-value]{The unsigned integer with the default value.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    unsigned integer properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-uint}")

(export 'param-spec-uint)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_uint" param-spec-uint)
    (:pointer (:struct param-spec-uint))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an unsigned integer with the minimum value}
  @argument[maximum]{an unsigned integer with the maximum value}
  @argument[default]{an unsigned integer with default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-uint} parameter specification.}
  @begin{short}
    Creates a new parameter specificaton instance specifying a property
    of @code{\"guint\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{parm-spec-uint}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint)
  (maximum :uint)
  (default :uint)
  (flags param-flags))

(export 'param-spec-uint)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uint
;;; g_value_set_uint
;;; ----------------------------------------------------------------------------

(defun (setf value-uint) (value gvalue)
  (cffi:foreign-funcall "g_value_set_uint"
                        (:pointer (:struct value)) gvalue
                        :uint value
                        :void)
  value)

(cffi:defcfun ("g_value_get_uint" value-uint) :uint
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-uint gvalue) => value}
  @syntax{(setf (g:value-uint gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"guint\"} type}
  @argument[value]{an unsigned integer value}
  @begin{short}
    Unsigned integer contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-uint} function gets the contents of a @symbol{g:value}
  instance of @code{\"guint\"} type. The @setf{value-uint} function sets the
  contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-uint)

;;; ----------------------------------------------------------------------------
;;; GParamSpecLong
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-long
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :long)
  (:maximum :long)
  (:default-value :long))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-long)
      "CStruct"
      (liber:symbol-documentation 'param-spec-long)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-long
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :long)
  (:maximum :long)
  (:default-value :long))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The long integer with the minimum value.}
      @entry[:maximum]{The long integer with the maximum value.}
      @entry[:default-value]{The long integer with the default value.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    long integer properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-long}")

(export 'param-spec-long)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_long
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_long" param-spec-long)
    (:pointer (:struct param-spec-long))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{a long integer with the minimum value}
  @argument[maximum]{a long integer with the maximum value}
  @argument[default]{a long integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-long} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"glong\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-long}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :long)
  (maximum :long)
  (default :long)
  (flags param-flags))

(export 'param-spec-long)

;;; ----------------------------------------------------------------------------
;;; g_value_get_long
;;; g_value_set_long
;;; ----------------------------------------------------------------------------

(defun (setf value-long) (value gvalue)
  (cffi:foreign-funcall "g_value_set_long"
                        (:pointer (:struct value)) gvalue
                        :long value
                        :void)
  value)

(cffi:defcfun ("g_value_get_long" value-long) :long
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-long gvalue) => value}
  @syntax{(setf (g:value-long gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"glong\"} type}
  @argument[value]{a long integer value}
  @begin{short}
    Long integer contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-long} function gets the contents of a @symbol{g:value}
  instance of @code{\"glong\"} type. The @setf{value-long} function sets the
  contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-long)

;;; ----------------------------------------------------------------------------
;;; GParamSpecULong
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-ulong
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :ulong)
  (:maximum :ulong)
  (:default-value :ulong))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-ulong)
      "CStruct"
      (liber:symbol-documentation 'param-spec-ulong)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-ulong
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :ulong)
  (:maximum :ulong)
  (:default-value :ulong))
  @end{declaration}
  @begin{values}
     @begin[code]{table}
       @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The unsigned long integer with the minimum value.}
      @entry[:maximum]{The unsigned long integer with the maximum value.}
      @entry[:default-value]{The unsigned long integer with the default value.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    unsigned long integer properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-ulong}")

(export 'param-spec-ulong)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ulong
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_ulong" param-spec-ulong)
    (:pointer (:struct param-spec-ulong))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an unsigned long integer with the minimum value}
  @argument[maximum]{an unsigned long integer with the maximum value}
  @argument[default]{an unsigned long integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-ulong} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"gulong\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-ulong}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :ulong)
  (maximum :ulong)
  (default :ulong)
  (flags param-flags))

(export 'param-spec-ulong)

;;; ----------------------------------------------------------------------------
;;; g_value_get_ulong
;;; g_value_set_ulong
;;; ----------------------------------------------------------------------------

(defun (setf value-ulong) (value gvalue)
  (cffi:foreign-funcall "g_value_set_ulong"
                        (:pointer (:struct value)) gvalue
                        :ulong value
                        :void)
  value)

(cffi:defcfun ("g_value_get_ulong" value-ulong) :ulong
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-ulong gvalue) => value}
  @syntax{(setf (g:value-ulong gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"gulong\"} type}
  @argument[value]{an unsigned long integer value}
  @begin{short}
    Unsigned long integer contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-ulong} function gets the contents of a @symbol{g:value}
  instance of @code{\"gulong\"} type. The @setf{g:value-ulong} function sets
  the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-ulong)

;;; ----------------------------------------------------------------------------
;;; GParamSpecInt64
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-int64
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :int64)
  (:maximum :int64)
  (:default-value :int64))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-int64)
      "CStruct"
      (liber:symbol-documentation 'param-spec-int64)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-int64
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :int64)
  (:maximum :int64)
  (:default-value :int64))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The 64-bit integer with the value.}
      @entry[:maximum]{The 64-bit integer with the maximum value.}
      @entry[:default-value]{The 64-bit integer with the default value.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    64 bit integer properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-int64}")

(export 'param-spec-int64)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int64
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_int64" param-spec-int64)
    (:pointer (:struct param-spec-int64))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{a 64-bit integer with the minimum value}
  @argument[maximum]{a 64-bit integer with the maximum value}
  @argument[default]{a 64-bit- integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-int64} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"gint64\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-int64}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int64)
  (maximum :int64)
  (default :int64)
  (flags param-flags))

(export 'param-spec-int64)

;;; ----------------------------------------------------------------------------
;;; g_value_get_int64
;;; g_value_set_int64
;;; ----------------------------------------------------------------------------

(defun (setf value-int64) (value gvalue)
  (cffi:foreign-funcall "g_value_set_int64"
                        (:pointer (:struct value)) gvalue
                        :int64 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_int64" value-int64) :int64
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-int64 gvalue) => value}
  @syntax{(setf (g:value-int64 gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"gint64\"} type}
  @argument[value]{a 64-bit integer value}
  @begin{short}
    The 64-bit integer contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-int64} function gets the contents of a @symbol{g:value}
  instance of @code{\"gint64\"} type. The @setf{g:value-int64} function sets
  the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-int64)

;;; ----------------------------------------------------------------------------
;;; GParamSpecUInt64
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-uint64
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-uint64)
      "CStruct"
      (liber:symbol-documentation 'param-spec-uint64)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-uint64
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The unsigned 64-bit integer with the minimum value.}
      @entry[:maximum]{The unsigned 64-bit integer with the maximum value.}
      @entry[:default-value]{The unsigned 64-bit integer with the default
        value.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    unsigned 64 bit integer properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-uint64}")

(export 'param-spec-uint64)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint64
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_uint64" param-spec-uint64)
    (:pointer (:struct param-spec-uint64))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an unsigned 64-bit integer with the minimum value}
  @argument[maximum]{an unsigned 64-bit integer with the maximum value}
  @argument[default]{an unsigned 64-bit integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-uint64} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"guint64\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-uint64}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint64)
  (maximum :uint64)
  (default :uint64)
  (flags param-flags))

(export 'param-spec-uint64)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uint64
;;; g_value_set_uint64
;;; ----------------------------------------------------------------------------

(defun (setf value-uint64) (value gvalue)
  (cffi:foreign-funcall "g_value_set_uint64"
                        (:pointer (:struct value)) gvalue
                        :uint64 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_uint64" value-uint64) :uint64
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-uint64 gvalue) => value}
  @syntax{(setf (g:value-uint64 gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"guint64\"} type}
  @argument[value]{an unsigned 64-bit integer value}
  @begin{short}
    Unsigned 64-bit integer contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-uint64} function gets the contents of a @symbol{g:value}
  instance of @code{\"guint64\"} type. The @setf{g:value-uint64} function sets
  the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-uint64)

;;; ----------------------------------------------------------------------------
;;; GParamSpecFloat
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-float
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :float)
  (:maximum :float)
  (:default-value :float)
  (:epsilon :float))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-float)
      "CStruct"
      (liber:symbol-documentation 'param-spec-float)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-float
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :float)
  (:maximum :float)
  (:default-value :float)
  (:epsilon :float))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The float with the minimum value.}
      @entry[:maximum]{The float with the maximum value.}
      @entry[:default-value]{The float with the default value.}
      @entry[:epsilon]{The float value, values closer than epsilon will be
        considered identical, the default value is 1e-30.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    float properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-float}")

(export 'param-spec-float)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_float
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_float" param-spec-float)
    (:pointer (:struct param-spec-float))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{a float with the minimum value}
  @argument[maximum]{a float with the maximum value}
  @argument[default]{a float with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-float} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"gfloat\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-float}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :float)
  (maximum :float)
  (default :float)
  (flags param-flags))

(export 'param-spec-float)

;;; ----------------------------------------------------------------------------
;;; g_value_get_float
;;; g_value_set_float
;;; ----------------------------------------------------------------------------

(defun (setf value-float) (value gvalue)
  (cffi:foreign-funcall "g_value_set_float"
                        (:pointer (:struct value)) gvalue
                        :float value
                        :void)
  value)

(cffi:defcfun ("g_value_get_float" value-float) :float
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-float gvalue) => value}
  @syntax{(setf (g:value-float gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"gfloat\"} type}
  @argument[value]{a float value}
  @begin{short}
    Float contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-float} function gets the contents of a @symbol{g:value}
  instance of @code{\"gfloat\"} type. The @setf{g:value-float} function sets
  the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-float)

;;; ----------------------------------------------------------------------------
;;; GParamSpecDouble
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-double
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :double)
  (:maximum :double)
  (:default-value :double)
  (:epsilon :double))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-double)
      "CStruct"
      (liber:symbol-documentation 'param-spec-double)
 "@version{2024-12-12}
  @begin{declaration}
(cffi:defcstruct param-spec-double
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :double)
  (:maximum :double)
  (:default-value :double)
  (:epsilon :double))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:minimum]{The double float with the minimum value.}
      @entry[:maximum]{The double float with the maximum value.}
      @entry[:default-value]{The double float with the default value.}
      @entry[:epsilon]{The double float value, values closer than epsilon will
        be considered identical, the default value is 1e-90.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    double properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-double}")

(export 'param-spec-double)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_double
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_double" param-spec-double)
    (:pointer (:struct param-spec-double))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{a double float with the minimum value}
  @argument[maximum]{a double float with the maximum value}
  @argument[default]{a double float with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-double} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"gdouble\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-double}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :double)
  (maximum :double)
  (default :double)
  (flags param-flags))

(export 'param-spec-double)

;;; ----------------------------------------------------------------------------
;;; g_value_get_double
;;; g_value_set_double
;;; ----------------------------------------------------------------------------

(defun (setf value-double) (value gvalue)
  (cffi:foreign-funcall "g_value_set_double"
                        (:pointer (:struct value)) gvalue
                        :double value
                        :void)
  value)

(cffi:defcfun ("g_value_get_double" value-double) :double
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-double gvalue) => value}
  @syntax{(setf (g:value-double gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"gdouble\"} type}
  @argument[value]{a double float value}
  @begin{short}
    Double float contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-double} function gets the contents of a @symbol{g:value}
  instance of @code{\"gdouble\"} type. The @setf{g:value-double} function sets
  the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-double)

;;; ----------------------------------------------------------------------------
;;; GParamSpecEnum
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-enum
  (:parent-instance (:pointer (:struct param-spec)))
  (:enum-class (:pointer (:struct enum-class)))
  (:default-value :int))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-enum)
      "CStruct"
      (liber:symbol-documentation 'param-spec-enum)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-enum
  (:parent-instance (:pointer (:struct param-spec)))
  (:enum-class (:pointer enum-class))
  (:default-value :int))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:enum-class]{The @symbol{g:enum-class} class instance for the
        enum.}
      @entry[:default-value]{The integer with the default value.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    enum properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-enum}")

(export 'param-spec-enum)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_enum
;;; ----------------------------------------------------------------------------

;; TODO:
;; This accepts any integer for default-value, but does not check for a valid
;; enum parameter. Can this be implemented better?

(cffi:defcfun ("g_param_spec_enum" param-spec-enum)
    (:pointer (:struct param-spec-enum))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @class{g:type-t} type ID derived from the @code{\"GEnum\"}
    type}
  @argument[default]{an integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-enum} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"GEnum\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-enum}
  @see-symbol{g:param-flags}
  @see-class{g:type-t}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (default :int)
  (flags param-flags))

(export 'param-spec-enum)

;;; ----------------------------------------------------------------------------
;;; g_value_get_enum
;;; g_value_set_enum
;;; ----------------------------------------------------------------------------

(defun (setf %value-enum) (value gvalue)
  (cffi:foreign-funcall "g_value_set_enum"
                        (:pointer (:struct value)) gvalue
                        :int value
                        :void)
  value)

(defun (setf value-enum) (value gvalue)
  (set-gvalue-enum gvalue value))

(cffi:defcfun ("g_value_get_enum" %value-enum) :int
  (gvalue (:pointer (:struct value))))

(defun value-enum (gvalue)
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-enum gvalue) => value}
  @syntax{(setf (g:value-enum gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance whose type is derived from
    @code{\"GEnum\"}}
  @argument[value]{an integer or keyword for the enumeration value}
  @begin{short}
    Enumeration value contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-enum} function gets the contents of a @symbol{g:value}
  instance of type @code{\"GEnum\"}. The @setf{g:value-enum} function sets the
  contents.
  @begin[Examples]{dictionary}
    @begin{pre}
(gobject:with-value (gvalue \"GEmblemOrigin\")
  (setf (g:value-enum gvalue) :device)
  (g:value-enum gvalue))
=> :DEVICE
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}"
  (get-gvalue-enum gvalue))

(export 'value-enum)

;;; ----------------------------------------------------------------------------
;;; GParamSpecFlags
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-flags
  (:parent-instance (:pointer (:struct param-spec)))
  (:flags-class (:pointer (:struct flags-class)))
  (:default-value :uint))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-flags)
      "CStruct"
      (liber:symbol-documentation 'param-spec-flags)
 "@version{2024-12-22}
  @begin{pre}
(cffi:defcstruct param-spec-flags
  (:parent-instance (:pointer (:struct param-spec)))
  (:flags-class (:pointer flags-class))
  (:default-value :uint))
  @end{pre}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:flags-class]{The @symbol{g:flags-class} class instance for the
        flags.}
      @entry[:default-value]{The unsigned integer with the default value.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    flags properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-flags}")

(export 'param-spec-flags)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_flags
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_flags" param-spec-flags)
    (:pointer (:struct param-spec-flags))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @class{g:type-t} type ID derived from the @code{\"GFlags\"}
    type}
  @argument[default]{an unsigned integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-flags} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"GFlags\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-flags}
  @see-symbol{g:param-flags}
  @see-class{g:type-t}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (default :uint)
  (flags param-flags))

(export 'param-spec-flags)

;;; ----------------------------------------------------------------------------
;;; g_value_get_flags
;;; g_value_set_flags
;;; ----------------------------------------------------------------------------

(defun (setf %value-flags) (value gvalue)
  (cffi:foreign-funcall "g_value_set_flags"
                        (:pointer (:struct value)) gvalue
                        :uint value
                        :void)
  value)

(defun (setf value-flags) (value gvalue)
  (set-gvalue-flags gvalue value))

(cffi:defcfun ("g_value_get_flags" %value-flags) :uint
  (gvalue (:pointer (:struct value))))

(defun value-flags (gvalue)
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-flags gvalue) => value}
  @syntax{(setf (g:value-flags gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance whose type is derived from
    @code{\"GFlags\"}}
  @argument[value]{a list with keywords or an unsigned integer with the flags
    value}
  @begin{short}
    Flags contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-flags} function gets the contents of a @symbol{g:value}
  instance of type @code{\"GFlags\"}. The @setf{g:value-flags} function sets
  the contents.
  @begin[Examples]{dictionary}
    @begin{pre}
(gobject:with-value (gvalue \"GApplicationFlags\")
  (setf (g:value-flags gvalue) '(:handles-open :is-service))
  (g:value-flags gvalue))
=> (:IS-SERVICE :HANDLES-OPEN)
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}"
  (get-gvalue-flags gvalue))

(export 'value-flags)

;;; ----------------------------------------------------------------------------
;;; GParamSpecString
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-string
  (:parent-instance (:pointer (:struct param-spec)))
  (:default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (:substitutor :char)
  ;; TODO: The definition in the C API is different. Check this.
  ;;;   guint         null_fold_if_empty : 1;
  ;;;   guint         ensure_non_null : 1;
  (:flags-for-null :uint))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-string)
      "CStruct"
      (liber:symbol-documentation 'param-spec-string)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-string
  (:parent-instance (:pointer (:struct param-spec)))
  (:default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (:substitutor :char)
  (:flags-for-null :uint))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:default-value]{The string with the default value.}
      @entry[:cset-frist]{The string containing the allowed values for the
        first byte.}
      @entry[:cset-nth]{The string containing the allowed values for the
        subsequent bytes.}
      @entry[:substitutor]{The character with the replacement byte for bytes
        which do not match @code{:cset-first} or @code{cset-nth}.}
      @entry[:flags-for-null]{The unsigned integer whether to replace empty
        string by @code{nil} and @code{nil} strings by an empty string.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    string properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-string}")

(export 'param-spec-string)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_string" param-spec-string)
    (:pointer (:struct param-spec-string))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[default]{a string with default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-string} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    @code{\"gchararray\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-string}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (default :string)
  (flags param-flags))

(export 'param-spec-string)

;;; ----------------------------------------------------------------------------
;;; g_value_get_string
;;; g_value_set_string
;;; ----------------------------------------------------------------------------

(defun (setf value-string) (value gvalue)
  (cffi:foreign-funcall "g_value_set_string"
                        (:pointer (:struct value)) gvalue
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_value_get_string" value-string) :string
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-string gvalue) => value}
  @syntax{(setf (g:value-string gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of @code{\"gchararray\"} type}
  @argument[value]{caller-owned string to be duplicated for the
    @symbol{g:value}}
  @begin{short}
    String content of @arg{gvalue}.
  @end{short}
  The @fun{g:value-string} function gets the contents of a @symbol{g:value}
  instance of @code{\"gchararray\"} type. The @setf{g:value-string} function
  sets the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-string)

;;; ----------------------------------------------------------------------------
;;; GParamSpecParam
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-param
  (:parent-instance (:pointer (:struct param-spec))))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-param)
      "CStruct"
      (liber:symbol-documentation 'param-spec-param)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-param
  (:parent-instance (:pointer (:struct param-spec))))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    properties of @code{\"GParam\"} type.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-param}")

(export 'param-spec-param)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_param
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_param" param-spec-param)
    (:pointer (:struct param-spec-param))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @class{g:type-t} type ID derived from the @code{\"GParam\"}
    type}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-param} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"GParam\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-param}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (flags param-flags))

(export 'param-spec-param)

;;; ----------------------------------------------------------------------------
;;; g_value_get_param
;;; g_value_set_param
;;; ----------------------------------------------------------------------------

(defun (setf value-param) (value gvalue)
  (cffi:foreign-funcall "g_value_set_param"
                        (:pointer (:struct value)) gvalue
                        (:pointer (:struct param-spec)) value
                        :void)
  value)

(cffi:defcfun ("g_value_get_param" value-param) (:pointer (:struct param-spec))
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-param gvalue) => value}
  @syntax{(setf (g:value-param gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance whose type is derived from
    @code{\"GParam\"}}
  @argument[value]{a @symbol{g:param-spec} value}
  @begin{short}
    The @symbol{g:param-spec} content of @arg{gvalue}.
  @end{short}
  The @fun{g:value-param} function gets the contents of a @symbol{g:value}
  instance of @code{\"GParam\"} type. The @setf{g:value-param} function sets
  the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-param)

;;; ----------------------------------------------------------------------------
;;; GParamSpecBoxed
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-boxed
  (:parent-instance (:pointer (:struct param-spec))))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-boxed)
      "CStruct"
      (liber:symbol-documentation 'param-spec-boxed)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-boxed
  (:parent-instance (:pointer (:struct param-spec))))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    boxed properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-boxed}")

(export 'param-spec-boxed)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boxed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_boxed" param-spec-boxed)
    (:pointer (:struct param-spec-boxed))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @code{\"GBoxed\"} derived type of this property}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-boxed} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    derived of @code{\"GBoxed\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-boxed}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (flags param-flags))

(export 'param-spec-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_get_boxed
;;; g_value_set_boxed
;;; ----------------------------------------------------------------------------

(defun (setf value-boxed) (value gvalue)
  (cffi:foreign-funcall "g_value_set_boxed"
                        (:pointer (:struct value)) gvalue
                        :pointer value
                        :void)
  value)

(cffi:defcfun ("g_value_get_boxed" value-boxed) :pointer
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-boxed gvalue) => value}
  @syntax{(setf (g:value-boxed gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"GBoxed\"} type}
  @argument[value]{a boxed value}
  @begin{short}
    Boxed contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-boxed} function gets the contents of a @symbol{g:value}
  instance derived of the @code{\"GBoxed\"} type. The @setf{g:value-boxed}
  function sets the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_take_boxed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_take_boxed" value-take-boxed) :void
 #+liber-documentation
 "@version{2024-12-22}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"GBoxed\"} type}
  @argument[value]{a value}
  @begin{short}
    Sets the contents of a @symbol{g:value} instance of @code{\"GBoxed\"} type
    to @arg{value}.
  @end{short}
  The function takes over the ownership of the callers reference to @arg{value}.
  The caller does not have to unref it any more.
  @see-symbol{g:value}
  @see-function{g:value-boxed}"
  (gvalue (:pointer (:struct value)))
  (value :pointer))

(export 'value-take-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_set_boxed_take_ownership ()
;;;
;;; void g_value_set_boxed_take_ownership (GValue *value, gconstpointer v_boxed)
;;;
;;; Warning
;;;
;;; g_value_set_boxed_take_ownership has been deprecated since version 2.4 and
;;; should not be used in newly written code. Use g_value_take_boxed() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_BOXED derived type
;;;
;;; v_boxed :
;;;     duplicated unowned boxed value to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_dup_boxed ()
;;;
;;; gpointer g_value_dup_boxed (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_BOXED derived GValue. Upon getting, the boxed
;;; value is duplicated and needs to be later freed with g_boxed_free(), e.g.
;;; like: g_boxed_free (G_VALUE_TYPE (value), return_value);
;;;
;;; value :
;;;     a valid GValue of G_TYPE_BOXED derived type
;;;
;;; Returns :
;;;     boxed contents of value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GParamSpecPointer
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-pointer
  (:parent-instance (:pointer (:struct param-spec))))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-pointer)
      "CStruct"
      (liber:symbol-documentation 'param-spec-pointer)
 "@version{#2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-pointer
  (:parent-instance (:pointer (:struct param-spec))))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    pointer properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-pointer}")

(export 'param-spec-pointer)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pointer
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_pointer" param-spec-pointer)
    (:pointer (:struct param-spec-pointer))
 #+liber-documentation
 "@version{#2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-pointer} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of @code{\"gpointer\"} type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-pointer}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (flags param-flags))

(export 'param-spec-pointer)

;;; ----------------------------------------------------------------------------
;;; g_value_get_pointer
;;; g_value_set_pointer
;;; ----------------------------------------------------------------------------

(defun (setf value-pointer) (value gvalue)
  (cffi:foreign-funcall "g_value_set_pointer"
                        (:pointer (:struct value)) gvalue
                        :pointer value
                        :void)
  value)

(cffi:defcfun ("g_value_get_pointer" value-pointer) :pointer
 #+liber-documentation
 "@version{#2024-12-22}
  @syntax{(g:value-pointer gvalue) => value}
  @syntax{(setf (g:value-pointer gvalue) value)}
  @argument[gvalue]{a valid @symbol{g:value} instance of @code{gpointer}}
  @argument[value]{pointer value to be set}
  @begin{short}
    Pointer contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-pointer} function gets the contents of a @symbol{g:value}
  instance of @code{\"gpointer\"} type. The @setf{g:value-pointer} function
  sets the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-pointer)

;;; ----------------------------------------------------------------------------
;;; GParamSpecObject
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-object
  (:parent-instance (:pointer (:struct param-spec))))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-object)
      "CStruct"
      (liber:symbol-documentation 'param-spec-object)
 "@version{#2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-object
  (:parent-instance (:pointer (:struct param-spec))))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    object properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-object}")

(export 'param-spec-object)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_object
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_object" param-spec-object)
    (:pointer (:struct param-spec-object))
 #+liber-documentation
 "@version{#2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @code{\"GObject\"} derived type of this property}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-object} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of a @code{\"GObject\"} dervived type.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-object}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (flags param-flags))

(export 'param-spec-object)

;;; ----------------------------------------------------------------------------
;;; g_value_get_object
;;; g_value_set_object
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_set_object" %value-set-object) :void
  (gvalue (:pointer (:struct value)))
  (value :pointer))

(defun (setf value-object) (value gvalue)
  (%value-set-object gvalue
                     (if value
                         (object-pointer value)
                         (cffi:null-pointer)))
  value)

(cffi:defcfun ("g_value_get_object" %value-get-object) :pointer
  (value (:pointer (:struct value))))

(defun value-object (gvalue)
 #+liber-documentation
 "@version{#2024-12-22}
  @syntax{(g:value-object gvalue) => value}
  @syntax{(setf (g:value-object gvalue) value)}
  @argument[gvalue]{a valid @symbol{g:value} instance of @code{\"GObject\"}
    derived type}
  @argument[value]{object value of derived @code{\"GObject\"} type}
  @begin{short}
    Object contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-object} function gets the contents of a @symbol{g:value}
  instance of a derived @code{\"GObject\"} type. The @setf{g:value-object)}
  function sets the contents of a @symbol{g:value} instance to @arg{value}.

  The @setf{g:value-object} function increases the reference count of
  @arg{value}, the @symbol{g:value} instance holds a reference to @arg{value}.
  If you do not wish to increase the reference count of the object, that is,
  you wish to pass your current reference to the @symbol{g:value} instance
  because you no longer need it, use the @fun{g:value-take-object} function
  instead.

  It is important that your @symbol{g:value} instance holds a reference to
  @arg{value}, either its own, or one it has taken, to ensure that the object
  will not be destroyed while the @symbol{g:value} instance still exists).
  @see-symbol{g:value}"
  (get-or-create-gobject-for-pointer (%value-get-object gvalue)))

(export 'value-object)

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecUnichar
;;;
;;; struct GParamSpecUnichar {
;;;   GParamSpec    parent_instance;
;;;
;;;   gunichar      default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for unichar
;;; (unsigned integer) properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gunichar default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_unichar ()
;;;
;;; GParamSpec * g_param_spec_unichar (const gchar *name,
;;;                                    const gchar *nick,
;;;                                    const gchar *blurb,
;;;                                    gunichar default_value,
;;;                                    GParamFlags flags);
;;;
;;; Creates a new GParamSpecUnichar instance specifying a G_TYPE_UINT property.
;;; GValue structures for this property can be accessed with g_value_set_uint()
;;; and g_value_get_uint().
;;;
;;; See g_param_spec_internal() for details on property names.
;;;
;;; name :
;;;     canonical name of the property specified
;;;
;;; nick :
;;;     nick name for the property specified
;;;
;;; blurb :
;;;     description of the property specified
;;;
;;; default_value :
;;;     default value for the property specified
;;;
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     a newly created parameter specification
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GParamSpecValueArray                                    not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-value-array
  (:parent-instance (:pointer (:struct param-spec)))
  (:element-spec (:pointer (:struct param-spec)))
  (:fixed-n-elements :uint))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-value-array)
      "CStruct"
      (liber:symbol-documentation 'param-spec-value-array)
 "@version{#2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-value-array
  (:parent-instance (:pointer (:struct param-spec)))
  (:element-spec (:pointer (:struct param-spec)))
  (:fixed-n-elements :uint))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:private-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:element-spec]{The @symbol{g:param-spec} describing the elements
        contained in arrays of this property, may be @code{NULL}}
      @entry[:fixed-n-elements]{The @code{:uint}, if greater than 0, arrays of
        this property will always have this many elements}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    @code{GValueArray} properties.
  @end{short}
  @see-symbol{g:param-spec}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_value_array                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_value_array" param-spec-value-array)
    (:pointer (:struct param-spec-value-array))
 #+liber-documentation
 "@version{#2024-12-22}
  @argument[name]{a @code{:string} with the canonical name}
  @argument[nick]{a @code{:string} with the nick name}
  @argument[blurb]{a @code{:string} with the description}
  @argument[element-spec]{a @symbol{g:param-spec} describing the elements
    contained in arrays of this property, may be @code{NULL}}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-value-array} parameter
    specification.}
  @begin{short}
    Creates a new parameter specificytion instance specifying a
    @code{GValueArray} property. @code{GValueArray} is a @code{GBoxed} type, as
    such, @symbol{g:value} structures for this property can be accessed with
    @fun{g:value-set-boxed} and @fun{g:value-get-boxed}.
  @end{short}

  See the function @fun{g:param-spec-internal} for details on property names.
  @see-symbol{g:param-spec-value-array}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (element-spec (:pointer (:struct param-spec)))
  (flags param-flags))

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecOverride
;;;
;;; struct GParamSpecOverride {
;;; };
;;;
;;; This is a type of GParamSpec type that simply redirects operations to
;;; another paramspec. All operations other than getting or setting the value
;;; are redirected, including accessing the nick and blurb, validating a value,
;;; and so forth. See g_param_spec_get_redirect_target() for retrieving the
;;; overidden property. GParamSpecOverride is used in implementing
;;; g_object_class_override_property(), and will not be directly useful unless
;;; you are implementing a new base type similar to GObject.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_override ()
;;;
;;; GParamSpec * g_param_spec_override (const gchar *name,
;;;                                     GParamSpec *overridden);
;;;
;;; Creates a new property of type GParamSpecOverride. This is used to direct
;;; operations to another paramspec, and will not be directly useful unless you
;;; are implementing a new base type similar to GObject.
;;;
;;; name :
;;;     the name of the property.
;;;
;;; overridden :
;;;     The property that is being overridden
;;;
;;; Returns :
;;;     the newly created GParamSpec
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GParamSpecGType
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-gtype
  (:parent-instance (:pointer (:struct param-spec)))
  (:is-a-type type-t))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-gtype)
      "CStruct"
      (liber:symbol-documentation 'param-spec-gtype)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec-gtype
  (:parent-instance (:pointer (:struct param-spec)))
  (:is-a-type type-t))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
      @entry[:is-a-type]{The @class{g:type-t} type ID whose subtypes can occur
        as values.}
    @end{table}
  @end{values}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    @class{g:type-t} type ID properties.
  @end{short}
  @see-symbol{g:param-spec}
  @see-symbol{g:type-t}
  @see-function{g:param-spec-gtype}")

(export 'param-spec-gtype)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_gtype
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_gtype" param-spec-gtype)
    (:pointer (:struct param-spec-gtype))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[is-a-type]{a @class{g:type-t} type ID whose subtypes are allowed as
    values of the property (use the \"void\" type for any type)}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{The newly created @symbol{g:param-spec-gtype} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a @code{\"GType\"}
    property.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-class{g:type-t}
  @see-symbol{g:param-spec-gtype}
  @see-symbol{g:param-flags}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (is-a-type type-t)
  (flags param-flags))

(export 'param-spec-gtype)

;;; ----------------------------------------------------------------------------
;;; g_value_get_gtype
;;; g_value_set_gtype
;;; ----------------------------------------------------------------------------

;; TODO: Is this implementation correct?!

(defun (setf value-gtype) (value gvalue)
  (let ((value (glib:gtype value)))
    (cffi:foreign-funcall "g_value_set_gtype"
                          (:pointer (:struct value)) gvalue
                          type-t value
                          :void)
    value))

(cffi:defcfun ("g_value_get_gtype" value-gtype) type-t
 #+liber-documentation
 "@version{2024-12-22}
  @syntax{(g:value-gtype gvalue) => value}
  @syntax{(setf (g:value-gtype gvalue) value)}
  @argument[gvalue]{a valid @symbol{g:value} instance of @class{g:type-t}
    type ID}
  @argument[value]{a @class{g:type-t} type ID value}
  @return{The @class{g:type-t} type ID stored in @arg{gvalue}.}
  @begin{short}
    The @fun{g:value-gtype} function gets the contents of a @class{g:type-t}
    type ID value.
  @end{short}
  The @setf{g:value-gtype} function sets the contents.
  @see-symbol{g:value}
  @see-class{g:type-t}"
  (gvalue (:pointer (:struct value))))

(export 'value-gtype)

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecVariant
;;;
;;; struct GParamSpecVariant {
;;;   GParamSpec    parent_instance;
;;;   GVariantType *type;
;;;   GVariant     *default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for GVariant
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; GVariantType *type;
;;;     a GVariantType, or NULL
;;;
;;; GVariant *default_value;
;;;     a GVariant, or NULL
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_variant ()
;;;
;;; GParamSpec * g_param_spec_variant (const gchar *name,
;;;                                    const gchar *nick,
;;;                                    const gchar *blurb,
;;;                                    const GVariantType *type,
;;;                                    GVariant *default_value,
;;;                                    GParamFlags flags);
;;;
;;; Creates a new GParamSpecVariant instance specifying a GVariant property.
;;;
;;; If default_value is floating, it is consumed.
;;;
;;; See g_param_spec_internal() for details on property names.
;;;
;;; name :
;;;     canonical name of the property specified
;;;
;;; nick :
;;;     nick name for the property specified
;;;
;;; blurb :
;;;     description of the property specified
;;;
;;; type :
;;;     a GVariantType
;;;
;;; default_value :
;;;     a GVariant of type type to use as the default value, or NULL
;;;
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     the newly created GParamSpec
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_get_variant
;;; g_value_set_variant
;;; ----------------------------------------------------------------------------

(defun (setf value-variant) (value gvalue)
  (cffi:foreign-funcall "g_value_set_variant"
                        (:pointer (:struct value)) gvalue
                        (:pointer (:struct glib:variant)) value
                        :void)
  value)

(cffi:defcfun ("g_value_get_variant" value-variant)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{#2024-12-22}
  @syntax{(g:value-variant gvalue) => value}
  @syntax{(setf (g:value-variant gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{\"GVariant\"} type}
  @argument[value]{a @symbol{g:variant} value}
  @begin{short}
    Variant contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-variant} function gets the contents of a @symbol{g:value}
  instance of type @code{\"GVariant\"}. The @setf{g:value-variant} function
  sets the contents.
  @see-symbol{g:value}
  @see-symbol{g:variant}"
  (gvalue (:pointer (:struct value))))

(export 'value-variant)

;;; ----------------------------------------------------------------------------
;;; g_value_dup_variant ()
;;;
;;; GVariant * g_value_dup_variant (const GValue *value);
;;;
;;; Get the contents of a variant GValue, increasing its refcount.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_VARIANT
;;;
;;; Returns :
;;;     variant contents of value, should be unrefed using g_variant_unref()
;;;     when no longer needed
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_take_variant ()
;;;
;;; void g_value_take_variant (GValue *value, GVariant *variant);
;;;
;;; Set the contents of a variant GValue to variant, and takes over the
;;; ownership of the caller's reference to variant; the caller does not have
;;; to unref it any more (i.e. the reference count of the variant is not
;;; increased).
;;;
;;; If variant was floating then its floating reference is converted to a hard
;;; reference.
;;;
;;; If you want the GValue to hold its own reference to variant, use
;;; g_value_set_variant() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_VARIANT
;;;
;;; variant :
;;;     a GVariant, or NULL
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.param.lisp -----------------------------------------
