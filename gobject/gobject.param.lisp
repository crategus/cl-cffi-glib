;;; ----------------------------------------------------------------------------
;;; gobject.param.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;; Parameters and Values
;;;
;;;     Standard Parameter and Value Types
;;;
;;; Synopsis
;;;
;;;     G_IS_PARAM_SPEC_BOOLEAN
;;;     G_PARAM_SPEC_BOOLEAN
;;;     G_VALUE_HOLDS_BOOLEAN
;;;     G_TYPE_PARAM_BOOLEAN
;;;
;;;     GParamSpecBoolean
;;;
;;;     g_param_spec_boolean
;;;     g_value_set_boolean
;;;     g_value_get_boolean
;;;
;;;     G_IS_PARAM_SPEC_CHAR
;;;     G_PARAM_SPEC_CHAR
;;;     G_VALUE_HOLDS_CHAR
;;;     G_TYPE_PARAM_CHAR
;;;
;;;     GParamSpecChar
;;;
;;;     g_param_spec_char
;;;     g_value_set_char
;;;     g_value_get_char
;;;     g_value_get_schar
;;;     g_value_set_schar
;;;
;;;     G_IS_PARAM_SPEC_UCHAR
;;;     G_PARAM_SPEC_UCHAR
;;;     G_VALUE_HOLDS_UCHAR
;;;     G_TYPE_PARAM_UCHAR
;;;
;;;     GParamSpecUChar
;;;
;;;     g_param_spec_uchar
;;;     g_value_set_uchar
;;;     g_value_get_uchar
;;;
;;;     G_IS_PARAM_SPEC_INT
;;;     G_PARAM_SPEC_INT
;;;     G_VALUE_HOLDS_INT
;;;     G_TYPE_PARAM_INT
;;;
;;;     GParamSpecInt
;;;
;;;     g_param_spec_int
;;;     g_value_set_int
;;;     g_value_get_int
;;;
;;;     G_IS_PARAM_SPEC_UINT
;;;     G_PARAM_SPEC_UINT
;;;     G_VALUE_HOLDS_UINT
;;;     G_TYPE_PARAM_UINT
;;;
;;;     GParamSpecUInt
;;;
;;;     g_param_spec_uint
;;;     g_value_set_uint
;;;     g_value_get_uint
;;;
;;;     G_IS_PARAM_SPEC_LONG
;;;     G_PARAM_SPEC_LONG
;;;     G_VALUE_HOLDS_LONG
;;;     G_TYPE_PARAM_LONG
;;;
;;;     GParamSpecLong
;;;
;;;     g_param_spec_long
;;;     g_value_set_long
;;;     g_value_get_long
;;;
;;;     G_IS_PARAM_SPEC_ULONG
;;;     G_PARAM_SPEC_ULONG
;;;     G_VALUE_HOLDS_ULONG
;;;     G_TYPE_PARAM_ULONG
;;;
;;;     GParamSpecULong
;;;
;;;     g_param_spec_ulong
;;;     g_value_set_ulong
;;;     g_value_get_ulong
;;;
;;;     G_IS_PARAM_SPEC_INT64
;;;     G_PARAM_SPEC_INT64
;;;     G_VALUE_HOLDS_INT64
;;;     G_TYPE_PARAM_INT64
;;;
;;;     GParamSpecInt64
;;;
;;;     g_param_spec_int64
;;;     g_value_set_int64
;;;     g_value_get_int64
;;;
;;;     G_IS_PARAM_SPEC_UINT64
;;;     G_PARAM_SPEC_UINT64
;;;     G_VALUE_HOLDS_UINT64
;;;     G_TYPE_PARAM_UINT64
;;;
;;;     GParamSpecUInt64
;;;
;;;     g_param_spec_uint64
;;;     g_value_set_uint64
;;;     g_value_get_uint64
;;;
;;;     G_IS_PARAM_SPEC_FLOAT
;;;     G_PARAM_SPEC_FLOAT
;;;     G_VALUE_HOLDS_FLOAT
;;;     G_TYPE_PARAM_FLOAT
;;;
;;;     GParamSpecFloat
;;;
;;;     g_param_spec_float
;;;     g_value_set_float
;;;     g_value_get_float
;;;
;;;     G_IS_PARAM_SPEC_DOUBLE
;;;     G_PARAM_SPEC_DOUBLE
;;;     G_VALUE_HOLDS_DOUBLE
;;;     G_TYPE_PARAM_DOUBLE
;;;
;;;     GParamSpecDouble
;;;
;;;     g_param_spec_double
;;;     g_value_set_double
;;;     g_value_get_double
;;;
;;;     G_IS_PARAM_SPEC_ENUM
;;;     G_PARAM_SPEC_ENUM
;;;     G_VALUE_HOLDS_ENUM
;;;     G_TYPE_PARAM_ENUM
;;;
;;;     GParamSpecEnum
;;;
;;;     g_param_spec_enum
;;;     g_value_set_enum
;;;     g_value_get_enum
;;;
;;;     G_IS_PARAM_SPEC_FLAGS
;;;     G_PARAM_SPEC_FLAGS
;;;     G_VALUE_HOLDS_FLAGS
;;;     G_TYPE_PARAM_FLAGS
;;;
;;;     GParamSpecFlags
;;;
;;;     g_param_spec_flags
;;;     g_value_set_flags
;;;     g_value_get_flags
;;;
;;;     G_IS_PARAM_SPEC_STRING
;;;     G_PARAM_SPEC_STRING
;;;     G_VALUE_HOLDS_STRING
;;;     G_VALUE_IS_INTERNED_STRING
;;;     G_TYPE_PARAM_STRING
;;;     G_VALUE_INTERNED_STRING
;;;
;;;     GParamSpecString
;;;     gchararray
;;;
;;;     g_param_spec_string
;;;     g_value_set_string
;;;     g_value_set_static_string
;;;     g_value_take_string
;;;     g_value_set_string_take_ownership
;;;     g_value_get_string
;;;     g_value_dup_string
;;;     g_value_set_interned_string
;;;
;;;     G_IS_PARAM_SPEC_PARAM
;;;     G_PARAM_SPEC_PARAM
;;;     G_VALUE_HOLDS_PARAM
;;;     G_TYPE_PARAM_PARAM
;;;
;;;     GParamSpecParam
;;;
;;;     g_param_spec_param
;;;     g_value_set_param
;;;     g_value_take_param
;;;     g_value_set_param_take_ownership
;;;     g_value_get_param
;;;     g_value_dup_param
;;;
;;;     G_IS_PARAM_SPEC_BOXED
;;;     G_PARAM_SPEC_BOXED
;;;     G_VALUE_HOLDS_BOXED
;;;     G_TYPE_PARAM_BOXED
;;;
;;;     GParamSpecBoxed
;;;
;;;     g_param_spec_boxed
;;;     g_value_set_boxed
;;;     g_value_set_static_boxed
;;;     g_value_take_boxed
;;;     g_value_set_boxed_take_ownership
;;;     g_value_get_boxed
;;;     g_value_dup_boxed
;;;
;;;     G_IS_PARAM_SPEC_POINTER
;;;     G_PARAM_SPEC_POINTER
;;;     G_VALUE_HOLDS_POINTER
;;;     G_TYPE_PARAM_POINTER
;;;
;;;     GParamSpecPointer
;;;
;;;     g_param_spec_pointer
;;;     g_value_set_pointer
;;;     g_value_get_pointer
;;;
;;;     G_IS_PARAM_SPEC_OBJECT
;;;     G_PARAM_SPEC_OBJECT
;;;     G_VALUE_HOLDS_OBJECT
;;;     G_TYPE_PARAM_OBJECT
;;;
;;;     GParamSpecObject
;;;
;;;     g_param_spec_object
;;;     g_value_set_object
;;;     g_value_take_object
;;;     g_value_set_object_take_ownership
;;;     g_value_get_object
;;;     g_value_dup_object
;;;
;;;     G_IS_PARAM_SPEC_UNICHAR
;;;     G_PARAM_SPEC_UNICHAR
;;;     G_TYPE_PARAM_UNICHAR
;;;
;;;     GParamSpecUnichar
;;;
;;;     g_param_spec_unichar
;;;
;;;     G_IS_PARAM_SPEC_VALUE_ARRAY
;;;     G_PARAM_SPEC_VALUE_ARRAY
;;;     G_TYPE_PARAM_VALUE_ARRAY
;;;
;;;     GParamSpecValueArray
;;;
;;;     g_param_spec_value_array
;;;
;;;     G_IS_PARAM_SPEC_OVERRIDE
;;;     G_PARAM_SPEC_OVERRIDE
;;;     G_TYPE_PARAM_OVERRIDE
;;;
;;;     GParamSpecOverride
;;;
;;;     g_param_spec_override
;;;
;;;     G_IS_PARAM_SPEC_GTYPE
;;;     G_PARAM_SPEC_GTYPE
;;;     G_VALUE_HOLDS_GTYPE
;;;     G_TYPE_PARAM_GTYPE
;;;
;;;     GParamSpecGType
;;;
;;;     g_param_spec_gtype
;;;     g_value_get_gtype
;;;     g_value_set_gtype
;;;
;;;     G_IS_PARAM_SPEC_VARIANT
;;;     G_PARAM_SPEC_VARIANT
;;;     G_VALUE_HOLDS_VARIANT
;;;     G_TYPE_PARAM_VARIANT
;;;
;;;     GParamSpecVariant
;;;
;;;     g_param_spec_variant
;;;     g_value_get_variant
;;;     g_value_dup_variant
;;;     g_value_set_variant
;;;     g_value_take_variant
;;;
;;; Description
;;;
;;; GValue provides an abstract container structure which can be copied,
;;; transformed and compared while holding a value of any (derived) type, which
;;; is registered as a GType with a GTypeValueTable in its GTypeInfo structure.
;;; Parameter specifications for most value types can be created as GParamSpec
;;; derived instances, to implement e.g. GObject properties which operate on
;;; GValue containers.
;;;
;;; Parameter names need to start with a letter (a-z or A-Z). Subsequent
;;; characters can be letters, numbers or a '-'. All other characters are
;;; replaced by a '-' during construction.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_BOOLEAN()
;;;
;;; #define G_IS_PARAM_SPEC_BOOLEAN(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_BOOLEAN))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_BOOLEAN.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_BOOLEAN()
;;;
;;; #define G_PARAM_SPEC_BOOLEAN(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_BOOLEAN, GParamSpecBoolean))
;;;
;;; Cast a GParamSpec instance into a GParamSpecBoolean.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_BOOLEAN()
;;;
;;; #define G_VALUE_HOLDS_BOOLEAN(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_BOOLEAN))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_BOOLEAN.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_BOOLEAN
;;;
;;; #define G_TYPE_PARAM_BOOLEAN (g_param_spec_types[2])
;;;
;;; The GType of GParamSpecBoolean.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecBoolean
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-boolean
  (:parent-instance (:pointer (:struct param-spec)))
  (:default-value :boolean))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-boolean)
      "CStruct"
      (liber:symbol-documentation 'param-spec-boolean)
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g;param-spec} derived structure that contains the meta data for
    boolean properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-boolean
  (:parent-instance (:pointer (:struct param-spec)))
  (:default-value :boolean))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:default-value]{A boolean default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-boolean}")

(export 'param-spec-boolean)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boolean ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_boolean" param-spec-boolean)
    (:pointer (:struct param-spec-boolean))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[default]{a boolean with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-boolean} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    type @var{+g-type-boolean+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-boolean}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-boolean+}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (default :boolean)
  (flags param-flags))

(export 'param-spec-boolean)

;;; ----------------------------------------------------------------------------
;;; g_value_set_boolean ()
;;; g_value_get_boolean () -> value-boolean
;;; ----------------------------------------------------------------------------

(defun (setf value-boolean) (value gvalue)
  (cffi:foreign-funcall "g_value_set_boolean"
                        (:pointer (:struct value)) gvalue
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("g_value_get_boolean" value-boolean) :boolean
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-boolean gvalue) => value}
  @syntax[]{(setf (g:value-boolan gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-boolean+}}
  @argument[value]{a boolean value}
  @begin{short}
    Boolean contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-boolean} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-boolean+}. The @sym{(setf g:value-boolean)} function
  sets the contents of the @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-boolean+}"
  (gvalue (:pointer (:struct value))))

(export 'value-boolean)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_CHAR()
;;;
;;; #define G_IS_PARAM_SPEC_CHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_CHAR))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_CHAR.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_CHAR()
;;;
;;; #define G_PARAM_SPEC_CHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_CHAR, GParamSpecChar))
;;;
;;; Cast a GParamSpec instance into a GParamSpecChar.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_CHAR()
;;;
;;; #define G_VALUE_HOLDS_CHAR(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_CHAR))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_CHAR.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_CHAR
;;;
;;; #define G_TYPE_PARAM_CHAR (g_param_spec_types[0])
;;;
;;; The GType of GParamSpecChar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecChar
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    character properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-char
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :int8)
  (:maximum :int8)
  (:default-value :int8))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{An 8-bit integer with the minimum value.}
    @entry[:maximum]{An 8-bit integer with the maximum value.}
    @entry[:default-value]{An 8-bit integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-char}")

(export 'param-spec-char)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_char ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_char" param-spec-char)
    (:pointer (:struct param-spec-char))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an 8-bit integer with the minimum value}
  @argument[maximum]{an 8-bit integer with the maximum value}
  @argument[default]{an 8-bit integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-char} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    type @var{+g-type-char+}.
  @end{short}
  @see-symbol{g:param-spec-char}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-char+}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int8)
  (maximum :int8)
  (default :int8)
  (flags param-flags))

(export 'param-spec-char)

;;; ----------------------------------------------------------------------------
;;; g_value_set_char ()
;;; g_value_get_char () -> value-char
;;; ----------------------------------------------------------------------------

(defun (setf value-char) (value gvalue)
  (cffi:foreign-funcall "g_value_set_char"
                        (:pointer (:struct value)) gvalue
                        :int8 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_char" value-char) :int8
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-char gvalue) => value}
  @syntax[]{(setf (g:value-char gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-char+}}
  @argument[value]{an 8-bit integer with the character value}
  @begin{short}
     Character contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-char} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-char+}. The @sym{(setf g:value-char)} function sets the
  contents of a @symbol{g:value} to @arg{value}.
  @begin[Warning]{dictionary}
    The @sym{g:value-char} function has been deprecated since version 2.32 and
    should not be used in newly written code. The function return type is
    broken, see the @fun{g:value-schar} function.
  @end{dictionary}
  @see-symbol{g:value}
  @see-variable{+g-type-char+}
  @see-function{g:value-schar}"
  (gvalue (:pointer (:struct value))))

(export 'value-char)

;;; ----------------------------------------------------------------------------
;;; g_value_get_schar ()
;;; g_value_set_schar () -> value-schar
;;; ----------------------------------------------------------------------------

(defun (setf value-schar) (value gvalue)
  (cffi:foreign-funcall "g_value_set_schar"
                        (:pointer (:struct value)) gvalue
                        :int8 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_schar" value-schar) :int8
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-schar gvalue) => value}
  @syntax[]{(setf (g:value-schar gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-char+}}
  @argument[value]{an integer with the character value}
  @begin{short}
    Signed 8-bit integer contents of @arg{gvalue}.
  @end{short}
  The @fun{g:value-schar} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-char+}. The @sym{(setf g:value-schar)} function sets
  the contents of a @symbol{g:value} to @arg{value}.

  Since 2.32
  @see-symbol{g:value}
  @see-variable{+g-type-char+}"
  (gvalue (:pointer (:struct value))))

(export 'value-schar)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_UCHAR()
;;;
;;; #define G_IS_PARAM_SPEC_UCHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_UCHAR))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_UCHAR.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_UCHAR()
;;;
;;; #define G_PARAM_SPEC_UCHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_UCHAR, GParamSpecUChar))
;;;
;;; Cast a GParamSpec instance into a GParamSpecUChar.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_UCHAR()
;;;
;;; #define G_VALUE_HOLDS_UCHAR(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_UCHAR))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_UCHAR.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_UCHAR
;;;
;;; #define G_TYPE_PARAM_UCHAR (g_param_spec_types[1])
;;;
;;; The GType of GParamSpecUChar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecUChar
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    unsigned character properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-uchar
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :uint8)
  (:maximum :uint8)
  (:default-value :uint8))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{An unsigned 8-bit integer with the minimum value.}
    @entry[:maximum]{An unsigned 8-bit integer with the maximum value.}
    @entry[:default-value]{An unsigned 8-bit integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-uchar}")

(export 'param-spec-uchar)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uchar ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_uchar" param-spec-uchar)
    (:pointer (:struct param-spec-uchar))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an unsigned 8-bit integer with the minimum value}
  @argument[maximum]{an unsigned 8-bit integer with the maximum value}
  @argument[default]{an unsigned 8-bit integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-uchar} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    type @var{+g-type-uchar+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-uchar}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-uchar+}
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
;;; g_value_get_uchar ()
;;; g_value_set_uchar () -> value-uchar
;;; ----------------------------------------------------------------------------

(defun (setf value-uchar) (value gvalue)
  (cffi:foreign-funcall "g_value_set_uchar"
                        (:pointer (:struct value)) gvalue
                        :uint8 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_uchar" value-uchar) :uint8
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-uchar gvalue) => value}
  @syntax[]{(setf (g:value-uchar gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-uchar+}}
  @argument[value]{an unsigned 8-bit integer with the unsigned character value}
  @begin{short}
    Unsigned character contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-uchar} function gets the contents of a @symol{g:value} of
  type @var{+g-type-uchar+}. The @sym{(setf g:value-uchar)} function sets the
  contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-uchar+}"
  (gvalue (:pointer (:struct value))))

(export 'value-uchar)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_INT()
;;;
;;; #define G_IS_PARAM_SPEC_INT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_INT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_INT.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_INT()
;;;
;;; #define G_PARAM_SPEC_INT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_INT, GParamSpecInt))
;;;
;;; Cast a GParamSpec instance into a GParamSpecInt.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_INT()
;;;
;;; #define G_VALUE_HOLDS_INT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_INT))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_INT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_INT
;;;
;;; #define G_TYPE_PARAM_INT (g_param_spec_types[3])
;;;
;;; The GType of GParamSpecInt.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecInt
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    integer properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-int
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :int)
  (:maximum :int)
  (:default-value :int))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{An integer with the minimum value.}
    @entry[:maximum]{An integer with the maximum value.}
    @entry[:default-value]{An integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-int}")

(export 'param-spec-int)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_int" param-spec-int)
    (:pointer (:struct param-spec-int))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an integer with the minimum value}
  @argument[maximum]{an integer with the maximum value}
  @argument[default]{an integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-int} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-int+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-int}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-int+}
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
;;; g_value_get_int ()
;;; g_value_set_int () -> value-int
;;; ----------------------------------------------------------------------------

(defun (setf value-int) (value gvalue)
  (cffi:foreign-funcall "g_value_set_int"
                        (:pointer (:struct value)) gvalue
                        :int value
                        :void)
  value)

(cffi:defcfun ("g_value_get_int" value-int) :int
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-int gvalue) => value}
  @syntax[]{(setf (g:value-int gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-int+}}
  @argument[value]{an integer value}
  @begin{short}
    Integer contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-int} function gets the contents of a @symbol{g:value} of
  type @var{+g-type-int+}. The @sym{(setf value-int)} function sets the
  contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-int+}"
  (gvalue (:pointer (:struct value))))

(export 'value-int)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_UINT()
;;;
;;; #define G_IS_PARAM_SPEC_UINT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_UINT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_UINT.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_UINT()
;;;
;;; #define G_PARAM_SPEC_UINT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_UINT, GParamSpecUInt))
;;;
;;; Cast a GParamSpec instance into a GParamSpecUInt.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; G_VALUE_HOLDS_UINT()
;;;
;;; #define G_VALUE_HOLDS_UINT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_UINT))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_UINT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_UINT
;;;
;;; #define G_TYPE_PARAM_UINT (g_param_spec_types[4])
;;;
;;; The GType of GParamSpecUInt.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecUInt
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    unsigned integer properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-uint
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :uint)
  (:maximum :uint)
  (:default-value :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{An unsigned integer with the minimum value.}
    @entry[:maximum]{An unsigned integer with the maximum value.}
    @entry[:default-value]{An unsigned integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-uint}")

(export 'param-spec-uint)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_uint" param-spec-uint)
    (:pointer (:struct param-spec-uint))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an unsigned integer with the minimum value}
  @argument[maximum]{an unsigned integer with the maximum value}
  @argument[default]{an unsigned integer with default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-uint} parameter specification.}
  @begin{short}
    Creates a new parameter specificaton instance specifying a property
    of type @var{+g-type-uint+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{parm-spec-uint}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-uint}
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
;;; g_value_get_uint ()
;;; g_value_set_uint () -> value-uint
;;; ----------------------------------------------------------------------------

(defun (setf value-uint) (value gvalue)
  (cffi:foreign-funcall "g_value_set_uint"
                        (:pointer (:struct value)) gvalue
                        :uint value
                        :void)
  value)

(cffi:defcfun ("g_value_get_uint" value-uint) :uint
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-uint gvalue) => value}
  @syntax[]{(setf (g:value-uint gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-uint+}}
  @argument[value]{an unsigned integer value}
  @begin{short}
    Unsigned integer contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-uint} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-uint+}. The @sym{(setf value-uint)} function sets the
  contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-uint+}"
  (gvalue (:pointer (:struct value))))

(export 'value-uint)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_LONG()
;;;
;;; #define G_IS_PARAM_SPEC_LONG(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_LONG))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_LONG.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_LONG()
;;;
;;; #define G_PARAM_SPEC_LONG(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_LONG, GParamSpecLong))
;;;
;;; Cast a GParamSpec instance into a GParamSpecLong.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_LONG()
;;;
;;; #define G_VALUE_HOLDS_LONG(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_LONG))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_LONG.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_LONG
;;;
;;; #define G_TYPE_PARAM_LONG           (g_param_spec_types[5])
;;;
;;; The GType of GParamSpecLong.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecLong
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    long integer properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-long
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :long)
  (:maximum :long)
  (:default-value :long))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{A long integer with the minimum value.}
    @entry[:maximum]{A long integer with the maximum value.}
    @entry[:default-value]{A long integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-long}")

(export 'param-spec-long)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_long ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_long" param-spec-long)
    (:pointer (:struct param-spec-long))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{a long integer with the minimum value}
  @argument[maximum]{a long integer with the maximum value}
  @argument[default]{a long integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-long} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-long+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-long}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-long+}
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
;;; g_value_get_long ()
;;; g_value_set_long () -> value-long
;;; ----------------------------------------------------------------------------

(defun (setf value-long) (value gvalue)
  (cffi:foreign-funcall "g_value_set_long"
                        (:pointer (:struct value)) gvalue
                        :long value
                        :void)
  value)

(cffi:defcfun ("g_value_get_long" value-long) :long
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-long gvalue) => value}
  @syntax[]{(setf (g:value-long gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-long+}}
  @argument[value]{a long integer value}
  @begin{short}
    Long integer contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-long} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-long+}. The @sym{(setf value-long)} function sets the
  contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-long+}"
  (gvalue (:pointer (:struct value))))

(export 'value-long)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_ULONG()
;;;
;;; #define G_IS_PARAM_SPEC_ULONG(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_ULONG))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_ULONG.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_ULONG()
;;;
;;; #define G_PARAM_SPEC_ULONG(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_ULONG, GParamSpecULong))
;;;
;;; Cast a GParamSpec instance into a GParamSpecULong.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_ULONG()
;;;
;;; #define G_VALUE_HOLDS_ULONG(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_ULONG))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_ULONG.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_ULONG
;;;
;;; #define G_TYPE_PARAM_ULONG (g_param_spec_types[6])
;;;
;;; The GType of GParamSpecULong.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecULong
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    unsigned long integer properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-ulong
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :ulong)
  (:maximum :ulong)
  (:default-value :ulong))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{An unsigned long integer with the minimum value.}
    @entry[:maximum]{An unsigned long integer with the maximum value.}
    @entry[:default-value]{An unsigned long integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-ulong}")

(export 'param-spec-ulong)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ulong ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_ulong" param-spec-ulong)
    (:pointer (:struct param-spec-ulong))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an unsigned long integer with the minimum value}
  @argument[maximum]{an unsigned long integer with the maximum value}
  @argument[default]{an unsigned long integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-ulong} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-ulong+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-ulong}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-ulong+}
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
;;; g_value_get_ulong ()
;;; g_value_set_ulong () -> value-ulong
;;; ----------------------------------------------------------------------------

(defun (setf value-ulong) (value gvalue)
  (cffi:foreign-funcall "g_value_set_ulong"
                        (:pointer (:struct value)) gvalue
                        :ulong value
                        :void)
  value)

(cffi:defcfun ("g_value_get_ulong" value-ulong) :ulong
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-ulong gvalue) => value}
  @syntax[]{(setf (g:value-ulong gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-ulong+}}
  @argument[value]{an unsigned long integer value}
  @begin{short}
    Unsigned long integer contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-ulong} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-ulong+}. The @sym{(setf value-ulong)} function sets
  the contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-ulong+}"
  (gvalue (:pointer (:struct value))))

(export 'value-ulong)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_INT64()
;;;
;;; #define G_IS_PARAM_SPEC_INT64(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_INT64))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_INT64.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_INT64()
;;;
;;; #define G_PARAM_SPEC_INT64(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_INT64, GParamSpecInt64))
;;;
;;; Cast a GParamSpec instance into a GParamSpecInt64.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_INT64()
;;;
;;; #define G_VALUE_HOLDS_INT64(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_INT64))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_INT64.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_INT64
;;;
;;; #define G_TYPE_PARAM_INT64 (g_param_spec_types[7])
;;;
;;; The GType of GParamSpecInt64.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecInt64
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    64 bit integer properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-int64
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :int64)
  (:maximum :int64)
  (:default-value :int64))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{A 64-bit integer with the value.}
    @entry[:maximum]{A 64-bit integer with the maximum value.}
    @entry[:default-value]{A 64-bit integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-int64}")

(export 'param-spec-int64)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int64 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_int64" param-spec-int64)
    (:pointer (:struct param-spec-int64))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{a 64-bit integer with the minimum value}
  @argument[maximum]{a 64-bit integer with the maximum value}
  @argument[default]{a 64-bit- integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-int64} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-int64+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-int64}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-int64+}
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
;;; g_value_get_int64 ()
;;; g_value_set_int64 () -> value-int64
;;; ----------------------------------------------------------------------------

(defun (setf value-int64) (value gvalue)
  (cffi:foreign-funcall "g_value_set_int64"
                        (:pointer (:struct value)) gvalue
                        :int64 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_int64" value-int64) :int64
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-int64 gvalue) => value}
  @syntax[]{(setf (g:value-int64 gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of type @var{+g-type-int64+}}
  @argument[value]{a 64-bit integer value}
  @begin{short}
    The 64-bit integer contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-int64} function gets the contents of a @symbol{g:value}
  instance of type @var{+g-type-int64+}. The @sym{(setf value-int64)} function
  set the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-int64+}"
  (gvalue (:pointer (:struct value))))

(export 'value-int64)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_UINT64()
;;;
;;; #define G_IS_PARAM_SPEC_UINT64(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_UINT64))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_UINT64.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_UINT64()
;;;
;;; #define G_PARAM_SPEC_UINT64(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_UINT64, GParamSpecUInt64))
;;;
;;; Cast a GParamSpec instance into a GParamSpecUInt64.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_UINT64()
;;;
;;; #define G_VALUE_HOLDS_UINT64(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_UINT64))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_UINT64.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_UINT64
;;;
;;; #define G_TYPE_PARAM_UINT64 (g_param_spec_types[8])
;;;
;;; The GType of GParamSpecUInt64.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecUInt64
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    unsigned 64 bit integer properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-uint64
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{An unsigned 64-bit integer with the minimum value.}
    @entry[:maximum]{An unsigned 64-bit integer with the maximum value.}
    @entry[:default-value]{An unsigned 64-bit integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-uint64}")

(export 'param-spec-uint64)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint64 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_uint64" param-spec-uint64)
    (:pointer (:struct param-spec-uint64))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{an unsigned 64-bit integer with the minimum value}
  @argument[maximum]{an unsigned 64-bit integer with the maximum value}
  @argument[default]{an unsigned 64-bit integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-uint64} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-uint64+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-uint64}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-uint64+}
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
;;; g_value_get_uint64 ()
;;; g_value_set_uint64 () -> value-uint64
;;; ----------------------------------------------------------------------------

(defun (setf value-uint64) (value gvalue)
  (cffi:foreign-funcall "g_value_set_uint64"
                        (:pointer (:struct value)) gvalue
                        :uint64 value
                        :void)
  value)

(cffi:defcfun ("g_value_get_uint64" value-uint64) :uint64
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-uint64 gvalue) => value}
  @syntax[]{(setf (g:value-uint64 gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-uint64}}
  @argument[value]{an unsigned 64-bit integer value}
  @begin{short}
    Unsigned 64-bit integer contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-uint64} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-uint64+}. The @sym{(setf value-uint64)} function sets
  the contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-uint64+}"
  (gvalue (:pointer (:struct value))))

(export 'value-uint64)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_FLOAT()
;;;
;;; #define G_IS_PARAM_SPEC_FLOAT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_FLOAT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_FLOAT.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_FLOAT()
;;;
;;; #define G_PARAM_SPEC_FLOAT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_FLOAT, GParamSpecFloat))
;;;
;;; Cast a GParamSpec instance into a GParamSpecFloat.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_FLOAT()
;;;
;;; #define G_VALUE_HOLDS_FLOAT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_FLOAT))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_FLOAT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_FLOAT
;;;
;;; #define G_TYPE_PARAM_FLOAT (g_param_spec_types[12])
;;;
;;; The GType of GParamSpecFloat.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecFloat
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    float properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-float
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :float)
  (:maximum :float)
  (:default-value :float)
  (:epsilon :float))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{A float with the minimum value.}
    @entry[:maximum]{A float with the maximum value.}
    @entry[:default-value]{A float with the default value.}
    @entry[:epsilon]{A float value, values closer than epsilon will be
      considered identical, the default value is 1e-30.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-float}")

(export 'param-spec-float)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_float ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_float" param-spec-float)
    (:pointer (:struct param-spec-float))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{a float with the minimum value}
  @argument[maximum]{a float with the maximum value}
  @argument[default]{a float with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-float} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-float+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-float}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-float+}
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
;;; g_value_get_float ()
;;; g_value_set_float () -> value-float
;;; ----------------------------------------------------------------------------

(defun (setf value-float) (value gvalue)
  (cffi:foreign-funcall "g_value_set_float"
                        (:pointer (:struct value)) gvalue
                        :float value
                        :void)
  value)

(cffi:defcfun ("g_value_get_float" value-float) :float
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-float gvalue) => value}
  @syntax[]{(setf (g:value-float gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-float+}}
  @argument[value]{a float value}
  @begin{short}
    Float contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-float} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-float+}. The @sym{(setf value-float)} function sets
  the contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-float+}"
  (gvalue (:pointer (:struct value))))

(export 'value-float)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_DOUBLE()
;;;
;;; #define G_IS_PARAM_SPEC_DOUBLE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_DOUBLE))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_DOUBLE.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_DOUBLE()
;;;
;;; #define G_PARAM_SPEC_DOUBLE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_DOUBLE, GParamSpecDouble))
;;;
;;; Cast a GParamSpec instance into a GParamSpecDouble.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_DOUBLE()
;;;
;;; #define G_VALUE_HOLDS_DOUBLE(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_DOUBLE))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_DOUBLE.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_DOUBLE
;;;
;;; #define G_TYPE_PARAM_DOUBLE (g_param_spec_types[13])
;;;
;;; The GType of GParamSpecDouble.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecDouble
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    double properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-double
  (:parent-instance (:pointer (:struct param-spec)))
  (:minimum :double)
  (:maximum :double)
  (:default-value :double)
  (:epsilon :double))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:minimum]{A double float with the minimum value.}
    @entry[:maximum]{A double float with the maximum value.}
    @entry[:default-value]{A double float with the default value.}
    @entry[:epsilon]{A double float value, values closer than epsilon will be
      considered identical, the default value is 1e-90.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-double}")

(export 'param-spec-double)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_double ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_double" param-spec-double)
    (:pointer (:struct param-spec-double))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[minimum]{a double float with the minimum value}
  @argument[maximum]{a double float with the maximum value}
  @argument[default]{a double float with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-double} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-double+} property.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-double}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-double+}
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
;;; g_value_get_double ()
;;; g_value_set_double () -> value-double
;;; ----------------------------------------------------------------------------

(defun (setf value-double) (value gvalue)
  (cffi:foreign-funcall "g_value_set_double"
                        (:pointer (:struct value)) gvalue
                        :double value
                        :void)
  value)

(cffi:defcfun ("g_value_get_double" value-double) :double
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-double gvalue) => value}
  @syntax[]{(setf (g:value-double gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-double+}}
  @argument[value]{a double float value}
  @begin{short}
    Double float contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-double} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-double+}. The @sym{(setf value-double)} function sets
  the contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-double+}"
  (gvalue (:pointer (:struct value))))

(export 'value-double)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_ENUM()
;;;
;;; #define G_IS_PARAM_SPEC_ENUM(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_ENUM))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_ENUM.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_ENUM()
;;;
;;; #define G_PARAM_SPEC_ENUM(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_ENUM, GParamSpecEnum))
;;;
;;; Cast a GParamSpec instance into a GParamSpecEnum.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_ENUM()
;;;
;;; #define G_VALUE_HOLDS_ENUM(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_ENUM))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_ENUM.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_ENUM
;;;
;;; #define G_TYPE_PARAM_ENUM (g_param_spec_types[10])
;;;
;;; The GType of GParamSpecEnum.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecEnum
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-enum
  (:parent-instance (:pointer (:struct param-spec)))
  (:enum-class (:pointer (:struct enum-class)))
  (:default-value :int))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-enum)
      "CStruct"
      (liber:symbol-documentation 'param-spec-enum)
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    enum properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-enum
  (:parent-instance (:pointer (:struct param-spec)))
  (:enum-class (:pointer enum-class))
  (:default-value :int))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:enum-class]{A @symbol{g:enum-class} class instance for the enum.}
    @entry[:default-value]{A integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-enum}")

(export 'param-spec-enum)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_enum ()
;;; ----------------------------------------------------------------------------

;; TODO:
;; This accepts any integer for default-value, but does not check for a valid
;; enum parameter. Can this be implemented better?

(cffi:defcfun ("g_param_spec_enum" param-spec-enum)
    (:pointer (:struct param-spec-enum))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @class{g:type-t} type derived from @var{+g-type-enum+}}
  @argument[default]{an integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-enum} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-enum+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-enum}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-enum+}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (default :int)
  (flags param-flags))

(export 'param-spec-enum)

;;; ----------------------------------------------------------------------------
;;; g_value_get_enum ()
;;; g_value_set_enum () -> value-enum
;;; ----------------------------------------------------------------------------

;; TODO:
;; This accepts any integer, but does not check for a valid enum parameter.
;; Can this be implemented much better?

(defun (setf value-enum) (value gvalue)
  (cffi:foreign-funcall "g_value_set_enum"
                        (:pointer (:struct value)) gvalue
                        :int value
                        :void)
  value)

(cffi:defcfun ("g_value_get_enum" value-enum) :int
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-enum gvalue) => value}
  @syntax[]{(setf (g:value-enum gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} whose type is derived from
    @var{+g-type-enum+}}
  @argument[value]{an integer wiht the enumeration value}
  @begin{short}
    Enumeration value contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-enum} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-enum+}. The @sym{(setf value-enum)} function sets the
  contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-enum+}"
  (gvalue (:pointer (:struct value))))

(export 'value-enum)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_FLAGS()
;;;
;;; #define G_IS_PARAM_SPEC_FLAGS(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_FLAGS))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_FLAGS.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_FLAGS()
;;;
;;; #define G_PARAM_SPEC_FLAGS(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_FLAGS, GParamSpecFlags))
;;;
;;; Cast a GParamSpec instance into a GParamSpecFlags.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_FLAGS()
;;;
;;; #define G_VALUE_HOLDS_FLAGS(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_FLAGS))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_FLAGS.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_FLAGS
;;;
;;; #define G_TYPE_PARAM_FLAGS (g_param_spec_types[11])
;;;
;;; The GType of GParamSpecFlags.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecFlags
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-flags
  (:parent-instance (:pointer (:struct param-spec)))
  (:flags-class (:pointer (:struct flags-class)))
  (:default-value :uint))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-flags)
      "CStruct"
      (liber:symbol-documentation 'param-spec-flags)
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    flags properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-flags
  (:parent-instance (:pointer (:struct param-spec)))
  (:flags-class (:pointer flags-class))
  (:default-value :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:flags-class]{A @symbol{g:flags-class} class instance for the flags.}
    @entry[:default-value]{An unsigned integer with the default value.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-flags}")

(export 'param-spec-flags)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_flags ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_flags" param-spec-flags)
    (:pointer (:struct param-spec-flags))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @class{g:type-t} type derived from @var{+g-type-flags+}}
  @argument[default]{an unsigned integer with the default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-flags} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-flags+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-flags}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-flags+}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (default :uint)
  (flags param-flags))

(export 'param-spec-flags)

;;; ----------------------------------------------------------------------------
;;; g_value_get_flags ()
;;; g_value_set_flags () -> value-flags
;;; ----------------------------------------------------------------------------

(defun (setf value-flags) (value gvalue)
  (cffi:foreign-funcall "g_value_set_flags"
                        (:pointer (:struct value)) gvalue
                        :uint value
                        :void)
  value)

(cffi:defcfun ("g_value_get_flags" value-flags) :uint
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-flags gvalue) => value}
  @syntax[]{(setf (g:value-flags gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} whose type is derived from
    @var{+g-type-flags+}}
  @argument[value]{an unsigned integer with the flags value}
  @begin{short}
    Flags contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-flags} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-flags+}. The @sym{(setf value-flags)} function sets
  the contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-flags+}"
  (gvalue (:pointer (:struct value))))

(export 'value-flags)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_STRING()
;;;
;;; #define G_IS_PARAM_SPEC_STRING(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_STRING))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_STRING.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_STRING()
;;;
;;; #define G_PARAM_SPEC_STRING(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_STRING, GParamSpecString))
;;;
;;; Casts a GParamSpec instance into a GParamSpecString.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_STRING()
;;;
;;; #define G_VALUE_HOLDS_STRING(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_STRING))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_STRING.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_IS_INTERNED_STRING()
;;;
;;; #define G_VALUE_IS_INTERNED_STRING(value)
;;;         (G_VALUE_HOLDS_STRING
;;;           (value) && ((value)->data[1].v_uint
;;;                   & G_VALUE_INTERNED_STRING)) GLIB_AVAILABLE_MACRO_IN_2_66
;;;
;;; Checks whether value contains a string which is canonical.
;;;
;;; value:
;;;     a valid GValue structure
;;;
;;; Returns:
;;;     TRUE if the value contains a string in its canonical representation, as
;;;     returned by g_intern_string(). See also g_value_set_interned_string().
;;;
;;; Since 2.66
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_STRING
;;;
;;; #define G_TYPE_PARAM_STRING (g_param_spec_types[14])
;;;
;;; The GType of GParamSpecString.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_INTERNED_STRING
;;;
;;; #define G_VALUE_INTERNED_STRING (1 << 28) GLIB_AVAILABLE_MACRO_IN_2_66
;;;
;;; For string values, indicates that the string contained is canonical and will
;;; exist for the duration of the process. See g_value_set_interned_string().
;;;
;;; Since 2.66
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecString
;;;
;;; TODO: This is taken from the C documentation. Check the implementation.
;;;
;;; struct GParamSpecString {
;;;   GParamSpec    parent_instance;
;;;   gchar        *default_value;
;;;   gchar        *cset_first;
;;;   gchar        *cset_nth;
;;;   gchar         substitutor;
;;;   guint         null_fold_if_empty : 1;
;;;   guint         ensure_non_null : 1;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for string
;;; properties.
;;;
;;; gchar *default_value :
;;;     default value for the property specified
;;;
;;; gchar *cset_first :
;;;     a string containing the allowed values for the first byte
;;;
;;; gchar *cset_nth :
;;;     a string containing the allowed values for the subsequent bytes
;;;
;;; gchar substitutor :
;;;     the replacement byte for bytes which don't match cset_first or cset_nth
;;;
;;; guint null_fold_if_empty : 1;
;;;     replace empty string by NULL
;;;
;;; guint ensure_non_null : 1;
;;;     replace NULL strings by an empty string
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
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    string properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-string
  (:parent-instance (:pointer (:struct param-spec)))
  (:default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (:substitutor :char)
  (:flags-for-null :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:default-value]{A string with the default value.}
    @entry[:cset-frist]{A string with the containing the allowed values for the
      first byte.}
    @entry[:cset-nth]{A string with the containing the allowed values for the
      subsequent bytes.}
    @entry[:substitutor]{A character with the replacement byte for bytes
      which do not match @code{:cset-first} or @code{cset-nth}.}
    @entry[:flags-for-null]{An unsigned integer, replace empty string by
      @code{nil} and @code{nil} strings by an empty string.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-string}")

(export 'param-spec-string)

;;; ----------------------------------------------------------------------------
;;; gchararray
;;;
;;; typedef gchar* gchararray;
;;;
;;; A C representable type name for G_TYPE_STRING.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_string" param-spec-string)
    (:pointer (:struct param-spec-string))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[default]{a string with default value}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-string} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    type \"gchararray\".
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-string}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-string+}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (default :string)
  (flags param-flags))

(export 'param-spec-string)

;;; ----------------------------------------------------------------------------
;;; g_value_get_string ()
;;; g_value_set_string () -> value-string
;;; ----------------------------------------------------------------------------

(defun (setf value-string) (value gvalue)
  (cffi:foreign-funcall "g_value_set_string"
                        (:pointer (:struct value)) gvalue
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_value_get_string" value-string) :string
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-string gvalue) => value}
  @syntax[]{(setf (g:value-string gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type \"gchararray\"}
  @argument[value]{caller-owned string to be duplicated for the
    @symbol{g:value}}
  @begin{short}
    String content of @arg{gvalue}.
  @end{short}
  The @sym{g:value-string} function gets the contents of a @symbol{g:value}
  of type \"gchararray\". The @sym{(setf value-string)} function sets
  the contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-string+}"
  (gvalue (:pointer (:struct value))))

(export 'value-string)

;;; ----------------------------------------------------------------------------
;;; g_value_set_static_string ()
;;;
;;; void g_value_set_static_string (GValue *value, const gchar *v_string);
;;;
;;; Set the contents of a G_TYPE_STRING GValue to v_string. The string is
;;; assumed to be static, and is thus not duplicated when setting the GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; v_string :
;;;     static string to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_take_string ()
;;;
;;; void g_value_take_string (GValue *value, gchar *v_string);
;;;
;;; Sets the contents of a G_TYPE_STRING GValue to v_string.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; v_string :
;;;     string to take ownership of
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_string_take_ownership ()
;;;
;;; void g_value_set_string_take_ownership (GValue *value, gchar *v_string);
;;;
;;; Warning
;;;
;;; g_value_set_string_take_ownership has been deprecated since version 2.4 and
;;; should not be used in newly written code. Use g_value_take_string() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; v_string :
;;;     duplicated unowned string to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_dup_string ()
;;;
;;; gchar * g_value_dup_string (const GValue *value);
;;;
;;; Get a copy the contents of a G_TYPE_STRING GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; Returns :
;;;     a newly allocated copy of the string content of value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_interned_string ()
;;;
;;; void
;;; g_value_set_interned_string (GValue *value,
;;;                              const gchar *v_string);
;;;
;;; Set the contents of a G_TYPE_STRING GValue to v_string . The string is
;;; assumed to be static and interned (canonical, for example from
;;; g_intern_string()), and is thus not duplicated when setting the GValue.
;;;
;;; value:
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; v_string:
;;;     static string to be set
;;;
;;; Since 2.66
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_PARAM()
;;;
;;; #define G_IS_PARAM_SPEC_PARAM(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_PARAM))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_PARAM.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_PARAM()
;;;
;;; #define G_PARAM_SPEC_PARAM(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_PARAM, GParamSpecParam))
;;;
;;; Casts a GParamSpec instance into a GParamSpecParam.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_PARAM()
;;;
;;; #define G_VALUE_HOLDS_PARAM(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_PARAM))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_PARAM.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_PARAM
;;;
;;; #define G_TYPE_PARAM_PARAM (g_param_spec_types[15])
;;;
;;; The GType of GParamSpecParam.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecParam
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-param
  (:parent-instance (:pointer (:struct param-spec))))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-param)
      "CStruct"
      (liber:symbol-documentation 'param-spec-param)
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    properties of type @var{+g-type-param+}.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-param
  (:parent-instance (:pointer (:struct param-spec))))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-param}")

(export 'param-spec-param)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_param ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_param" param-spec-param)
    (:pointer (:struct param-spec-param))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @class{g:type-t} type derived from @var{+g-type-param+}}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-param} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-param+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-param}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-param+}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (flags param-flags))

(export 'param-spec-param)

;;; ----------------------------------------------------------------------------
;;; g_value_get_param ()
;;; g_value_set_param () -> value-param
;;; ----------------------------------------------------------------------------

(defun (setf value-param) (value gvalue)
  (cffi:foreign-funcall "g_value_set_param"
                        (:pointer (:struct value)) gvalue
                        (:pointer (:struct param-spec)) value
                        :void)
  value)

(cffi:defcfun ("g_value_get_param" value-param) (:pointer (:struct param-spec))
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-param gvalue) => value}
  @syntax[]{(setf (g:value-param gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} whose type is derived from
    @var{+value-param+}}
  @argument[value]{a @symbol{g:param-spec} value}
  @begin{short}
    The @symbol{g:param-spec} content of @arg{gvalue}.
  @end{short}
  The @sym{g:value-param} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-param+}. The @sym{(setf value-param)} function sets
  the contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-param+}"
  (gvalue (:pointer (:struct value))))

(export 'value-param)

;;; ----------------------------------------------------------------------------
;;; g_value_take_param ()
;;;
;;; void g_value_take_param (GValue *value, GParamSpec *param);
;;;
;;; Sets the contents of a G_TYPE_PARAM GValue to param and takes over the
;;; ownership of the callers reference to param; the caller does not have to
;;; unref it any more.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_PARAM
;;;
;;; param :
;;;     the GParamSpec to be set
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_param_take_ownership ()
;;;
;;; void g_value_set_param_take_ownership (GValue *value, GParamSpec *param);
;;;
;;; Warning
;;;
;;; g_value_set_param_take_ownership has been deprecated since version 2.4 and
;;; should not be used in newly written code. Use g_value_take_param() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_PARAM
;;;
;;; param :
;;;     the GParamSpec to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_dup_param ()
;;;
;;; GParamSpec * g_value_dup_param (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_PARAM GValue, increasing its reference count.
;;;
;;; value :
;;;     a valid GValue whose type is derived from G_TYPE_PARAM
;;;
;;; Returns :
;;;     GParamSpec content of value, should be unreferenced when no longer
;;;     needed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_BOXED()
;;;
;;; #define G_IS_PARAM_SPEC_BOXED(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_BOXED))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_BOXED.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_BOXED()
;;;
;;; #define G_PARAM_SPEC_BOXED(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_BOXED, GParamSpecBoxed))
;;;
;;; Cast a GParamSpec instance into a GParamSpecBoxed.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_BOXED()
;;;
;;; #define G_VALUE_HOLDS_BOXED(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_BOXED))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_BOXED.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_BOXED
;;;
;;; #define G_TYPE_PARAM_BOXED (g_param_spec_types[16])
;;;
;;; The GType of GParamSpecBoxed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecBoxed
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-boxed
  (:parent-instance (:pointer (:struct param-spec))))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-boxed)
      "CStruct"
      (liber:symbol-documentation 'param-spec-boxed)
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    boxed properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-boxed
  (:parent-instance (:pointer (:struct param-spec))))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-boxed}")

(export 'param-spec-boxed)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boxed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_boxed" param-spec-boxed)
    (:pointer (:struct param-spec-boxed))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @var{+g-type-boxed+} derived type of this property}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-boxed} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    derived of type @var{+g-type-boxed+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-boxed}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-boxed+}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (flags param-flags))

(export 'param-spec-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_get_boxed ()
;;; g_value_set_boxed () -> value-boxed
;;; ----------------------------------------------------------------------------

(defun (setf value-boxed) (value gvalue)
  (cffi:foreign-funcall "g_value_set_boxed"
                        (:pointer (:struct value)) gvalue
                        :pointer value
                        :void)
  value)

(cffi:defcfun ("g_value_get_boxed" value-boxed) :pointer
 #+liber-documentation
 "@version{2022-12-28}
  @syntax[]{(g:value-boxed gvalue) => value}
  @syntax[]{(setf (g:value-boxed gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} instance of @code{GBoxed} type}
  @argument[value]{a boxed value}
  @begin{short}
    Boxed contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-boxed} function gets the contents of a @symbol{g:value}
  derived of the @var{+g-type-boxed+} type. The @sym{(setf g:value-boxed)}
  function sets the contents of a @symbol{g:value} instance to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-boxed+}"
  (gvalue (:pointer (:struct value))))

(export 'value-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_set_static_boxed ()
;;;
;;; void g_value_set_static_boxed (GValue *value, gconstpointer v_boxed);
;;;
;;; Set the contents of a G_TYPE_BOXED derived GValue to v_boxed. The boxed
;;; value is assumed to be static, and is thus not duplicated when setting the
;;; GValue.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_BOXED derived type
;;;
;;; v_boxed :
;;;     static boxed value to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_take_boxed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_take_boxed" value-take-boxed) :void
 #+liber-documentation
 "@version{2022-12-28}
  @argument[gvalue]{a @symbol{g:value} instance of @code{GBoxed} type}
  @argument[value]{a value}
  @begin{short}
    Sets the contents of a @symbol{g:value} instance of @code{GBoxed} type
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
;;; G_IS_PARAM_SPEC_POINTER()
;;;
;;; #define G_IS_PARAM_SPEC_POINTER(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_POINTER))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_POINTER.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_POINTER()
;;;
;;; #define G_PARAM_SPEC_POINTER(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_POINTER, GParamSpecPointer))
;;;
;;; Casts a GParamSpec instance into a GParamSpecPointer.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_POINTER()
;;;
;;; #define G_VALUE_HOLDS_POINTER(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_POINTER))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_POINTER.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_POINTER
;;;
;;; #define G_TYPE_PARAM_POINTER (g_param_spec_types[17])
;;;
;;; The GType of GParamSpecPointer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecPointer
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-pointer
  (:parent-instance (:pointer (:struct param-spec))))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-pointer)
      "CStruct"
      (liber:symbol-documentation 'param-spec-pointer)
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    pointer properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-pointer
  (:parent-instance (:pointer (:struct param-spec))))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-pointer}")

(export 'param-spec-pointer)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pointer ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_pointer" param-spec-pointer)
    (:pointer (:struct param-spec-pointer))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-pointer} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-pointer+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-pointer}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-pointer+}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (flags param-flags))

(export 'param-spec-pointer)

;;; ----------------------------------------------------------------------------
;;; g_value_get_pointer ()
;;; g_value_set_pointer () -> value-pointer
;;; ----------------------------------------------------------------------------

(defun (setf value-pointer) (value gvalue)
  (cffi:foreign-funcall "g_value_set_pointer"
                        (:pointer (:struct value)) gvalue
                        :pointer value
                        :void)
  value)

(cffi:defcfun ("g_value_get_pointer" value-pointer) :pointer
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-pointer gvalue) => value}
  @syntax[]{(setf (g:value-pointer gvalue) value)}
  @argument[gvalue]{a valid @symbol{g:value} of @code{gpointer}}
  @argument[value]{pointer value to be set}
  @begin{short}
    Pointer contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-pointer} function gets the contents of a @symbol{g:value}
  of type @var{+g-type-pointer+}. The @sym{(setf value-pointer)} function
  sets the contents of a @symbol{g:value} to @arg{value}.
  @see-symbol{g:value}
  @see-variable{+g-type-pointer+}"
  (gvalue (:pointer (:struct value))))

(export 'value-pointer)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_OBJECT()
;;;
;;; #define G_IS_PARAM_SPEC_OBJECT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_OBJECT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_OBJECT.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_OBJECT()
;;;
;;; #define G_PARAM_SPEC_OBJECT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_OBJECT, GParamSpecObject))
;;;
;;; Casts a GParamSpec instance into a GParamSpecObject.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_OBJECT()
;;;
;;; #define G_VALUE_HOLDS_OBJECT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_OBJECT))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_OBJECT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_OBJECT
;;;
;;; #define G_TYPE_PARAM_OBJECT (g_param_spec_types[19])
;;;
;;; The GType of GParamSpecObject.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecObject
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-object
  (:parent-instance (:pointer (:struct param-spec))))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-object)
      "CStruct"
      (liber:symbol-documentation 'param-spec-object)
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    object properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-object
  (:parent-instance (:pointer (:struct param-spec))))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-object}")

(export 'param-spec-object)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_object ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_object" param-spec-object)
    (:pointer (:struct param-spec-object))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[gtype]{a @var{+g-type-object+} derived type of this property}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-object} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of a dervived type @var{+g-type-object+}.
  @end{short}
  See the @fun{g:param-spec-internal} function for details on property names.
  @see-symbol{g:param-spec-object}
  @see-symbol{g:param-flags}
  @see-variable{+g-type-object+}
  @see-function{g:param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (gtype type-t)
  (flags param-flags))

(export 'param-spec-object)

;;; ----------------------------------------------------------------------------
;;; g_value_get_object ()
;;; g_value_set_object () -> value-object
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_set_object" %value-set-object) :void
  (gvalue (:pointer (:struct value)))
  (value :pointer))

(defun (setf value-object) (value gvalue)
  (%value-set-object gvalue
                     (if value
                         (pointer value)
                         (cffi:null-pointer)))
  value)

(cffi:defcfun ("g_value_get_object" %value-get-object) :pointer
  (value (:pointer (:struct value))))

(defun value-object (gvalue)
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-object gvalue) => value}
  @syntax[]{(setf (g:value-object gvalue) value)}
  @argument[gvalue]{a valid @symbol{g:value} of @var{+g-type-object+}
    derived type}
  @argument[value]{object value of derived type @var{+g-type-object+}}
  @begin{short}
    Object contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-object} function gets the contents of a @symbol{g:value}
  of a derived type @var{+g-type-object+}. The @sym{(setf value-object)}
  function sets the contents of a @symbol{g:value} to @arg{value}.

  The @sym{(setf value-object)} function increases the reference count of
  @arg{value}, the @symbol{g:value} holds a reference to @arg{value}. If you do
  not wish to increase the reference count of the object, i.e. you wish to pass
  your current reference to the @symbol{g:value} because you no longer need it,
  use the @fun{g:value-take-object} function instead.

  It is important that your @symbol{g:value} holds a reference to @arg{value},
  either its own, or one it has taken, to ensure that the object will not be
  destroyed while the @symbol{g:value} still exists).
  @see-symbol{g:value}
  @see-variable{+g-type-object+}"
  (get-g-object-for-pointer (%value-get-object gvalue)))

(export 'value-object)

;;; ----------------------------------------------------------------------------
;;; g_value_take_object ()
;;;
;;; void g_value_take_object (GValue *value, gpointer v_object);
;;;
;;; Sets the contents of a G_TYPE_OBJECT derived GValue to v_object and takes
;;; over the ownership of the callers reference to v_object; the caller does
;;; not have to unref it any more (i.e. the reference count of the object is
;;; not increased).
;;;
;;; If you want the GValue to hold its own reference to v_object, use
;;; g_value_set_object() instead.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_OBJECT derived type
;;;
;;; v_object :
;;;     object value to be set
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_object_take_ownership ()
;;;
;;; void g_value_set_object_take_ownership (GValue *value, gpointer v_object);
;;;
;;; Warning
;;;
;;; g_value_set_object_take_ownership has been deprecated since version 2.4 and
;;; should not be used in newly written code. Use g_value_take_object() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_OBJECT derived type
;;;
;;; v_object :
;;;     object value to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_dup_object ()
;;;
;;; gpointer g_value_dup_object (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_OBJECT derived GValue, increasing its reference
;;; count. If the contents of the GValue are NULL, then NULL will be returned.
;;;
;;; value :
;;;     a valid GValue whose type is derived from G_TYPE_OBJECT
;;;
;;; Returns :
;;;     object content of value, should be unreferenced when no longer needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_UNICHAR()
;;;
;;; #define G_IS_PARAM_SPEC_UNICHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_UNICHAR))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_UNICHAR.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_UNICHAR()
;;;
;;; #define G_PARAM_SPEC_UNICHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_UNICHAR, GParamSpecUnichar))
;;;
;;; Cast a GParamSpec instance into a GParamSpecUnichar.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_UNICHAR
;;;
;;; #define G_TYPE_PARAM_UNICHAR (g_param_spec_types[9])
;;;
;;; The GType of GParamSpecUnichar.
;;; ----------------------------------------------------------------------------

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
;;; G_IS_PARAM_SPEC_VALUE_ARRAY()
;;;
;;; #define G_IS_PARAM_SPEC_VALUE_ARRAY(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_VALUE_ARRAY))
;;;
;;; Warning
;;;
;;; G_IS_PARAM_SPEC_VALUE_ARRAY has been deprecated since version 2.32 and
;;; should not be used in newly written code. Use GArray instead of GValueArray
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_VALUE_ARRAY.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_VALUE_ARRAY()
;;;
;;; #define G_PARAM_SPEC_VALUE_ARRAY(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_VALUE_ARRAY, GParamSpecValueArray))
;;;
;;; Warning
;;;
;;; G_PARAM_SPEC_VALUE_ARRAY has been deprecated since version 2.32 and should
;;; not be used in newly written code. Use GArray instead of GValueArray
;;;
;;; Cast a GParamSpec instance into a GParamSpecValueArray.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_VALUE_ARRAY
;;;
;;; #define G_TYPE_PARAM_VALUE_ARRAY (g_param_spec_types[18])
;;;
;;; Warning
;;;
;;; G_TYPE_PARAM_VALUE_ARRAY has been deprecated since version 2.32 and should
;;; not be used in newly written code. Use GArray instead of GValueArray
;;;
;;; The GType of GParamSpecValueArray.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecValueArray
;;; ----------------------------------------------------------------------------

;; We do not expot this structure.

(cffi:defcstruct param-spec-value-array
  (:parent-instance (:pointer (:struct param-spec)))
  (:element-spec (:pointer (:struct param-spec)))
  (:fixed-n-elements :uint))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-value-array)
      "CStruct"
      (liber:symbol-documentation 'param-spec-value-array)
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    @code{GValueArray} properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-value-array
  (:parent-instance (:pointer (:struct param-spec)))
  (:element-spec (:pointer (:struct param-spec)))
  (:fixed-n-elements :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:private-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:element-spec]{a @symbol{g:param-spec} describing the elements
      contained in arrays of this property, may be @code{NULL}}
    @entry[:fixed-n-elements]{a @code{:uint}, if greater than 0, arrays of this
      property will always have this many elements}
  @end{table}
  @see-symbol{g:param-spec}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_value_array ()
;;; ----------------------------------------------------------------------------

;; We dot not export this function.

(cffi:defcfun ("g_param_spec_value_array" param-spec-value-array)
    (:pointer (:struct param-spec-value-array))
 #+liber-documentation
 "@version{#2020-8-27}
  @argument[name]{a @code{:string} with the canonical name}
  @argument[nick]{a @code{:string} with the nick name}
  @argument[blurb]{a @code{:string} with the description}
  @argument[element-spec]{a @symbol{g:param-spec} describing the elements
    contained in arrays of this property, may be @code{NULL}}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-value-array} parameter
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
;;; G_IS_PARAM_SPEC_OVERRIDE()
;;;
;;; #define G_IS_PARAM_SPEC_OVERRIDE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_OVERRIDE))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_OVERRIDE.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Returns :
;;;     TRUE on success.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_OVERRIDE()
;;;
;;; #define G_PARAM_SPEC_OVERRIDE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_OVERRIDE, GParamSpecOverride))
;;;
;;; Casts a GParamSpec into a GParamSpecOverride.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_OVERRIDE
;;;
;;; #define G_TYPE_PARAM_OVERRIDE (g_param_spec_types[20])
;;;
;;; The GType of GParamSpecOverride.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

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
;;; G_IS_PARAM_SPEC_GTYPE()
;;;
;;; #define G_IS_PARAM_SPEC_GTYPE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_GTYPE))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_GTYPE.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Returns :
;;;     TRUE on success.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_GTYPE()
;;;
;;; #define G_PARAM_SPEC_GTYPE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_GTYPE, GParamSpecGType))
;;;
;;; Casts a GParamSpec into a GParamSpecGType.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_GTYPE()
;;;
;;; #define G_VALUE_HOLDS_GTYPE(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_GTYPE))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_GTYPE.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_GTYPE
;;;
;;; #define G_TYPE_PARAM_GTYPE (g_param_spec_types[21])
;;;
;;; The GType of GParamSpecGType.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecGType
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec-gtype
  (:parent-instance (:pointer (:struct param-spec)))
  (:is-a-type type-t))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec-gtype)
      "CStruct"
      (liber:symbol-documentation 'param-spec-gtype)
 "@version{#2022-12-31}
  @begin{short}
    A @symbol{g:param-spec} derived structure that contains the meta data for
    @class{g:type-t} properties.
  @end{short}
  @begin{pre}
(cffi:defcstruct param-spec-gtype
  (:parent-instance (:pointer (:struct param-spec)))
  (:is-a-type type-t))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g:param-spec} portion.}
    @entry[:is-a-type]{A @class{g:type-t} type whose subtypes can occur as
      values.}
  @end{table}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-gtype}")

(export 'param-spec-gtype)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_gtype ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_gtype" param-spec-gtype)
    (:pointer (:struct param-spec-gtype))
 #+liber-documentation
 "@version{#2022-12-31}
  @argument[name]{a string with the canonical name}
  @argument[nick]{a string with the nick name}
  @argument[blurb]{a string with the description}
  @argument[is-a-type]{a @class{g:type-t} type whose subtypes are allowed as
    values of the property (use the \"void\" type for any type)}
  @argument[flags]{a @symbol{g:param-flags} value}
  @return{A newly created @symbol{g:param-spec-gtype} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a \"GType\"
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
;;; g_value_get_gtype ()
;;; g_value_set_gtype () -> value-gtype
;;; ----------------------------------------------------------------------------

(defun (setf value-gtype) (value gvalue)
  (cffi:foreign-funcall "g_value_set_gtype"
                        (:pointer (:struct value)) gvalue
                        type-t value
                        :void)
  value)

(cffi:defcfun ("g_value_get_gtype" value-gtype) type-t
 #+liber-documentation
 "@version{#2022-12-31}
  @syntax[]{(g:value-gtype gvalue) => value}
  @syntax[]{(setf (g:value-gtype gvalue) value)}
  @argument[gvalue]{a valid @symbol{g:value} instance of @class{g:type-t} type}
  @argument[value]{a @class{g:type-t} type value}
  @return{The @class{g:type-t} type stored in @arg{gvalue}.}
  @begin{short}
    The @sym{g:type-gtype} function gets the contents of a @class{g:type-t} type
    value.
  @end{short}
  The @sym{(setf g:value-gtype)} function sets the contents.
  @see-symbol{g:value}
  @see-class{g:type-t}"
  (gvalue (:pointer (:struct value))))

(export 'value-gtype)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_VARIANT()
;;;
;;; #define G_IS_PARAM_SPEC_VARIANT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_VARIANT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_VARIANT.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_VARIANT()
;;;
;;; #define G_PARAM_SPEC_VARIANT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_VARIANT, GParamSpecVariant))
;;;
;;; Casts a GParamSpec into a GParamSpecVariant.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_VARIANT()
;;;
;;; #define G_VALUE_HOLDS_VARIANT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_VARIANT))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_VARIANT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_VARIANT
;;;
;;; #define G_TYPE_PARAM_VARIANT (g_param_spec_types[22])
;;;
;;; The GType of GParamSpecVariant.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

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
;;; g_value_get_variant ()
;;; g_value_set_variant () -> value-variant
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
 "@version{#2022-12-31}
  @syntax[]{(g:value-variant gvalue) => value}
  @syntax[]{(setf (g:value-variant gvalue) value)}
  @argument[gvalue]{a @symbol{g:value} of type @var{+g-type-variant+}}
  @argument[value]{a @symbol{g:variant} value}
  @begin{short}
    Variant contents of @arg{gvalue}.
  @end{short}
  The @sym{g:value-variant} function gets the contents of a @symbol{g:value}
  of type @symbol{g:variant}. The @sym{(setf value-variant)} function sets
  the contents of a @symbol{g:value} to @arg{value}.
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
