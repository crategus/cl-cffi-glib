;;; ----------------------------------------------------------------------------
;;; gobject.type-info.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GObject
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
;;; Type Information
;;;
;;;     The GLib Runtime type identification and management system
;;;
;;; Types and Values
;;;
;;;     G_TYPE_INVALID
;;;     G_TYPE_NONE
;;;     G_TYPE_INTERFACE
;;;     G_TYPE_CHAR
;;;     G_TYPE_UCHAR
;;;     G_TYPE_BOOLEAN
;;;     G_TYPE_INT
;;;     G_TYPE_UINT
;;;     G_TYPE_LONG
;;;     G_TYPE_ULONG
;;;     G_TYPE_INT64
;;;     G_TYPE_UINT64
;;;     G_TYPE_ENUM
;;;     G_TYPE_FLAGS
;;;     G_TYPE_FLOAT
;;;     G_TYPE_DOUBLE
;;;     G_TYPE_STRING
;;;     G_TYPE_POINTER
;;;     G_TYPE_BOXED
;;;     G_TYPE_PARAM
;;;     G_TYPE_OBJECT
;;;     G_TYPE_GTYPE
;;;     G_TYPE_VARIANT
;;;     G_TYPE_CHECKSUM
;;;     G_TYPE_RESERVED_GLIB_FIRST                         not exported
;;;     G_TYPE_RESERVED_GLIB_LAST                          not exported
;;;     G_TYPE_RESERVED_BSE_FIRST                          not exported
;;;     G_TYPE_RESERVED_BSE_LAST                           not exported
;;;     G_TYPE_RESERVED_USER_FIRST                         not exported
;;;
;;;     G_TYPE_FUNDAMENTAL_MAX                             not exported
;;;
;;;     GType
;;;     GTypeInterface
;;;     GTypeInstance
;;;     GTypeClass
;;;     GTypeInfo                                          not exported
;;;     GTypeFundamentalInfo                               not exported
;;;     GInterfaceInfo                                     not exported
;;;     GTypeValueTable                                    not exported
;;;     G_TYPE_FLAG_RESERVED_ID_BIT                        not implemented
;;;     GTypeDebugFlags                                    not implemented
;;;     GTypeQuery                                         not exported
;;;     GTypeFlags                                         not exported
;;;     GTypeFundamentalFlags                              not exported
;;;
;;; Functions
;;;
;;;     G_TYPE_FUNDAMENTAL
;;;     G_TYPE_MAKE_FUNDAMENTAL                            not exported
;;;     G_TYPE_IS_ABSTRACT
;;;     G_TYPE_IS_DERIVED
;;;     G_TYPE_IS_FUNDAMENTAL
;;;     G_TYPE_IS_VALUE_TYPE
;;;     G_TYPE_HAS_VALUE_TABLE                             not exported
;;;     G_TYPE_IS_CLASSED
;;;     G_TYPE_IS_INSTANTIATABLE
;;;     G_TYPE_IS_DERIVABLE
;;;     G_TYPE_IS_DEEP_DERIVABLE
;;;     G_TYPE_IS_INTERFACE
;;;     G_TYPE_FROM_INSTANCE
;;;     G_TYPE_FROM_CLASS
;;;     G_TYPE_FROM_INTERFACE
;;;     G_TYPE_INSTANCE_GET_CLASS
;;;     G_TYPE_INSTANCE_GET_INTERFACE                      not implemented
;;;     G_TYPE_INSTANCE_GET_PRIVATE                        not implemented
;;;     G_TYPE_CLASS_GET_PRIVATE                           not implemented
;;;     G_TYPE_CHECK_INSTANCE                              not implemented
;;;     G_TYPE_CHECK_INSTANCE_CAST                         not implemented
;;;     G_TYPE_CHECK_INSTANCE_TYPE
;;;     G_TYPE_CHECK_INSTANCE_FUNDAMENTAL_TYPE
;;;     G_TYPE_CHECK_CLASS_CAST                            not implemented
;;;     G_TYPE_CHECK_CLASS_TYPE
;;;     G_TYPE_CHECK_VALUE                                 not implemented
;;;     G_TYPE_CHECK_VALUE_TYPE                            not implemented
;;;
;;;     g_type_init                                        deprecated
;;;     g_type_init_with_debug_flags                       deprecated
;;;     g_type_name
;;;     g_type_qname                                       not implemented
;;;     g_type_from_name
;;;     g_type_parent
;;;     g_type_depth
;;;     g_type_next_base
;;;     g_type_is_a
;;;     g_type_class_ref
;;;     g_type_class_peek
;;;     g_type_class_peek_static                           not exported
;;;     g_type_class_unref
;;;     g_type_class_peek_parent
;;;     g_type_class_add_private                           not exported
;;;     g_type_add_class_private                           not implemented
;;;     g_type_interface_peek
;;;     g_type_interface_peek_parent                       not implemented
;;;     g_type_default_interface_ref
;;;     g_type_default_interface_peek
;;;     g_type_default_interface_unref
;;;     g_type_children
;;;     g_type_interfaces
;;;     g_type_interface_prerequisites
;;;     g_type_set_qdata
;;;     g_type_get_qdata
;;;     g_type_query                                       not exported
;;;
;;;     GBaseInitFunc
;;;     GBaseFinalizeFunc
;;;     GClassInitFunc
;;;     GClassFinalizeFunc
;;;     GInstanceInitFunc
;;;     GInterfaceInitFunc
;;;     GInterfaceFinalizeFunc
;;;     GTypeClassCacheFunc
;;;
;;;     g_type_register_static                             not exported
;;;     g_type_register_static_simple                      not exported
;;;     g_type_register_dynamic                            not implemented
;;;     g_type_register_fundamental                        not implemented
;;;     g_type_add_interface_static                        not exported
;;;     g_type_add_interface_dynamic                       not implemented
;;;     g_type_interface_add_prerequisite                  not exported
;;;     g_type_get_plugin                                  not implemented
;;;     g_type_interface_get_plugin                        not implemented
;;;     g_type_fundamental_next
;;;     g_type_fundamental
;;;     g_type_create_instance                             not implemented
;;;     g_type_free_instance                               not implemented
;;;     g_type_add_class_cache_func                        not implemented
;;;     g_type_remove_class_cache_func                     not implemented
;;;     g_type_class_unref_uncached                        not implemented
;;;     g_type_add_interface_check                         not implemented
;;;     g_type_remove_interface_check                      not implemented
;;;
;;;     GTypeInterfaceCheckFunc
;;;
;;;     g_type_value_table_peek
;;;     g_type_ensure
;;;     g_type_get_type_registration_serial                not implemented
;;;     g_type_get_instance_count                          not implemented
;;;
;;;     G_DECLARE_FINAL_TYPE                               not implemented
;;;     G_DECLARE_DERIVABLE_TYPE                           not implemented
;;;     G_DECLARE_INTERFACE                                not implemented
;;;     G_DEFINE_TYPE                                      not implemented
;;;     G_DEFINE_TYPE_WITH_PRIVATE                         not implemented
;;;     G_DEFINE_TYPE_WITH_CODE                            not implemented
;;;     G_DEFINE_ABSTRACT_TYPE                             not implemented
;;;     G_DEFINE_ABSTRACT_TYPE_WITH_PRIVATE                not implemented
;;;     G_DEFINE_ABSTRACT_TYPE_WITH_CODE                   not implemented
;;;     G_ADD_PRIVATE                                      not implemented
;;;     G_PRIVATE_OFFSET                                   not implemented
;;;     G_PRIVATE_FIELD                                    not implemented
;;;     G_PRIVATE_FIELD_P                                  not implemented
;;;     G_DEFINE_INTERFACE                                 not implemented
;;;     G_DEFINE_INTERFACE_WITH_CODE                       not implemented
;;;     G_IMPLEMENT_INTERFACE                              not implemented
;;;     G_DEFINE_TYPE_EXTENDED                             not implemented
;;;     G_DEFINE_BOXED_TYPE                                not implemented
;;;     G_DEFINE_BOXED_TYPE_WITH_CODE                      not implemented
;;;     G_DEFINE_POINTER_TYPE                              not implemented
;;;     G_DEFINE_POINTER_TYPE_WITH_CODE                    not implemented
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;; This constant is not exported.
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defconstant +type-fundamental-shift+ 2))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INVALID
;;; ----------------------------------------------------------------------------

(defconstant +type-invalid+ #.(ash 0 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-invalid+)
      "Constant"
      (documentation '+type-invalid+ 'variable)
 "@version{2022-12-29}
  @variable-value{0}
  @begin{short}
    An invalid type used as error return value in some functions which return
    a @class{g:type-t} type ID.
  @end{short}
  @see-class{g:type-t}")

(export '+type-invalid+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_NONE
;;; ----------------------------------------------------------------------------

(defconstant +type-none+ #.(ash 1 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-none+)
      "Constant"
      (documentation '+type-none+ 'variable)
 "@version{2022-12-29}
  @variable-value{4}
  @begin{short}
    A fundamental type which is used as a replacement for the C @code{void}
    return type.
  @end{short}
  @see-class{g:type-t}")

(export '+type-none+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INTERFACE
;;; ----------------------------------------------------------------------------

(defconstant +type-interface+ #.(ash 2 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-interface+)
      "Constant"
      (documentation '+type-interface+ 'variable)
 "@version{2022-12-29}
  @variable-value{8}
  @begin{short}
    The fundamental type from which all interface types are derived.
  @end{short}
  @see-class{g:type-t}")

(export '+type-interface+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHAR
;;; ----------------------------------------------------------------------------

(defconstant +type-char+ #.(ash 3 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-char+)
      "Constant"
      (documentation '+type-char+ 'variable)
 "@version{2022-12-29}
  @variable-value{12}
  @begin{short}
    The fundamental type corresponding to @code{gchar}.
  @end{short}
  The type designated by @code{G_TYPE_CHAR} is unconditionally an 8-bit signed
  integer. This may or may not be the same type as the C type @code{gchar}.
  @see-class{g:type-t}")

(export '+type-char+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_UCHAR
;;; ----------------------------------------------------------------------------

(defconstant +type-uchar+ #.(ash 4 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-uchar+)
      "Constant"
      (documentation '+type-uchar+ 'variable)
 "@version{2022-12-29}
  @variable-value{16}
  @begin{short}
    The fundamental type corresponding to @code{guchar}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-uchar+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_BOOLEAN
;;; ----------------------------------------------------------------------------

(defconstant +type-boolean+ #.(ash 5 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-boolean+)
      "Constant"
      (documentation '+type-boolean+ 'variable)
 "@version{2022-12-29}
  @variable-value{20}
  @begin{short}
    The fundamental type corresponding to @code{gboolean}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-boolean+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INT
;;; ----------------------------------------------------------------------------

(defconstant +type-int+ #.(ash 6 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-int+)
      "Constant"
      (documentation '+type-int+ 'variable)
 "@version{2022-12-29}
  @variable-value{24}
  @begin{short}
    The fundamental type corresponding to @code{gint}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-int+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_UINT
;;; ----------------------------------------------------------------------------

(defconstant +type-uint+ #.(ash 7 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-uint+)
      "Constant"
      (documentation '+type-uint+ 'variable)
 "@version{2022-12-29}
  @variable-value{28}
  @begin{short}
    The fundamental type corresponding to @code{guint}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-uint+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_LONG
;;; ----------------------------------------------------------------------------

(defconstant +type-long+ #.(ash 8 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-long+)
      "Constant"
      (documentation '+type-long+ 'variable)
 "@version{2022-12-29}
  @variable-value{32}
  @begin{short}
    The fundamental type corresponding to @code{glong}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-long+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_ULONG
;;; ----------------------------------------------------------------------------

(defconstant +type-ulong+ #.(ash 9 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-ulong+)
      "Constant"
      (documentation '+type-ulong+ 'variable)
 "@version{2022-12-29}
  @variable-value{36}
  @begin{short}
    The fundamental type corresponding to @code{gulong}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-ulong+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INT64
;;; ----------------------------------------------------------------------------

(defconstant +type-int64+ #.(ash 10 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-int64+)
      "Constant"
      (documentation '+type-int64+ 'variable)
 "@version{2022-12-29}
  @variable-value{40}
  @begin{short}
    The fundamental type corresponding to @code{gint64}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-int64+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_UINT64
;;; ----------------------------------------------------------------------------

(defconstant +type-uint64+ #.(ash 11 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-uint64+)
      "Constant"
      (documentation '+type-uint64+ 'variable)
 "@version{2022-12-29}
  @variable-value{44}
  @begin{short}
    The fundamental type corresponding to @code{guint64}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-uint64+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_ENUM
;;; ----------------------------------------------------------------------------

(defconstant +type-enum+ #.(ash 12 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-enum+)
      "Constant"
      (documentation '+type-enum+ 'variable)
 "@version{2022-12-29}
  @variable-value{48}
  @begin{short}
    The fundamental type from which all enumeration types are derived.
  @end{short}
  @see-class{g:type-t}")

(export '+type-enum+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FLAGS
;;; ----------------------------------------------------------------------------

(defconstant +type-flags+ #.(ash 13 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-flags+)
      "Constant"
      (documentation '+type-flags+ 'variable)
 "@version{2022-12-29}
  @variable-value{52}
  @begin{short}
    The fundamental type from which all flags types are derived.
  @end{short}
  @see-class{g:type-t}")

(export '+type-flags+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FLOAT
;;; ----------------------------------------------------------------------------

(defconstant +type-float+ #.(ash 14 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-float+)
      "Constant"
      (documentation '+type-float+ 'variable)
 "@version{2022-12-29}
  @variable-value{56}
  @begin{short}
    The fundamental type corresponding to @code{gfloat}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-float+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_DOUBLE
;;; ----------------------------------------------------------------------------

(defconstant +type-double+ #.(ash 15 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-double+)
      "Constant"
      (documentation '+type-double+ 'variable)
 "@version{2022-12-29}
  @variable-value{60}
  @begin{short}
    The fundamental type corresponding to @code{gdouble}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-double+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_STRING
;;; ----------------------------------------------------------------------------

(defconstant +type-string+ #.(ash 16 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-string+)
      "Constant"
      (documentation '+type-string+ 'variable)
 "@version{2022-12-29}
  @variable-value{64}
  @begin{short}
    The fundamental type corresponding to nul-terminated C strings.
  @end{short}
  @see-class{g:type-t}")

(export '+type-string+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_POINTER
;;; ----------------------------------------------------------------------------

(defconstant +type-pointer+ #.(ash 17 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-pointer+)
      "Constant"
      (documentation '+type-pointer+ 'variable)
 "@version{2022-12-29}
  @variable-value{68}
  @begin{short}
    The fundamental type corresponding to @code{gpointer}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-pointer+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_BOXED
;;; ----------------------------------------------------------------------------

(defconstant +type-boxed+ #.(ash 18 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-boxed+)
      "Constant"
      (documentation '+type-boxed+ 'variable)
 "@version{2022-12-29}
  @variable-value{72}
  @begin{short}
    The fundamental type from which all boxed types are derived.
  @end{short}
  @see-class{g:type-t}")

(export '+type-boxed+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM
;;; ----------------------------------------------------------------------------

(defconstant +type-param+ #.(ash 19 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-param+)
      "Constant"
      (documentation '+type-param+ 'variable)
 "@version{2022-12-29}
  @variable-value{76}
  @begin{short}
    The fundamental type from which all @code{GParamSpec} types are derived.
  @end{short}
  @see-class{g:type-t}
  @see-symbol{g:param-spec}")

(export '+type-param+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_OBJECT
;;; ----------------------------------------------------------------------------

(defconstant +type-object+ #.(ash 20 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-object+)
      "Constant"
      (documentation '+type-object+ 'variable)
 "@version{2022-12-29}
  @variable-value{80}
  @begin{short}
    The fundamental type for @code{GObject}.
  @end{short}
  @see-class{g:type-t}
  @see-class{g:object}")

(export '+type-object+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_GTYPE
;;; ----------------------------------------------------------------------------

(glib-init:at-init ()
  (cffi:foreign-funcall "g_gtype_get_type" :size))

(defconstant +type-gtype+ (cffi:foreign-funcall "g_gtype_get_type" :size))

#+liber-documentation
(setf (liber:alias-for-variable '+type-gtype+)
      "Constant"
      (documentation '+type-gtype+ 'variable)
 "@version{2022-12-29}
  @begin{short}
    The fundamental type for @code{GType}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-gtype+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VARIANT
;;; ----------------------------------------------------------------------------

(defconstant +type-variant+ #.(ash 21 +type-fundamental-shift+))

#+liber-documentation
(setf (liber:alias-for-variable '+type-variant+)
      "Constant"
      (documentation '+type-variant+ 'variable)
 "@version{2022-12-29}
  @variable-value{84}
  @short{The fundamental type corresponding to @code{GVariant}.}

  All floating @code{GVariant} instances passed through the @code{GType} system
  are consumed. Note that callbacks in closures, and signal handlers for signals
  of return type @code{GVariant}, must never return floating variants.
  @see-class{g:type-t}
  @see-type{g:variant}")

(export '+type-variant+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECKSUM
;;; ----------------------------------------------------------------------------

(glib-init:at-init ()
  (cffi:foreign-funcall "g_checksum_get_type" :size))

(defconstant +type-checksum+
             (cffi:foreign-funcall "g_checksum_get_type" :size))

#+liber-documentation
(setf (liber:alias-for-variable '+type-checksum+)
      "Constant"
      (documentation '+type-checksum+ 'variable)
 "@version{2022-12-29}
  @begin{short}
    The @class{g:type-t} type ID for a boxed type holding a @code{GChecksum}.
  @end{short}
  @see-class{g:type-t}")

(export '+type-checksum+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_GLIB_FIRST                             not exported
;;; ----------------------------------------------------------------------------

(defconstant +type-reserved-glib-first+ 22)

#+liber-documentation
(setf (liber:alias-for-variable '+type-reserved-glib-first+)
      "Constant"
      (documentation '+type-reserved-glib-first+ 'variable)
 "@version{#2022-12-31}
  @variable-value{22}
  @begin{short}
    First fundamental type number to create a new fundamental type ID with
    @fun{g-type-make-fundamental} reserved for GLib.
  @end{short}
  @see-class{type-t}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_GLIB_LAST                              not exported
;;; ----------------------------------------------------------------------------

(defconstant +type-reserved-glib-last+ 31)

#+liber-documentation
(setf (liber:alias-for-variable '+type-reserved-glib-last+)
      "Constant"
      (documentation '+type-reserved-glib-last+ 'variable)
 "@version{#2022-12-31}
  @variable-value{31}
  @begin{short}
    Last fundamental type number reserved for GLib.
  @end{short}
  @see-class{type-t}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_BSE_FIRST                              not exported
;;; ----------------------------------------------------------------------------

(defconstant +type-reserved-bse-first+ 32)

#+liber-documentation
(setf (liber:alias-for-variable '+type-reserved-bse-first+)
      "Constant"
      (documentation '+type-reserved-bse-first+ 'variable)
 "@version{#2022-12-31}
  @variable-value{32}
  @begin{short}
    First fundamental type number to create a new fundamental type ID with
    @fun{g-type-make-fundamental} reserved for BSE.
  @end{short}
  @see-class{type-t}
  @see-function{g-type-make-fundamental}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_BSE_LAST                               not exported
;;; ----------------------------------------------------------------------------

(defconstant +type-reserved-bse-last+ 48)

#+liber-documentation
(setf (liber:alias-for-variable '+type-reserved-bse-last+)
      "Constant"
      (documentation '+type-reserved-bse-last+ 'variable)
 "@version{#2022-12-31}
  @variable-value{48}
  @begin{short}
    Last fundamental type number reserved for BSE.
  @end{short}
  @see-class{type-t}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_USER_FIRST                             not exported
;;; ----------------------------------------------------------------------------

(defconstant +type-reserved-user-first+ 49)

#+liber-documentation
(setf (liber:alias-for-variable '+type-reserved-user-first+)
      "Constant"
      (documentation '+type-reserved-user-first+ 'variable)
 "@version{#2022-12-31}
  @variable-value{49}
  @begin{short}
    First available fundamental type number to create new fundamental type ID
    with @fun{g-type-make-fundamental}.
  @end{short}
  @see-class{type-t}
  @see-function{g-type-make-fundamental}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FUNDAMENTAL_MAX                                 not exported
;;; ----------------------------------------------------------------------------

(defconstant +type-fundamental-max+
             #.(ash 255 +type-fundamental-shift+)
 #+liber-documentation
 "@version{#2013-3-31}
  @variable-value{1020}
  An integer constant that represents the number of identifiers reserved for
  types that are assigned at compile time.")

;;; ----------------------------------------------------------------------------

;; Check for equality of types. This is faster than the function type-is-a.

;; TODO: There is a bug. The function type-t= is used to check against the type
;;       g:+type-invalid+. But this does not work.

(defun gtype= (gtype1 gtype2)
  (eq (glib:gtype gtype1) (glib:gtype gtype2)))

(defun gtype/= (gtype1 gtype2)
  (not (eq (glib:gtype gtype1) (glib:gtype gtype2))))

;;; ----------------------------------------------------------------------------

;; type-t converts automatically between the Lisp type gtype,
;; an integer or a string and the foreign type GType

(cffi:define-foreign-type type-t ()
  ((mangled-p :initarg :mangled-p
              :reader gtype-mangled-p
              :initform nil
              :documentation
              "Whether the type designator is mangled with
               the @code{G_SIGNAL_TYPE_STATIC_SCOPE} flag."))
  (:documentation
    "@version{#2023-9-19}
     @begin{short}
       Values of this CFFI foreign @class{g:type-t} type ID identify the C
       GType.
     @end{short}
     The @class{g:type-t} type ID is designated by its name, a string, or a
     numeric identifier. Functions accept @class{g:type-t} type ID designators
     as a string or integer and return them as a string. The @fun{g:type-name}
     and @fun{g:type-from-name} functions are used to convert between the name
     and the numeric identifier. Numeric identifier of the @class{g:type-t}
     type ID may be different between different program runs. But string
     identifier of a @class{g:type-t} type ID does not change.
     @begin[Examples]{dictionary}
       @begin{pre}
g:+type-double+ => 60
(g:type-name g:+type-double+) => \"gdouble\"
(g:type-from-name \"gdouble\") => #<GTYPE :name \"gdouble\" :id 60>
       @end{pre}
     @end{dictionary}
     @see-function{g:type-name}
     @see-function{g:type-from-name}")
  (:actual-type :size)
  (:simple-parser type-t))

(defun gtype-unmangle (gtype)
  (logxor gtype (ldb (byte 1 0) gtype)))

(defmethod cffi:translate-from-foreign (value (gtype type-t))
  (glib:gtype (if (gtype-mangled-p gtype)
                  (gtype-unmangle value)
                  value)))

(defmethod cffi:translate-to-foreign (value (gtype type-t))
  (glib:gtype-id (glib:gtype value)))

(export 'type-t)

;;; ----------------------------------------------------------------------------
;;; struct GTypeInterface
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-interface
  (:type type-t)
  (:instance-type type-t))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-interface)
      "CStruct"
      (liber:symbol-documentation 'type-interface)
 "@version{2022-12-29}
  @short{An opaque structure used as the base of all interface types.}
  @begin{pre}
(cffi:defcstruct type-interface
  (:type type-t)
  (:instance-type type-t))
  @end{pre}
  @see-class{g:type-t}
  @see-symbol{g:type-class}
  @see-symbol{g:type-instance}
  @see-function{g:type-is-interface}")

(export 'type-interface)

;;; ----------------------------------------------------------------------------
;;; struct GTypeClass
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-class
  (:type type-t))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-class)
      "CStruct"
      (liber:symbol-documentation 'type-class)
 "@version{2022-12-29}
  @short{An opaque structure used as the base of all classes.}
  @begin{pre}
(cffi:defcstruct type-class
  (:type type-t))
  @end{pre}
  @see-class{g:type-t}
  @see-symbol{g:type-interface}
  @see-symbol{g:type-instance}")

(export 'type-class)

;;; ----------------------------------------------------------------------------
;;; struct GTypeInstance
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-instance
  (:class (:pointer (:struct type-class))))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-instance)
      "CStruct"
      (liber:symbol-documentation 'type-instance)
 "@version{2022-12-29}
  @short{An opaque structure used as the base of all type instances.}
  @begin{pre}
(cffi:defcstruct type-instance
  (:class (:pointer (:struct type-class))))
  @end{pre}
  @see-class{g:type-t}
  @see-symbol{g:type-class}
  @see-symbol{g:type-interface}")

(export 'type-instance)

;;; ----------------------------------------------------------------------------
;;; struct GTypeInfo                                       not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-info
  (:class-size :uint16)
  (:base-init-fn :pointer)
  (:base-finalize-fn :pointer)
  (:class-init-fn :pointer)
  (:class-finalize-fn :pointer)
  (:class-data :pointer)
  (:instance-size :uint16)
  (:n-preallocs :uint16)
  (:instance-init-fn :pointer)
  (:value-table :pointer))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-info)
      "CStruct"
      (liber:symbol-documentation 'type-info)
 "@version{#2022-12-31}
  @begin{short}
    This structure is used to provide the type system with the information
    required to initialize and destruct (finalize) a type's class and its
    instances.
  @end{short}
  The initialized structure is passed to the function
  @fun{type-register-static}. The type system will perform a deep copy of
  this structure, so its memory does not need to be persistent across
  invocation of the function @fun{type-register-static}.
  @begin{pre}
(cffi:defcstruct type-info
  (:class-size :uint16)
  (:base-init-fn :pointer)
  (:base-finalize-fn :pointer)
  (:class-init-fn :pointer)
  (:class-finalize-fn :pointer)
  (:class-data :pointer)
  (:instance-size :uint16)
  (:n-preallocs :uint16)
  (:instance-init-fn :pointer)
  (:value-table :pointer))
  @end{pre}
  @begin[code]{table}
    @begin[:class-size]{entry}
      Size of the class structure, required for interface, classed and
      instantiatable types.
    @end{entry}
    @begin[:base-init-fn]{entry}
      Location of the base initialization function (optional).
    @end{entry}
    @begin[:base-finalize-fn]{entry}
      Location of the base finalization function (optional).
    @end{entry}
    @begin[:class-init-fn]{entry}
      Location of the class initialization function for classed and
      instantiatable types. Location of the default vtable inititalization
      function for interface types (optional). This function is used both to
      fill in virtual functions in the class or default vtable, and to do type
      specific setup such as registering signals and object properties.
    @end{entry}
    @begin[:class-finalize-fn]{entry}
      Location of the class finalization function for classed and
      instantiatable types. Location fo the default vtable finalization
      function for interface types (optional).
    @end{entry}
    @begin[:class-data]{entry}
      User supplied data passed to the class init/finalize functions.
    @end{entry}
    @begin[:instance-size]{entry}
      Size of the instance (object) structure (required for instantiatable
      types only).
    @end{entry}
    @begin[:n-preallocs]{entry}
      Prior to GLib 2.10, it specified the number of pre-allocated (cached)
      instances to reserve memory for (0 indicates no caching). Since
      GLib 2.10, it is ignored, since instances are allocated with the slice
      allocator now.
    @end{entry}
    @begin[:instance-init-fn]{entry}
      Location of the instance initialization function (optional, for
      instantiatable types only).
    @end{entry}
    @begin[:value-table]{entry}
      A @symbol{type-value-table} function table for generic handling of
      @symbol{value}s of this type, usually only useful for fundamental
      types.
    @end{entry}
  @end{table}
  @see-symbol{value}
  @see-symbol{type-value-table}
  @see-function{type-register-static}")

;;; ----------------------------------------------------------------------------
;;; enum GTypeFundamentalFlags                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defbitfield type-fundamental-flags
  :classed
  :instantiatable
  :derivable
  :deep-derivable)

#+liber-documentation
(setf (liber:alias-for-symbol 'type-fundamental-flags)
      "Bitfield"
      (liber:symbol-documentation 'type-fundamental-flags)
 "@version{#2022-12-31}
  @begin{short}
    Bit masks used to check or determine specific characteristics of a
   fundamental type.
  @end{short}
  See the functions @fun{type-is-classed}, @fun{type-is-instantiatable},
  and @fun{type-is-deep-variable} to check a type for these flags.
  @begin{pre}
(cffi:defbitfield type-fundamental-flags
  :classed
  :instantiatable
  :derivable
  :deep-derivable)
  @end{pre}
  @begin[code]{table}
    @entry[:classed]{Indicates a classed type.}
    @entry[:instantiatable]{Indicates an instantiable type (implies classed).}
    @entry[:derivable]{Indicates a flat derivable type.}
    @entry[:deep-derivable]{Indicates a deep derivable type (implies
      derivable).}
  @end{table}
  @see-class{type-t}
  @see-function{type-is-classed}
  @see-function{type-is-instantiatable}
  @see-function{type-is-derivable}
  @see-function{type-is-deep-derivable}")

;;; ----------------------------------------------------------------------------
;;; struct GTypeFundamentalInfo                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-fundamental-info
  (:type-flags type-fundamental-flags))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-fundamental-info)
      "CStruct"
      (liber:symbol-documentation 'type-fundamental-info)
 "@version{#2013-4-1}
  @begin{short}
    A structure that provides information to the type system which is used
    specifically for managing fundamental types.
  @end{short}
  @begin{pre}
(cffi:defcstruct type-fundamental-info
  (:type-flags type-fundamental-flags))
  @end{pre}
  @begin[code]{table}
    @entry[type-flags]{@symbol{type-fundamental-flags} describing
      the characteristics of the fundamental type.}
  @end{table}
  @see-symbol{type-fundamental-flags}")

;;; ----------------------------------------------------------------------------
;;; struct GInterfaceInfo                                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct interface-info
  (:interface-init :pointer)
  (:interface-finalize :pointer)
  (:interface-data :pointer))

#+liber-documentation
(setf (liber:alias-for-symbol 'interface-info)
      "CStruct"
      (liber:symbol-documentation 'interface-info)
 "@version{#2022-12-31}
  @begin{short}
    A structure that provides information to the type system which is used
    specifically for managing interface types.
  @end{short}
  @begin{pre}
(cffi:defcstruct interface-info
  (:interface-init :pointer)
  (:interface-finalize :pointer)
  (:interface-data :pointer))
  @end{pre}
  @begin[@code]{table}
    @entry[:interface-init]{Location of the interface initialization function.}
    @entry[:interface-finalize]{Location of the interface finalization
      function.}
    @entry[:interface-data]{User supplied data passed to the interface
      init/finalize functions.}
  @end{table}
  @see-symbol{type-interface}")

;;; ----------------------------------------------------------------------------
;;; struct GTypeValueTable                                 not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-value-table
  (:value-init :pointer)
  (:value-free :pointer)
  (:value-copy :pointer)
  (:value-peek-pointer :pointer)
  (:collect-format (:string :free-from-foreign nil :free-to-foreign nil))
  (:collect-value :pointer)
  (:lcopy-format (:string :free-from-foreign nil :free-to-foreign nil))
  (:lcopy-value :pointer))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-value-table)
      "CStruct"
      (liber:symbol-documentation 'type-value-table)
 "@version{#2022-12-31}
  @begin{short}
    The @symbol{type-value-table} structure provides the functions required by
    the @symbol{value} implementation, to serve as a container for values of a
    type.
  @end{short}
  @begin{pre}
(cffi:defcstruct type-value-table
  (:value-init :pointer)
  (:value-free :pointer)
  (:value-copy :pointer)
  (:value-peek-pointer :pointer)
  (:collect-format (:string :free-from-foreign nil :free-to-foreign nil))
  (:collect-value :pointer)
  (:lcopy-format (:string :free-from-foreign nil :free-to-foreign nil))
  (:lcopy-value :pointer))
  @end{pre}
  @begin[code]{table}
    @begin[:value-init]{entry}
      Default initialize values contents by poking values directly into the
      @code{value->data array}. The data array of the @symbol{value} passed
      into this function was zero-filled with @code{memset()}, so no care has
      to be taken to free any old contents. E.g. for the implementation of a
      string value that may never be @code{NULL}, the implementation might look
      like:
      @begin{pre}
 value->data[0].v_pointer = g_strdup (\"\");
      @end{pre}
    @end{entry}
    @begin[:value-free]{entry}
      Free any old contents that might be left in the data array of the passed
      in value. No resources may remain allocated through the @symbol{value}
      contents after this function returns. E.g. for our above string type:
      @begin{pre}
 // only free strings without a specific flag for static storage
 if (!(value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS))
 g_free (value->data[0].v_pointer);
      @end{pre}
    @end{entry}
    @begin[:value-copy]{entry}
      @arg{dest-value} is a @symbol{value} with zero-filled data section and
      @arg{src-value} is a properly setup @symbol{value} of same or derived
      type. The purpose of this function is to copy the contents of
      @arg{src-value} into @arg{dest-value} in a way, that even after
      @arg{src-value} has been freed, the contents of @arg{dest-value} remain
      valid. String type example:
      @begin{pre}
 dest_value->data[0].v_pointer = g_strdup (src_value->data[0].v_pointer);
      @end{pre}
    @end{entry}
    @begin[:value-peek-pointer]{entry}
      If the value contents fit into a pointer, such as objects or strings,
      return this pointer, so the caller can peek at the current contents. To
      extend on our above string example:
      @begin{pre}
 return value->data[0].v_pointer;
      @end{pre}
    @end{entry}
    @begin[:collect-format]{entry}
      A string format describing how to collect the contents of this value
      bit-by-bit. Each character in the format represents an argument to be
      collected, and the characters themselves indicate the type of the
      argument. Currently supported arguments are:
      @begin{table}
        @entry['i']{Integers, passed as @code{collect_values[].v_int}.}
        @entry['l']{Longs, passed as @code{collect_values[].v_long}.}
        @entry['d']{Doubles, passed as @code{collect_values[].v_double}.}
        @entry['p']{Pointers, passed as @code{collect_values[].v_pointer}.}
      @end{table}
      It should be noted that for variable argument list construction, ANSI C
      promotes every type smaller than an integer to an int, and floats to
      doubles. So for collection of short int or char, 'i' needs to be used,
      and for collection of floats 'd'.
    @end{entry}
    @begin[:collect-value]{entry}
      The @code{collect_value()} function is responsible for converting the
      values collected from a variable argument list into contents suitable for
      storage in a @symbol{value}. This function should setup value similar
      to @code{value_init()}; e.g. for a string value that does not allow
      @code{NULL} pointers, it needs to either spew an error, or do an implicit
      conversion by storing an empty string. The value passed in to this
      function has a zero-filled data array, so just like for
      @code{value_init()} it is guaranteed to not contain any old contents that
      might need freeing. @code{n_collect_values} is exactly the string length
      of collect_format, and @code{collect_values} is an array of unions
      @code{GTypeCValue} with length @code{n_collect_values}, containing the
      collected values according to  @code{collect_format}. @code{collect_flags}
      is an argument provided as a hint by the caller. It may contain the flag
      @code{G_VALUE_NOCOPY_CONTENTS} indicating, that the collected value
      contents may be considered \"static\" for the duration of the value
      lifetime. Thus an extra copy of the contents stored in
      @code{collect_values} is not required for assignment to value. For our
      above string example, we continue with:
      @begin{pre}
 if (!collect_values[0].v_pointer)
     value->data[0].v_pointer = g_strdup (\"\");
 else if (collect_flags & G_VALUE_NOCOPY_CONTENTS)
   {
     value->data[0].v_pointer = collect_values[0].v_pointer;
     // keep a flag for the value_free() implementation to not free this
     // string
     value->data[1].v_uint = G_VALUE_NOCOPY_CONTENTS;
   @}
   else
     value->data[0].v_pointer = g_strdup (collect_values[0].v_pointer);
   return NULL;
      @end{pre}
      It should be noted, that it is generally a bad idea to follow the
      @code{G_VALUE_NOCOPY_CONTENTS} hint for reference counted types. Due to
      reentrancy requirements and reference count assertions performed by the
      signal emission code, reference counts should always be incremented for
      reference counted contents stored in the @code{value->data} array. To
      deviate from our string example for a moment, and taking a look at an
      exemplary implementation for @code{collect_value()} of GObject:
      @begin{pre}
 if (collect_values[0].v_pointer)
 {
     GObject *object = G_OBJECT (collect_values[0].v_pointer);
     // never honour G_VALUE_NOCOPY_CONTENTS for ref-counted types
     value->data[0].v_pointer = g_object_ref (object);
     return NULL;
 @}
 else
     return g_strdup_printf (\"Object passed as invalid NULL pointer\");
 @}
      @end{pre}
      The reference count for valid objects is always incremented, regardless
      of @code{collect_flags}. For invalid objects, the example returns a newly
      allocated string without altering value. Upon success,
      @code{collect_value()} needs to return @code{NULL}. If, however, an error
      condition occurred, @code{collect_value()} may spew an error by returning
      a newly allocated non-@code{NULL} string, giving a suitable description
      of the error condition. The calling code makes no assumptions about the
      value contents being valid upon error returns, value is simply thrown
      away without further freeing. As such, it is a good idea to not allocate
      @symbol{value} contents, prior to returning an error, however,
      @code{collect_values()} is not obliged to return a correctly setup value
      for error returns, simply because any non-@code{NULL} return is
      considered a fatal condition so further program behaviour is undefined.
    @end{entry}
    @begin[:lcopy-format]{entry}
      Format description of the arguments to collect for @code{lcopy_value},
      analogous to @code{collect_format}. Usually, @code{lcopy_format} string
      consists only of 'p's to provide @code{lcopy_value()} with pointers to
      storage locations.
    @end{entry}
    @begin[:lcopy-value]{entry}
      This function is responsible for storing the value contents into
      arguments passed through a variable argument list which got collected
      into @code{collect_values} according to @code{lcopy_format}.
      @code{n_collect_values} equals the string length of @code{lcopy_format},
      and collect_flags may contain @code{G_VALUE_NOCOPY_CONTENTS}. In contrast
      to @code{collect_value()}, @code{lcopy_value()} is obliged to always
      properly support @code{G_VALUE_NOCOPY_CONTENTS}. Similar to
      @code{collect_value()} the function may prematurely abort by returning a
      newly allocated string describing an error condition. To complete the
      string example:
      @begin{pre}
 gchar **string_p = collect_values[0].v_pointer;
 if (!string_p)
   return g_strdup_printf (\"string location passed as NULL\");
 if (collect_flags & G_VALUE_NOCOPY_CONTENTS)
   *string_p = value->data[0].v_pointer;
 else
   *string_p = g_strdup (value->data[0].v_pointer);
      @end{pre}
      And an illustrative version of @code{lcopy_value()} for reference-counted
      types:
      @begin{pre}
 GObject **object_p = collect_values[0].v_pointer;
 if (!object_p)
   return g_strdup_printf (\"object location passed as NULL\");
 if (!value->data[0].v_pointer)
   *object_p = NULL;
 else if (collect_flags & G_VALUE_NOCOPY_CONTENTS) /* always honour */
   *object_p = value->data[0].v_pointer;
 else
   *object_p = g_object_ref (value->data[0].v_pointer);
 return NULL;
      @end{pre}
    @end{entry}
  @end{table}
  @see-symbol{value}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FLAG_RESERVED_ID_BIT
;;;
;;; #define G_TYPE_FLAG_RESERVED_ID_BIT ((GType) (1 << 0))
;;;
;;; A bit in the type number that's supposed to be left untouched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GTypeDebugFlags
;;;
;;; typedef enum {
;;;   G_TYPE_DEBUG_NONE    = 0,
;;;   G_TYPE_DEBUG_OBJECTS = 1 << 0,
;;;   G_TYPE_DEBUG_SIGNALS = 1 << 1,
;;;   G_TYPE_DEBUG_MASK    = 0x03
;;; } GTypeDebugFlags;
;;;
;;; Warning
;;;
;;; GTypeDebugFlags has been deprecated since version 2.36 and should not be
;;; used in newly written code. g_type_init() is now done automatically
;;;
;;; These flags used to be passed to g_type_init_with_debug_flags() which is now
;;; deprecated.
;;;
;;; If you need to enable debugging features, use the GOBJECT_DEBUG environment
;;; variable.
;;;
;;; G_TYPE_DEBUG_NONE
;;;     Print no messages.
;;;
;;; G_TYPE_DEBUG_OBJECTS
;;;     Print messages about object bookkeeping.
;;;
;;; G_TYPE_DEBUG_SIGNALS
;;;     Print messages about signal emissions.
;;;
;;; G_TYPE_DEBUG_MASK
;;;     Mask covering all debug flags.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GTypeQuery                                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-query
  (:type type-t)
  (:type-name :string)
  (:class-size :uint)
  (:instance-size :uint))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-query)
      "CStruct"
      (liber:symbol-documentation 'type-query)
 "@version{#2020-10-31}
  @begin{short}
    A structure holding information for a specific type.
  @end{short}
  It is filled in by the function @fun{type-query}.
  @begin{pre}
(cffi:defcstruct type-query
  (:type type-t)
  (:type-name :string)
  (:class-size :uint)
  (:instance-size :uint))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @class{g:type-t} type ID of the type.}
    @entry[type-name]{A string with the name of the type.}
    @entry[class-size]{An unsigned integer with the size of the class
      structure.}
    @entry[instance-size]{An unsigned integer with the size of the instance
      structure.}
  @end{table}
  @see-class{type-t}
  @see-function{type-query}")

;;; ----------------------------------------------------------------------------
;;; enum GTypeFlags                                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defbitfield type-flags
  (:abstract #.(ash 1 4))
  (:value-abstract #.(ash 1 5)))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-flags)
      "Bitfield"
      (liber:symbol-documentation 'type-flags)
 "@version{#2022-12-31}
  @short{Bit masks used to check or determine characteristics of a type.}
  See the function @fun{type-is-abstract} to check a type for the flag
  @code{:abstract} and the function @fun{type-is-value-abstract} to check a
  type for the flag @code{:value-abstract}.
  @begin{pre}
(cffi:defbitfield type-flags
  (:abstract #.(ash 1 4))
  (:value-abstract #.(ash 1 5)))
  @end{pre}
  @begin[code]{table}
    @entry[:abstract]{Indicates an abstract type. No instances can be created
      for an abstract type.}
    @entry[:value-abstract]{Indicates an abstract value type, i.e. a type that
      introduces a value table, but can not be used for the function
      @fun{value-init}.}
  @end{table}
  @see-class{type-t}
  @see-function{value-init}
  @see-function{type-is-abstract}
  @see-function{type-is-value-abstract}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FUNDAMENTAL()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_fundamental" type-fundamental) type-t
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a valid @class{g:type-t} type ID}
  @return{The fundamental @class{g:type-t} type ID of the @arg{gtype} argument.}
  @begin{short}
    The fundamental type which is the ancestor of the @arg{gtype} argument.
  @end{short}
  Fundamental types are types that serve as ultimate bases for the derived
  types, thus they are the roots of distinct inheritance hierarchies.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-fundamental \"GtkButton\") => #<GTYPE :name \"GObject\" :id 80>
(g:type-fundamental \"GtkOrientable\") => #<GTYPE :name \"GInterface\" :id 8>
(g:type-fundamental \"GtkWindowType\") => #<GTYPE :name \"GEnum\" :id 48>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (gtype type-t))

(export 'type-fundamental)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MAKE_FUNDAMENTAL()                              not exported
;;; ----------------------------------------------------------------------------

(defun type-make-fundamental (x)
 #+liber-documentation
 "@version{#2020-11-1}
  @argument[x]{a fundamental type number}
  @return{The type ID of the @class{type-t}.}
  @begin{short}
    Get the type ID for the fundamental type number @arg{x}.
  @end{short}
  Use the function @fun{type-fundamental-next} instead of this function to
  create new fundamental type number.
  @begin[Note]{dictionary}
    The @fun{g:type-make-fundamental} function does not return a Lisp
    @class{type-t} value, but the ID number of the @class{type-t}.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-make-fundamental 5)
=> 20
(gtype (g:type-make-fundamental 5))
=> <GTYPE :name \"gboolean\" :id 20>
    @end{pre}
  @end{dictionary}
  @see-class{type-t}
  @see-function{type-fundamental-next}"
  (ash x +type-fundamental-shift+))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_ABSTRACT()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_test_flags" %type-test-flags) :boolean
  (gtype type-t)
  (flag type-flags))

(defun type-is-abstract (gtype)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is an abstract type.}
  @begin{short}
    Checks if @arg{gtype} is an abstract type.
  @end{short}
  An abstract type cannot be instantiated and is normally used as an abstract
  base class for derived classes.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-is-abstract \"GtkWidget\") => T
(g:type-is-abstract \"GtkButton\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (%type-test-flags gtype :abstract))

(export 'type-is-abstract)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_DERIVED()
;;; ----------------------------------------------------------------------------

(defun type-is-derived (gtype)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a dervied type.}
  @begin{short}
    Checks if @arg{gtype} is derived or in object oriented terminology
    inherited from another type.
  @end{short}
  This holds true for all non-fundamental types.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-is-derived \"gboolean\") => NIL
(g:type-is-derived \"GObject\") => NIL
(g:type-is-derived \"GtkWidget\") => T
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-is-fundamental}"
  (> (glib:gtype-id (glib:gtype gtype)) +type-fundamental-max+))

(export 'type-is-derived)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_FUNDAMENTAL()
;;; ----------------------------------------------------------------------------

(defun type-is-fundamental (gtype)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a fundamental type.}
  @begin{short}
    Checks if @arg{gtype} is a fundamental type.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-is-fundamental \"gboolean\") => T
(g:type-is-fundamental \"GObject\") => T
(g:type-is-fundamental \"GtkWidget\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-is-derived}"
  (<= (glib:gtype-id (glib:gtype gtype)) +type-fundamental-max+))

(export 'type-is-fundamental)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE_TYPE()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_check_is_value_type" %type-check-is-value-type) :boolean
  (gtype type-t))

(defun type-is-value-type (gtype)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a value type.}
  @begin{short}
    Checks if @arg{gtype} is a value type and can be used with the function
    @fun{value-init}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-is-value-type \"gint\") => T
(g:type-is-value-type \"GObject\") => T
(g:type-is-value-type \"GEnum\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:value}
  @see-function{g:value-init}"
  (%type-check-is-value-type gtype))

(export 'type-is-value-type)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_HAS_VALUE_TABLE()                               not exported
;;; ----------------------------------------------------------------------------

(defun type-has-value-table (gtype)
 #+liber-documentation
 "@version{#2020-11-1}
  @argument[type]{a @class{type-t} ID}
  @return{@em{True} if @arg{gtype} has a @code{GTypeValueTable}.}
  @begin{short}
    Checks if @arg{gtype} has a @code{GTypeValueTable}.
  @end{short}
  This function calls the function @fun{type-value-table-peek} to do the
  check.
  @see-class{type-t}
  @see-symbol{type-value-table}
  @see-function{type-value-table-peek}"
  (not (cffi:null-pointer-p (type-value-table-peek gtype))))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_CLASSED()
;;; ----------------------------------------------------------------------------

;; We need this second variant of the function g_type_test_flags(), because
;; we can not pass a flag of type type-fundamental-flag to our first version.
;; See the implementation for G_TYPE_IS_ABSTRACT.

(cffi:defcfun ("g_type_test_flags" %type-test-fundamental-flags) :boolean
  (gtype type-t)
  (flag type-fundamental-flags))

(defun type-is-classed (gtype)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a classed type.}
  @short{Checks if @arg{gtype} is a classed type.}
  @see-class{g:type-t}
  @see-symbol{g:type-class}"
  (%type-test-fundamental-flags gtype :classed))

(export 'type-is-classed)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_INSTANTIATABLE()                             not exported
;;; ----------------------------------------------------------------------------

(defun type-is-instantiatable (gtype)
 #+liber-documentation
 "@version{#2020-11-13}
  @argument[gtype]{a @class{type-t} ID}
  @return{@em{True} if @arg{gtype} can be instantiated.}
  @begin{short}
    Checks if @arg{gtype} can be instantiated.
  @end{short}
  Instantiation is the process of creating an instance (object) of this type.
  @see-class{type-t}"
  (%type-test-fundamental-flags gtype :instantiatable))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_DERIVABLE()                                  not exported
;;; ----------------------------------------------------------------------------

(defun type-is-derivable (gtype)
 #+liber-documentation
 "@version{#2020-11-1}
  @argument[gtype]{a @class{type-t} ID}
  @return{@em{True} if @arg{gtype} is a derivable type.}
  @begin{short}
    Checks if @arg{gtype} is a derivable type.
  @end{short}
  A derivable type can be used as the base class of a flat (single-level) class
  hierarchy.
  @see-class{type-t}
  @see-function{type-is-deep-derivable}"
  (%type-test-fundamental-flags gtype :derivable))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_DEEP_DERIVABLE()                             not exported
;;; ----------------------------------------------------------------------------

(defun type-is-deep-derivable (gtype)
 #+liber-documentation
 "@version{#2020-11-1}
  @argument[gtype]{a @class{type-t} ID}
  @return{@em{True} if @arg{gtype} is a deep derivable type.}
  @begin{short}
    Checks if @arg{gtype} is a deep derivable type.
  @end{short}
  A deep derivable type can be used as the base class of a deep (multi-level)
  class hierarchy.
  @see-class{type-t}
  @see-function{type-is-derivable}"
  (%type-test-fundamental-flags gtype :deep-derivable))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_INTERFACE()
;;; ----------------------------------------------------------------------------

(defun type-is-interface (gtype)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is an interface type.}
  @begin{short}
    Checks if @arg{gtype} is an interface type.
  @end{short}
  An interface type provides a pure API, the implementation of which is
  provided by another type, which is then said to conform to the interface.
  GLib interfaces are somewhat analogous to Java interfaces and C++ classes
  containing only pure virtual functions, with the difference that GType
  interfaces are not derivable.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-is-interface \"GAction\") => T
(g:type-is-interface (g:gtype \"GAction\")) => T
(g:type-is-interface \"GSimpleAction\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-interface}"
  (eq (glib:gtype +type-interface+) (type-fundamental gtype)))

(export 'type-is-interface)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FROM_INSTANCE()
;;; ----------------------------------------------------------------------------

(defun type-from-instance (instance)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[instance]{a valid @symbol{g:type-instance} instance}
  @return{The @class{g:type-t} type ID of @arg{instance}.}
  @short{Get the type identifier from a given instance.}
  This function should only be used in type implementations.
  @begin[Note]{dictionary}
    Signals an error if the @arg{instance} argument is not a valid
    @symbol{g:type-instance} instance.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-from-instance (make-instance 'gtk-button))
=> #<GTYPE :name \"GtkButton\" :id 134914152>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-instance}
  @see-function{g:type-from-class}"
  (let ((ptr (if (cffi:pointerp instance) instance (object-pointer instance))))
    (type-from-class (cffi:foreign-slot-value ptr
                                              '(:struct type-instance)
                                              :class))))

(export 'type-from-instance)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FROM_CLASS()
;;; ----------------------------------------------------------------------------

(defun type-from-class (class)
 #+liber-documentation
 "@version{2023-11-14}
  @argument[class]{a valid @symbol{g:type-class} instance}
  @return{The @class{g:type-t} type ID of @arg{class}.}
  @short{Get the type identifier from a given class instance.}
  This function should only be used in type implementations.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-from-class (g:type-class-ref \"GObject\"))
=> #<GTYPE :name \"GObject\" :id 80>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-class}
  @see-function{g:type-from-instance}
  @see-function{g:type-from-interface}"
  (cffi:foreign-slot-value class '(:struct type-class) :type))

(export 'type-from-class)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FROM_INTERFACE()
;;; ----------------------------------------------------------------------------

(defun type-from-interface (iface)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[iface]{a valid @symbol{g:type-interface} instance}
  @return{The @class{g:type-t} type ID of @arg{iface}.}
  @short{Get the type identifier from a given interface instance.}
  This function should only be used in type implementations.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-from-interface (g:type-default-interface-ref \"GtkOrientable\"))
=> #<GTYPE :name \"GtkOrientable\" :id 134920864>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-interface}
  @see-function{g:type-from-instance}
  @see-function{g:type-from-class}"
  (cffi:foreign-slot-value iface '(:struct type-interface) :type))

(export 'type-from-interface)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INSTANCE_GET_CLASS() -> type-instance-class
;;; ----------------------------------------------------------------------------

(defun type-instance-class (instance)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[instance]{a @symbol{g:type-instance} instance}
  @return{The @symbol{g:type-class} instance of @arg{instance}.}
  @begin{short}
    Get the class structure of a given instance structure.
  @end{short}
  This function should only be used in type implementations.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-instance-class (make-instance 'gtk-button))
=> #.(SB-SYS:INT-SAP #X0813E608)
(g:type-from-class *)
=> #<GTYPE :name \"GtkButton\" :id 134914160>
    @end{pre}
  @end{dictionary}
  @see-symbol{g:type-instance}
  @see-symbol{g:type-class}"
  (let ((ptr (if (cffi:pointerp instance) instance (object-pointer instance))))
    (cffi:foreign-slot-value ptr '(:struct type-instance) :class)))

(export 'type-instance-class)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INSTANCE_GET_INTERFACE()
;;;
;;; #define G_TYPE_INSTANCE_GET_INTERFACE(instance, g_type, c_type)
;;;         (_G_TYPE_IGI ((instance), (g_type), c_type))
;;;
;;; Get the interface structure for interface g_type of a given instance.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance :
;;;     Location of the GTypeInstance structure.
;;;
;;; g_type :
;;;     The GType of the interface to be returned.
;;;
;;; c_type :
;;;     The C type of the interface structure.
;;;
;;; Returns :
;;;     a pointer to the interface structure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INSTANCE_GET_PRIVATE()
;;;
;;; #define G_TYPE_INSTANCE_GET_PRIVATE(instance, g_type, c_type)
;;;         ((c_type*) g_type_instance_get_private ((GTypeInstance*) (instance),
;;;                                                 (g_type)))
;;;
;;; G_TYPE_INSTANCE_GET_PRIVATE has been deprecated since version 2.58 and
;;; should not be used in newly written code. Use G_ADD_PRIVATE and the
;;; generated your_type_get_instance_private() function instead
;;;
;;; Gets the private structure for a particular type. The private structure must
;;; have been registered in the class_init function with
;;; g_type_class_add_private().
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance :
;;;     the instance of a type deriving from private_type.
;;;
;;; g_type :
;;;     the type identifying which private data to retrieve.
;;;
;;; c_type :
;;;     The C type for the private structure.
;;;
;;; Returns :
;;;     a pointer to the private data structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CLASS_GET_PRIVATE()
;;;
;;; #define G_TYPE_CLASS_GET_PRIVATE(klass, g_type, c_type)
;;;       ((c_type*) g_type_class_get_private ((GTypeClass*) (klass), (g_type)))
;;;
;;; Gets the private class structure for a particular type. The private
;;; structure must have been registered in the get_type() function with
;;; g_type_add_class_private().
;;;
;;; This macro should only be used in type implementations.
;;;
;;; klass :
;;;     the class of a type deriving from private_type.
;;;
;;; g_type :
;;;     the type identifying which private data to retrieve.
;;;
;;; c_type :
;;;     The C type for the private structure.
;;;
;;; Returns :
;;;     a pointer to the private data structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE()
;;;
;;; #define G_TYPE_CHECK_INSTANCE(instance)
;;;         (_G_TYPE_CHI ((GTypeInstance*) (instance)))
;;;
;;; Checks if instance is a valid GTypeInstance structure, otherwise issues a
;;; warning and returns FALSE.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance :
;;;     Location of a GTypeInstance structure.
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE_CAST()
;;;
;;; #define G_TYPE_CHECK_INSTANCE_CAST(instance, g_type, c_type)
;;;         (_G_TYPE_CIC ((instance), (g_type), c_type))
;;;
;;; Checks that instance is an instance of the type identified by g_type and
;;; issues a warning if this is not the case. Returns instance casted to a
;;; pointer to c_type.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance :
;;;     Location of a GTypeInstance structure.
;;;
;;; g_type :
;;;     The type to be returned.
;;;
;;; c_type :
;;;     The corresponding C type of g_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE_TYPE()
;;; ----------------------------------------------------------------------------

(defun type-check-instance-type (instance gtype)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[instance]{a @symbol{g:type-instance} instance}
  @argument[gtype]{a @class{g:type-t} type ID to be checked}
  @return{@em{True} on success.}
  @begin{short}
    Checks if @arg{instance} is an instance of the type identified by
    @arg{gtype} or derived.
  @end{short}
  This function should only be used in type implementations.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-check-instance-type (make-instance 'gtk-button) \"GObject\") => T
(g:type-check-instance-type (make-instance 'gtk-button) \"GtkWindow\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-instance}
  @see-function{g:type-check-class-type}"
  (type-is-a (type-from-instance instance) gtype))

(export 'type-check-instance-type)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE_FUNDAMENTAL_TYPE()
;;;
;;; #define G_TYPE_CHECK_INSTANCE_FUNDAMENTAL_TYPE(instance, g_type)
;;;         (_G_TYPE_CIFT ((instance), (g_type)))
;;;
;;; Checks if instance is an instance of the fundamental type identified by
;;; g_type . If instance is NULL, FALSE will be returned.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance:
;;;     Location of a GTypeInstance structure.
;;;
;;; g_type:
;;;     The fundamental type to be checked
;;;
;;; Returns:
;;;     TRUE on success
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_CLASS_CAST()
;;;
;;; #define G_TYPE_CHECK_CLASS_CAST(g_class, g_type, c_type)
;;;         (_G_TYPE_CCC ((g_class), (g_type), c_type))
;;;
;;; Checks that g_class is a class structure of the type identified by g_type
;;; and issues a warning if this is not the case. Returns g_class casted to a
;;; pointer to c_type.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; g_class :
;;;     Location of a GTypeClass structure.
;;;
;;; g_type :
;;;     The type to be returned.
;;;
;;; c_type :
;;;     The corresponding C type of class structure of g_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_CLASS_TYPE()
;;; ----------------------------------------------------------------------------

(defun type-check-class-type (class gtype)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[class]{a @symbol{g:type-class} instance}
  @argument[gtype]{a @class{g:type-t} type ID to be checked}
  @return{@em{True} on success.}
  @begin{short}
    Checks if @arg{class} is a class structure of the type identified by
    @arg{gtype} or derived.
  @end{short}
  This function should only be used in type implementations.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-check-class-type (g:type-class-ref \"GtkButton\") \"GObject\") => T
(g:type-check-class-type (g:type-class-ref \"GtkButton\") \"GtkWindow\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-class}
  @see-function{g:type-check-instance-type}"
  (type-is-a (type-from-class class) gtype))

(export 'type-check-class-type)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_VALUE()
;;;
;;; #define G_TYPE_CHECK_VALUE(value) (_G_TYPE_CHV ((value)))
;;;
;;; Checks if value has been initialized to hold values of a value type.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; value :
;;;     a GValue
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_VALUE_TYPE()
;;;
;;; #define G_TYPE_CHECK_VALUE_TYPE
;;;         (value, g_type) (_G_TYPE_CVH ((value), (g_type)))
;;;
;;; Checks if value has been initialized to hold values of type g_type.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; value :
;;;     a GValue
;;;
;;; g_type :
;;;     The type to be checked.
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_init ()
;;;
;;; void g_type_init (void);
;;;
;;; Warning
;;;
;;; g_type_init has been deprecated since version 2.36 and should not be used in
;;; newly written code. The type system is now initialised automatically.
;;;
;;; This function used to initialise the type system. Since GLib 2.36, the type
;;; system is initialised automatically and this function does nothing.
;;; ----------------------------------------------------------------------------

;; Not implemented because the function is deprecated.

;;; ----------------------------------------------------------------------------
;;; g_type_init_with_debug_flags ()
;;;
;;; void g_type_init_with_debug_flags (GTypeDebugFlags debug_flags);
;;;
;;; Warning
;;;
;;; g_type_init_with_debug_flags has been deprecated since version 2.36 and
;;; should not be used in newly written code. the type system is now initialised
;;; automatically
;;;
;;; This function used to initialise the type system with debugging flags. Since
;;; GLib 2.36, the type system is initialised automatically and this function
;;; does nothing.
;;;
;;; If you need to enable debugging features, use the GOBJECT_DEBUG environment
;;; variable.
;;;
;;; debug_flags :
;;;     Bitwise combination of GTypeDebugFlags values for debugging purposes.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; g_type_name ()
;;; ----------------------------------------------------------------------------

;; TODO: The type g:+type-invalid+ is special handled in gtype-id. gtype-id
;; returns nil and not an integer. Therefore this function does not accept
;; g:+type-invalid+ as an argument like the C function.

(declaim (inline type-name))

(defun type-name (gtype)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a @class{g:type-t} type ID to return the type name for}
  @return{The string with the type name.}
  @begin{short}
    Get the unique name that is assigned to a type ID.
  @end{short}
  Note that this function, like all other GType API, cannot cope with invalid
  type IDs. Randomized type IDs should not be passed in and will most likely
  lead to a crash.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-name g:+type-double+) => \"gdouble\"
(g:type-name g:+type-enum+) => \"GEnum\"
(g:type-name (g:gtype \"GtkButton\")) => \"GtkButton\"
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-from-name}"
  (glib:gtype-name (glib:gtype gtype)))

(export 'type-name)

;;; ----------------------------------------------------------------------------
;;; g_type_qname ()                                        not exported
;;; ----------------------------------------------------------------------------

;; This function is not exported. In the Lisp binding there is no difference
;; to the function type-name.

(cffi:defcfun ("g_type_qname" type-qname) glib:quark-as-string
 #+liber-documentation
 "@version{#2013-4-1}
  @argument[gtype]{type to return quark of @arg{gtype} name for}
  @return{The @arg{gtype} names quark or 0.}
  Get the corresponding quark of the @arg{gtype} IDs name."
  (gtype type-t))

;;; ----------------------------------------------------------------------------
;;; g_type_from_name ()
;;; ----------------------------------------------------------------------------

;; TODO: Handle the ID 0 more consistent as an invalid GType.

(cffi:defcfun ("g_type_from_name" %type-from-name) :size
  (name :string))

(defun type-from-name (name)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[name]{a string with the type name to lookup}
  @return{Corresponding @class{g:type-t} type ID for @arg{name}.}
  @begin{short}
    Lookup the type ID from a given type name.
  @end{short}
  This is the preferred method to find out by name whether a specific type has
  been registered yet.
  @begin[Example]{dictionary}
    @begin{pre}
(g:type-from-name \"gdouble\") => #<GTYPE :name \"gdouble\" :id 60>
(g:type-from-name \"GtkContainer\")
=> #<GTYPE :name \"GtkContainer\" :id 94893595931264>
(g:type-from-name \"GtkCheckButton\")
=> #<GTYPE :name \"GtkCheckButton\" :id 94893595969952>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-name}"
  (glib:gtype name))

(export 'type-from-name)

;;; ----------------------------------------------------------------------------
;;; g_type_parent ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_parent" type-parent) type-t
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a derived @class{g:type-t} type ID}
  @return{The parent type of @arg{gtype}.}
  @begin{short}
    Returns the direct parent type of the passed in GType.
  @end{short}
  If the passed in GType has no parent, i.e. is a fundamental type, @code{nil}
  is returned.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-parent \"GtkDialog\") => #<GTYPE :name \"GtkWindow\" :id 94209734228256>
(g:type-parent \"GtkWindow\") => #<GTYPE :name \"GtkWidget\" :id 94209734120944>
(g:type-parent \"GApplication\") => #<GTYPE :name \"GObject\" :id 80>
(g:type-parent \"GtkActionable\") => #<GTYPE :name \"GInterface\" :id 8>
(g:type-parent \"GObject\") => NIL
(g:type-parent \"gdouble\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-children}"
  (gtype type-t))

(export 'type-parent)

;;; ----------------------------------------------------------------------------
;;; g_type_depth ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_depth" type-depth) :uint
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{The unsigned integer with the depth of @arg{gtype}.}
  @begin{short}
    Returns the length of the ancestry of the passed in GType.
  @end{short}
  This includes the GType itself, so that e.g. a fundamental type has depth 1.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-depth \"gdouble\") => 1
(g:type-depth \"GtkButton\") => 6
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (gtype type-t))

(export 'type-depth)

;;; ----------------------------------------------------------------------------
;;; g_type_next_base ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_next_base" type-next-base) type-t
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[leaf]{descendant of @arg{root} and the type to be returned}
  @argument[root]{immediate parent of the returned type}
  @begin{return}
    Immediate @class{g:type-t} child type ID of @arg{root} and anchestor of
    @arg{leaf}.
  @end{return}
  @begin{short}
    Given a @arg{leaf} and a @arg{root} which is contained in its anchestry,
    return the type that @arg{root} is the immediate parent of.
  @end{short}
  In other words, this function determines the type that is derived directly
  from @arg{root} which is also a base class of @arg{leaf}. Given a root type
  and a leaf type, this function can be used to determine the types and order
  in which the leaf type is descended from the root type.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-next-base \"GtkButton\" \"GtkContainer\")
=> #<GTYPE :name \"GtkBin\" :id 94872974765408>
(g:type-next-base \"GtkButton\" \"GtkWidget\")
=> #<GTYPE :name \"GtkContainer\" :id 94872974765040>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (leaf type-t)
  (root type-t))

(export 'type-next-base)

;;; ----------------------------------------------------------------------------
;;; g_type_is_a ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_is_a" %type-is-a) :boolean
  (gtype type-t)
  (is-a-type type-t))

(defun type-is-a (gtype is-a-type)
 #+liber-documentation
 "@version{2022-12-29}
  @argument[gtype]{a @class{g:type-t} type ID to check anchestry for}
  @argument[is-a-type]{possible anchestor of @arg{gtype} or the interface
    that @arg{gtype} could conform to}
  @return{@em{True} if the @arg{gtype} argument is a @arg{is-a-type}.}
  @begin{short}
    If the @arg{is-a-type} argument is a derivable type, check whether the
    @arg{gtype} argument is a descendant of @arg{is-a-type}.
  @end{short}
  If @arg{is-a-type} is an interface, check whether @arg{gtype} conforms to it.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-is-a \"gboolean\" g:+type-boolean+) => T
(g:type-is-a \"GtkTextIter\" g:+type-boxed+) => T
(g:type-is-a \"GtkWindowType\" g:+type-enum+) => T
(g:type-is-a \"GtkApplicationInhibitFlags\" g:+type-flags+) => T
(g:type-is-a \"GtkActionable\" g:+type-interface+) => T
(g:type-is-a \"unknown\" g:+type-invalid+) => T
(g:type-is-a \"GtkApplication\" g:+type-object+) => T
(g:type-is-a \"GParamBoolean\" g:+type-param+) => T
(g:type-is-a \"GVariant\" g:+type-variant+) => T
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (let ((*warn-unknown-gtype* nil)) ; no warnings for the test function
    (%type-is-a gtype is-a-type)))

(export 'type-is-a)

;;; ----------------------------------------------------------------------------
;;; g_type_class_ref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_ref" %type-class-ref)
    (:pointer (:struct type-class))
  (gtype type-t))

(defun type-class-ref (gtype)
 #+liber-documentation
 "@version{2023-11-12}
  @argument[gtype]{a @class{g:type-t} type ID of a classed type}
  @return{The pointer to the @symbol{g:type-class} instance for the given
    @arg{gtype}.}
  @begin{short}
    Increments the reference count of the class instance belonging to
    @arg{gtype} and returns the pointer to the class instance.
  @end{short}
  This function will create the class instance if it does not exist already.
  Returns @code{nil} when @arg{gtype} is not a valid type ID for classed type.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-class-ref \"GApplication\") => #.(SB-SYS:INT-SAP #X55FA5306FE40)
(g:type-from-class *) => #<GTYPE :name \"GApplication\" :id 94533623145984>
(g:type-class-ref \"GAction\") => NIL
(g:type-class-ref \"gdouble\") => NIL
(g:type-class-ref \"unknown\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-class}
  @see-function{g:type-class-peek}
  @see-function{g:type-class-unref}"
  (let ((*warn-unknown-gtype* nil))
    (let ((gtype (glib:gtype gtype)))
      (when (and gtype (type-is-classed gtype))
        (let ((class (%type-class-ref gtype)))
          (unless (cffi:null-pointer-p class)
            class))))))

(export 'type-class-ref)

;;; ----------------------------------------------------------------------------
;;; g_type_class_peek ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_peek" %type-class-peek)
    (:pointer (:struct type-class))
  (gtype type-t))

(defun type-class-peek (gtype)
 #+liber-documentation
 "@version{2023-11-12}
  @argument[gtype]{a @class{g:type-t} type ID of a classed type}
  @begin{return}
    The @symbol{g:type-class} instance for the given @arg{gtype} type ID or
    @code{nil} if the class does not currently exist.
  @end{return}
  @begin{short}
    This function is essentially the same as the @fun{g:type-class-ref}
    function, except that the classes reference count is not incremented.
  @end{short}
  As a consequence, this function may return @code{nil} if the class of the
  type passed in does not currently exist (has not been referenced before).
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-class-peek \"GSimpleAction\") => #.(SB-SYS:INT-SAP #X55FA5306FE40)
(g:type-from-class *) => #<GTYPE :name \"GSimpleAction\" :id 94533623145984>
(g:type-class-peek \"GAction\") => NIL
(g:type-class-peek \"gdouble\") => NIL
(g:type-class-peek \"unknown\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-class}
  @see-function{g:type-class-ref}"
  (let ((*warn-unknown-gtype* nil))
    (let ((gtype (glib:gtype gtype)))
      (when (and gtype (type-is-classed gtype))
        (let ((class (%type-class-peek gtype)))
          (unless (cffi:null-pointer-p class)
            class))))))

(export 'type-class-peek)

;;; ----------------------------------------------------------------------------
;;; g_type_class_peek_static ()                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_peek_static" type-class-peek-static)
    (:pointer (:struct type-class))
 #+liber-documentation
 "@version{#2013-4-1}
  @argument[gtype]{a type ID of a classed type}
  @begin{return}
    The @symbol{type-class} instance for the given @arg{gtype} ID or
    @code{NULL} if the class does not currently exist or is dynamically loaded.
  @end{return}
  @begin{short}
    A more efficient version of @fun{type-class-peek} which works only for
    static types.
  @end{short}
  @see-function{type-class-peek}"
  (gtype type-t))

;;; ----------------------------------------------------------------------------
;;; g_type_class_unref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_unref" type-class-unref) :void
 #+liber-documentation
 "@version{2023-11-12}
  @argument[class]{a pointer to the @symbol{g:type-class} instance to
    unreference}
  @begin{short}
    Decrements the reference count of the class instance being passed in.
  @end{short}
  Once the last reference count of a class has been released, classes may be
  finalized by the type system.
  @see-symbol{g:type-class}
  @see-function{g:type-class-ref}"
  (class (:pointer (:struct type-class))))

(export 'type-class-unref)

;;; ----------------------------------------------------------------------------
;;; g_type_class_peek_parent ()                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_peek_parent" type-class-peek-parent)
    (:pointer (:struct type-class))
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[class]{a @symbol{g:type-class} instance to retrieve the parent
    class for}
  @return{The parent class of @arg{class}.}
  @begin{short}
    This is a convenience function often needed in class initializers. It
    returns the class intance of the immediate parent type of the @arg{class}
    passed in.
  @end{short}
  Since derived classes hold a reference count on their parent classes as long
  as they are instantiated, the returned class will always exist. This function
  is essentially equivalent to:
  @begin{pre}
(g:type-class-peek (g:type-parent class))
  @end{pre}
  @see-symbol{g:type-class}"
  (g-class (:pointer (:struct type-class))))

;;; ----------------------------------------------------------------------------
;;; g_type_class_add_private ()                            not exported
;;;
;;; g_type_class_add_private has been deprecated since version 2.58 and should
;;; not be used in newly written code. Use the G_ADD_PRIVATE() macro with the
;;; G_DEFINE_* family of macros to add instance private data to a type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_add_private" type-class-add-private) :void
 #+liber-documentation
 "@version{#2014-4-1}
  @argument[class]{class structure for an instantiatable type}
  @argument[private-size]{size of private structure}
  @begin{short}
    Registers a private structure for an instantiatable type.
  @end{short}

  When an object is allocated, the private structures for the type and all of
  its parent types are allocated sequentially in the same memory block as the
  public structures.

  Note that the accumulated size of the private structures of a type and all
  its parent types cannot excced 64 KiB.

  This function should be called in the type's @code{class_init()} function.
  The private structure can be retrieved using the
  @code{G_TYPE_INSTANCE_GET_PRIVATE()} macro.

  The following example shows attaching a private structure
  @code{MyObjectPrivate} to an object @code{MyObject} defined in the standard
  @code{GObject} fashion of type's @code{class_init()} function. Note the use of
  a structure member \"priv\" to avoid the overhead of repeatedly calling
  @code{MY_OBJECT_GET_PRIVATE()}.
  @begin{pre}
 typedef struct _MyObject        MyObject;
 typedef struct _MyObjectPrivate MyObjectPrivate;

 struct _MyObject {
  GObject parent;

  MyObjectPrivate *priv;
 @};

 struct _MyObjectPrivate {
   int some_field;
 @};

 static void
 my_object_class_init (MyObjectClass *klass)
 {
   g_type_class_add_private (klass, sizeof (MyObjectPrivate));
 @}

 static void
 my_object_init (MyObject *my_object)
 {
   my_object->priv = G_TYPE_INSTANCE_GET_PRIVATE (my_object,
                                                  MY_TYPE_OBJECT,
                                                  MyObjectPrivate);
 @}

 static int
 my_object_get_some_field (MyObject *my_object)
 {
   MyObjectPrivate *priv;

   g_return_val_if_fail (MY_IS_OBJECT (my_object), 0);

   priv = my_object->priv;

   return priv->some_field;
 @}
  @end{pre}"
  (class (:pointer (:struct type-class)))
  (private-size :size))

;;; ----------------------------------------------------------------------------
;;; g_type_add_class_private ()
;;;
;;; void g_type_add_class_private (GType class_type, gsize private_size);
;;;
;;; Registers a private class structure for a classed type; when the class is
;;; allocated, the private structures for the class and all of its parent types
;;; are allocated sequentially in the same memory block as the public
;;; structures. This function should be called in the type's get_type() function
;;; after the type is registered. The private structure can be retrieved using
;;; the G_TYPE_CLASS_GET_PRIVATE() macro.
;;;
;;; class_type :
;;;     GType of an classed type.
;;;
;;; private_size :
;;;     size of private structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_peek ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_interface_peek" %type-interface-peek)
    (:pointer (:struct type-interface))
  (instance-class (:pointer (:struct type-class)))
  (iface-type type-t))

(defun type-interface-peek (instance-class iface-type)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[instance-class]{a @symbol{g:type-class} instance}
  @argument[iface-type]{a @class{g:type-t} interface type ID which this
    @arg{instance-class} conforms to}
  @begin{return}
    The @symbol{g:type-interface} instance of @arg{iface-type} if
    implemented by @arg{instance-class}, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Returns the interface structure of an interface to which the passed in
    @arg{instance-class} conforms.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-interface-peek (g:type-class-ref \"GtkBox\") \"GtkOrientable\")
=> #.(SB-SYS:INT-SAP #X080C6858)
(g:type-from-interface *)
=> #S(GTYPE :NAME \"GtkOrientable\" :%ID 134887472)
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-class}
  @see-symbol{g:type-interface}"
  (let ((iface (%type-interface-peek instance-class iface-type)))
    (when (not (cffi:null-pointer-p iface))
      iface)))

(export 'type-interface-peek)

;;; ----------------------------------------------------------------------------
;;; g_type_interface_peek_parent ()
;;;
;;; gpointer g_type_interface_peek_parent (gpointer g_iface);
;;;
;;; Returns the corresponding GTypeInterface structure of the parent type of the
;;; instance type to which g_iface belongs. This is useful when deriving the
;;; implementation of an interface from the parent type and then possibly
;;; overriding some methods.
;;;
;;; g_iface :
;;;     A GTypeInterface structure.
;;;
;;; Returns :
;;;     The corresponding GTypeInterface structure of the parent type of the
;;;     instance type to which g_iface belongs, or NULL if the parent type
;;;     does not conform to the interface.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_ref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_default_interface_ref" %type-default-interface-ref)
    (:pointer (:struct type-interface))
  (gtype type-t))

(defun type-default-interface-ref (gtype)
 #+liber-documentation
 "@version{2023-12-12}
  @argument[gtype]{a @class{g:type-t} interface type ID}
  @return{The pointer to the default @code{vtable} for the interface of type
    @arg{gtype}, or @code{nil} if @arg{gtype} is not an interface type.}
  @begin{short}
    Increments the reference count for the interface type, and returns the
    default interface @code{vtable} for the type.
  @end{short}
  Call the @fun{g:type-default-interface-unref} function when you are done
  using the interface.

  If the interface type is not currently in use, then the default @code{vtable}
  for the type will be created and initalized by calling the base interface init
  and default @code{vtable} init functions for the type. Calling this function
  is useful when you want to make sure that signals and properties for an
  interface have been installed.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-default-interface-ref \"GAction\") => #.(SB-SYS:INT-SAP #X55D446D53DE0)
(g:type-from-interface *) => #<GTYPE :name \"GAction\" :id 94370208235568>
(g:type-default-interface-ref \"GSimpleAction\") => NIL
(g:type-default-interface-ref \"gdouble\") => NIL
(g:type-default-interface-ref \"unknown\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-default-interface-unref}"
  (let ((*warn-unknown-gtype* nil))
    (let ((gtype (glib:gtype gtype)))
      (when (and gtype (type-is-interface gtype))
        (let ((iface (%type-default-interface-ref gtype)))
          (unless (cffi:null-pointer-p iface)
            iface))))))

(export 'type-default-interface-ref)

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_peek ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_default_interface_peek" %type-default-interface-peek)
    (:pointer (:struct type-interface))
  (gtype type-t))

(defun type-default-interface-peek (gtype)
#+liber-documentation
 "@version{2023-11-12}
  @argument[gtype]{a @class{g:type-t} interface type ID}
  @begin{return}
    The default @code{vtable} for the interface of type @arg{gtype}, @code{nil}
    if the type is not currently in use or is not an interface type.
  @end{return}
  @begin{short}
    If the interface type @arg{gtype} is currently in use, returns its default
    interface @code{vtable}.
  @end{short}
  @see-class{g:type-t}
  @see-function{g:type-default-interface-ref}"
  (let ((*warn-unknown-gtype* nil))
    (let ((gtype (glib:gtype gtype)))
      (when (and gtype (type-is-interface gtype))
        (let ((iface (%type-default-interface-peek gtype)))
          (unless (cffi:null-pointer-p iface)
            iface))))))

(export 'type-default-interface-peek)

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_unref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_default_interface_unref" type-default-interface-unref)
    :void
 #+liber-documentation
 "@version{2023-11-12}
  @argument[iface]{a pointer to the default @code{vtable} instance for an
    interface, as returned by the @fun{g:type-default-interface-ref} function}
  @begin{short}
    Decrements the reference count for the type corresponding to the interface
    default @code{vtable} of @arg{iface}.
  @end{short}
  If the type is dynamic, then when no one is using the interface and all
  references have been released, the finalize function for the default
  @arg{vtable} of the interface will be called.
  @see-class{g:type-t}
  @see-function{g:type-default-interface-ref}"
  (iface (:pointer (:struct type-interface))))

(export 'type-default-interface-unref)

;;; ----------------------------------------------------------------------------
;;; g_type_children ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_children" %type-children) (:pointer :size)
  (gtype type-t)
  (n-children (:pointer :uint)))

(defun type-children (gtype)
 #+liber-documentation
 "@version{2023-7-29}
  @argument[gtype]{a @class{g:type-t} parent type ID}
  @return{The list of @class{g:type-t} child types.}
  @short{Returns a list of type IDs, listing the child types of @arg{gtype}.}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-children \"GtkButton\")
=> (#<GTYPE :name \"GtkToggleButton\" :id 94069209378496>
    #<GTYPE :name \"GtkLinkButton\" :id 94069209378816>
    #<GTYPE :name \"GtkLockButton\" :id 94069209383872>)
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-parent}"
  (cffi:with-foreign-object (n-children :uint)
    (let ((ptr (%type-children gtype n-children)))
      (prog1
        (iter (for count from 0 below (cffi:mem-ref n-children :uint))
              (collect (cffi:mem-aref ptr 'type-t count)))
        (glib:free ptr)))))

(export 'type-children)

;;; ----------------------------------------------------------------------------
;;; g_type_interfaces ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_interfaces" %type-interfaces) (:pointer :size)
  (gtype type-t)
  (n-interfaces (:pointer :uint)))

(defun type-interfaces (gtype)
 #+liber-documentation
 "@version{2023-7-29}
  @argument[gtype]{a @class{g:type-t} type ID to list interface types for}
  @return{The list of @class{g:type-t} interface type IDs.}
  @begin{short}
    Returns a list of type IDs, listing the interface types that @arg{gtype}
    conforms to.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-interfaces \"GtkButton\")
=> (#<GTYPE :name \"GtkAccessible\" :id 94209734120256>
    #<GTYPE :name \"GtkBuildable\" :id 94209734088896>
    #<GTYPE :name \"GtkConstraintTarget\" :id 94209734121424>
    #<GTYPE :name \"GtkActionable\" :id 94209734120688>)
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (cffi:with-foreign-object (n-interfaces :uint)
    (let ((ptr (%type-interfaces gtype n-interfaces)))
      (prog1
        (iter (for count from 0 below (cffi:mem-ref n-interfaces :uint))
              (collect (cffi:mem-aref ptr 'type-t count)))
        (glib:free ptr)))))

(export 'type-interfaces)

;;; ----------------------------------------------------------------------------
;;; g_type_interface_prerequisites ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_interface_prerequisites" %type-interface-prerequisites)
    (:pointer :size)
  (itype type-t)
  (n-prerequisites (:pointer :uint)))

(defun type-interface-prerequisites (itype)
 #+liber-documentation
 "@version{2023-7-29}
  @argument[itype]{a @class{g:type-t} interface type ID}
  @begin{return}
    A list of @class{g:type-t} type IDs containing the prerequisites of
    @arg{itype}.
  @end{return}
  @begin{short}
    Returns the prerequisites of an interfaces type.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-interface-prerequisites \"GtkOrientable\")
=> (#<GTYPE :name \"GObject\" :id 80>)
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (cffi:with-foreign-object (n-prerequisites :uint)
    (let ((ptr (%type-interface-prerequisites itype n-prerequisites)))
      (prog1
        (iter (for count from 0 below (cffi:mem-ref n-prerequisites :uint))
              (collect (cffi:mem-aref ptr 'type-t count)))
        (glib:free ptr)))))

(export 'type-interface-prerequisites)

;;; ----------------------------------------------------------------------------
;;; g_type_get_qdata ()
;;; g_type_set_qdata () -> type-qdata
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_set_qdata" %type-set-qdata) :void
  (gtype type-t)
  (quark glib:quark-as-string)
  (data :pointer))

(defun (setf type-qdata) (data gtype quark)
  (let ((ptr (%type-get-qdata gtype quark)))
    (cond ((null data)
           ;; Remove data and free the stable-poiner
           (%type-set-qdata gtype quark (cffi:null-pointer))
           (when (not (cffi:null-pointer-p ptr))
             (glib:free-stable-pointer ptr)))
          ((cffi:null-pointer-p ptr)
           (setf ptr (glib:allocate-stable-pointer data))
           (%type-set-qdata gtype quark ptr))
          (t
           (setf (glib:get-stable-pointer-value ptr) data)
           (%type-set-qdata gtype quark ptr)))
    data))

(cffi:defcfun ("g_type_get_qdata" %type-get-qdata) :pointer
  (gtype type-t)
  (quark glib:quark-as-string))

(defun type-qdata (gtype quark)
 #+liber-documentation
 "@version{2022-12-29}
  @syntax[]{(g:type-qdata type quark) => data}
  @syntax[]{(setf (g:type-qdata type quark) data)}
  @argument[gtype]{a @class{g:type-t} type ID}
  @argument[quark]{a @type{g:quark-as-string} ID to identify the data}
  @argument[data]{the data}
  @begin{short}
    The @fun{g:type-qdata} function obtains data which has previously been
    attached to @arg{gtype} with the @setf{g:type-qdata} function.
  @end{short}

  Note that this does not take subtyping into account. Data attached to one
  type cannot be retrieved from a subtype.
  @begin[Examples]{dictionary}
    @begin{pre}
(setf (g:type-qdata \"gboolean\" \"mydata\") \"a string\") => \"a string\"
(g:type-qdata \"gboolean\" \"mydata\") => \"a string\"
(setf (g:type-qdata \"gboolean\" \"mydata\") '(a b c)) => (A B C)
(g:type-qdata \"gboolean\" \"mydata\") => (A B C)
(setf (g:type-qdata \"gboolean\" \"mydata\") nil) => NIL
(g:type-qdata \"gboolean\" \"mydata\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-type{g:quark-as-string}"
  (let ((ptr (%type-get-qdata gtype quark)))
    (when (not (cffi:null-pointer-p ptr))
      (glib:get-stable-pointer-value ptr))))

(export 'type-qdata)

;;; ----------------------------------------------------------------------------
;;; g_type_query ()                                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_query" type-query) :void
 #+liber-documentation
 "@version{#2020-10-31}
  @argument[gtype]{a @class{g:type-t} type ID of a static, classed type}
  @argument[query]{a user provided @symbol{g:type-query} instance that is
    filled in with constant values upon success}
  @begin{short}
    Queries the type system for information about a specific type.
  @end{short}
  This function will fill in a user provided structure to hold type specific
  information. If an invalid @class{g:type-t} type ID is passed in, the type
  member of the @symbol{g:type-query} is 0. All members filled into the
  @symbol{g:type-query} structure should be considered constant and have to be
  left untouched.
  @see-symbol{g:type-query}"
  (gtype type-t)
  (query (:pointer (:struct type-query))))

;;; ----------------------------------------------------------------------------
;;; GBaseInitFunc ()
;;;
;;; void (*GBaseInitFunc) (gpointer g_class);
;;;
;;; A callback function used by the type system to do base initialization of the
;;; class structures of derived types. It is called as part of the
;;; initialization process of all derived classes and should reallocate or reset
;;; all dynamic class members copied over from the parent class. For example,
;;; class members (such as strings) that are not sufficiently handled by a plain
;;; memory copy of the parent class into the derived class have to be altered.
;;; See GClassInitFunc() for a discussion of the class intialization process.
;;;
;;; g_class :
;;;     The GTypeClass structure to initialize.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GBaseFinalizeFunc ()
;;;
;;; void (*GBaseFinalizeFunc) (gpointer g_class);
;;;
;;; A callback function used by the type system to finalize those portions of a
;;; derived types class structure that were setup from the corresponding
;;; GBaseInitFunc() function. Class finalization basically works the inverse way
;;; in which class intialization is performed. See GClassInitFunc() for a
;;; discussion of the class intialization process.
;;;
;;; g_class :
;;;     The GTypeClass structure to finalize.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClassInitFunc ()
;;;
;;; void (*GClassInitFunc) (gpointer g_class, gpointer class_data);
;;;
;;; A callback function used by the type system to initialize the class of a
;;; specific type. This function should initialize all static class members.
;;; The initialization process of a class involves:
;;;
;;;     1 - Copying common members from the parent class over to the derived
;;;         class structure.
;;;
;;;     2 - Zero initialization of the remaining members not copied over from
;;;         the parent class.
;;;
;;;     3 - Invocation of the GBaseInitFunc() initializers of all parent types
;;;         and the class' type.
;;;
;;;     4 - Invocation of the class' GClassInitFunc() initializer.
;;;
;;; Since derived classes are partially initialized through a memory copy of
;;; the parent class, the general rule is that GBaseInitFunc() and
;;; GBaseFinalizeFunc() should take care of necessary reinitialization and
;;; release of those class members that were introduced by the type that
;;; specified these GBaseInitFunc()/GBaseFinalizeFunc(). GClassInitFunc()
;;; should only care about initializing static class members, while dynamic
;;; class members (such as allocated strings or reference counted resources)
;;; are better handled by a GBaseInitFunc() for this type, so proper
;;; initialization of the dynamic class members is performed for class
;;; initialization of derived types as well. An example may help to correspond
;;; the intend of the different class initializers:
;;;
;;;   typedef struct {
;;;     GObjectClass parent_class;
;;;     gint         static_integer;
;;;     gchar       *dynamic_string;
;;;   } TypeAClass;
;;;   static void
;;;   type_a_base_class_init (TypeAClass *class)
;;;   {
;;;     class->dynamic_string = g_strdup ("some string");
;;;   }
;;;   static void
;;;   type_a_base_class_finalize (TypeAClass *class)
;;;   {
;;;     g_free (class->dynamic_string);
;;;   }
;;;   static void
;;;   type_a_class_init (TypeAClass *class)
;;;   {
;;;     class->static_integer = 42;
;;;   }
;;;
;;;   typedef struct {
;;;     TypeAClass   parent_class;
;;;     gfloat       static_float;
;;;     GString     *dynamic_gstring;
;;;   } TypeBClass;
;;;   static void
;;;   type_b_base_class_init (TypeBClass *class)
;;;   {
;;;     class->dynamic_gstring = g_string_new ("some other string");
;;;   }
;;;   static void
;;;   type_b_base_class_finalize (TypeBClass *class)
;;;   {
;;;     g_string_free (class->dynamic_gstring);
;;;   }
;;;   static void
;;;   type_b_class_init (TypeBClass *class)
;;;   {
;;;     class->static_float = 3.14159265358979323846;
;;;   }
;;;
;;; Initialization of TypeBClass will first cause initialization of TypeAClass
;;; (derived classes reference their parent classes, see g_type_class_ref() on
;;; this). Initialization of TypeAClass roughly involves zero-initializing its
;;; fields, then calling its GBaseInitFunc() type_a_base_class_init() to
;;; allocate its dynamic members (dynamic_string), and finally calling its
;;; GClassInitFunc() type_a_class_init() to initialize its static members
;;; (static_integer). The first step in the initialization process of
;;; TypeBClass is then a plain memory copy of the contents of TypeAClass into
;;; TypeBClass and zero-initialization of the remaining fields in TypeBClass.
;;; The dynamic members of TypeAClass within TypeBClass now need
;;; reinitialization which is performed by calling type_a_base_class_init()
;;; with an argument of TypeBClass. After that, the GBaseInitFunc() of
;;; TypeBClass, type_b_base_class_init() is called to allocate the dynamic
;;; members of TypeBClass (dynamic_gstring), and finally the GClassInitFunc()
;;; of TypeBClass, type_b_class_init(), is called to complete the initialization
;;; process with the static members (static_float). Corresponding finalization
;;; counter parts to the GBaseInitFunc() functions have to be provided to
;;; release allocated resources at class finalization time.
;;;
;;; g_class :
;;;     The GTypeClass structure to initialize.
;;;
;;; class_data :
;;;     The class_data member supplied via the GTypeInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClassFinalizeFunc ()
;;;
;;; void (*GClassFinalizeFunc) (gpointer g_class, gpointer class_data);
;;;
;;; A callback function used by the type system to finalize a class. This
;;; function is rarely needed, as dynamically allocated class resources should
;;; be handled by GBaseInitFunc() and GBaseFinalizeFunc(). Also, specification
;;; of a GClassFinalizeFunc() in the GTypeInfo structure of a static type is
;;; invalid, because classes of static types will never be finalized (they are
;;; artificially kept alive when their reference count drops to zero).
;;;
;;; g_class :
;;;     The GTypeClass structure to finalize.
;;;
;;; class_data :
;;;     The class_data member supplied via the GTypeInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInstanceInitFunc ()
;;;
;;; void (*GInstanceInitFunc) (GTypeInstance *instance, gpointer g_class);
;;;
;;; A callback function used by the type system to initialize a new instance of
;;; a type. This function initializes all instance members and allocates any
;;; resources required by it. Initialization of a derived instance involves
;;; calling all its parent types instance initializers, so the class member of
;;; the instance is altered during its initialization to always point to the
;;; class that belongs to the type the current initializer was introduced for.
;;;
;;; instance :
;;;     The instance to initialize.
;;;
;;; g_class :
;;;     The class of the type the instance is created for.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInterfaceInitFunc ()
;;;
;;; void (*GInterfaceInitFunc) (gpointer g_iface, gpointer iface_data);
;;;
;;; A callback function used by the type system to initialize a new interface.
;;; This function should initialize all internal data and allocate any resources
;;; required by the interface.
;;;
;;; g_iface :
;;;     The interface structure to initialize.
;;;
;;; iface_data :
;;;     The interface_data supplied via the GInterfaceInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInterfaceFinalizeFunc ()
;;;
;;; void (*GInterfaceFinalizeFunc) (gpointer g_iface, gpointer iface_data);
;;;
;;; A callback function used by the type system to finalize an interface. This
;;; function should destroy any internal data and release any resources
;;; allocated by the corresponding GInterfaceInitFunc() function.
;;;
;;; g_iface :
;;;     The interface structure to finalize.
;;;
;;; iface_data :
;;;     The interface_data supplied via the GInterfaceInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeClassCacheFunc ()
;;;
;;; gboolean (*GTypeClassCacheFunc) (gpointer cache_data, GTypeClass *g_class);
;;;
;;; A callback function which is called when the reference count of a class
;;; drops to zero. It may use g_type_class_ref() to prevent the class from being
;;; freed. You should not call g_type_class_unref() from a GTypeClassCacheFunc
;;; function to prevent infinite recursion, use g_type_class_unref_uncached()
;;; instead.
;;;
;;; The functions have to check the class id passed in to figure whether they
;;; actually want to cache the class of this type, since all classes are routed
;;; through the same GTypeClassCacheFunc chain.
;;;
;;; cache_data :
;;;     data that was given to the g_type_add_class_cache_func() call
;;;
;;; g_class :
;;;     The GTypeClass structure which is unreferenced
;;;
;;; Returns :
;;;     TRUE to stop further GTypeClassCacheFuncs from being called, FALSE to
;;;     continue.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_register_static ()                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_register_static" type-register-static) type-t
 #+liber-documentation
 "@version{#2020-11-1}
  @argument[parent-type]{a @class{g:type-t} type ID from which this type will
    be derived}
  @argument[type-name]{a string used as the name of the new type}
  @argument[info]{a @symbol{g:type-info} instance for this type}
  @argument[flags]{bitwise combination of @symbol{g:type-flags} values}
  @return{The new @class{g:type-t} type ID identifier.}
  @begin{short}
    Registers @arg{g:type-name} as the name of a new static type derived from
    @arg{parent-type}.
  @end{short}
  The type system uses the information contained in the @symbol{g:type-info}
  structure pointed to by @arg{info} to manage the type and its instances (if
  not abstract). The value of @arg{flags} determines the nature (e.g. abstract
  or not) of the type.
  @see-class{g:type-t}
  @see-symbol{g:type-info}
  @see-symbol{g:type-flags}"
  (parent-type type-t)
  (type-name :string)
  (info (:pointer (:struct type-info)))
  (flags type-flags))

;;; ----------------------------------------------------------------------------
;;; g_type_register_static_simple ()                       not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_register_static_simple"
                type-register-static-simple) type-t
 #+liber-documentation
 "@version{#2020-11-1}
  @argument[parent-type]{a @class{g:type-t} type ID from which this type will
    be derived}
  @argument[type-name]{a string used as the name of the new type}
  @argument[class-size]{an unsigned integer with the size of the class
    structure}
  @argument[class-init]{a pointer with the location of the class initialization
    function}
  @argument[instance-size]{an unsigned integer with the size of the instance
    structure}
  @argument[instance-init]{a pointer with the location of the instance
    initialization function}
  @argument[flags]{bitwise combination of @symbol{g:type-flags} values}
  @return{The new @class{g:type-t} type ID identifier.}
  @begin{short}
    Registers @arg{g:type-name} as the name of a new static type derived from
    @arg{parent-type}.
  @end{short}
  The value of flags determines the nature (e.g. abstract or not) of the type.
  It works by filling a @symbol{g:type-info} instance and calling the function
  @fun{g:type-register-static}.
  @see-class{g:type-t}
  @see-symbol{g:type-flags}
  @see-function{g:type-register-static}"
  (parent-type type-t)
  (type-name :string)
  (class-size :uint)
  (class-init :pointer)
  (instance-size :uint)
  (instance-init :pointer)
  (flags type-flags))

;;; ----------------------------------------------------------------------------
;;; g_type_register_dynamic ()
;;;
;;; GType g_type_register_dynamic (GType parent_type,
;;;                                const gchar *type_name,
;;;                                GTypePlugin *plugin,
;;;                                GTypeFlags flags);
;;;
;;; Registers type_name as the name of a new dynamic type derived from
;;; parent_type. The type system uses the information contained in the
;;; GTypePlugin structure pointed to by plugin to manage the type and its
;;; instances (if not abstract). The value of flags determines the nature (e.g.
;;; abstract or not) of the type.
;;;
;;; parent_type :
;;;     Type from which this type will be derived.
;;;
;;; type_name :
;;;     0-terminated string used as the name of the new type.
;;;
;;; plugin :
;;;     The GTypePlugin structure to retrieve the GTypeInfo from.
;;;
;;; flags :
;;;     Bitwise combination of GTypeFlags values.
;;;
;;; Returns :
;;;     The new type identifier or G_TYPE_INVALID if registration failed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_register_fundamental ()
;;;
;;; GType g_type_register_fundamental (GType type_id,
;;;                                    const gchar *type_name,
;;;                                    const GTypeInfo *info,
;;;                                    const GTypeFundamentalInfo *finfo,
;;;                                    GTypeFlags flags);
;;;
;;; Registers type_id as the predefined identifier and type_name as the name of
;;; a fundamental type. If type_id is already registered, or a type named
;;; type_name is already registered, the behaviour is undefined. The type
;;; system uses the information contained in the GTypeInfo structure pointed to
;;; by info and the GTypeFundamentalInfo structure pointed to by finfo to
;;; manage the type and its instances. The value of flags determines additional
;;; characteristics of the fundamental type.
;;;
;;; type_id :
;;;     A predefined type identifier.
;;;
;;; type_name :
;;;     0-terminated string used as the name of the new type.
;;;
;;; info :
;;;     The GTypeInfo structure for this type.
;;;
;;; finfo :
;;;     The GTypeFundamentalInfo structure for this type.
;;;
;;; flags :
;;;     Bitwise combination of GTypeFlags values.
;;;
;;; Returns :
;;;     The predefined type identifier.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_static ()                         not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_add_interface_static" type-add-interface-static) :void
 #+liber-documentation
 "@version{#2020-11-1}
  @argument[instance-type]{a @class{g:type-t} type ID of an instantiable type}
  @argument[interface-type]{a @class{g:type-t} type ID of an interface type}
  @argument[info]{a @symbol{interface-info} instance for this
    (@arg{instance-type}, @arg{interface-type}) combination}
  @begin{short}
    Adds the static @arg{interface-type} to @arg{instantiable-type}.
  @end{short}
  The information contained in the @symbol{interface-info} instance pointed
  to by @arg{info} is used to manage the relationship.
  @see-class{g:type-t}
  @see-symbol{g:interface-info}"
  (instance-type type-t)
  (interface-type type-t)
  (info (:pointer (:struct interface-info))))

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_dynamic ()
;;;
;;; void g_type_add_interface_dynamic (GType instance_type,
;;;                                    GType interface_type,
;;;                                    GTypePlugin *plugin);
;;;
;;; Adds the dynamic interface_type to instantiable_type. The information
;;; contained in the GTypePlugin structure pointed to by plugin is used to
;;; manage the relationship.
;;;
;;; instance_type :
;;;     the GType value of an instantiable type.
;;;
;;; interface_type :
;;;     the GType value of an interface type.
;;;
;;; plugin :
;;;     the GTypePlugin structure to retrieve the GInterfaceInfo from.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_add_prerequisite ()                   not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_interface_add_prerequisite"
                type-interface-add-prerequisite) :void
 #+liber-documentation
 "@version{#2013-6-11}
  @argument[interface-type]{a @class{g:type-t} type ID of an interface type}
  @argument[prerequisite-type]{a @class{g:type-t} type ID of an interface or
    instantiatable type}
  @begin{short}
    Adds @arg{prerequisite-type} to the list of prerequisites of
    @arg{interface-type}.
  @end{short}
  This means that any type implementing @arg{interface-type} must also implement
  @arg{prerequisite-type}. Prerequisites can be thought of as an alternative to
  interface derivation (which the GType API does not support). An interface can
  have at most one instantiatable prerequisite type."
  (interface-type type-t)
  (prerequisite-type type-t))

;;; ----------------------------------------------------------------------------
;;; g_type_get_plugin ()
;;;
;;; GTypePlugin * g_type_get_plugin (GType type);
;;;
;;; Returns the GTypePlugin structure for type or NULL if type does not have a
;;; GTypePlugin structure.
;;;
;;; type :
;;;     The GType to retrieve the plugin for.
;;;
;;; Returns :
;;;     The corresponding plugin if type is a dynamic type, NULL otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_get_plugin ()
;;;
;;; GTypePlugin * g_type_interface_get_plugin (GType instance_type,
;;;                                            GType interface_type);
;;;
;;; Returns the GTypePlugin structure for the dynamic interface interface_type
;;; which has been added to instance_type, or NULL if interface_type has not
;;; been added to instance_type or does not have a GTypePlugin structure. See
;;; g_type_add_interface_dynamic().
;;;
;;; instance_type :
;;;     the GType value of an instantiatable type.
;;;
;;; interface_type :
;;;     the GType value of an interface type.
;;;
;;; Returns :
;;;     the GTypePlugin for the dynamic interface interface_type of
;;;     instance_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_fundamental_next ()                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_fundamental_next" type-fundamental-next) :size
 #+liber-documentation
 "@version{#2013-6-17}
  @begin{return}
    The nextmost fundamental type ID to be registered, or 0 if the type
    system ran out of fundamental type IDs.
  @end{return}
  Returns the next free fundamental type ID which can be used to register a
  new fundamental type with @code{g_type_register_fundamental()}. The returned
  type ID represents the highest currently registered fundamental type
  identifier.")

;;; ----------------------------------------------------------------------------
;;; g_type_fundamental ()
;;; ----------------------------------------------------------------------------

;; See G_TYPE_FUNDAMENTAL

;;; ----------------------------------------------------------------------------
;;; g_type_create_instance ()
;;;
;;; GTypeInstance * g_type_create_instance (GType type);
;;;
;;; Creates and initializes an instance of type if type is valid and can be
;;; instantiated. The type system only performs basic allocation and structure
;;; setups for instances: actual instance creation should happen through
;;; functions supplied by the type's fundamental type implementation. So use of
;;; g_type_create_instance() is reserved for implementators of fundamental types
;;; only. E.g. instances of the GObject hierarchy should be created via
;;; g_object_new() and never directly through g_type_create_instance() which
;;; does not handle things like singleton objects or object construction. Note:
;;; Do not use this function, unless you're implementing a fundamental type.
;;; Also language bindings should not use this function but g_object_new()
;;; instead.
;;;
;;; type :
;;;     An instantiatable type to create an instance for.
;;;
;;; Returns :
;;;     An allocated and initialized instance, subject to further treatment by
;;;     the fundamental type implementation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_free_instance ()
;;;
;;; void g_type_free_instance (GTypeInstance *instance);
;;;
;;; Frees an instance of a type, returning it to the instance pool for the type,
;;; if there is one.
;;;
;;; Like g_type_create_instance(), this function is reserved for implementors of
;;; fundamental types.
;;;
;;; instance :
;;;     an instance of a type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_class_cache_func ()
;;;
;;; void g_type_add_class_cache_func (gpointer cache_data,
;;;                                   GTypeClassCacheFunc cache_func);
;;;
;;; Adds a GTypeClassCacheFunc to be called before the reference count of a
;;; class goes from one to zero. This can be used to prevent premature class
;;; destruction. All installed GTypeClassCacheFunc functions will be chained
;;; until one of them returns TRUE. The functions have to check the class id
;;; passed in to figure whether they actually want to cache the class of this
;;; type, since all classes are routed through the same GTypeClassCacheFunc
;;; chain.
;;;
;;; cache_data :
;;;     data to be passed to cache_func
;;;
;;; cache_func :
;;;     a GTypeClassCacheFunc
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_remove_class_cache_func ()
;;;
;;; void g_type_remove_class_cache_func (gpointer cache_data,
;;;                                      GTypeClassCacheFunc cache_func);
;;;
;;; Removes a previously installed GTypeClassCacheFunc. The cache maintained by
;;; cache_func has to be empty when calling g_type_remove_class_cache_func() to
;;; avoid leaks.
;;;
;;; cache_data :
;;;     data that was given when adding cache_func
;;;
;;; cache_func :
;;;     a GTypeClassCacheFunc
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_class_unref_uncached ()
;;;
;;; void g_type_class_unref_uncached (gpointer g_class);
;;;
;;; A variant of g_type_class_unref() for use in GTypeClassCacheFunc
;;; implementations. It unreferences a class without consulting the chain of
;;; GTypeClassCacheFuncs, avoiding the recursion which would occur otherwise.
;;;
;;; g_class :
;;;     The GTypeClass structure to unreference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_check ()
;;;
;;; void g_type_add_interface_check (gpointer check_data,
;;;                                  GTypeInterfaceCheckFunc check_func);
;;;
;;; Adds a function to be called after an interface vtable is initialized for
;;; any class (i.e. after the interface_init member of GInterfaceInfo has been
;;; called).
;;;
;;; This function is useful when you want to check an invariant that depends on
;;; the interfaces of a class. For instance, the implementation of GObject uses
;;; this facility to check that an object implements all of the properties that
;;; are defined on its interfaces.
;;;
;;; check_data :
;;;     data to pass to check_func
;;;
;;; check_func :
;;;     function to be called after each interface is initialized.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_remove_interface_check ()
;;;
;;; void g_type_remove_interface_check (gpointer check_data,
;;;                                     GTypeInterfaceCheckFunc check_func);
;;;
;;; Removes an interface check function added with g_type_add_interface_check().
;;;
;;; check_data :
;;;     callback data passed to g_type_add_interface_check()
;;;
;;; check_func :
;;;     callback function passed to g_type_add_interface_check()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeInterfaceCheckFunc ()
;;;
;;; void (*GTypeInterfaceCheckFunc) (gpointer check_data, gpointer g_iface);
;;;
;;; A callback called after an interface vtable is initialized. See
;;; g_type_add_interface_check().
;;;
;;; check_data :
;;;     data passed to g_type_add_interface_check().
;;;
;;; g_iface :
;;;     the interface that has been initialized
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_value_table_peek ()                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_value_table_peek" type-value-table-peek)
    (:pointer (:struct type-value-table))
 #+liber-documentation
 "@version{#2013-6-11}
  @argument[type]{a @class{g:type-t} type ID}
  @begin{return}
    Location of the @symbol{g:type-value-table} associated with @arg{type} or
    @code{null-pointer} if there is no @symbol{g:type-value-table} associated
    with @arg{type}.
  @end{return}
  @begin{short}
    Returns the location of the @symbol{g:type-value-table} associated with
    @arg{type}.
  @end{short}
  Note that this function should only be used from source code that implements
  or has internal knowledge of the implementation of @arg{type}.
  @see-function{g:type-has-value-table}"
  (gtype type-t))

;;; ----------------------------------------------------------------------------
;;; g_type_ensure ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_ensure" %type-ensure) :void
  (gtype type-t))

(defun type-ensure (gtype)
 #+liber-documentation
 "@version{2023-11-12}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a valid type ID, otherwise @em{false}.}
  @begin{short}
    Ensures that the indicated @arg{gtype} has been registered with the type
    system, and its initializer method has been run.
  @end{short}
  @see-class{g:type-t}"
  (let ((*warn-unknown-gtype* nil))
    (let ((gtype (glib:gtype gtype)))
      (when gtype
        (%type-ensure gtype)
        t))))

(export 'type-ensure)

;;; ----------------------------------------------------------------------------
;;; g_type_get_type_registration_serial ()
;;;
;;; guint g_type_get_type_registration_serial (void);
;;;
;;; Returns an opaque serial number that represents the state of the set of
;;; registered types. Any time a type is registred this serial changes, which
;;; means you can cache information based on type lookups (such as
;;; g_type_from_name) and know if the cache is still valid at a later time by
;;; comparing the current serial with the one at the type lookup.
;;;
;;; Returns :
;;;     An unsigned int, representing the state of type registrations.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_get_instance_count ()
;;;
;;; int
;;; g_type_get_instance_count (GType type);
;;;
;;; Returns the number of instances allocated of the particular type; this is
;;; only available if GLib is built with debugging support and the
;;; instance_count debug flag is set (by setting the GOBJECT_DEBUG variable to
;;; include instance-count).
;;;
;;; type:
;;;     a GType
;;;
;;; Returns:
;;;     the number of instances allocated of the given type; if instance counts
;;;     are not available, returns 0.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DECLARE_FINAL_TYPE()
;;;
;;; #define G_DECLARE_FINAL_TYPE
;;;         (ModuleObjName, module_obj_name, MODULE, OBJ_NAME, ParentName)
;;;
;;; A convenience macro for emitting the usual declarations in the header file
;;; for a type which is not (at the present time) intended to be subclassed.
;;;
;;; You might use it in a header as follows:
;;;
;;; #ifndef _myapp_window_h_
;;; #define _myapp_window_h_
;;;
;;; #include <gtk/gtk.h>
;;;
;;; #define MY_APP_TYPE_WINDOW my_app_window_get_type ()
;;; G_DECLARE_FINAL_TYPE (MyAppWindow, my_app_window, MY_APP, WINDOW, GtkWindow)
;;;
;;; MyAppWindow *    my_app_window_new    (void);
;;;
;;; ...
;;;
;;; #endif
;;;
;;; This results in the following things happening:
;;;
;;; the usual my_app_window_get_type() function is declared with a return type
;;; of GType
;;;
;;; the MyAppWindow types is defined as a typedef of struct _MyAppWindow. The
;;; struct itself is not defined and should be defined from the .c file before
;;; G_DEFINE_TYPE() is used.
;;;
;;; the MY_APP_WINDOW() cast is emitted as static inline function along with
;;; the MY_APP_IS_WINDOW() type checking function
;;;
;;; the MyAppWindowClass type is defined as a struct containing GtkWindowClass.
;;; This is done for the convenience of the person defining the type and should
;;; not be considered to be part of the ABI. In particular, without a firm
;;; declaration of the instance structure, it is not possible to subclass the
;;; type and therefore the fact that the size of the class structure is exposed
;;; is not a concern and it can be freely changed at any point in the future.
;;;
;;; g_autoptr() support being added for your type, based on the type of your
;;; parent class
;;;
;;; You can only use this function if your parent type also supports
;;; g_autoptr().
;;;
;;; Because the type macro (MY_APP_TYPE_WINDOW in the above example) is not a
;;; callable, you must continue to manually define this as a macro for yourself.
;;;
;;; The declaration of the _get_type() function is the first thing emitted by
;;; the macro. This allows this macro to be used in the usual way with export
;;; control and API versioning macros.
;;;
;;; If you want to declare your own class structure, use
;;; G_DECLARE_DERIVABLE_TYPE().
;;;
;;; If you are writing a library, it is important to note that it is possible
;;; to convert a type from using G_DECLARE_FINAL_TYPE() to
;;; G_DECLARE_DERIVABLE_TYPE() without breaking API or ABI. As a precaution,
;;; you should therefore use G_DECLARE_FINAL_TYPE() until you are sure that it
;;; makes sense for your class to be subclassed. Once a class structure has
;;; been exposed it is not possible to change its size or remove or reorder
;;; items without breaking the API and/or ABI.
;;;
;;; ModuleObjName:
;;;     The name of the new type, in camel case (like GtkWidget)
;;;
;;; module_obj_name:
;;;     The name of the new type in lowercase, with words separated by '_'
;;;     (like 'gtk_widget')
;;;
;;; MODULE:
;;;     The name of the module, in all caps (like 'GTK')
;;;
;;; OBJ_NAME:
;;;     The bare name of the type, in all caps (like 'WIDGET')
;;;
;;; ParentName:
;;;     the name of the parent type, in camel case (like GtkWidget)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DECLARE_DERIVABLE_TYPE()
;;;
;;; #define G_DECLARE_DERIVABLE_TYPE
;;;         (ModuleObjName, module_obj_name, MODULE, OBJ_NAME, ParentName)
;;;
;;; A convenience macro for emitting the usual declarations in the header file
;;; for a type which is intended to be subclassed.
;;;
;;; You might use it in a header as follows:
;;;
;;; #ifndef _gtk_frobber_h_
;;; #define _gtk_frobber_h_
;;;
;;; #define GTK_TYPE_FROBBER gtk_frobber_get_type ()
;;; GDK_AVAILABLE_IN_3_12
;;; G_DECLARE_DERIVABLE_TYPE (GtkFrobber, gtk_frobber, GTK, FROBBER, GtkWidget)
;;;
;;; struct _GtkFrobberClass
;;; {
;;;   GtkWidgetClass parent_class;
;;;
;;;   void (* handle_frob)  (GtkFrobber *frobber,
;;;                          guint       n_frobs);
;;;
;;;   gpointer padding[12];
;;; };
;;;
;;; GtkWidget *    gtk_frobber_new   (void);
;;;
;;; ...
;;;
;;; #endif
;;;
;;; This results in the following things happening:
;;;
;;; the usual gtk_frobber_get_type() function is declared with a return type of
;;; GType
;;;
;;; the GtkFrobber struct is created with GtkWidget as the first and only item.
;;; You are expected to use a private structure from your .c file to store your
;;; instance variables.
;;;
;;; the GtkFrobberClass type is defined as a typedef to struct _GtkFrobberClass,
;;; which is left undefined. You should do this from the header file directly
;;; after you use the macro.
;;;
;;; the GTK_FROBBER() and GTK_FROBBER_CLASS() casts are emitted as static inline
;;; functions along with the GTK_IS_FROBBER() and GTK_IS_FROBBER_CLASS() type
;;; checking functions and GTK_FROBBER_GET_CLASS() function.
;;;
;;; g_autoptr() support being added for your type, based on the type of your
;;; parent class
;;;
;;; You can only use this function if your parent type also supports
;;; g_autoptr().
;;;
;;; Because the type macro (GTK_TYPE_FROBBER in the above example) is not a
;;; callable, you must continue to manually define this as a macro for yourself.
;;;
;;; The declaration of the _get_type() function is the first thing emitted by
;;; the macro. This allows this macro to be used in the usual way with export
;;; control and API versioning macros.
;;;
;;; If you are writing a library, it is important to note that it is possible to
;;; convert a type from using G_DECLARE_FINAL_TYPE() to
;;; G_DECLARE_DERIVABLE_TYPE() without breaking API or ABI. As a precaution, you
;;; should therefore use G_DECLARE_FINAL_TYPE() until you are sure that it makes
;;; sense for your class to be subclassed. Once a class structure has been
;;; exposed it is not possible to change its size or remove or reorder items
;;; without breaking the API and/or ABI. If you want to declare your own class
;;; structure, use G_DECLARE_DERIVABLE_TYPE(). If you want to declare a class
;;; without exposing the class or instance structures, use
;;; G_DECLARE_FINAL_TYPE().
;;;
;;; If you must use G_DECLARE_DERIVABLE_TYPE() you should be sure to include
;;; some padding at the bottom of your class structure to leave space for the
;;; addition of future virtual functions.
;;;
;;; ModuleObjName:
;;;     The name of the new type, in camel case (like GtkWidget)
;;;
;;; module_obj_name:
;;;     The name of the new type in lowercase, with words separated by '_'
;;;     (like 'gtk_widget')
;;;
;;; MODULE:
;;;     The name of the module, in all caps (like 'GTK')
;;;
;;; OBJ_NAME:
;;;     The bare name of the type, in all caps (like 'WIDGET')
;;;
;;; ParentName:
;;;     the name of the parent type, in camel case (like GtkWidget)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DECLARE_INTERFACE()
;;;
;;; #define G_DECLARE_INTERFACE
;;;         (ModuleObjName, module_obj_name, MODULE, OBJ_NAME, PrerequisiteName)
;;;
;;; A convenience macro for emitting the usual declarations in the header file
;;; for a GInterface type.
;;;
;;; You might use it in a header as follows:
;;;
;;; #ifndef _my_model_h_
;;; #define _my_model_h_
;;;
;;; #define MY_TYPE_MODEL my_model_get_type ()
;;; GDK_AVAILABLE_IN_3_12
;;; G_DECLARE_INTERFACE (MyModel, my_model, MY, MODEL, GObject)
;;;
;;; struct _MyModelInterface
;;; {
;;;   GTypeInterface g_iface;
;;;
;;;   gpointer (* get_item)  (MyModel *model);
;;; };
;;;
;;; gpointer my_model_get_item (MyModel *model);
;;;
;;; ...
;;;
;;; #endif
;;;
;;; This results in the following things happening:
;;;
;;; the usual my_model_get_type() function is declared with a return type of
;;; GType
;;;
;;; the MyModelInterface type is defined as a typedef to struct
;;; _MyModelInterface, which is left undefined. You should do this from the
;;; header file directly after you use the macro.
;;;
;;; the MY_MODEL() cast is emitted as static inline functions along with the
;;; MY_IS_MODEL() type checking function and MY_MODEL_GET_IFACE() function.
;;;
;;; g_autoptr() support being added for your type, based on your prerequisite
;;; type.
;;;
;;; You can only use this function if your prerequisite type also supports
;;; g_autoptr().
;;;
;;; Because the type macro (MY_TYPE_MODEL in the above example) is not a
;;; callable, you must continue to manually define this as a macro for yourself.
;;;
;;; The declaration of the _get_type() function is the first thing emitted by
;;; the macro. This allows this macro to be used in the usual way with export
;;; control and API versioning macros.
;;;
;;; ModuleObjName:
;;;     The name of the new type, in camel case (like GtkWidget)
;;;
;;; module_obj_name:
;;;     The name of the new type in lowercase, with words separated by '_'
;;;     (like 'gtk_widget')
;;;
;;; MODULE:
;;;     The name of the module, in all caps (like 'GTK')
;;;
;;; OBJ_NAME:
;;;     The bare name of the type, in all caps (like 'WIDGET')
;;;
;;; PrerequisiteName:
;;;     the name of the prerequisite type, in camel case (like GtkWidget)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE()
;;;
;;; #define G_DEFINE_TYPE(TN, t_n, T_P)
;;;         G_DEFINE_TYPE_EXTENDED (TN, t_n, T_P, 0, {})
;;;
;;; A convenience macro for type implementations, which declares a class
;;; initialization function, an instance initialization function (see GTypeInfo
;;; for information about these) and a static variable named t_n_parent_class
;;; pointing to the parent class. Furthermore, it defines a *_get_type()
;;; function. See G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE_WITH_PRIVATE()
;;;
;;; #define G_DEFINE_TYPE_WITH_PRIVATE(TN, t_n, T_P)
;;;         G_DEFINE_TYPE_EXTENDED (TN, t_n, T_P, 0, G_ADD_PRIVATE (TN))
;;;
;;; A convenience macro for type implementations, which declares a class
;;; initialization function, an instance initialization function (see GTypeInfo
;;; for information about these), a static variable named t_n_parent_class
;;; pointing to the parent class, and adds private instance data to the type.
;;; Furthermore, it defines a *_get_type() function. See
;;; G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; Note that private structs added with this macros must have a struct name of
;;; the form TN Private.
;;;
;;; The private instance data can be retrieved using the automatically generated
;;; getter function t_n_get_instance_private().
;;;
;;; See also: G_ADD_PRIVATE()
;;;
;;; TN:
;;;     The name of the new type, in Camel case.
;;;
;;; t_n:
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P:
;;;     The GType of the parent type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_TYPE_WITH_CODE(TN, t_n, T_P, _C_)
;;;        _G_DEFINE_TYPE_EXTENDED_BEGIN (TN, t_n, T_P, 0) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for type implementations. Similar to G_DEFINE_TYPE(),
;;; but allows you to insert custom code into the *_get_type() function, e.g.
;;; interface implementations via G_IMPLEMENT_INTERFACE(). See
;;; G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_ABSTRACT_TYPE()
;;;
;;; #define G_DEFINE_ABSTRACT_TYPE(TN, t_n, T_P)
;;;         G_DEFINE_TYPE_EXTENDED (TN, t_n, T_P, G_TYPE_FLAG_ABSTRACT, {})
;;;
;;; A convenience macro for type implementations. Similar to G_DEFINE_TYPE(),
;;; but defines an abstract type. See G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_ABSTRACT_TYPE_WITH_PRIVATE()
;;;
;;; #define G_DEFINE_ABSTRACT_TYPE_WITH_PRIVATE(TN, t_n, T_P)
;;;         G_DEFINE_TYPE_EXTENDED (TN, t_n, T_P, G_TYPE_FLAG_ABSTRACT,
;;;                                 G_ADD_PRIVATE (TN))
;;;
;;; Similar to G_DEFINE_TYPE_WITH_PRIVATE(), but defines an abstract type. See
;;; G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN:
;;;     The name of the new type, in Camel case.
;;;
;;; t_n:
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P:
;;;     The GType of the parent type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_ABSTRACT_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_ABSTRACT_TYPE_WITH_CODE(TN, t_n, T_P, _C_)
;;;        _G_DEFINE_TYPE_EXTENDED_BEGIN (TN, t_n, T_P, G_TYPE_FLAG_ABSTRACT) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for type implementations. Similar to
;;; G_DEFINE_TYPE_WITH_CODE(), but defines an abstract type and allows you to
;;; insert custom code into the *_get_type() function, e.g. interface
;;; implementations via G_IMPLEMENT_INTERFACE(). See G_DEFINE_TYPE_EXTENDED()
;;; for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the type_name_get_type() function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_ADD_PRIVATE()
;;;
;;; #define G_ADD_PRIVATE(TypeName)
;;;
;;; A convenience macro to ease adding private data to instances of a new type
;;; in the _C_ section of G_DEFINE_TYPE_WITH_CODE() or
;;; G_DEFINE_ABSTRACT_TYPE_WITH_CODE().
;;;
;;; For instance:
;;;
;;; typedef struct _MyObject MyObject;
;;; typedef struct _MyObjectClass MyObjectClass;
;;;
;;; typedef struct {
;;;   gint foo;
;;;   gint bar;
;;; } MyObjectPrivate;
;;;
;;; G_DEFINE_TYPE_WITH_CODE (MyObject, my_object, G_TYPE_OBJECT,
;;;                          G_ADD_PRIVATE (MyObject))
;;;
;;; Will add MyObjectPrivate as the private data to any instance of the MyObject
;;; type.
;;;
;;; G_DEFINE_TYPE_* macros will automatically create a private function based on
;;; the arguments to this macro, which can be used to safely retrieve the
;;; private data from an instance of the type; for instance:
;;;
;;; gint
;;; my_object_get_foo (MyObject *obj)
;;; {
;;;   MyObjectPrivate *priv = my_object_get_instance_private (obj);
;;;
;;;   g_return_val_if_fail (MY_IS_OBJECT (obj), 0);
;;;
;;;   return priv->foo;
;;; }
;;;
;;; void
;;; my_object_set_bar (MyObject *obj,
;;;                    gint      bar)
;;; {
;;;   MyObjectPrivate *priv = my_object_get_instance_private (obj);
;;;
;;;   g_return_if_fail (MY_IS_OBJECT (obj));
;;;
;;;   if (priv->bar != bar)
;;;     priv->bar = bar;
;;; }
;;;
;;; Note that this macro can only be used together with the G_DEFINE_TYPE_*
;;; macros, since it depends on variable names from those macros.
;;;
;;; Also note that private structs added with these macros must have a struct
;;; name of the form TypeNamePrivate.
;;;
;;; It is safe to call the _get_instance_private function on NULL or invalid
;;; objects since it's only adding an offset to the instance pointer. In that
;;; case the returned pointer must not be dereferenced.
;;;
;;; TypeName:
;;;     the name of the type in CamelCase
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PRIVATE_OFFSET()
;;;
;;; #define G_PRIVATE_OFFSET(TypeName, field)
;;;
;;; Evaluates to the offset of the field inside the instance private data
;;; structure for TypeName .
;;;
;;; Note that this macro can only be used together with the G_DEFINE_TYPE_* and
;;; G_ADD_PRIVATE() macros, since it depends on variable names from those
;;; macros.
;;;
;;; TypeName:
;;;     the name of the type in CamelCase
;;;
;;; field:
;;;     the name of the field in the private data structure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PRIVATE_FIELD()
;;;
;;; #define G_PRIVATE_FIELD(TypeName, inst, field_type, field_name)
;;;
;;; Evaluates to the field_name inside the inst private data structure for
;;; TypeName .
;;;
;;; Note that this macro can only be used together with the G_DEFINE_TYPE_* and
;;; G_ADD_PRIVATE() macros, since it depends on variable names from those
;;; macros.
;;;
;;; TypeName:
;;;     the name of the type in CamelCase
;;;
;;; inst:
;;;     the instance of TypeName you wish to access
;;;
;;; field_type:
;;;     the type of the field in the private data structure
;;;
;;; field_name:
;;;     the name of the field in the private data structure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PRIVATE_FIELD_P()
;;;
;;; #define G_PRIVATE_FIELD_P(TypeName, inst, field_name)
;;;
;;; Evaluates to a pointer to the field_name inside the inst private data
;;; structure for TypeName .
;;;
;;; Note that this macro can only be used together with the G_DEFINE_TYPE_* and
;;; G_ADD_PRIVATE() macros, since it depends on variable names from those
;;; macros.
;;;
;;; TypeName:
;;;     the name of the type in CamelCase
;;;
;;; inst:
;;;     the instance of TypeName you wish to access
;;;
;;; field_name:
;;;     the name of the field in the private data structure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_INTERFACE()
;;;
;;; #define G_DEFINE_INTERFACE(TN, t_n, T_P)
;;;         G_DEFINE_INTERFACE_WITH_CODE(TN, t_n, T_P, ;)
;;;
;;; A convenience macro for GTypeInterface definitions, which declares a default
;;; vtable initialization function and defines a *_get_type() function.
;;;
;;; The macro expects the interface initialization function to have the name
;;; t_n ## _default_init, and the interface structure to have the name
;;; TN ## Interface.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the prerequisite type for the interface, or 0
;;;     (G_TYPE_INVALID) for no prerequisite type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_INTERFACE_WITH_CODE()
;;;
;;; #define G_DEFINE_INTERFACE_WITH_CODE(TN, t_n, T_P, _C_)
;;;        _G_DEFINE_INTERFACE_EXTENDED_BEGIN(TN, t_n, T_P) {_C_;}
;;;        _G_DEFINE_INTERFACE_EXTENDED_END()
;;;
;;; A convenience macro for GTypeInterface definitions. Similar to
;;; G_DEFINE_INTERFACE(), but allows you to insert custom code into the
;;; *_get_type() function, e.g. additional interface implementations via
;;; G_IMPLEMENT_INTERFACE(), or additional prerequisite types. See
;;; G_DEFINE_TYPE_EXTENDED() for a similar example using
;;; G_DEFINE_TYPE_WITH_CODE().
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the prerequisite type for the interface, or 0
;;;     (G_TYPE_INVALID) for no prerequisite type.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IMPLEMENT_INTERFACE()
;;;
;;; #define G_IMPLEMENT_INTERFACE(TYPE_IFACE, iface_init)
;;;
;;; A convenience macro to ease interface addition in the _C_ section of
;;; G_DEFINE_TYPE_WITH_CODE() or G_DEFINE_ABSTRACT_TYPE_WITH_CODE(). See
;;; G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; Note that this macro can only be used together with the
;;; G_DEFINE_TYPE_* macros, since it depends on variable names from those
;;; macros.
;;;
;;; TYPE_IFACE :
;;;     The GType of the interface to add
;;;
;;; iface_init :
;;;     The interface init function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE_EXTENDED()
;;;
;;; #define G_DEFINE_TYPE_EXTENDED(TN, t_n, T_P, _f_, _C_)
;;;        _G_DEFINE_TYPE_EXTENDED_BEGIN (TN, t_n, T_P, _f_) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; The most general convenience macro for type implementations, on which
;;; G_DEFINE_TYPE(), etc are based.
;;;
;;;   G_DEFINE_TYPE_EXTENDED (GtkGadget,
;;;                           gtk_gadget,
;;;                           GTK_TYPE_WIDGET,
;;;                           0,
;;;                           G_IMPLEMENT_INTERFACE (TYPE_GIZMO,
;;;                                                  gtk_gadget_gizmo_init));
;;;
;;; expands to
;;;
;;;   static void     gtk_gadget_init       (GtkGadget      *self);
;;;   static void     gtk_gadget_class_init (GtkGadgetClass *klass);
;;;   static gpointer gtk_gadget_parent_class = NULL;
;;;   static void     gtk_gadget_class_intern_init (gpointer klass)
;;;   {
;;;     gtk_gadget_parent_class = g_type_class_peek_parent (klass);
;;;     gtk_gadget_class_init ((GtkGadgetClass*) klass);
;;;   }
;;;
;;;   GType
;;;   gtk_gadget_get_type (void)
;;;   {
;;;     static volatile gsize g_define_type_id__volatile = 0;
;;;     if (g_once_init_enter (&g_define_type_id__volatile))
;;;       {
;;;         GType g_define_type_id =
;;;           g_type_register_static_simple
;;;                              (GTK_TYPE_WIDGET,
;;;                               g_intern_static_string ("GtkGadget"),
;;;                               sizeof (GtkGadgetClass),
;;;                               (GClassInitFunc) gtk_gadget_class_intern_init,
;;;                               sizeof (GtkGadget),
;;;                               (GInstanceInitFunc) gtk_gadget_init,
;;;                               (GTypeFlags) flags);
;;;         {
;;;           const GInterfaceInfo g_implement_interface_info = {
;;;             (GInterfaceInitFunc) gtk_gadget_gizmo_init
;;;           };
;;;           g_type_add_interface_static (g_define_type_id,
;;;                                        TYPE_GIZMO,
;;;                                        &g_implement_interface_info);
;;;         }
;;;         g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
;;;       }
;;;     return g_define_type_id__volatile;
;;;   }
;;;
;;; The only pieces which have to be manually provided are the definitions of
;;; the instance and class structure and the definitions of the instance and
;;; class init functions.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; _f_ :
;;;     GTypeFlags to pass to g_type_register_static()
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_BOXED_TYPE()
;;;
;;; #define G_DEFINE_BOXED_TYPE(TypeName, type_name, copy_func, free_func)
;;;         G_DEFINE_BOXED_TYPE_WITH_CODE (TypeName,
;;;                                        type_name,
;;;                                        copy_func,
;;;                                        free_func, {})
;;;
;;; A convenience macro for boxed type implementations, which defines a
;;; type_name_get_type() function registering the boxed type.
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; copy_func :
;;;     the GBoxedCopyFunc for the new type
;;;
;;; free_func :
;;;     the GBoxedFreeFunc for the new type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_BOXED_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_BOXED_TYPE_WITH_CODE(TypeName, type_name, copy_func, free_func, _C_)
;;;        _G_DEFINE_BOXED_TYPE_BEGIN (TypeName, type_name, copy_func, free_func) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for boxed type implementations. Similar to
;;; G_DEFINE_BOXED_TYPE(), but allows to insert custom code into the
;;; type_name_get_type() function, e.g. to register value transformations with
;;; g_value_register_transform_func().
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; copy_func :
;;;     the GBoxedCopyFunc for the new type
;;;
;;; free_func :
;;;     the GBoxedFreeFunc for the new type
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_POINTER_TYPE()
;;;
;;; #define G_DEFINE_POINTER_TYPE(TypeName, type_name)
;;;         G_DEFINE_POINTER_TYPE_WITH_CODE (TypeName, type_name, {})
;;;
;;; A convenience macro for pointer type implementations, which defines a
;;; type_name_get_type() function registering the pointer type.
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_POINTER_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_POINTER_TYPE_WITH_CODE(TypeName, type_name, _C_)
;;;        _G_DEFINE_POINTER_TYPE_BEGIN (TypeName, type_name) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for pointer type implementations. Similar to
;;; G_DEFINE_POINTER_TYPE(), but allows to insert custom code into the
;;; type_name_get_type() function.
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.type-info.lisp -------------------------------------
