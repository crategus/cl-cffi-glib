;;; ----------------------------------------------------------------------------
;;; gobject.type-info.lisp
;;;
;;; The documentation in this file is taken from the GObject Reference Manual
;;; version 2.84 and modified to document the Lisp binding for the GObject
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
;;; Type Information
;;;
;;;     The GLib Runtime type identification and management system
;;;
;;; Types and Values
;;;
;;;     G_TYPE_RESERVED_GLIB_FIRST                          not exported
;;;     G_TYPE_RESERVED_GLIB_LAST                           not exported
;;;     G_TYPE_RESERVED_BSE_FIRST                           not exported
;;;     G_TYPE_RESERVED_BSE_LAST                            not exported
;;;     G_TYPE_RESERVED_USER_FIRST                          not exported
;;;
;;;     G_TYPE_FUNDAMENTAL_MAX                              not exported
;;;
;;;     GType
;;;     GTypeInterface
;;;     GTypeInstance
;;;     GTypeClass
;;;
;;;     GTypeInfo                                           not exported
;;;     GTypeFundamentalInfo                                not exported
;;;     GInterfaceInfo                                      not exported
;;;     GTypeValueTable                                     not exported
;;;     GTypeDebugFlags                                     Deprecated 2.36
;;;     GTypeQuery                                          not exported
;;;     GTypeFlags                                          not exported
;;;     GTypeFundamentalFlags                               not exported
;;;
;;; Functions
;;;
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
;;;     G_TYPE_INSTANCE_GET_INTERFACE                       not implemented
;;;     G_TYPE_INSTANCE_GET_PRIVATE                         Deprecated 2.58
;;;     G_TYPE_CLASS_GET_PRIVATE                            not implemented
;;;     G_TYPE_CHECK_INSTANCE                               not implemented
;;;     G_TYPE_CHECK_INSTANCE_CAST                          not implemented
;;;     G_TYPE_CHECK_INSTANCE_TYPE
;;;     G_TYPE_CHECK_INSTANCE_FUNDAMENTAL_TYPE
;;;     G_TYPE_CHECK_CLASS_CAST                            not implemented
;;;     G_TYPE_CHECK_CLASS_TYPE
;;;     G_TYPE_CHECK_VALUE                                 not implemented
;;;     G_TYPE_CHECK_VALUE_TYPE                            not implemented
;;;
;;;     g_type_init                                        Deprecated 2.36
;;;     g_type_init_with_debug_flags                       Deprecated 2.36
;;;     g_type_name
;;;     g_type_qname                                       not implemented
;;;     g_type_from_name                                   not implemented
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
;;;
;;;     g_type_get_type_registration_serial                not implemented
;;;     g_type_get_instance_count                          not implemented
;;; ----------------------------------------------------------------------------

(in-package :gobject)

(eval-when (:load-toplevel :compile-toplevel :execute)
(defconstant +type-fundamental-shift+ 2))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_GTYPE
;;; ----------------------------------------------------------------------------

;; TODO: Is this necessary!?

(glib-init:at-init ()
  (cffi:foreign-funcall "g_gtype_get_type" :size))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECKSUM
;;; ----------------------------------------------------------------------------

;; TODO: Is this necessary!?

(glib-init:at-init ()
  (cffi:foreign-funcall "g_checksum_get_type" :size))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_GLIB_FIRST                             not exported
;;; ----------------------------------------------------------------------------

#+nil
(defconstant +type-reserved-glib-first+ 22)

#+nil
(setf (liber:alias-for-variable '+type-reserved-glib-first+)
      "Constant"
      (documentation '+type-reserved-glib-first+ 'variable)
 "@version{#2022-12-31}
  @variable-value{22}
  @begin{short}
    First fundamental type number to create a new fundamental type ID with the
    @fun{g:type-make-fundamental} function reserved for GLib.
  @end{short}
  @see-class{g:type-t}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_GLIB_LAST                              not exported
;;; ----------------------------------------------------------------------------

#+nil
(defconstant +type-reserved-glib-last+ 31)

#+nil
(setf (liber:alias-for-variable '+type-reserved-glib-last+)
      "Constant"
      (documentation '+type-reserved-glib-last+ 'variable)
 "@version{#2022-12-31}
  @variable-value{31}
  @begin{short}
    Last fundamental type ID reserved for GLib.
  @end{short}
  @see-class{g:type-t}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_BSE_FIRST                              not exported
;;; ----------------------------------------------------------------------------

#+nil
(defconstant +type-reserved-bse-first+ 32)

#+nil
(setf (liber:alias-for-variable '+type-reserved-bse-first+)
      "Constant"
      (documentation '+type-reserved-bse-first+ 'variable)
 "@version{#2022-12-31}
  @variable-value{32}
  @begin{short}
    First fundamental type ID to create a new fundamental type ID with the
    @fun{g:type-make-fundamental} function reserved for BSE.
  @end{short}
  @see-class{g:type-t}
  @see-function{g:type-make-fundamental}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_BSE_LAST                               not exported
;;; ----------------------------------------------------------------------------

#+nil
(defconstant +type-reserved-bse-last+ 48)

#+nil
(setf (liber:alias-for-variable '+type-reserved-bse-last+)
      "Constant"
      (documentation '+type-reserved-bse-last+ 'variable)
 "@version{#2022-12-31}
  @variable-value{48}
  @begin{short}
    Last fundamental type ID reserved for BSE.
  @end{short}
  @see-class{g:type-t}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_USER_FIRST                             not exported
;;; ----------------------------------------------------------------------------

#+nil
(defconstant +type-reserved-user-first+ 49)

#+nil
(setf (liber:alias-for-variable '+type-reserved-user-first+)
      "Constant"
      (documentation '+type-reserved-user-first+ 'variable)
 "@version{#2022-12-31}
  @variable-value{49}
  @begin{short}
    First available fundamental type ID to create new fundamental type ID
    with the @fun{g-type-make-fundamental} function.
  @end{short}
  @see-class{g:type-t}
  @see-function{g:type-make-fundamental}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FUNDAMENTAL_MAX                                  not exported
;;; ----------------------------------------------------------------------------

(defconstant +type-fundamental-max+ (ash 255 +type-fundamental-shift+))

;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type type-t ()
  ((mangled-p :initarg :mangled-p
              :reader gtype-mangled-p
              :initform nil
              :documentation
              "Whether the type designator is mangled with
               the @code{G_SIGNAL_TYPE_STATIC_SCOPE} flag."))
  (:actual-type :size)
  (:simple-parser type-t))

#+liber-documentation
(setf (liber:alias-for-class 'type-t)
      "Type"
      (documentation 'type-t 'type)
 "@version{2024-12-08}
  @begin{short}
    The @class{g:type-t} type specifier represents the unique @code{GType}
    identifier of a registered type.
  @end{short}
  The @class{g:type-t} type specifier is identified by its @symbol{g:gtype}
  representation, a string for the name, or a numeric identifier. Functions
  accept @class{g:type-t} type specifiers as a @symbol{g:gtype} instance, a
  string or an integer and return a @symbol{g:gtype} instance. Use the
  @fun{g:gtype} function to create a @symbol{g:gtype} instance. You can get the
  name or the numeric identifier with the @fun{g:gtype-name} and
  @fun{g:gtype-id} functions.
  @begin[Fundamental types]{dictionary}
    The fundamental types known to GObject and the types of values that
    correspond to the CFFI interface und Common Lisp.
    @begin{pre}
ID     NAME               CFFI type      Lisp type
------------------------------------------------------------
 4     \"void\"           :void            NULL
 8     \"GInterface\"     :pointer
12     \"gchar\"          :char
16     \"guchar\"         :uchar
20     \"gboolean\"       :boolean         boolean
24     \"gint\"           :int
28     \"guint\"          :uint
32     \"glong\"          :long
36     \"gulong\"         :ulong
40     \"gint64\"         :int64
44     \"guint64\"        :uint64
48     \"GEnum\"                           g:enum
52     \"GFlags\"                          g:flags
56     \"gfloat\"         :float           single-float
60     \"gdouble\"        :double          double-float
64     \"gchararray\"     :string
68     \"gpointer\"       :pointer
72     \"GBoxed\"         :pointer         g:boxed
76     \"GParam\"         :pointer         g:param
80     \"GObject\"        :pointer         g:object
84     \"GVariant\"       :pointer         g:variant
    @end{pre}
  @end{dictionary}
  @begin[Lisp symbols for types]{dictionary}
    In the Lisp API, a Lisp symbol is registered for each GType. The
    @fun{g:symbol-for-gtype} function gets this Lisp symbol.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Create a @symbol{g:gtype} instance from a string or a numeric identifier.
    @begin{pre}
(g:gtype \"gdouble\") => #<GTYPE :name \"gdouble\" :id 60>
(g:gtype 60) => #<GTYPE :name \"gdouble\" :id 60>
    @end{pre}
    Get the name and the numeric identifier from a @symbol{g:gtype} instance.
    @begin{pre}
(glib:gtype-id (g:gtype \"gdouble\")) => 60
(glib:gtype-name (g:gtype \"gdouble\")) => \"gdouble\"
    @end{pre}
    Convert from foreign.
    @begin{pre}
(cffi:convert-from-foreign 60 'g:type-t)
=> #<GTYPE :name \"gdouble\" :id 60>
(cffi:convert-from-foreign \"gdouble\" 'g:type-t)
=> #<GTYPE :name \"gdouble\" :id 60>
(cffi:convert-from-foreign (g:gtype \"gdouble\") 'g:type-t)
=> #<GTYPE :name \"gdouble\" :id 60>
    @end{pre}
    Convert to foreign.
    @begin{pre}
(cffi:convert-to-foreign 60 'g:type-t) => 60
(cffi:convert-to-foreign \"gdouble\" 'g:type-t) => 60
(cffi:convert-to-foreign (g:gtype \"gdouble\") 'g:type-t) => 60
    @end{pre}
    Get the Lisp symbol for a GType:
    @begin{pre}
(g:symbol-for-gtype \"GApplication\")
=> GIO:APPLICATION
=> T
    @end{pre}
  @end{dictionary}
  @see-function{g:gtype}
  @see-function{g:gtype-name}
  @see-function{g:gtype-id}
  @see-function{g:symbol-for-gtype}")

;;; ----------------------------------------------------------------------------

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
;;; GTypeClass
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-class
  (:type type-t))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-class)
      "CStruct"
      (liber:symbol-documentation 'type-class)
 "@version{2024-12-08}
  @begin{declaration}
(cffi:defcstruct type-class
  (:type type-t))
  @end{declaration}
  @short{An opaque structure used as the base of all classes.}
  @see-class{g:type-t}
  @see-symbol{g:type-interface}
  @see-symbol{g:type-instance}")

(export 'type-class)

;;; ----------------------------------------------------------------------------
;;; GTypeInterface
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-interface
  (:type type-t)
  (:instance-type type-t))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-interface)
      "CStruct"
      (liber:symbol-documentation 'type-interface)
 "@version{2024-12-08}
  @begin{declaration}
(cffi:defcstruct type-interface
  (:type type-t)
  (:instance-type type-t))
  @end{declaration}
  @short{An opaque structure used as the base of all interface types.}
  @see-class{g:type-t}
  @see-symbol{g:type-class}
  @see-symbol{g:type-instance}
  @see-function{g:type-is-interface}")

(export 'type-interface)

;;; ----------------------------------------------------------------------------
;;; GTypeInstance
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-instance
  (:class (:pointer (:struct type-class))))

#+liber-documentation
(setf (liber:alias-for-symbol 'type-instance)
      "CStruct"
      (liber:symbol-documentation 'type-instance)
 "@version{2024-12-08}
  @begin{declaration}
(cffi:defcstruct type-instance
  (:class (:pointer (:struct type-class))))
  @end{declaration}
  @short{An opaque structure used as the base of all type instances.}
  @see-class{g:type-t}
  @see-symbol{g:type-class}
  @see-symbol{g:type-interface}")

(export 'type-instance)

;;; ----------------------------------------------------------------------------
;;; GTypeInfo                                               not exported
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

;;; ----------------------------------------------------------------------------
;;; GTypeFundamentalFlags                                   not exported
;;; ----------------------------------------------------------------------------

(cffi:defbitfield type-fundamental-flags
  :classed
  :instantiatable
  :derivable
  :deep-derivable)

;;; ----------------------------------------------------------------------------
;;; GTypeFundamentalInfo                                    not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-fundamental-info
  (:type-flags type-fundamental-flags))

;;; ----------------------------------------------------------------------------
;;; GInterfaceInfo                                          not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct interface-info
  (:interface-init :pointer)
  (:interface-finalize :pointer)
  (:interface-data :pointer))

;;; ----------------------------------------------------------------------------
;;; GTypeValueTable                                         not exported
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

;;; ----------------------------------------------------------------------------
;;; GTypeDebugFlags                                         Deprecated 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeQuery                                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct type-query
  (:type type-t)
  (:type-name :string)
  (:class-size :uint)
  (:instance-size :uint))

;;; ----------------------------------------------------------------------------
;;; GTypeFlags                                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defbitfield type-flags
  (:abstract #.(ash 1 4))
  (:value-abstract #.(ash 1 5)))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MAKE_FUNDAMENTAL                                 not exported
;;; ----------------------------------------------------------------------------

#+nil
(defun type-make-fundamental (x)
 #+liber-documentation
 "@version{#2020-11-01}
  @argument[x]{a fundamental type number}
  @return{The @class{g:type-t} type ID.}
  @begin{short}
    Get the type ID for the fundamental type number @arg{x}.
  @end{short}
  Use the @fun{g:type-fundamental-next} function instead of this function to
  create new fundamental type number.
  @begin[Note]{dictionary}
    The @fun{g:type-make-fundamental} function does not return a Lisp
    @class{g:type-t} type ID, but the ID number of the @class{g:type-t} type ID.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-make-fundamental 5)
=> 20
(gtype (g:type-make-fundamental 5))
=> <GTYPE :name \"gboolean\" :id 20>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-fundamental-next}"
  (ash x +type-fundamental-shift+))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_ABSTRACT
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_test_flags" %type-test-flags) :boolean
  (gtype type-t)
  (flag type-flags))

(defun type-is-abstract (gtype)
 #+liber-documentation
 "@version{2024-12-08}
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
;;; G_TYPE_IS_DERIVED
;;; ----------------------------------------------------------------------------

(defun type-is-derived (gtype)
 #+liber-documentation
 "@version{2024-12-08}
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
;;; G_TYPE_IS_FUNDAMENTAL
;;; ----------------------------------------------------------------------------

(defun type-is-fundamental (gtype)
 #+liber-documentation
 "@version{2024-12-08}
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
;;; G_TYPE_IS_VALUE_TYPE
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_check_is_value_type" %type-check-is-value-type) :boolean
  (gtype type-t))

(defun type-is-value-type (gtype)
 #+liber-documentation
 "@version{2024-12-08}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a value type.}
  @begin{short}
    Checks if @arg{gtype} is a value type and can be used with the
    @fun{g:value-init} function.
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
;;; G_TYPE_HAS_VALUE_TABLE                                  not exported
;;; ----------------------------------------------------------------------------

#+nil
(defun type-has-value-table (gtype)
 #+liber-documentation
 "@version{#2020-11-01}
  @argument[type]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} has a @code{GTypeValueTable}.}
  @begin{short}
    Checks if @arg{gtype} has a @code{GTypeValueTable}.
  @end{short}
  This function calls the @fun{g:type-value-table-peek} function to do the
  check.
  @see-class{g:type-t}
  @see-symbol{g:type-value-table}
  @see-function{g:type-value-table-peek}"
  (not (cffi:null-pointer-p (type-value-table-peek gtype))))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_CLASSED
;;; ----------------------------------------------------------------------------

;; We need this second variant of the function g_type_test_flags(), because
;; we can not pass a flag of type type-fundamental-flag to our first version.
;; See the implementation for G_TYPE_IS_ABSTRACT.

(cffi:defcfun ("g_type_test_flags" %type-test-fundamental-flags) :boolean
  (gtype type-t)
  (flag type-fundamental-flags))

(defun type-is-classed (gtype)
 #+liber-documentation
 "@version{2024-12-08}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a classed type.}
  @short{Checks if @arg{gtype} is a classed type.}
  @see-class{g:type-t}
  @see-symbol{g:type-class}"
  (%type-test-fundamental-flags gtype :classed))

(export 'type-is-classed)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_INSTANTIATABLE                                not exported
;;; ----------------------------------------------------------------------------

#+nil
(defun type-is-instantiatable (gtype)
 #+liber-documentation
 "@version{#2020-11-13}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} can be instantiated.}
  @begin{short}
    Checks if @arg{gtype} can be instantiated.
  @end{short}
  Instantiation is the process of creating an instance (object) of this type.
  @see-class{g:type-t}"
  (%type-test-fundamental-flags gtype :instantiatable))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_DERIVABLE                                     not exported
;;; ----------------------------------------------------------------------------

#+nil
(defun type-is-derivable (gtype)
 #+liber-documentation
 "@version{#2020-11-01}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a derivable type.}
  @begin{short}
    Checks if @arg{gtype} is a derivable type.
  @end{short}
  A derivable type can be used as the base class of a flat (single-level) class
  hierarchy.
  @see-class{g:type-t}
  @see-function{g:type-is-deep-derivable}"
  (%type-test-fundamental-flags gtype :derivable))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_DEEP_DERIVABLE                                not exported
;;; ----------------------------------------------------------------------------

#+nil
(defun type-is-deep-derivable (gtype)
 #+liber-documentation
 "@version{#2020-11-01}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a deep derivable type.}
  @begin{short}
    Checks if @arg{gtype} is a deep derivable type.
  @end{short}
  A deep derivable type can be used as the base class of a deep (multi-level)
  class hierarchy.
  @see-class{g:type-t}
  @see-function{g:type-is-derivable}"
  (%type-test-fundamental-flags gtype :deep-derivable))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_INTERFACE
;;; ----------------------------------------------------------------------------

(defun type-is-interface (gtype)
 #+liber-documentation
 "@version{2024-12-08}
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
  (eq (glib:gtype "GInterface") (type-fundamental gtype)))

(export 'type-is-interface)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FROM_INSTANCE
;;; ----------------------------------------------------------------------------

(defun type-from-instance (instance)
 #+liber-documentation
 "@version{2025-01-04}
  @argument[instance]{a valid @symbol{g:type-instance} instance}
  @return{The @class{g:type-t} type ID of @arg{instance}.}
  @short{Gets the type identifier from a given instance.}
  @begin[Notes]{dictionary}
    Signals an error if the @arg{instance} argument is not a valid
    @symbol{g:type-instance} instance.
  @end{dictionary}
  @begin[Examles]{dictionary}
    @begin{pre}
(g:type-from-instance (make-instance 'g:simple-action))
=> #<GTYPE :name \"GSimpleAction\" :id 97556076810288>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-instance}
  @see-function{g:type-from-class}"
  (let ((ptr (if (cffi:pointerp instance)
                 instance
                 (object-pointer instance))))
    (type-from-class (cffi:foreign-slot-value ptr
                                              '(:struct type-instance)
                                              :class))))

(export 'type-from-instance)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FROM_CLASS
;;; ----------------------------------------------------------------------------

(defun type-from-class (class)
 #+liber-documentation
 "@version{2025-01-04}
  @argument[class]{a valid @symbol{g:type-class} instance}
  @return{The @class{g:type-t} type ID of @arg{class}.}
  @short{Gets the type identifier from a given class instance.}
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
;;; G_TYPE_FROM_INTERFACE
;;; ----------------------------------------------------------------------------

(defun type-from-interface (iface)
 #+liber-documentation
 "@version{2025-01-04}
  @argument[iface]{a valid @symbol{g:type-interface} instance}
  @return{The @class{g:type-t} type ID of @arg{iface}.}
  @short{Gets the type identifier from a given interface instance.}
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
;;; G_TYPE_INSTANCE_GET_CLASS
;;; ----------------------------------------------------------------------------

(defun type-instance-class (instance)
 #+liber-documentation
 "@version{2024-12-08}
  @argument[instance]{a @symbol{g:type-instance} instance}
  @return{The @symbol{g:type-class} instance of @arg{instance}.}
  @begin{short}
    Get the class structure of a given @arg{instance}.
  @end{short}
  This function should only be used in type implementations.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-instance-class (make-instance 'g:simple-action))
=> #.(SB-SYS:INT-SAP #X58BA0B4DF320)
(g:type-from-class *)
=> #<GTYPE :name \"GSimpleAction\" :id 97556076810288>
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
;;; Get the interface structure for interface g_type of a given instance.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INSTANCE_GET_PRIVATE                             Deprecated 2.58
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CLASS_GET_PRIVATE
;;;
;;; Gets the private class structure for a particular type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE
;;;
;;; Checks if instance is a valid GTypeInstance structure, otherwise issues a
;;; warning and returns FALSE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE_CAST
;;;
;;; Checks that instance is an instance of the type identified by g_type and
;;; issues a warning if this is not the case.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE_TYPE
;;; ----------------------------------------------------------------------------

(defun type-check-instance-type (instance gtype)
 #+liber-documentation
 "@version{2024-12-08}
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
(g:type-check-instance-type (make-instance 'g:simple-action) \"GObject\")
=> T
(g:type-check-instance-type (make-instance 'g:simple-action) \"gboolean\")
=> NIL
(g:type-check-instance-type (make-instance 'g:simple-action) \"GAction\")
=> T
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:type-instance}
  @see-function{g:type-check-class-type}"
  (type-is-a (type-from-instance instance) gtype))

(export 'type-check-instance-type)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE_FUNDAMENTAL_TYPE
;;;
;;; Checks if instance is an instance of the fundamental type identified by
;;; g_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_CLASS_CAST
;;;
;;; Checks that g_class is a class structure of the type identified by g_type
;;; and issues a warning if this is not the case.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_CLASS_TYPE
;;; ----------------------------------------------------------------------------

(defun type-check-class-type (class gtype)
 #+liber-documentation
 "@version{2024-12-08}
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
;;; G_TYPE_CHECK_VALUE
;;;
;;; Checks if value has been initialized to hold values of a value type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_VALUE_TYPE
;;;
;;; Checks if value has been initialized to hold values of type g_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_init                                             Deprecated 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_init_with_debug_flags                            Deprecated 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_name
;;; ----------------------------------------------------------------------------

(defun type-name (gtype)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[gtype]{a @class{g:type-t} type ID to return the type name for}
  @return{The string for the type name, or @code{nil}.}
  @begin{short}
    Get the unique name that is assigned to a type ID.
  @end{short}
  Note that this function returns @code{nil}, when @arg{gtype} is not known.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-name 60) => \"gdouble\"
(g:type-name \"gdouble\") => \"gdouble\"
(g:type-name (g:gtype \"gdouble\")) => \"gdouble\"
(g:type-name \"unknown\")
=> WARNING: unknown is not known to the GType system
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (let ((gtype (glib:gtype gtype)))
    (when gtype (glib:gtype-name gtype))))

(export 'type-name)

;;; ----------------------------------------------------------------------------
;;; g_type_qname                                            not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_from_name                                        not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_parent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_parent" type-parent) type-t
 #+liber-documentation
 "@version{2024-12-08}
  @argument[gtype]{a derived @class{g:type-t} type ID}
  @return{The parent @class{g:type-t} type ID of @arg{gtype}.}
  @begin{short}
    Returns the direct parent type of the passed in @arg{gtype}.
  @end{short}
  If the passed in @arg{gtype} has no parent, for example, is a fundamental
  type, @code{nil} is returned.
  @begin[Examples]{dictionary}
    @begin{pre}
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
;;; g_type_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_children" %type-children) (:pointer :size)
  (gtype type-t)
  (n-children (:pointer :uint)))

(defun type-children (gtype)
 #+liber-documentation
 "@version{2024-12-08}
  @argument[gtype]{a @class{g:type-t} parent type ID}
  @return{The list of @class{g:type-t} child type IDs.}
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
;;; g_type_depth
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_depth" type-depth) :uint
 #+liber-documentation
 "@version{2025-09-27}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{The unsigned integer for the depth of @arg{gtype}.}
  @begin{short}
    Returns the length of the ancestry of the passed in @arg{gtype}.
  @end{short}
  This includes @arg{gtype} itself, so that, for example, a fundamental type
  has depth 1.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-depth \"gdouble\") => 1
(g:type-depth \"GtkButton\") => 4
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (gtype type-t))

(export 'type-depth)

;;; ----------------------------------------------------------------------------
;;; g_type_next_base
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_next_base" type-next-base) type-t
 #+liber-documentation
 "@version{2024-12-08}
  @argument[leaf]{a @class{g:type-t} type ID descendant of @arg{root} and the
    type to be returned}
  @argument[root]{a @class{g:type-t} type ID immediate parent of the returned
    type}
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
(g:type-next-base \"GtkToggleButton\" \"GtkWidget\")
=> #<GTYPE :name \"GtkButton\" :id 94607001843920>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (leaf type-t)
  (root type-t))

(export 'type-next-base)

;;; ----------------------------------------------------------------------------
;;; g_type_is_a
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_is_a" %type-is-a) :boolean
  (gtype type-t)
  (is-a-type type-t))

(defun type-is-a (gtype is-a-type)
 #+liber-documentation
 "@version{2024-12-08}
  @argument[gtype]{a @class{g:type-t} type ID to check anchestry for}
  @argument[is-a-type]{a @class{g:type-t} type ID with the possible anchestor
    of @arg{gtype} or the interface that @arg{gtype} could conform to}
  @return{@em{True} if the @arg{gtype} argument is a @arg{is-a-type}.}
  @begin{short}
    If the @arg{is-a-type} argument is a derivable type, check whether the
    @arg{gtype} argument is a descendant of @arg{is-a-type}.
  @end{short}
  If @arg{is-a-type} is an interface, check whether @arg{gtype} conforms to it.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-is-a \"gboolean\" \"gboolean\") => T
(g:type-is-a \"GBytes\" \"GBoxed\") => T
(g:type-is-a \"GApplicationFlags\" \"GFlags\") => T
(g:type-is-a \"GApplication\" \"GObject\") => T
(g:type-is-a \"GParamBoolean\" \"GParam\") => T
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (let ((*warn-unknown-gtype* nil)) ; no warnings for the test function
    (%type-is-a gtype is-a-type)))

(export 'type-is-a)

;;; ----------------------------------------------------------------------------
;;; g_type_class_ref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_ref" %type-class-ref)
    (:pointer (:struct type-class))
  (gtype type-t))

(defun type-class-ref (gtype)
 #+liber-documentation
 "@version{2024-12-08}
  @argument[gtype]{a @class{g:type-t} type ID of a classed type}
  @return{The pointer to the @symbol{g:type-class} instance for the given
    @arg{gtype}.}
  @begin{short}
    Increments the reference count of the class instance belonging to
    @arg{gtype} and returns the pointer to the class instance.
  @end{short}
  This function will create the class instance if it does not exist already.
  Returns @code{nil} when @arg{gtype} is not a valid type ID for a classed type.
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
;;; g_type_class_peek
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_peek" %type-class-peek)
    (:pointer (:struct type-class))
  (gtype type-t))

(defun type-class-peek (gtype)
 #+liber-documentation
 "@version{2024-12-08}
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
;;; g_type_class_peek_static                                not exported
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_type_class_peek_static" type-class-peek-static)
    (:pointer (:struct type-class))
 #+liber-documentation
 "@version{#2013-04-01}
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
;;; g_type_class_unref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_unref" type-class-unref) :void
 #+liber-documentation
 "@version{2024-12-08}
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
;;; g_type_class_peek_parent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_class_peek_parent" type-class-peek-parent)
    (:pointer (:struct type-class))
 #+liber-documentation
 "@version{#2025-03-27}
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
  @see-symbol{g:type-class}
  @see-function{g:type-parent}
  @see-function{g:type-class-peek}"
  (class (:pointer (:struct type-class))))

(export 'type-class-peek-parent)

;;; ----------------------------------------------------------------------------
;;; g_type_class_add_private                                not exported
;;;
;;; g_type_class_add_private has been deprecated since version 2.58 and should
;;; not be used in newly written code. Use the G_ADD_PRIVATE() macro with the
;;; G_DEFINE_* family of macros to add instance private data to a type
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_type_class_add_private" type-class-add-private) :void
 #+liber-documentation
 "@version{#2014-04-01}
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
;;; g_type_add_class_private
;;;
;;; Registers a private class structure for a classed type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_peek
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_interface_peek" %type-interface-peek)
    (:pointer (:struct type-interface))
  (class (:pointer (:struct type-class)))
  (itype type-t))

(defun type-interface-peek (class itype)
 #+liber-documentation
 "@version{2024-12-08}
  @argument[class]{a @symbol{g:type-class} instance}
  @argument[itype]{a @class{g:type-t} interface type ID which this @arg{class}
    conforms to}
  @begin{return}
    The @symbol{g:type-interface} instance of @arg{itype} if implemented by
    @arg{class}, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Returns the interface structure of an interface to which the passed in
    @arg{class} conforms.
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
  (let ((iface (%type-interface-peek class itype)))
    (when (not (cffi:null-pointer-p iface))
      iface)))

(export 'type-interface-peek)

;;; ----------------------------------------------------------------------------
;;; g_type_interface_peek_parent
;;;
;;; Returns the corresponding GTypeInterface structure of the parent type of the
;;; instance type to which g_iface belongs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_ref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_default_interface_ref" %type-default-interface-ref)
    (:pointer (:struct type-interface))
  (gtype type-t))

(defun type-default-interface-ref (gtype)
 #+liber-documentation
 "@version{2024-12-08}
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
;;; g_type_default_interface_peek
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_default_interface_peek" %type-default-interface-peek)
    (:pointer (:struct type-interface))
  (gtype type-t))

(defun type-default-interface-peek (gtype)
#+liber-documentation
 "@version{2024-12-08}
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
;;; g_type_default_interface_unref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_default_interface_unref" type-default-interface-unref)
    :void
 #+liber-documentation
 "@version{2024-12-08}
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
;;; g_type_interfaces
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_interfaces" %type-interfaces) (:pointer :size)
  (gtype type-t)
  (n-interfaces (:pointer :uint)))

(defun type-interfaces (gtype)
 #+liber-documentation
 "@version{2024-12-08}
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
;;; g_type_interface_prerequisites
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_interface_prerequisites" %type-interface-prerequisites)
    (:pointer :size)
  (itype type-t)
  (n-prerequisites (:pointer :uint)))

(defun type-interface-prerequisites (itype)
 #+liber-documentation
 "@version{2024-12-08}
  @argument[itype]{a @class{g:type-t} interface type ID}
  @begin{return}
    The list of @class{g:type-t} type IDs containing the prerequisites of
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
;;; g_type_get_qdata
;;; g_type_set_qdata
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
 "@version{2025-09-27}
  @syntax{(g:type-qdata type quark) => data}
  @syntax{(setf (g:type-qdata type quark) data)}
  @argument[gtype]{a @class{g:type-t} type ID}
  @argument[quark]{a string for the ID to identify the data}
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
  @see-class{g:type-t}"
  (let ((ptr (%type-get-qdata gtype quark)))
    (when (not (cffi:null-pointer-p ptr))
      (glib:get-stable-pointer-value ptr))))

(export 'type-qdata)

;;; ----------------------------------------------------------------------------
;;; g_type_query                                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_query" type-query) :void
  (gtype type-t)
  (query (:pointer (:struct type-query))))

;;; ----------------------------------------------------------------------------
;;; GBaseInitFunc
;;;
;;; A callback function used by the type system to do base initialization of the
;;; class structures of derived types.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GBaseFinalizeFunc
;;;
;;; A callback function used by the type system to finalize those portions of a
;;; derived types class structure that were setup from the corresponding
;;; GBaseInitFunc() function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClassInitFunc
;;;
;;; A callback function used by the type system to initialize the class of a
;;; specific type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClassFinalizeFunc
;;;
;;; A callback function used by the type system to finalize a class.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInstanceInitFunc
;;;
;;; A callback function used by the type system to initialize a new instance of
;;; a type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInterfaceInitFunc
;;;
;;; A callback function used by the type system to initialize a new interface.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInterfaceFinalizeFunc
;;;
;;; A callback function used by the type system to finalize an interface.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeClassCacheFunc
;;;
;;; A callback function which is called when the reference count of a class
;;; drops to zero.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_register_static                                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_register_static" type-register-static) type-t
  (parent-type type-t)
  (type-name :string)
  (info (:pointer (:struct type-info)))
  (flags type-flags))

;;; ----------------------------------------------------------------------------
;;; g_type_register_static_simple                           not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_register_static_simple"
                type-register-static-simple) type-t
  (parent-type type-t)
  (type-name :string)
  (class-size :uint)
  (class-init :pointer)
  (instance-size :uint)
  (instance-init :pointer)
  (flags type-flags))

;;; ----------------------------------------------------------------------------
;;; g_type_register_dynamic
;;;
;;; Registers type_name as the name of a new dynamic type derived from
;;; parent_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_register_fundamental
;;;
;;; Registers type_id as the predefined identifier and type_name as the name of
;;; a fundamental type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_static                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_add_interface_static" type-add-interface-static) :void
  (instance-type type-t)
  (interface-type type-t)
  (info (:pointer (:struct interface-info))))

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_dynamic
;;;
;;; Adds the dynamic interface_type to instantiable_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_add_prerequisite                       not exported
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_type_interface_add_prerequisite"
                type-interface-add-prerequisite) :void
 #+liber-documentation
 "@version{#2013-06-11}
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
;;; g_type_get_plugin
;;;
;;; Returns the GTypePlugin structure for type or NULL if type does not have a
;;; GTypePlugin structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_get_plugin
;;;
;;; Returns the GTypePlugin structure for the dynamic interface interface_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_fundamental_next                                 not exported
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_type_fundamental_next" type-fundamental-next) :size
 #+liber-documentation
 "@version{#2013-06-17}
  @begin{return}
    The nextmost fundamental type ID to be registered, or 0 if the type
    system ran out of fundamental type IDs.
  @end{return}
  Returns the next free fundamental type ID which can be used to register a
  new fundamental type with @code{g_type_register_fundamental()}. The returned
  type ID represents the highest currently registered fundamental type
  identifier.")

;;; ----------------------------------------------------------------------------
;;; g_type_fundamental
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_type_fundamental" type-fundamental) type-t
 #+liber-documentation
 "@version{2024-12-08}
  @argument[gtype]{a valid @class{g:type-t} type ID}
  @return{The fundamental @class{g:type-t} type ID of the @arg{gtype} argument.}
  @begin{short}
    The fundamental type which is the ancestor of the @arg{gtype} argument.
  @end{short}
  Fundamental types are types that serve as ultimate bases for the derived
  types, thus they are the roots of distinct inheritance hierarchies.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-fundamental \"GSimpleAction\") => #<GTYPE :name \"GObject\" :id 80>
(g:type-fundamental \"GAction\") => #<GTYPE :name \"GInterface\" :id 8>
(g:type-fundamental \"GBytes\") => #<GTYPE :name \"GBoxed\" :id 72>
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (gtype type-t))

(export 'type-fundamental)

;;; ----------------------------------------------------------------------------
;;; g_type_create_instance
;;;
;;; Creates and initializes an instance of type if type is valid and can be
;;; instantiated.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_free_instance
;;;
;;; Frees an instance of a type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_class_cache_func
;;;
;;; Adds a GTypeClassCacheFunc to be called before the reference count of a
;;; class goes from one to zero.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_remove_class_cache_func ()
;;;
;;; Removes a previously installed GTypeClassCacheFunc.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_class_unref_uncached
;;;
;;; A variant of g_type_class_unref() for use in GTypeClassCacheFunc
;;; implementations.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_check
;;;
;;; Adds a function to be called after an interface vtable is initialized for
;;; any class.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_remove_interface_check
;;;
;;; Removes an interface check function added with g_type_add_interface_check().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeInterfaceCheckFunc
;;;
;;; A callback called after an interface vtable is initialized.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_value_table_peek                                 not exported
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_type_value_table_peek" type-value-table-peek)
    (:pointer (:struct type-value-table))
 #+liber-documentation
 "@version{#2013-06-11}
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
;;; g_type_ensure
;;; ----------------------------------------------------------------------------

(defun type-ensure (gtype)
 #+liber-documentation
 "@version{2024-08-12}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{The @arg{gtype} type ID, or @code{nil}.}
  @begin{short}
    Ensures that the indicated @arg{gtype} has been registered with the type
    system, and its initializer method has been run.
  @end{short}
  If the @arg{gtype} argument is a string and @arg{gtype} is not already
  registered, the type initializer is called. On success, the @arg{gtype} type
  ID is returned, otherwise @code{nil}.
  @see-class{g:type-t}"
  (maybe-call-type-initializer gtype))

(export 'type-ensure)

;;; ----------------------------------------------------------------------------
;;; g_type_get_type_registration_serial
;;;
;;; Returns an opaque serial number that represents the state of the set of
;;; registered types.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_get_instance_count
;;;
;;; Returns the number of instances allocated of the particular type.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.type-info.lisp -------------------------------------
