;;; ----------------------------------------------------------------------------
;;; gobject.enumeration.lisp
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
;;; Enumeration and Flag Types
;;;
;;;     Enumeration and flags types
;;;
;;; Types and Values
;;;
;;;     GEnumClass
;;;     GFlagsClass
;;;     GEnumValue
;;;     GFlagsValue
;;;
;;; Functions
;;;
;;;     G_ENUM_CLASS_TYPE
;;;     G_ENUM_CLASS_TYPE_NAME
;;;     G_TYPE_IS_ENUM
;;;     G_ENUM_CLASS
;;;     G_IS_ENUM_CLASS
;;;
;;;     G_TYPE_IS_FLAGS
;;;     G_FLAGS_CLASS
;;;     G_IS_FLAGS_CLASS
;;;     G_FLAGS_CLASS_TYPE
;;;     G_FLAGS_CLASS_TYPE_NAME
;;;
;;;     g_enum_get_value
;;;     g_enum_get_value_by_name
;;;     g_enum_get_value_by_nick
;;;     g_enum_to_string
;;;
;;;     g_flags_get_first_value
;;;     g_flags_get_value_by_name
;;;     g_flags_get_value_by_nick
;;;     g_flags_to_string
;;;
;;;     g_enum_register_static
;;;     g_flags_register_static
;;;     g_enum_complete_type_info
;;;     g_flags_complete_type_info
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; define-g-enum
;;;
;;; Defines a GEnum type for enumeration. Generates the corresponding CFFI
;;; definition.
;;;
;;; Example:
;;;
;;; (define-g-enum "GdkGrabStatus" grab-status ()
;;;   :success
;;;   :already-grabbed
;;;   :invalid-time
;;;   :not-viewable
;;;   :frozen)
;;;
;;; (define-g-enum "GdkExtensionMode" gdk-extension-mode
;;;    (:export t
;;;     :type-initializer "gdk_extension_mode_get_type")
;;;    (:none 0)
;;;    (:all 1)
;;;    (:cursor 2))
;;;
;;; gtype :
;;;     a string. Specifies the GEnum name
;;;
;;; name :
;;;     a symbol. Names the enumeration type.
;;;
;;; export :
;;;     a boolean. If true, name will be exported.
;;;
;;; base-type :
;;;     A symbol denoting a foreign type, default value :int
;;;
;;; allow-undeclared-values :
;;;     Whether ot pass through integer values that were not explicitly declared
;;;     in the enumeration when translating from foreign memory.
;;;
;;; type-initializer :
;;;     a NIL or a string or a function designator. If non-NIL, specifies the
;;;     function that initializes the type: string specifies a C function that
;;;     returns the GType value and function designator specifies the Lisp
;;;     function.
;;;
;;; values :
;;;     values for enum. Each value is a keyword or a list
;;;     (keyword integer-value). keyword corresponds to Lisp value of
;;;     enumeration, and integer-value is an C integer for enumeration item.
;;;     If integer-value is not specified, it is generated automatically
;;;    (see CFFI manual)
;;; ----------------------------------------------------------------------------

(defmacro define-g-enum (gtype name (&key (export t)
                                          (base-type :int)
                                          (allow-undeclared-values nil)
                                          type-initializer)
                                     &body values)
  `(progn
     (cffi:defcenum (,name ,base-type
                           :allow-undeclared-values ,allow-undeclared-values)
               ,@values)
     (setf (glib:symbol-for-gtype ,gtype) ',name)
     ,@(when export
         (list `(export ',name
                        (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
         (list `(glib-init:at-init ()
                   ,(type-initializer-call type-initializer))))))

(export 'define-g-enum)

(defun type-initializer-call (type-initializer)
  (etypecase type-initializer
    (string `(if (cffi:foreign-symbol-pointer ,type-initializer)
                 (cffi:foreign-funcall-pointer
                     (cffi:foreign-symbol-pointer ,type-initializer)
                     ()
                     :size)
                 (warn "Type initializer '~A' is not available"
                       ,type-initializer)))
    (symbol `(funcall ',type-initializer))))

;; parse-g-value-enum  is called from the function parse-g-value.

(defun parse-g-value-enum (gvalue)
  (let* ((gtype (value-type gvalue))
         (enum-type (glib:symbol-for-gtype (glib:gtype-name gtype))))
    (unless enum-type
      (error "Enum ~A is not registered" (glib:gtype-name gtype)))
    (cffi:convert-from-foreign (value-enum gvalue) enum-type)))

;; This function is called from set-g-value to set a GEnum Value.

(defun set-g-value-enum (gvalue value)
  (let* ((gtype (value-type gvalue))
         (enum-type (glib:symbol-for-gtype (glib:gtype-name gtype))))
    (unless enum-type
      (error "Enum ~A is not registered" (glib:gtype-name gtype)))
    (setf (value-enum gvalue) (cffi:convert-to-foreign value enum-type))))

;;; ----------------------------------------------------------------------------
;;; struct GEnumValue
;;; ----------------------------------------------------------------------------

(cffi:defcstruct enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'enum-value)
      "CStruct"
      (liber:symbol-documentation 'enum-value)
 "@version{#2022-12-29}
  @begin{short}
    A structure which contains a single enum value, its name, and its nickname.
  @end{short}
  @begin{pre}
(cffi:defcstruct enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))
  @end{pre}
  @begin[code]{table}
    @entry[:value]{The enum value.}
    @entry[:name]{The name of the value.}
    @entry[:nick]{The nickname of the value.}
  @end{table}
  @see-symbol{g:enum-class}")

(export 'enum-value)

;;; ----------------------------------------------------------------------------
;;; struct GEnumClass
;;; ----------------------------------------------------------------------------

(cffi:defcstruct enum-class
  (:type-class (:pointer (:struct type-class)))
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer (:struct enum-value))))

#+liber-documentation
(setf (liber:alias-for-symbol 'enum-class)
      "CStruct"
      (liber:symbol-documentation 'enum-class)
 "@version{#2022-12-29}
  @begin{short}
    The class of an enumeration type holds information about its possible
    values.
  @end{short}
  @begin{pre}
(cffi:defcstruct enum-class
  (:type-class (:pointer (:struct type-class)))
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer enum-value)))
  @end{pre}
  @begin[code]{table}
    @entry[:type-class]{The parent class.}
    @entry[:minimum]{The smallest possible value.}
    @entry[:maximum]{The largest possible value.}
    @entry[:n-values]{The number of possible values.}
    @entry[:values]{An array of @symbol{g:enum-value} instances describing the
      individual values.}
  @end{table}
  @see-symbol{g:enum-value}")

(export 'enum-class)

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE                                      not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. We do not export this function.
;; It is a call of type-from-class.

(defun enum-class-type (class)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[class]{a @symbol{g:enum-class} instance}
  @return{The @class{g:type-t} ID for @arg{class}.}
  @begin{short}
    Get the type identifier from a given @symbol{g:enum-class} instance.
  @end{short}
  @see-symbol{g:enum-class}
  @see-class{g:type-t}"
  (type-from-class class))

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE_NAME                                 not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. We do not export this function.
;; It is a call of type-name and type-from-class.

(defun enum-class-type-name (class)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[class]{a @symbol{g:enum-class} instance}
  @return{The string with the type name.}
  @begin{short}
    Get the type name from a given @symbol{g:enum-class} instance.
  @end{short}
  @see-symbol{g:enum-class}"
  (type-name (type-from-class class)))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_ENUM
;;; ----------------------------------------------------------------------------

(declaim (inline type-is-enum))

(defun type-is-enum (gtype)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[gtype]{a @class{g:type-t} ID}
  @return{@em{True} if @arg{gtype} is a \"GEnum\" type.}
  @begin{short}
    Checks whether @arg{gtype} is a \"GEnum\" type.
  @end{short}
  @see-class{g:type-t}"
  (eq (type-fundamental gtype) (glib:gtype "GEnum")))

(export 'type-is-enum)

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS()
;;;
;;; #define G_ENUM_CLASS(class)
;;;         (G_TYPE_CHECK_CLASS_CAST ((class), G_TYPE_ENUM, GEnumClass))
;;;
;;; Casts a derived GEnumClass structure into a GEnumClass structure.
;;;
;;; class :
;;;     a valid GEnumClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_ENUM_CLASS                                        not exported
;;; ----------------------------------------------------------------------------

(defun is-enum-class (class)
 #+liber-documentation
 "@version{#2020-10-16}
  @argument[class]{a @symbol{enum-class} structure}
  @begin{short}
    Checks whether class is a valid @symbol{enum-class} structure of type
    @var{+g-type-enum+} or derived.
  @end{short}
  @see-symbol{enum-class}
  @see-variable{+g-type-enum+}"
  (type-check-class-type class +g-type-enum+))

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value ()
;;;
;;; GEnumValue * g_enum_get_value (GEnumClass *enum_class, gint value);
;;;
;;; Returns the GEnumValue for a value.
;;;
;;; enum_class :
;;;     a GEnumClass
;;;
;;; value :
;;;     the value to look up
;;;
;;; Returns :
;;;     the GEnumValue for value, or NULL if value is not a member of the
;;;     enumeration
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value_by_name ()
;;;
;;; GEnumValue * g_enum_get_value_by_name (GEnumClass *enum_class,
;;;                                        const gchar *name);
;;;
;;; Looks up a GEnumValue by name.
;;;
;;; enum_class :
;;;     a GEnumClass
;;;
;;; name :
;;;     the name to look up
;;;
;;; Returns :
;;;     the GEnumValue with name name, or NULL if the enumeration does not have
;;;     a member with that name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value_by_nick ()
;;;
;;; GEnumValue * g_enum_get_value_by_nick (GEnumClass *enum_class,
;;;                                        const gchar *nick);
;;;
;;; Looks up a GEnumValue by nickname.
;;;
;;; enum_class :
;;;     a GEnumClass
;;;
;;; nick :
;;;     the nickname to look up
;;;
;;; Returns :
;;;     the GEnumValue with nickname nick, or NULL if the enumeration does not
;;;     have a member with that nickname
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_to_string ()
;;;
;;; gchar *
;;; g_enum_to_string (GType g_enum_type, gint value);
;;;
;;; Pretty-prints value in the form of the enum’s name.
;;;
;;; This is intended to be used for debugging purposes. The format of the
;;; output may change in the future.
;;;
;;; g_enum_type :
;;;     the type identifier of a GEnumClass type
;;;
;;; value :
;;;     the value
;;;
;;; Returns :
;;;     a newly-allocated text string.
;;;
;;; Since 2.54
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; define-g-flags
;;;
;;; Defines a GFlags type for enumeration that can combine its values.
;;; Generates corresponding CFFI definition. Values of this type are lists of
;;; keywords that are combined.
;;;
;;; Example:
;;;
;;; (define-g-flags \"GdkWindowState\" window-state ()
;;;   (:withdrawn 1)
;;;   (:iconified 2) (:maximized 4) (:sticky 8) (:fullscreen 16)
;;;   (:above 32) (:below 64))
;;;
;;; gtype :
;;;     a string. Specifies the GEnum name
;;;
;;; name :
;;;     a symbol. Names the enumeration type.
;;;
;;; export :
;;;     a boolean. If true, name will be exported.
;;;
;;; base-type :
;;;     A symbol denoting a foreign type. The default is :int
;;;
;;; type-initializer :
;;;     a  NIL or a string or a function designator. If non-NIL, specifies the
;;;     function that initializes the type: string specifies a C function that
;;;     returns the GType value and function designator specifies the Lisp
;;;     function.
;;;
;;; values :
;;;     values for flags. Each value is a keyword or a list
;;;     (keyword integer-value). keyword corresponds to Lisp value of a flag,
;;;     and integer-value is an C integer for flag. If integer-value is not
;;;     specified, it is generated automatically (see CFFI manual)
;;; ----------------------------------------------------------------------------

(defmacro define-g-flags (gtype name (&key (export t)
                                           (base-type :int)
                                           type-initializer)
                                      &body values)
  `(progn
     (cffi:defbitfield ,name ,base-type ,@values)
     (setf (glib:symbol-for-gtype ,gtype) ',name)
     ,@(when export
         (list `(export ',name
                        (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
         (list `(glib-init::at-init ()
                   ,(type-initializer-call type-initializer))))))

(export 'define-g-flags)

;; parse-g-value-flags is called from the function parse-g-value.

(defun parse-g-value-flags (gvalue)
  (let* ((gtype (value-type gvalue))
         (flags-type (glib:symbol-for-gtype (glib:gtype-name gtype))))
    (unless flags-type
      (error "Flags ~A is not registered." (glib:gtype-name gtype)))
    (cffi:convert-from-foreign (value-flags gvalue) flags-type)))

;; This function is called from set-g-value to set a GFlag value.

(defun set-g-value-flags (gvalue value)
  (let* ((gtype (value-type gvalue))
         (flags-type (glib:symbol-for-gtype (glib:gtype-name gtype))))
    (unless flags-type
      (error "Flags ~A is not registered." (glib:gtype-name gtype)))
    (setf (value-flags gvalue) (cffi:convert-to-foreign value flags-type))))

;;; ----------------------------------------------------------------------------
;;; Struct g-flags-value
;;; ----------------------------------------------------------------------------

(cffi:defcstruct flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'flags-value)
      "CStruct"
      (liber:symbol-documentation 'flags-value)
 "@version{#2022-12-29}
  @begin{short}
    A structure which contains a single flags value, its name, and its nickname.
  @end{short}
  @begin{pre}
(cffi:defcstruct flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))
  @end{pre}
  @begin[code]{table}
    @entry[:value]{The flags value.}
    @entry[:name]{The name of the value.}
    @entry[:nick]{The nickname of the value.}
  @end{table}
  @see-symbol{g:flags-class}")

(export 'flags-value)

;;; ----------------------------------------------------------------------------
;;; Struct g-flags-class
;;; ----------------------------------------------------------------------------

(cffi:defcstruct flags-class
  (:type-class (:pointer (:struct type-class)))
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer (:struct flags-value))))

#+liber-documentation
(setf (liber:alias-for-symbol 'flags-class)
      "CStruct"
      (liber:symbol-documentation 'flags-class)
 "@version{#2022-12-29}
  @begin{short}
    The class of a flags type holds information about its possible values.
  @end{short}
  @begin{pre}
(cffi:defcstruct flags-class
  (:type-class (:pointer (:struct type-class)))
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer flags-value)))
  @end{pre}
  @begin[code]{table}
    @entry[:type-class]{The parent class.}
    @entry[:mask]{A mask covering all possible values.}
    @entry[:n-values]{The number of possible values.}
    @entry[:values]{An array of @symbol{g:flags-value} instances describing
      the individual values.}
  @end{table}
  @see-symbol{g:flags-value}")

(export 'flags-class)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_FLAGS
;;; ----------------------------------------------------------------------------

(declaim (inline type-is-flags))

(defun type-is-flags (gtype)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[type]{a @class{g:type-t} ID}
  @return{@em{True} if @arg{type} is a \"GFlags\" type.}
  @begin{short}
    Checks whether @arg{type} is a \"GFlags\" type.
  @end{short}
  @see-class{g:type-t}"
  (eq (type-fundamental gtype) (glib:gtype "GFlags")))

(export 'type-is-flags)

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS
;;;
;;; #define G_FLAGS_CLASS(class)
;;;         (G_TYPE_CHECK_CLASS_CAST ((class), G_TYPE_FLAGS, GFlagsClass))
;;;
;;; Casts a derived GFlagsClass structure into a GFlagsClass structure.
;;;
;;; class :
;;;     a valid GFlagsClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_FLAGS_CLASS                                       not exported
;;; ----------------------------------------------------------------------------

(defun is-flags-class (class)
 #+liber-documentation
 "@version{#2020-10-16}
  @argument[class]{a @symbol{flags-class} structure}
  @begin{short}
    Checks whether @arg{class} is a valid @symbol{flags-class} structure
    of type @var{+g-type-flags+} or derived.
  @end{short}
  @see-symbol{flags-class}
  @see-variable{+g-type-flags+}"
  (type-check-class-type class +g-type-flags+))

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE                                     not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. We do not export this function.
;; It is a call of type-from-class.

(defun flags-class-type (class)
 #+liber-documentation
 "@version{#2020-10-16}
  @argument[class]{a @symbol{flags-class} structure}
  @return{The @class{type-t} ID for @arg{class}.}
  @begin{short}
    Get the type identifier from a given @symbol{flags-class} structure.
  @end{short}
  @see-symbol{flags-class}
  @see-class{type-t}"
  (type-from-class class))

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE_NAME                                not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. We do not export this function.
;; It is a call of type-name and type-from-class.

(defun flags-class-type-name (class)
 #+liber-documentation
 "@version{#2020-10-16}
  @argument[class]{a @symbol{flags-class} structure}
  @return{The string with the type name.}
  @begin{short}
    Get the type name from a given @symbol{flags-class} structure.
  @end{short}
  @see-symbol{flags-class}"
  (type-name (type-from-class class)))

;;; ----------------------------------------------------------------------------
;;; g_flags_get_first_value ()
;;;
;;; GFlagsValue * g_flags_get_first_value (GFlagsClass *flags_class,
;;;                                        guint value);
;;;
;;; Returns the first GFlagsValue which is set in value.
;;;
;;; flags_class :
;;;     a GFlagsClass
;;;
;;; value :
;;;     the value
;;;
;;; Returns :
;;;     the first GFlagsValue which is set in value, or NULL if none is set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_value_by_name ()
;;;
;;; GFlagsValue * g_flags_get_value_by_name (GFlagsClass *flags_class,
;;;                                          const gchar *name);
;;;
;;; Looks up a GFlagsValue by name.
;;;
;;; flags_class :
;;;     a GFlagsClass
;;;
;;; name :
;;;     the name to look up
;;;
;;; Returns :
;;;     the GFlagsValue with name name, or NULL if there is no flag with that
;;;     name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_value_by_nick ()
;;;
;;; GFlagsValue * g_flags_get_value_by_nick (GFlagsClass *flags_class,
;;;                                          const gchar *nick);
;;;
;;; Looks up a GFlagsValue by nickname.
;;;
;;; flags_class :
;;;     a GFlagsClass
;;;
;;; nick :
;;;     the nickname to look up
;;;
;;; Returns :
;;;     the GFlagsValue with nickname nick, or NULL if there is no flag with
;;;     that nickname
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_to_string ()
;;;
;;; gchar *
;;; g_flags_to_string (GType flags_type, guint value);
;;;
;;; Pretty-prints value in the form of the flag names separated by | and sorted.
;;; Any extra bits will be shown at the end as a hexadecimal number.
;;;
;;; This is intended to be used for debugging purposes. The format of the
;;; output may change in the future.
;;;
;;; flags_type :
;;;     the type identifier of a GFlagsClass type
;;;
;;; value :
;;;     the value
;;;
;;; Returns :
;;;     a newly-allocated text string.
;;;
;;; Since 2.54
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_register_static ()                              not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. This function is not
;; exported.

(cffi:defcfun ("g_enum_register_static" enum-register-static) type-t
 #+liber-documentation
 "@version{#2013-6-10}
  @argument[name]{a string used as the name of the new type}
  @argument[static-values]{an array of @symbol{enum-value} structs for the
    possible enumeration values. The array is terminated by a struct with all
    members being 0. @class{object} keeps a reference to the data, so it
    cannot be stack-allocated.}
  @return{The new type identifier.}
  @begin{short}
    Registers a new static enumeration type with the name name.
  @end{short}

  It is normally more convenient to let glib-mkenums generate a
  @code{my_enum_get_type()} function from a usual C enumeration definition than
  to write one yourself using the @fun{g:enum-register-static} function."
  (name :string)
  (static-values (:pointer (:struct enum-value))))

;;; ----------------------------------------------------------------------------
;;; g_flags_register_static ()                             not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. This function is not
;; exported.

(cffi:defcfun ("g_flags_register_static" flags-register-static) type-t
 #+liber-documentation
 "@version{#2013-6-10}
  @argument[name]{a string used as the name of the new type}
  @argument[static-values]{an array of @symbol{flags-value} structs for the
    possible flags values. The array is terminated by a struct with all members
    being 0. @class{object} keeps a reference to the data, so it cannot be
    stack-allocated.}
  @return{The new type identifier.}
  @begin{short}
    Registers a new static flags type with the name name.
  @end{short}

  It is normally more convenient to let @code{glib-mkenums} generate a
  @code{my_flags_get_type()} function from a usual C enumeration definition
  than to write one yourself using the @fun{g:flags-register-static} function."
  (name :string)
  (static-values (:pointer (:struct flags-value))))

;;; ----------------------------------------------------------------------------
;;; g_enum_complete_type_info ()
;;;
;;; void g_enum_complete_type_info (GType g_enum_type,
;;;                                 GTypeInfo *info,
;;;                                 const GEnumValue *const_values);
;;;
;;; This function is meant to be called from the complete_type_info function of
;;; a GTypePlugin implementation, as in the following example:
;;;
;;;   static void
;;;   my_enum_complete_type_info (GTypePlugin     *plugin,
;;;                               GType            g_type,
;;;                               GTypeInfo       *info,
;;;                               GTypeValueTable *value_table)
;;;   {
;;;     static const GEnumValue values[] = {
;;;       { MY_ENUM_FOO, "MY_ENUM_FOO", "foo" },
;;;       { MY_ENUM_BAR, "MY_ENUM_BAR", "bar" },
;;;       { 0, NULL, NULL }
;;;     };
;;;
;;;     g_enum_complete_type_info (type, info, values);
;;;   }
;;;
;;; g_enum_type :
;;;     the type identifier of the type being completed
;;;
;;; info :
;;;     the GTypeInfo struct to be filled in
;;;
;;; const_values :
;;;     An array of GEnumValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_complete_type_info ()
;;;
;;; void g_flags_complete_type_info (GType g_flags_type,
;;;                                  GTypeInfo *info,
;;;                                  const GFlagsValue *const_values);
;;;
;;; This function is meant to be called from the complete_type_info() function
;;; of a GTypePlugin implementation, see the example for
;;; g_enum_complete_type_info() above.
;;;
;;; g_flags_type :
;;;     the type identifier of the type being completed
;;;
;;; info :
;;;     the GTypeInfo struct to be filled in
;;;
;;; const_values :
;;;     An array of GFlagsValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.enumeration.lisp -----------------------------------
