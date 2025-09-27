;;; ----------------------------------------------------------------------------
;;; gobject.enumeration.lisp
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

;; Register Lisp symbol for fundamental "GEnum" type
(glib-init:at-init ()
  (setf (glib:symbol-for-gtype "GEnum") 'enum))

(export 'enum)

;;; ----------------------------------------------------------------------------
;;; define-genum
;;;
;;; Defines a GEnum type for enumeration. Generates the corresponding CFFI
;;; definition.
;;;
;;; Example:
;;;
;;; (define-genum "GdkGrabStatus" grab-status ()
;;;   :success
;;;   :already-grabbed
;;;   :invalid-time
;;;   :not-viewable
;;;   :frozen)
;;;
;;; (define-genum "GdkExtensionMode" extension-mode
;;;    (:export t
;;;     :type-initializer "gdk_extension_mode_get_type")
;;;    (:none 0)
;;;    (:all 1)
;;;    (:cursor 2))
;;;
;;; gtype :
;;;     a string that specifies the GEnum type name
;;;
;;; name :
;;;     a symbol that names the enumeration type
;;;
;;; export :
;;;     a boolean whether NAME will be exported
;;;
;;; base-type :
;;;     a symbol denoting a foreign type, default value :int
;;;
;;; allow-undeclared-values :
;;;     whether to pass through integers that were not explicitly declared
;;;     in the enumeration when translating from foreign memory
;;;
;;; type-initializer :
;;;     NIL or a string or a function designator, if non-NIL, specifies the
;;;     function that initializes the type: a string specifies a C function that
;;;     returns the GType value and a function designator specifies the Lisp
;;;     function
;;;
;;; values :
;;;     values for the enumeration, each value is a keyword or a list
;;;     (keyword integer-value). keyword corresponds to Lisp value of the
;;;     enumeration, and integer-value is an C integer for the enumeration item,
;;;     if integer-value is not specified, it is generated automatically
;;;    (see the CFFI manual)
;;; ----------------------------------------------------------------------------

(defmacro define-genum (gtype name (&key (export t)
                                          (base-type :int)
                                          (allow-undeclared-values nil)
                                          type-initializer)
                                    &body values)
  `(progn
     (cffi:defcenum (,name ,base-type
                           :allow-undeclared-values ,allow-undeclared-values)
               ,@values)
     ,@(when export
         (list `(export ',name
                        (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
         (list `(glib-init:at-init ()
                   ,(glib:type-initializer-call type-initializer))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (glib:symbol-for-gtype ,gtype) ',name))))

(export 'define-genum)

;;; ----------------------------------------------------------------------------

;; Called from the gobject:value-enum function
(defun get-gvalue-enum (gvalue)
  (let* ((gtype (value-type gvalue))
         (enum-type (glib:symbol-for-gtype gtype)))
    (unless enum-type
      (error "GEnum ~A is not registered" (glib:gtype-name gtype)))
    (cffi:convert-from-foreign (%value-enum gvalue) enum-type)))

;; Called from the (setf gobject:value-enum) function
(defun set-gvalue-enum (gvalue value)
  (let* ((gtype (value-type gvalue))
         (enum-type (glib:symbol-for-gtype gtype)))
    (unless enum-type
      (error "GEnum ~A is not registered" (glib:gtype-name gtype)))
    (setf (%value-enum gvalue) (cffi:convert-to-foreign value enum-type))))

;;; ----------------------------------------------------------------------------
;;; GEnumValue
;;; ----------------------------------------------------------------------------

(cffi:defcstruct enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'enum-value)
      "CStruct"
      (liber:symbol-documentation 'enum-value)
 "@version{2024-06-09}
  @begin{declaration}
(cffi:defcstruct enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:value]{The enum value.}
      @entry[:name]{The name of the value.}
      @entry[:nick]{The nickname of the value.}
    @end{table}
  @end{values}
  @begin{short}
    A structure which contains a single enum value, its name, and its nickname.
  @end{short}
  @see-symbol{g:enum-class}")

(export 'enum-value)

;;; ----------------------------------------------------------------------------
;;; GEnumClass
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
 "@version{2024-06-09}
  @begin{declaration}
(cffi:defcstruct enum-class
  (:type-class (:pointer (:struct type-class)))
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer enum-value)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:type-class]{The parent class.}
      @entry[:minimum]{The smallest possible value.}
      @entry[:maximum]{The largest possible value.}
      @entry[:n-values]{The number of possible values.}
      @entry[:values]{The array of @symbol{g:enum-value} instances describing
        the individual values.}
    @end{table}
  @end{values}
  @begin{short}
    The class of an enumeration type holds information about its possible
    values.
  @end{short}
  @see-symbol{g:enum-value}")

(export 'enum-class)

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE                                       not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defun enum-class-type (class)
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[class]{a @symbol{g:enum-class} instance}
  @return{The @class{g:type-t} type ID for @arg{class}.}
  @begin{short}
    Get the type identifier from a given @symbol{g:enum-class} instance.
  @end{short}
  @see-symbol{g:enum-class}
  @see-class{g:type-t}"
  (type-from-class class))

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE_NAME                                  not implmented
;;; ----------------------------------------------------------------------------

#+nil
(defun enum-class-type-name (class)
 #+liber-documentation
 "@version{#2025-09-27}
  @argument[class]{a @symbol{g:enum-class} instance}
  @return{The string for the type name.}
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
 "@version{2024-06-09}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gtype} is a @code{\"GEnum\"} type.}
  @begin{short}
    Checks whether @arg{gtype} is a @code{\"GEnum\"} type.
  @end{short}
  @see-class{g:type-t}"
  (eq (type-fundamental gtype) (glib:gtype "GEnum")))

(export 'type-is-enum)

;;; ----------------------------------------------------------------------------
;;; G_IS_ENUM_CLASS                                         not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value ()                                     not implemented
;;;
;;; Returns the GEnumValue for a value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value_by_name ()                             not implemented
;;;
;;; Looks up a GEnumValue by name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value_by_nick ()                             not implemented
;;;
;;; Looks up a GEnumValue by nickname.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_to_string ()                                     not implemented
;;;
;;; Pretty-prints value in the form of the enum’s name.
;;;
;;; This is intended to be used for debugging purposes. The format of the
;;; output may change in the future.
;;; ----------------------------------------------------------------------------

;; Register Lisp symbol for fundamental "GFlags" type
(glib-init:at-init ()
  (setf (glib:symbol-for-gtype "GFlags") 'flags))

(export 'flags)

;;; ----------------------------------------------------------------------------
;;; define-gflags
;;;
;;; Defines a GFlags type for enumeration that can combine its values.
;;; Generates corresponding CFFI definition. Values of this type are lists of
;;; keywords that are combined.
;;;
;;; Example:
;;;
;;; (define-gflags \"GdkWindowState\" window-state ()
;;;   (:withdrawn 1)
;;;   (:iconified 2) (:maximized 4) (:sticky 8) (:fullscreen 16)
;;;   (:above 32) (:below 64))
;;;
;;; gtype :
;;;     a string that specifies the GEnum name
;;;
;;; name :
;;;     a symbol that names the enumeration type
;;;
;;; export :
;;;     a boolean, if true, NAME will be exported
;;;
;;; base-type :
;;;     a symbol denoting a foreign type, the default is :int
;;;
;;; type-initializer :
;;;     NIL or a string or a function designator, if non-NIL, specifies the
;;;     function that initializes the type: a string specifies a C function that
;;;     returns the GType value and a function designator specifies a Lisp
;;;     function
;;;
;;; values :
;;;     values for flags, each value is a keyword or a list
;;;     (keyword integer-value). keyword corresponds to Lisp value of a flag,
;;;     and integer-value is an C integer for flag if integer-value is not
;;;     specified, it is generated automatically (see CFFI manual)
;;; ----------------------------------------------------------------------------

#+nil
(defmacro define-g-flags (gtype name (&key (export t)
                                           (base-type :int)
                                           type-initializer)
                                      &body values)
  `(progn
     (cffi:defbitfield ,name ,base-type ,@values)
     ,@(when export
         (list `(export ',name
                        (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
         (list `(glib-init:at-init ()
                   ,(glib:type-initializer-call type-initializer))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (glib:symbol-for-gtype ,gtype) ',name))))

(defmacro define-gflags (gtype name (&key (export t)
                                           (base-type :int)
                                           type-initializer)
                                     &body values)
  `(progn
     (cffi:defbitfield ,name ,base-type ,@values)
     ,@(when export
         (list `(export ',name
                        (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
         (list `(glib-init:at-init ()
                   ,(glib:type-initializer-call type-initializer))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (glib:symbol-for-gtype ,gtype) ',name))))

(export 'define-gflags)

;;; ----------------------------------------------------------------------------

;; get-gvalue-flags is called from the function get-gvalue
(defun get-gvalue-flags (gvalue)
  (let* ((gtype (value-type gvalue))
         (flags-type (glib:symbol-for-gtype (glib:gtype-name gtype))))
    (unless flags-type
      (error "Flags ~A is not registered." (glib:gtype-name gtype)))
    (cffi:convert-from-foreign (%value-flags gvalue) flags-type)))

;; This function is called from set-gvalue to set a GFlag value
(defun set-gvalue-flags (gvalue value)
  (let* ((gtype (value-type gvalue))
         (flags-type (glib:symbol-for-gtype (glib:gtype-name gtype))))
    (unless flags-type
      (error "Flags ~A is not registered." (glib:gtype-name gtype)))
    (setf (%value-flags gvalue) (cffi:convert-to-foreign value flags-type))))

;;; ----------------------------------------------------------------------------
;;; GFlagsValue
;;; ----------------------------------------------------------------------------

(cffi:defcstruct flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'flags-value)
      "CStruct"
      (liber:symbol-documentation 'flags-value)
 "@version{2024-06-09}
  @begin{declaration}
(cffi:defcstruct flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:value]{The flags value.}
      @entry[:name]{The name of the value.}
      @entry[:nick]{The nickname of the value.}
    @end{table}
  @end{values}
  @begin{short}
    A structure which contains a single flags value, its name, and its nickname.
  @end{short}
  @see-symbol{g:flags-class}")

(export 'flags-value)

;;; ----------------------------------------------------------------------------
;;; GFlagsClass
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
 "@version{2024-06-09}
  @begin{declaration}
(cffi:defcstruct flags-class
  (:type-class (:pointer (:struct type-class)))
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer flags-value)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:type-class]{The parent class.}
      @entry[:mask]{The mask covering all possible values.}
      @entry[:n-values]{The number of possible values.}
      @entry[:values]{The array of @symbol{g:flags-value} instances describing
        the individual values.}
    @end{table}
  @end{values}
  @begin{short}
    The class of a flags type holds information about its possible values.
  @end{short}
  @see-symbol{g:flags-value}")

(export 'flags-class)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_FLAGS
;;; ----------------------------------------------------------------------------

(declaim (inline type-is-flags))

(defun type-is-flags (gtype)
 #+liber-documentation
 "@version{2024-06-09}
  @argument[type]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{type} is a @code{\"GFlags\"} type.}
  @begin{short}
    Checks whether @arg{type} is a @code{\"GFlags\"} type.
  @end{short}
  @see-class{g:type-t}"
  (eq (type-fundamental gtype) (glib:gtype "GFlags")))

(export 'type-is-flags)

;;; ----------------------------------------------------------------------------
;;; G_IS_FLAGS_CLASS                                        not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE                                      not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE_NAME                                 not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_first_value ()                              not implemented
;;;
;;; Returns the first GFlagsValue which is set in value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_value_by_name ()                            not implemented
;;;
;;; Looks up a GFlagsValue by name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_value_by_nick ()                            not implemented
;;;
;;; Looks up a GFlagsValue by nickname.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_to_string ()                                    not implemented
;;;
;;; Pretty-prints value in the form of the flag names separated by | and sorted.
;;; Any extra bits will be shown at the end as a hexadecimal number.
;;;
;;; This is intended to be used for debugging purposes. The format of the
;;; output may change in the future.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_register_static ()                               not implemented
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_enum_register_static" enum-register-static) type-t
 #+liber-documentation
 "@version{#2013-06-10}
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
;;; g_flags_register_static ()                              not implemented
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_flags_register_static" flags-register-static) type-t
 #+liber-documentation
 "@version{#2013-06-10}
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
;;; g_enum_complete_type_info ()                            not implemented
;;;
;;; This function is meant to be called from the complete_type_info function of
;;; a GTypePlugin implementation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_complete_type_info ()                           not implemented
;;;
;;; This function is meant to be called from the complete_type_info() function
;;; of a GTypePlugin implementation.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.enumeration.lisp -----------------------------------
