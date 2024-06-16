;;; ----------------------------------------------------------------------------
;;; gobject.g-value.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;; Generic values
;;;
;;;     A polymorphic type that can hold values of any other type
;;;
;;; Types and Values
;;;
;;;     GValue
;;;
;;; Functions
;;;
;;;     G_VALUE_INIT                                       not implemented
;;;     G_VALUE_HOLDS
;;;     G_VALUE_TYPE
;;;     G_VALUE_TYPE_NAME
;;;     G_TYPE_IS_VALUE
;;;     G_TYPE_IS_VALUE_ABSTRACT
;;;     G_IS_VALUE                                         not implemented
;;;     G_TYPE_VALUE
;;;     G_TYPE_VALUE_ARRAY                                 not implemented

;;;     g_value_init
;;;     g_value_copy
;;;     g_value_reset
;;;     g_value_unset
;;;     g_value_init_from_instance
;;;     g_value_set_instance                               not implemented
;;;     g_value_fits_pointer                               not implemented
;;;     g_value_peek_pointer                               not implemented
;;;     g_value_type_compatible
;;;     g_value_type_transformable
;;;     g_value_transform
;;;     GValueTransform
;;;     g_value_register_transform_func
;;;     g_strdup_value_contents
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GValue
;;; ----------------------------------------------------------------------------

(in-package :gobject)

(defmacro with-g-value ((var &rest args) &body body)
 #+liber-documentation
 "@version{2023-11-7}
  @syntax[]{gobject:with-g-value (gvalue) body) => result}
  @syntax[]{(gobject:with-g-value (gvalue gtype) body) => result}
  @syntax[]{(gobject:with-g-value (gvalue gtype value) body) => result}
  @argument[gvalue]{a @symbol{g:value} instance to create and initialize}
  @argument[gtype]{an optional @symbol{g:type-t} type}
  @argument[value]{an optional value corresponding to @arg{gtype} to set}
  @begin{short}
    The @fun{gobject:with-g-value} macro allocates a new  @symbol{g:value}
    instance, initializes it with the given values and executes the body that
    uses @arg{gvalue}.
  @end{short}
  After execution of the body the @fun{g:value-unset} function is called on
  @arg{gvalue}. This clears the current value in @arg{gvalue}, unsets the
  type and releases all resources associated with @arg{gvalue}.
  @begin[Note]{dictionary}
    The @arg{gvalue} parameter is initialized with the @fun{g:value-init}
    function and unset with the @fun{g:value-unset} function. The optional
    @arg{value} ist set with the @fun{g:value-set} function.
  @end{dictionary}
  @see-symbol{g:value}
  @see-function{g:value-init}
  @see-function{g:value-unset}
  @see-function{g:value-set}"
  (cond ((null args)
         ;; no arguments
         `(cffi:with-foreign-object (,var '(:struct value))
            (value-init ,var)
            (unwind-protect
              (progn ,@body)
              (value-unset ,var))))
        ((null (second args))
         (let ((gtype (first args)))
           ;; one argument with the gtype
           `(cffi:with-foreign-object (,var '(:struct value))
              (value-init ,var ,gtype)
              (unwind-protect
                (progn ,@body)
                (value-unset ,var)))))
        ((null (third args))
         ;; two argument with the gtype and an initial value to set
         (destructuring-bind (gtype value) args
           `(cffi:with-foreign-object (,var '(:struct value))
              (value-init ,var ,gtype)
              (value-set ,var ,value ,gtype)
              (unwind-protect
                (progn ,@body)
                (value-unset ,var)))))
        (t
          (error "WITH-G-VALUE: Wrong number of arguments"))))

(export 'with-g-value)

(defmacro with-g-values (vars &body body)
 #+liber-documentation
 "@version{2023-11-7}
  @syntax[]{(gobject:with-g-values (gvalue1 gvalue2 ... gvaluen) body) => result}
  @argument[gvalue1 ... gvaluen]{the newly created @symbol{g:value} instances}
  @argument[body]{a body that uses the bindings @arg{gvalue1 ... gvaluen}}
  @begin{short}
    The @fun{gobject:with-g-values} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each @arg{gvalue} can be initialized with a type and a value using the syntax
  for the @fun{gobject:with-g-value} macro.
  @see-macro{gobject:with-g-value}
  @see-symbol{g:value}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-g-value ,var
           (with-g-values ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-g-values)

;;; ----------------------------------------------------------------------------

;; TODO: Consider to implement GValue as a boxed type.

;;; ----------------------------------------------------------------------------

;; A generic function for getting the value of a g-value instance

(defgeneric parse-g-value-for-type (gvalue gtype kind))

(defmethod parse-g-value-for-type :around (gvalue gtype kind)
  (declare (ignorable gvalue kind))
  (assert (typep gtype '(or glib:gtype nil)))
  (call-next-method))

(defmethod parse-g-value-for-type (gvalue gtype kind)
  (if (eq gtype (type-fundamental gtype))
      (call-next-method)
      (parse-g-value-for-type gvalue
                              (type-fundamental gtype)
                              kind)))

(defmethod parse-g-value-for-type
    (gvalue(glib:gtype (eql (glib:gtype "gpointer"))) kind)
  (declare (ignore kind))
  (value-pointer gvalue))

(defmethod parse-g-value-for-type
    (gvalue (glib:gtype (eql (glib:gtype "GParam"))) kind)
  (declare (ignore kind))
  (value-param gvalue))

(defmethod parse-g-value-for-type
    (gvalue (glib:gtype (eql (glib:gtype "GObject"))) kind)
  (declare (ignore kind))
  (value-object gvalue))

(defmethod parse-g-value-for-type
    (gvalue (glib:gtype (eql (glib:gtype "GInterface"))) kind)
  (declare (ignore kind))
  (value-object gvalue))

;;; ----------------------------------------------------------------------------
;;; parse-g-value (gvalue parse-kind)
;;;
;;;
;;; Note:
;;;     It might be more consistent to name this function like the
;;;     corresponding functions as g-value-get ()
;;;
;;; value :
;;;     a C pointer to the GValue structure
;;;
;;; return :
;;;     value contained in the GValue structure. Type of value depends
;;;     on GValue type
;;; ----------------------------------------------------------------------------

(defun parse-g-value (gvalue &key (parse-kind :get-property))
  (let* ((gtype (value-type gvalue))
         (fundamental-type (type-fundamental gtype)))
    (ev-case fundamental-type
      ;; TODO: Do we need a nil value?!
      (nil
       (error "GValue is of invalid type (~A)" (glib:gtype-name gtype)))
      ((glib:gtype "void") nil)
      ((glib:gtype "gchar") (value-char gvalue))
      ((glib:gtype "guchar") (value-uchar gvalue))
      ((glib:gtype "gboolean") (value-boolean gvalue))
      ((glib:gtype "gint") (value-int gvalue))
      ((glib:gtype "guint") (value-uint gvalue))
      ((glib:gtype "glong") (value-long gvalue))
      ((glib:gtype "gulong") (value-ulong gvalue))
      ((glib:gtype "gint64") (value-int64 gvalue))
      ((glib:gtype "guint64") (value-uint64 gvalue))
      ((glib:gtype "GEnum") (value-enum gvalue))
      ((glib:gtype "GFlags") (value-flags gvalue))
      ((glib:gtype "gfloat") (value-float gvalue))
      ((glib:gtype "gdouble") (value-double gvalue))
      ((glib:gtype "gchararray") (value-string gvalue))
      ((glib:gtype "GVariant") (value-variant gvalue))
      (t (parse-g-value-for-type gvalue gtype parse-kind)))))

(defun value-get (gvalue)
 #+liber-documentation
 "@version{2023-7-10}
  @argument[gvalue]{a pointer to the @symbol{g:value} instance}
  @return{The value contained in the @symbol{g:value} instance. The type of
    the value corresponds to the @symbol{g:value} type.}
  @begin{short}
    Parses the @symbol{g:value} structure and returns the corresponding Lisp
    object.
  @end{short}
  This is a more general function which replaces the @code{g:value-...}
  functions. The function is not part of the GObject library.
  @see-symbol{g:value}
  @see-function{g:value-set}"
  (parse-g-value gvalue))

(export 'value-get)

;;; ----------------------------------------------------------------------------

;;; A generic function for setting the value of a GValue structure.

(defgeneric set-g-value-for-type (gvalue gtype value))

(defmethod set-g-value-for-type :around (gvalue gtype value)
  (declare (ignorable gvalue value))
  (assert (typep gtype '(or glib:gtype null)))
  (call-next-method))

(defmethod set-g-value-for-type (gvalue gtype value)
  (if (eq gtype (type-fundamental gtype))
      (call-next-method)
      (set-g-value-for-type gvalue (type-fundamental gtype) value)))

(defmethod set-g-value-for-type
    (gvalue (glib:gtype (eql (glib:gtype "gpointer"))) value)
  (setf (value-pointer gvalue) value))

(defmethod set-g-value-for-type
    (gvalue (glib:gtype (eql (glib:gtype "GParam"))) value)
  (setf (value-param gvalue) value))

(defmethod set-g-value-for-type
    (gvalue (glib:gtype (eql (glib:gtype "GObject"))) value)
  (setf (value-object gvalue) value))

(defmethod set-g-value-for-type
    (gvalue (glib:gtype (eql (glib:gtype "GInterface"))) value)
  (setf (value-object gvalue) value))

;;; ----------------------------------------------------------------------------
;;; set-g-value (gvalue value type zero-gvalue unset-gvalue init-gvalue)
;;;
;;; Assigns the GValue structure gvalue the value value of GType type. This is
;;; a more general function which replaces the functions (setf g-value-...)
;;; The function is not part of the GObject library.
;;;
;;; Note :
;;;     It might be more consistent to name this function like the
;;;     corresponding functions as g-value-set ()
;;;
;;; gvalue :
;;;     a C pointer to the GValue structure
;;;
;;; value :
;;;     a Lisp object that is to be assigned
;;;
;;; type :
;;;     a GType that is to be assigned
;;;
;;; zero-gvalue :
;;;     a boolean specifying whether GValue should be zero-initialized before
;;;     assigning. See g-value-zero.
;;;
;;; unset-gvalue :
;;;     a boolean specifying whether GValue should be 'unset' before assigning.
;;;     See g-value-unset. The 'true' value should not be passed to both
;;;     zero-gvalue and unset-gvalue arguments
;;;
;;; init :
;;;     a boolean specifying where GValue should be initialized
;;; ----------------------------------------------------------------------------

(defun set-g-value (gvalue value gtype &key zero-gvalue
                                            unset-gvalue
                                            (init-gvalue t))
  (setf gtype (glib:gtype gtype))
  (cond (zero-gvalue (%value-zero gvalue))
        (unset-gvalue (value-unset gvalue)))
  (when init-gvalue (value-init gvalue gtype))
  (let ((fundamental-type (type-fundamental gtype)))
    (ev-case fundamental-type
      ;; TODO: Do we need a nil value?!
      (nil (error "Invalid type (~A)" gtype))
      ((glib:gtype "void") nil)
      ((glib:gtype "gchar") (setf (value-char gvalue) value))
      ((glib:gtype "guchar") (setf (value-uchar gvalue) value))
      ((glib:gtype "gboolean") (setf (value-boolean gvalue) value))
      ((glib:gtype "gint") (setf (value-int gvalue) value))
      ((glib:gtype "guint") (setf (value-uint gvalue) value))
      ((glib:gtype "glong") (setf (value-long gvalue) value))
      ((glib:gtype "gulong") (setf (value-ulong gvalue) value))
      ((glib:gtype "gint64") (setf (value-int64 gvalue) value))
      ((glib:gtype "guint64") (setf (value-uint64 gvalue) value))
      ((glib:gtype "GEnum") (setf (value-enum gvalue) value))
      ((glib:gtype "GFlags") (setf (value-flags gvalue) value))
      ((glib:gtype "gfloat")
       (unless (realp value) (error "~A is not a real number" value))
       (setf (value-float gvalue) (coerce value 'single-float)))
      ((glib:gtype "gdouble")
       (unless (realp value) (error "~A is not a real number" value))
       (setf (value-double gvalue) (coerce value 'double-float)))
      ((glib:gtype "gchararray") (setf (value-string gvalue) value))
      ((glib:gtype "GVariant") (setf (value-variant gvalue) value))
      (t (set-g-value-for-type gvalue gtype value)))))

(defun value-set (gvalue value gtype)
 #+liber-documentation
 "@version{2023-11-7}
  @argument[gvalue]{a pointer to the @symbol{g:value} instance}
  @argument[value]{a Lisp object to set as the value of the @symbol{g:value}
    instance}
  @argument[gtype]{a @class{g:type-t} type of the @symbol{g:value} instance}
  @begin{short}
    Parses the @symbol{g:value} structure and sets the corresponding Lisp
    object.
  @end{short}
  This is a more general function which replaces the @code{g:value-...}
  functions. The function is not part of the GObject library.
  @see-symbol{g:value}
  @see-function{g:value-get}"
  (set-g-value gvalue value gtype))

(export 'value-set)

;;; ----------------------------------------------------------------------------
;;; GValue
;;; ----------------------------------------------------------------------------

(cffi:defcunion value-data
  (:int :int)
  (:uint :uint)
  (:long :long)
  (:ulong :ulong)
  (:int64 :int64)
  (:uint64 :uint64)
  (:float :float)
  (:double :double)
  (:pointer :pointer))

;;; ----------------------------------------------------------------------------

;; The generalized calculation of the size and offset works for sbcl and ccl on
;;  a 32-bit Linux. Check this for more system.

#-windows
(cffi:defcstruct (value :size
                  ;; Generalized caluclation of the size of the structure
                  #.(+ (cffi:foreign-type-size 'type-t)
                       (* 2 (cffi:foreign-type-size '(:union value-data)))))
  (:gtype type-t)
  (:data (:union value-data)
         ;; Generalized calculation of the offset
         :offset #.(cffi:foreign-type-size 'type-t) :count 2))

#+windows
(cffi:defcstruct value
  (:gtype type-t)
  (:data (:union value-data) :count 2)) ; Not a pointer. Is this correct?

#+liber-documentation
(setf (liber:alias-for-symbol 'value)
      "CStruct"
      (liber:symbol-documentation 'value)
 "@version{2023-11-7}
  @begin{short}
    The @symbol{g:value} structure is basically a variable container that
    consists of a type identifier and a specific value of that type.
  @end{short}
  The type identifier within a @symbol{g:value} instance always determines the
  type of the associated value. To create a undefined @symbol{g:value} instance,
  simply create a zero-filled @symbol{g:value} instance. To initialize the
  @symbol{g:value} instance, use the @fun{g:value-init} function. A
  @symbol{g:value} instance cannot be used until it is initialized. The basic
  type operations (such as freeing and copying) are determined by the
  @code{GTypeValueTable} associated with the type ID stored in the
  @symbol{g:value} instance. Other @symbol{g:value} operations (such as
  converting values between types) are provided by this interface.

  The data within the @symbol{g:value} instance has protected scope: it is
  accessible only to functions within a @code{GTypeValueTable} structure, or
  implementations of the @code{g:value-*} API. That is, code portions which
  implement new fundamental types. @symbol{g:value} users cannot make any
  assumptions about how data is stored within the 2 element data union, and the
  @class{g:type-t} member should only be accessed through the @fun{g:value-type}
  function.
  @begin[Examples]{dictionary}
    The code in the example program below demonstrates @symbol{g:value}'s
    features.
    @begin{pre}
;; A transformation from an integer to a string
(cffi:defcallback int2string :void ((src (:pointer (:struct g:value)))
                                    (dest (:pointer (:struct g:value))))
  (if (= (g:value-int src) 42)
      (setf (g:value-string dest) \"An important number\")
      (setf (g:value-string dest) \"What is that?\")))

(defun example-g-value ()
  ;; Declare two variables of type g:value.
  (gobject:with-g-values (value1 value2)

    ;; Initialization, setting and reading a value of type g:value
    (g:value-init value1 \"gchararray\")
    (setf (g:value-string value1) \"string\")
    (format t \"value1 = ~A~%\" (g:value-string value1))
    (format t \"gtype  = ~A~%\" (g:value-type value1))
    (format t \"name   = ~A~%~%\" (g:value-type-name value1))

    ;; The same in one step with the G:VALUE-SET function
    (g:value-set value2 \"a second string\" \"gchararray\")
    (format t \"value2 = ~A~%\" (parse-g-value value2))
    (format t \"gtype  = ~A~%\" (g:value-type value2))
    (format t \"name   = ~A~%~%\" (g:value-type-name value2))

    ;; Reuse value1 for an integer value.
    (g:value-unset value1)
    (g:value-init value1 \"gint\")
    (setf (g:value-int value1) 42)
    (format t \"value1 = ~A~%\" (g:value-get value1))
    (format t \"gtype  = ~A~%\" (g:value-type value1))
    (format t \"name   = ~A~%~%\" (g:value-type-name value1))

    ;; The types integer and string are transformable.
    (assert (g:value-type-transformable \"gint\" \"gchararray\"))

    ;; Transform value1 of type integer into value2 which is a string
    (g:value-transform value1 value2)
    (format t \"value1 = ~A~%\" (g:value-get value1))
    (format t \"value2 = ~A~%~%\" (g:value-get value2))

    ;; Some test functions.
    (assert (g:value-holds value1 \"gint\"))
    (format t \"value-holds is ~A~%\" (g:value-holds value1 \"gint\"))
    (format t \"is-value is ~A~%~%\" (g:type-is-value \"gint\"))

    ;; Reuse value2 again for a string.
    (g:value-unset value2)
    (g:value-init value2 \"gchararray\")
    (setf (g:value-string value2) \"string\")
    (format t \"value2 = ~A~%\" (g:value-get value2))

    ;; Register the transformation int2string
    (g:value-register-transform-func \"gint\"
                                     \"gchararray\"
                                     (cffi:callback int2string))
    ;; Try the transformation
    (g:value-transform value1 value2)
    (format t \"value2 = ~A~%~%\" (g:value-get value2))))
    @end{pre}
  @end{dictionary}
  @see-function{g:value-init}
  @see-class{g:type-t}
  @see-function{g:value-type}")

(export 'value)

;;; ----------------------------------------------------------------------------
;;; G_VALUE_INIT
;;;
;;; #define G_VALUE_INIT  { 0, { { 0 } } }
;;;
;;; A GValue must be initialized before it can be used. This macro can be used
;;; as initializer instead of an explicit { 0 } when declaring a variable, but
;;; it cannot be assigned to a variable.
;;;
;;;   GValue value = G_VALUE_INIT;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS()
;;; ----------------------------------------------------------------------------

(defun value-holds (value gtype)
 #+liber-documentation
 "@version{2023-6-25}
  @argument[value]{a @symbol{g:value} instance}
  @argument[gtype]{a @class{g:type-t} type}
  @return{@em{True} if @arg{value} holds a value of @arg{gtype} type.}
  @begin{short}
    Checks if @arg{value} holds or contains a value of @arg{gtype} type.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(cffi:with-foreign-object (value '(:struct g:value))
  (g:value-init value \"gint\")
  (g:value-holds value \"gint\"))
=> t
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}
  @see-class{g:type-t}"
  (eq (glib:gtype gtype) (value-type value)))

(export 'value-holds)

;;; ----------------------------------------------------------------------------
;;; G_VALUE_TYPE()
;;; ----------------------------------------------------------------------------

(defun value-type (value)
 #+liber-documentation
 "@version{2023-6-25}
  @argument[value]{a @symbol{g:value} instance}
  @return{The @class{g:type-t} type of @arg{value}.}
  @begin{short}
    Get the type identifier of @arg{value}.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(cffi:with-foreign-object (value '(:struct g:value))
  (g:value-init value \"gint\")
  (g:value-type value))
=> #<GTYPE :name \"gint\" :id 24>
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}
  @see-class{g:type-t}"
  (cffi:foreign-slot-value value '(:struct value) :gtype))

(export 'value-type)

;;; ----------------------------------------------------------------------------
;;; G_VALUE_TYPE_NAME()
;;; ----------------------------------------------------------------------------

(defun value-type-name (value)
 #+liber-documentation
 "@version{2023-6-25}
  @argument[value]{a @symbol{g:value} instance}
  @return{The type name of @arg{value}.}
  @begin{short}
    Gets the the type name of @arg{value}.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(cffi:with-foreign-object (value '(:struct g:value))
  (g:value-init value \"gint\")
  (g:value-type-name value))
=> \"gint\"
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}"
  (type-name (value-type value)))

(export 'value-type-name)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE()
;;; ----------------------------------------------------------------------------

(defun type-is-value (gtype)
 #+liber-documentation
 "@version{2023-7-10}
  @argument[gtype]{a @class{g:type-t}}
  @return{Whether @arg{gtype} is suitable as a @symbol{g:value} instance type.}
  @begin{short}
    Checks whether the passed in @arg{gtype} ID can be used for the
    @fun{g:value-init} function.
  @end{short}
  That is, this function checks whether this @arg{gtype} provides an
  implementation of the @code{GTypeValueTable} functions required for a type
  to create a @symbol{g:value} instance of.

  This function is equivalent to the @fun{g:type-is-value-type} function.
  @see-class{g:type-t}
  @see-function{g:value-init}
  @see-function{g:type-is-value-type}"
  (%type-check-is-value-type gtype))

(export 'type-is-value)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE_ABSTRACT()
;;; ----------------------------------------------------------------------------

(defun type-is-value-abstract (gtype)
 #+liber-documentation
 "@version{2023-7-10}
  @argument[gtype]{a @class{g:type-t} type}
  @return{@em{True} if @arg{gtype} is an abstract value type.}
  @begin{short}
    Checks if the @arg{gtype} argument is an abstract value type.
  @end{short}
  An abstract value type introduces a value table, but can not be used for
  the @fun{g:value-init} function and is normally used as an abstract base type
  for derived value types.
  @see-class{g:type-t}
  @see-function{g:value-init}"
  (%type-test-flags gtype :value-abstract))

(export 'type-is-value-abstract)

;;; ----------------------------------------------------------------------------
;;; G_IS_VALUE()
;;;
;;; #define G_IS_VALUE(value) (G_TYPE_CHECK_VALUE (value))
;;;
;;; Checks if value is a valid and initialized GValue structure.
;;;
;;; value :
;;;     A GValue structure.
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VALUE
;;; ----------------------------------------------------------------------------

(defun type-value ()
 "@version{2023-7-10}
  @return{The type ID of the \"GValue\" type.}
  @begin{short}
    The type ID of the \"GValue\" type which is a boxed type, used to pass
    around pointers to @symbol{g:value} instances.
  @end{short}
  @see-class{g:type-t}
  @see-symbol{g:value}"
  (glib:gtype (cffi:foreign-funcall "g_value_get_type" :size)))

(glib-init:at-init () (type-value))

(export 'type-value)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VALUE_ARRAY
;;;
;;; #define G_TYPE_VALUE_ARRAY (g_value_array_get_type ())
;;;
;;; Warning
;;;
;;; G_TYPE_VALUE_ARRAY has been deprecated since version 2.32 and should not be
;;; used in newly written code. Use GArray instead of GValueArray
;;;
;;; The type ID of the "GValueArray" type which is a boxed type, used to pass
;;; around pointers to GValueArrays.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_init ()
;;; ----------------------------------------------------------------------------

;; Called from value-init to initialize a GValue to zero

(defun %value-zero (value)
  (iter (for i from 0 below (cffi:foreign-type-size '(:struct value)))
        (setf (cffi:mem-ref value :uchar i) 0)))

(cffi:defcfun ("g_value_init" %value-init) (:pointer (:struct value))
  (value (:pointer (:struct value)))
  (gtype type-t))

;; TODO: The documentation is not complet.

(defun value-init (value &optional (gtype nil))
 #+liber-documentation
 "@version{2023-11-7}
  @argument[value]{an uninitialized @symbol{g:value} instance}
  @argument[gtype]{a @class{g:type-t} type the @arg{value} argument should hold
    values of}
  @return{The @symbol{g:value} instance that has been passed in.}
  @begin{short}
    Initializes @arg{value} with the @arg{gtype} type.
  @end{short}
  @see-symbol{g:value}
  @see-class{g:type-t}"
  (cond ((null gtype)
         (%value-zero value))
        (t
         (%value-zero value)
         (%value-init value gtype)))
  value)

(export 'value-init)

;;; ----------------------------------------------------------------------------
;;; g_value_copy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_copy" value-copy) :void
 #+liber-documentation
 "@version{2023-6-25}
  @argument[src]{an initialized @symbol{g:value} instance}
  @argument[dest]{an initialized @symbol{g:value} instance of the same
    type as @arg{src}}
  @begin{short}
    Copies the value of @arg{src} into @arg{dest}.
  @end{short}
  @see-symbol{g:value}"
  (src (:pointer (:struct value)))
  (dest (:pointer (:struct value))))

(export 'value-copy)

;;; ----------------------------------------------------------------------------
;;; g_value_reset ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_reset" value-reset) (:pointer (:struct value))
 #+liber-documentation
 "@version{2023-7-10}
  @argument[value]{an initialized @symbol{g:value} instance}
  @return{The @symbol{g:value} instance that has been passed in.}
  @begin{short}
    Clears the current value in @arg{value} and resets it to the default value
    as if the value had just been initialized.
  @end{short}
  @see-symbol{g:value}"
  (value (:pointer (:struct value))))

(export 'value-reset)

;;; ----------------------------------------------------------------------------
;;; g_value_unset ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_unset" value-unset) :void
 #+liber-documentation
 "@version{2023-11-7}
  @argument[value]{an initialized @symbol{g:value} instance}
  @begin{short}
    Clears the current value in @arg{value} and \"unsets\" the type, this
    releases all resources associated with this @arg{value} instance.
  @end{short}
  An unset value is the same as an uninitialized (zero-filled) @symbol{g:value}
  instance.
  @see-symbol{g:value}"
  (value (:pointer (:struct value))))

(export 'value-unset)

;;; ----------------------------------------------------------------------------
;;; g_value_init_from_instance ()
;;;
;;; void
;;; g_value_init_from_instance (GValue *value, gpointer instance);
;;;
;;; Initializes and sets value from an instantiatable type via the value_table's
;;; collect_value() function.
;;;
;;; Note: The value will be initialised with the exact type of instance . If you
;;; wish to set the value 's type to a different GType (such as a parent class
;;; GType), you need to manually call g_value_init() and g_value_set_instance().
;;;
;;; value:
;;;     An uninitialized GValue structure.
;;;
;;; instance:
;;;     the instance.
;;;
;;; Since 2.42
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_instance ()
;;;
;;; Sets value from an instantiatable type via the value_table's collect_value()
;;; function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_fits_pointer ()
;;;
;;; gboolean g_value_fits_pointer (const GValue *value);
;;;
;;; Determines if value will fit inside the size of a pointer value. This is
;;; an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     An initialized GValue structure.
;;;
;;; Returns :
;;;     TRUE if value will fit inside a pointer value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_peek_pointer ()
;;;
;;; gpointer g_value_peek_pointer (const GValue *value);
;;;
;;; value :
;;;     An initialized GValue structure.
;;;
;;; Returns :
;;;     the value contents as pointer. This function asserts that
;;;     g_value_fits_pointer() returned TRUE for the passed in value.
;;;     This is an internal function introduced mainly for C marshallers.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_type_compatible ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_type_compatible" value-type-compatible) :boolean
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[src]{a @class{g:type-t} source type to be copied}
  @argument[dest]{a @class{g:type-t} destination type for copying}
  @return{@em{True} if @fun{g:value-copy} is possible with @arg{src} and
    @arg{dest}.}
  @begin{short}
    Returns whether a @symbol{g:value} instance of type @arg{src} can be
    copied into a @symbol{g:value} instance of type @arg{dest}.
  @end{short}
  @see-class{g:type-t}
  @see-symbol{g:value}
  @see-function{g:value-copy}"
  (src type-t)
  (dest type-t))

(export 'value-type-compatible)

;;; ----------------------------------------------------------------------------
;;; g_value_type_transformable ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_type_transformable" value-type-transformable) :boolean
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[src]{a @class{g:type-t} source type}
  @argument[dest]{a @class{g:type-t} target type}
  @return{@em{True} if the transformation is possible, @em{false} otherwise.}
  @begin{short}
    Check whether the @fun{g:value-transform} function is able to transform
    values of type @arg{src} into values of type @arg{dest}.
  @end{short}
  @see-class{g:type-t}
  @see-function{g:value-transform}"
  (src type-t)
  (dest type-t))

(export 'value-type-transformable)

;;; ----------------------------------------------------------------------------
;;; g_value_transform ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_transform" value-transform) :boolean
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[src]{a @symbol{g:value} source value}
  @argument[dest]{a @symbol{g:value} target value}
  @return{Whether a transformation rule was found and could be applied. Upon
    failing transformations, @arg{dest} is left untouched.}
  @begin{short}
    Tries to cast the contents of @arg{src} into a type appropriate to store in
    @arg{dest}, e.g. to transform a \"gint\" value into a \"gfloat\" value.
  @end{short}
  Performing transformations between value types might incur precision
  lossage. Especially transformations into strings might reveal seemingly
  arbitrary results and should not be relied upon for production code.
  @begin[Example]{dictionary}
    @begin{pre}
;; A transformation from an integer to a string
(cffi:defcallback int2string :void ((src-value (:pointer (:struct g:value)))
                                    (dest-value (:pointer (:struct g:value))))
  (if (= (g:value-int src-value) 42)
      (setf (g:value-string dest-value) \"An important number\")
      (setf (g:value-string dest-value) \"What is that?\")))

;; Register the transformation int2string
(g:value-register-transform-func \"gint\"
                                 \"gchararray\"
                                 (cffi:callback int2string))
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}
  @see-function{g:value-register-transform-func}"
  (src (:pointer (:struct value)))
  (dest (:pointer (:struct value))))

(export 'value-transform)

;;; ----------------------------------------------------------------------------
;;; GValueTransform ()
;;;
;;; void
;;; (*GValueTransform) (const GValue *src_value,
;;;                     GValue *dest_value);
;;;
;;; The type of value transformation functions which can be registered with
;;; g_value_register_transform_func().
;;;
;;; dest_value will be initialized to the correct destination type.
;;;
;;; src_value :
;;;     Source value.
;;;
;;; dest_value :
;;;     Target value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_register_transform_func ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_register_transform_func" value-register-transform-func)
    :void
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[src]{a @class{g:type-t} source type}
  @argument[dest]{a @class{g:type-t} target type}
  @argument[func]{a callback function which transforms values of type
    @arg{src} into values of type @arg{dest}}
  @begin{short}
    Registers a value transformation function for use in the
    @fun{g:value-transform} function.
  @end{short}
  A previously registered transformation function for @arg{src} and @arg{dest}
  will be replaced.
  @see-class{g:type-t}
  @see-function{g:value-transform}"
  (src type-t)
  (dest type-t)
  (func :pointer))

(export 'value-register-transform-func)

;;; ----------------------------------------------------------------------------
;;; g_strdup_value_contents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_strdup_value_contents" strdup-value-contents) :string
 #+liber-documentation
 "@version{#2022-12-29}
  @argument[value]{a @symbol{g:value} instance which contents are to be
    described}
  @return{The string with the contents of @arg{value}.}
  @begin{short}
    Return a string, which describes the contents of a @symbol{g:value}
    instance.
  @end{short}
  The main purpose of this function is to describe @symbol{g:value} contents
  for debugging output, the way in which the contents are described may change
  between different GLib versions.
  @see-symbol{g:value}"
  (value (:pointer (:struct value))))

(export 'strdup-value-contents)

;;; --- End of file gobject.gvalue.lisp ----------------------------------------
