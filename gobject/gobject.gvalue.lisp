;;; ----------------------------------------------------------------------------
;;; gobject.gvalue.lisp
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
;;;     G_VALUE_INIT                                        not implemented
;;;     G_VALUE_HOLDS
;;;     G_VALUE_TYPE
;;;     G_VALUE_TYPE_NAME
;;;     G_TYPE_IS_VALUE
;;;     G_TYPE_IS_VALUE_ABSTRACT                            not implemented
;;;     G_IS_VALUE                                          not implemented
;;;     G_TYPE_VALUE                                        not implemented
;;;     G_TYPE_VALUE_ARRAY                                  not implemented
;;;
;;;     g_value_init
;;;     g_value_copy
;;;     g_value_reset
;;;     g_value_unset
;;;     g_value_init_from_instance                          not implemented
;;;     g_value_set_instance                                not implemented
;;;     g_value_fits_pointer                                not implemented
;;;     g_value_peek_pointer                                not implemented
;;;     g_value_type_compatible
;;;     g_value_type_transformable
;;;     GValueTransform
;;;     g_value_register_transform_func
;;;     g_value_transform
;;;     g_strdup_value_contents
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GValue
;;; ----------------------------------------------------------------------------

(in-package :gobject)

(defmacro with-value ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-08-23}
  @syntax{(g:with-value (gvalue) body) => result}
  @syntax{(g:with-value (gvalue gtype) body) => result}
  @syntax{(g:with-value (gvalue gtype value) body) => result}
  @argument[gvalue]{a @sym{g:value} instance to create and initialize}
  @argument[gtype]{an optional @sym{g:type-t} type ID}
  @argument[value]{an optional value corresponding to @arg{gtype} to set}
  @begin{short}
    The @fun{g:with-value} macro allocates a new @sym{g:value} instance,
    initializes it with the given values and executes the body that uses
    @arg{gvalue}.
  @end{short}
  After execution of the body the @fun{g:value-unset} function is called on
  @arg{gvalue}. This clears the current value in @arg{gvalue}, unsets the
  type and releases all resources associated with @arg{gvalue}.
  @begin[Notes]{dictionary}
    The @arg{gvalue} parameter is initialized with the @fun{g:value-init}
    function and unset with the @fun{g:value-unset} function. The optional
    @arg{value} is set with the @fun{g:value-set} function.
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
           ;; one argument for the gtype
           `(cffi:with-foreign-object (,var '(:struct value))
              (value-init ,var ,gtype)
              (unwind-protect
                (progn ,@body)
                (value-unset ,var)))))
        ((null (third args))
         ;; two arguments for the gtype and an initial value to set
         (destructuring-bind (gtype value) args
           `(cffi:with-foreign-object (,var '(:struct value))
              (value-init ,var ,gtype)
              (value-set ,var ,value ,gtype)
              (unwind-protect
                (progn ,@body)
                (value-unset ,var)))))
        (t
          (error "GOBJECT:WITH-VALUE: Wrong number of arguments"))))

(export 'with-value)

(defmacro with-values (vars &body body)
 #+liber-documentation
 "@version{2025-08-23}
  @syntax{(g:with-values (gvalue1 gvalue2 ... gvaluen) body) => result}
  @argument[gvalue1 ... gvaluen]{newly created @sym{g:value} instances}
  @argument[body]{a body that uses the bindings @arg{gvalue1 ... gvaluen}}
  @begin{short}
    The @fun{g:with-values} macro creates new variable bindings and executes
    the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each @arg{gvalue} can be initialized with a type and a value using the syntax
  for the @fun{g:with-value} macro.
  @see-macro{g:with-value}
  @see-symbol{g:value}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-value ,var
           (with-values ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-values)

;;; ----------------------------------------------------------------------------

;; Generic functions for getting the value of a GValue instance

(defgeneric get-gvalue-for-type (gvalue gtype))

(defmethod get-gvalue-for-type :around (gvalue gtype)
  (declare (ignorable gvalue gtype))
  (assert (typep gtype '(or glib:gtype nil)))
  (call-next-method))

(defmethod get-gvalue-for-type (gvalue gtype)
  (if (eq gtype (type-fundamental gtype))
      (call-next-method)
      (get-gvalue-for-type gvalue (type-fundamental gtype))))

(defmethod get-gvalue-for-type
           (gvalue (glib:gtype (eql (glib:gtype "gpointer"))))
  (cond (;; Handle a GType type
         (eq (value-type gvalue) (glib:gtype "GType"))
         (value-gtype gvalue))
        (t
         (value-pointer gvalue))))

(defmethod get-gvalue-for-type
           (gvalue (glib:gtype (eql (glib:gtype "GParam"))))
  (value-param gvalue))

(defmethod get-gvalue-for-type
           (gvalue (glib:gtype (eql (glib:gtype "GObject"))))
  (value-object gvalue))

(defmethod get-gvalue-for-type
           (gvalue (glib:gtype (eql (glib:gtype "GInterface"))))
  (value-object gvalue))

;;; ----------------------------------------------------------------------------

(defun get-gvalue (gvalue gtype)
  (let ((ftype (type-fundamental gtype)))
    (assert ftype nil "GValue with invalid GType (~a)" (glib:gtype-name gtype))
    (ev-case ftype
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
      (t (get-gvalue-for-type gvalue gtype)))))

(defun (setf value-get) (value gvalue &optional gtype)
  (let ((gtype (or (glib:gtype gtype) (value-type gvalue))))
    (value-set gvalue value gtype)))

(defun value-get (gvalue &optional gtype)
 #+liber-documentation
 "@version{2025-08-23}
  @syntax{(g:value-get gvalue) => value)}
  @syntax{(setf (g:value-get gvalue) value)}
  @argument[gvalue]{an initialized @sym{g:value} instance}
  @argument[value]{a value which corresponds to the type of @arg{gvalue}}
  @begin{short}
    The @fun{g:value-get} function parses the @sym{g:value} instance and returns
    the corresponding Lisp value.
  @end{short}
  The @setf{g:value-get} function sets the value of an initialized
  @sym{g:value} instance.

  Use the @fun{g:value-set} function for initialization and setting a
  @sym{g:value} instance in one step.
  @begin[Notes]{dictionary}
    This function replaces the @code{g_value_get_<type>()} funcions of the C
    API.
  @end{dictionary}
  @see-symbol{g:value}
  @see-function{g:value-set}"
  (let ((gtype (or (glib:gtype gtype) (value-type gvalue))))
    (get-gvalue gvalue gtype)))

(export 'value-get)

;;; ----------------------------------------------------------------------------

;; Generic functions for setting the value of a GValue instance

(defgeneric set-gvalue-for-type (gvalue gtype value))

(defmethod set-gvalue-for-type :around (gvalue gtype value)
  (declare (ignorable gvalue value))
  (assert (typep gtype '(or glib:gtype null)))
  (call-next-method))

(defmethod set-gvalue-for-type (gvalue gtype value)
  (if (eq gtype (type-fundamental gtype))
      (call-next-method)
      (set-gvalue-for-type gvalue (type-fundamental gtype) value)))

(defmethod set-gvalue-for-type
           (gvalue (glib:gtype (eql (glib:gtype "gpointer"))) value)
  (cond (;; Handle a GType type
         (eq (value-type gvalue) (glib:gtype "GType"))
         (setf (value-gtype gvalue) value))
        (t
         (setf (value-pointer gvalue) value))))

(defmethod set-gvalue-for-type
           (gvalue (glib:gtype (eql (glib:gtype "GParam"))) value)
  (setf (value-param gvalue) value))

(defmethod set-gvalue-for-type
           (gvalue (glib:gtype (eql (glib:gtype "GObject"))) value)
  (setf (value-object gvalue) value))

(defmethod set-gvalue-for-type
           (gvalue (glib:gtype (eql (glib:gtype "GInterface"))) value)
  (setf (value-object gvalue) value))

;;; ----------------------------------------------------------------------------

(defun set-gvalue (gvalue value gtype)
  (let ((ftype (type-fundamental gtype)))
    (assert ftype nil "GValue with invalid GType (~a)" (glib:gtype-name gtype))
    (value-init gvalue gtype)
    (ev-case ftype
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
      (t (set-gvalue-for-type gvalue gtype value)))))

(defun value-set (gvalue value gtype)
 #+liber-documentation
 "@version{2025-08-23}
  @argument[gvalue]{a @sym{g:value} instance to initialize and to set}
  @argument[value]{a value to set as the value of the @sym{g:value} instance}
  @argument[gtype]{a @class{g:type-t} type ID for the @sym{g:value} instance}
  @begin{short}
    The @fun{g:value-set} function initializes the @sym{g:value} instance and
    sets the value.
  @end{short}
  Unlike the @setf{g:value-get} function, this function first intializies the
  @sym{g:value} instance for a value of @arg{gtype} type.
  @begin[Notes]{dictionary}
    This function replaces the @code{g_value_set_<type>()} functions of the
    C API.
  @end{dictionary}
  @see-symbol{g:value}
  @see-function{g:value-get}"
  (let ((gtype (glib:gtype gtype)))
    (set-gvalue gvalue value gtype)))

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
 "@version{2025-08-23}
  @begin{short}
    The @sym{g:value} structure is a variable container that consists of a type
    identifier and a specific value of that type.
  @end{short}
  The type identifier within a @sym{g:value} instance determines the type of the
  associated value. To create an undefined @sym{g:value} instance, simply create
  a zero-filled @sym{g:value} instance. To initialize the
  @sym{g:value} instance, use the @fun{g:value-init} function. A @sym{g:value}
  instance cannot be used until it is initialized.
  @begin[Examples]{dictionary}
    These examples demonstrate some of the features.
    @begin{pre}
(defun example-gvalue ()
  ;; Declare two variables of type g:value
  (gobject:with-values (value1 value2)
    ;; Initialization, setting and reading a value of type g:value
    (g:value-set value1 \"string\" \"gchararray\")
    (format t \"value1 = ~a~%\" (g:value-get value1))
    (format t \"gtype  = ~a~%\" (g:value-type value1))
    (format t \"name   = ~a~%~%\" (g:value-type-name value1))

    ;; The same for the second value
    (g:value-init value2 \"gchararray\")
    (setf (g:value-get value2) \"a second string\")
    (format t \"value2 = ~a~%\" (g:value-get value2))
    (format t \"gtype  = ~a~%\" (g:value-type value2))
    (format t \"name   = ~a~%~%\" (g:value-type-name value2))

    ;; Reuse value1 for an integer
    (g:value-unset value1)
    (g:value-set value1 42 \"gint\" )
    (format t \"value1 = ~a~%\" (g:value-get value1))
    (format t \"gtype  = ~a~%\" (g:value-type value1))
    (format t \"name   = ~a~%~%\" (g:value-type-name value1))

    ;; Some test functions
    (assert (g:value-holds value1 \"gint\"))
    (format t \"value holds integer is ~a~%\" (g:value-holds value1 \"gint\"))
    (format t \"value is integer ~a~%~%\" (g:type-is-value \"gint\"))))
    @end{pre}
  @end{dictionary}
  @see-function{g:value-init}
  @see-class{g:type-t}
  @see-function{g:value-type}")

(export 'value)

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS
;;; ----------------------------------------------------------------------------

(defun value-holds (gvalue gtype)
 #+liber-documentation
 "@version{2025-08-23}
  @argument[gvalue]{a @sym{g:value} instance}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if @arg{gvalue} holds a value of @arg{gtype} type.}
  @begin{short}
    Checks if @arg{gvalue} holds or contains a value of @arg{gtype} type.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:with-value (gvalue \"gint\")
  (g:value-holds gvalue \"gint\"))
=> T
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}
  @see-class{g:type-t}"
  (eq (glib:gtype gtype) (value-type gvalue)))

(export 'value-holds)

;;; ----------------------------------------------------------------------------
;;; G_VALUE_TYPE
;;; ----------------------------------------------------------------------------

(defun value-type (gvalue)
 #+liber-documentation
 "@version{2025-08-23}
  @argument[gvalue]{a @sym{g:value} instance}
  @return{The @class{g:type-t} type ID of @arg{gvalue}.}
  @begin{short}
    Get the type identifier of @arg{gvalue}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:with-value (gvalue \"gint\")
  (g:value-type gvalue))
=> #<GTYPE :name \"gint\" :id 24>
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}
  @see-class{g:type-t}"
  (cffi:foreign-slot-value gvalue '(:struct value) :gtype))

(export 'value-type)

;;; ----------------------------------------------------------------------------
;;; G_VALUE_TYPE_NAME
;;; ----------------------------------------------------------------------------

(defun value-type-name (gvalue)
 #+liber-documentation
 "@version{2025-08-23}
  @argument[gvalue]{a @sym{g:value} instance}
  @return{The type name of @arg{gvalue}.}
  @begin{short}
    Gets the the type name of @arg{gvalue}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:with-value (gvalue \"gint\")
  (g:value-type-name gvalue))
=> \"gint\"
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}"
  (type-name (value-type gvalue)))

(export 'value-type-name)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the G:TYPE-IS-VALUE-TYPE function

(defun type-is-value (gtype)
 #+liber-documentation
 "@version{2025-08-23}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{Whether @arg{gtype} is suitable as a @sym{g:value} instance type.}
  @begin{short}
    Checks whether the passed in @arg{gtype} ID can be used for the
    @fun{g:value-init} function.
  @end{short}

  This function is equivalent to the @fun{g:type-is-value-type} function.
  @see-class{g:type-t}
  @see-function{g:value-init}
  @see-function{g:type-is-value-type}"
  (%type-check-is-value-type gtype))

(export 'type-is-value)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE_ABSTRACT
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_VALUE
;;;
;;; Checks if value is a valid and initialized GValue structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VALUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_init
;;; ----------------------------------------------------------------------------

(defun %value-zero (value)
  (iter (for i from 0 below (cffi:foreign-type-size '(:struct value)))
        (setf (cffi:mem-ref value :uchar i) 0)))

(cffi:defcfun ("g_value_init" %value-init) (:pointer (:struct value))
  (value (:pointer (:struct value)))
  (gtype type-t))

 (defun value-init (gvalue &optional gtype)
 #+liber-documentation
 "@version{2025-08-23}
  @argument[gvalue]{an uninitialized @sym{g:value} instance}
  @argument[gtype]{an optional @class{g:type-t} type ID the @arg{gvalue}
    argument should hold values of, or the default @code{nil} value}
  @return{The @sym{g:value} instance that has been passed in.}
  @begin{short}
    Sets @arg{gvalue} to zero and initializes it with the @arg{gtype} type.
  @end{short}
  If the @arg{gtype} argument is @code{nil}, the @arg{gvalue} argument is set
  to zero but not initialized.
  @see-symbol{g:value}
  @see-class{g:type-t}"
   (%value-zero gvalue)
   (if gtype
       (%value-init gvalue gtype)
       gvalue))

(export 'value-init)

;;; ----------------------------------------------------------------------------
;;; g_value_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_copy" value-copy) :void
 #+liber-documentation
 "@version{2025-08-23}
  @argument[src]{an initialized @sym{g:value} instance}
  @argument[dest]{an initialized @sym{g:value} instance of the same type as
    @arg{src}}
  @begin{short}
    Copies the value of @arg{src} into @arg{dest}.
  @end{short}
  @see-symbol{g:value}"
  (src (:pointer (:struct value)))
  (dest (:pointer (:struct value))))

(export 'value-copy)

;;; ----------------------------------------------------------------------------
;;; g_value_reset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_reset" value-reset) (:pointer (:struct value))
 #+liber-documentation
 "@version{2025-08-23}
  @argument[gvalue]{an initialized @sym{g:value} instance}
  @return{The @sym{g:value} instance that has been passed in.}
  @begin{short}
    Clears the current value in @arg{gvalue} and resets it to the default value
    as if the value had just been initialized.
  @end{short}
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-reset)

;;; ----------------------------------------------------------------------------
;;; g_value_unset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_unset" value-unset) :void
 #+liber-documentation
 "@version{2025-08-23}
  @argument[value]{an initialized @sym{g:value} instance}
  @begin{short}
    Clears the current value in @arg{gvalue} and \"unsets\" the type, this
    releases all resources associated with this @arg{gvalue} instance.
  @end{short}
  An unset value is the same as an uninitialized (zero-filled) @sym{g:value}
  instance.
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'value-unset)

;;; ----------------------------------------------------------------------------
;;; g_value_init_from_instance
;;;
;;; Initializes and sets value from an instantiatable type via the value_table's
;;; collect_value() function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_instance
;;;
;;; Sets value from an instantiatable type via the value_table's collect_value()
;;; function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_fits_pointer
;;;
;;; Determines if value will fit inside the size of a pointer value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_peek_pointer
;;;
;;; Returns the value contents as pointer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_type_compatible
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_value_type_compatible" value-type-compatible) :boolean
 #+liber-documentation
 "@version{2025-08-23}
  @argument[src]{a @class{g:type-t} source type ID to be copied}
  @argument[dest]{a @class{g:type-t} destination type ID for copying}
  @return{@em{True} if the @fun{g:value-copy} function is possible with
    @arg{src} and @arg{dest}.}
  @begin{short}
    Returns whether a @sym{g:value} instance of type @arg{src} can be copied
    into a @sym{g:value} instance of type @arg{dest}.
  @end{short}
  @see-class{g:type-t}
  @see-symbol{g:value}
  @see-function{g:value-copy}"
  (src type-t)
  (dest type-t))

(export 'value-type-compatible)

;;; ----------------------------------------------------------------------------
;;; g_value_type_transformable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GValueTransform
;;;
;;; The type of value transformation functions which can be registered with
;;; g_value_register_transform_func().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_register_transform_func
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_transform
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_strdup_value_contents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_strdup_value_contents" strdup-value-contents) :string
 #+liber-documentation
 "@version{2025-08-23}
  @argument[gvalue]{a @sym{g:value} instance which contents are to be described}
  @return{The string for the contents of @arg{gvalue}.}
  @begin{short}
    Returns a string, which describes the contents of a @sym{g:value} instance.
  @end{short}
  The main purpose of this function is to describe @sym{g:value} contents for
  debugging output, the way in which the contents are described may change
  between different GLib versions.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:with-values ((value1 \"gboolean\" nil)
                (value2 \"gint\" 199)
                (value3 \"gdouble\" 2.0)
                (value4 \"gchararray\" \"string\"))
  (values (g:strdup-value-contents value1)
          (g:strdup-value-contents value2)
          (g:strdup-value-contents value3)
          (g:strdup-value-contents value4)))
=> \"FALSE\"
=> \"199\"
=> \"2.000000\"
=> \"\"string\"\"
    @end{pre}
  @end{dictionary}
  @see-symbol{g:value}"
  (gvalue (:pointer (:struct value))))

(export 'strdup-value-contents)

;;; --- End of file gobject.gvalue.lisp ----------------------------------------
