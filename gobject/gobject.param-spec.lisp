;;; ----------------------------------------------------------------------------
;;; gobject.param-spec.lisp
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
;;; GParamSpec
;;;
;;;     Metadata for parameter specifications
;;;
;;; Types and Values
;;;
;;;     GParamSpec
;;;     GParamFlags
;;;
;;; Functions
;;;
;;;     G_TYPE_IS_PARAM
;;;     G_IS_PARAM_SPEC
;;;
;;;     G_PARAM_SPEC_TYPE
;;;     G_PARAM_SPEC_TYPE_NAME
;;;     G_PARAM_SPEC_VALUE_TYPE
;;;
;;;     g_param_spec_ref
;;;     g_param_spec_unref
;;;     g_param_spec_sink
;;;     g_param_spec_ref_sink
;;;     g_param_spec_get_default_value
;;;     g_param_value_set_default
;;;     g_param_value_defaults
;;;     g_param_value_validate
;;;     g_param_spec_get_name
;;;     g_param_spec_get_nick
;;;     g_param_spec_get_blurb
;;;     g_param_spec_internal
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;; Register Lisp symbol for fundamental "GParam" type
(glib-init:at-init ()
  (setf (glib:symbol-for-gtype "GParam") 'param))

(export 'param)

;;; ----------------------------------------------------------------------------
;;; GParamFlags
;;; ----------------------------------------------------------------------------

(cffi:defbitfield param-flags
  (:readable #.(ash 1 0))
  (:writable #.(ash 1 1))
  (:construct #.(ash 1 2))
  (:construct-only #.(ash 1 3))
  (:lax-validation #.(ash 1 4))
  (:static-name #.(ash 1 5))
  (:static-nick #.(ash 1 6))
  (:static-blurb #.(ash 1 7))
  (:deprecated #.(ash 1 31)))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-flags)
      "Bitfield"
      (liber:symbol-documentation 'param-flags)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defbitfield param-flags
  (:readable #.(ash 1 0))
  (:writable #.(ash 1 1))
  (:construct #.(ash 1 2))
  (:construct-only #.(ash 1 3))
  (:lax-validation #.(ash 1 4))
  (:static-name #.(ash 1 5))
  (:static-nick #.(ash 1 6))
  (:static-blurb #.(ash 1 7))
  (:deprecated #.(ash 1 31)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:readable]{The parameter is readable.}
      @entry[:writable]{The parameter is writable.}
      @entry[:construct]{The parameter will be set upon object construction.}
      @entry[:construct-only]{The parameter will only be set upon object
        construction.}
      @entry[:lax-validation]{Upon parameter conversion strict validation is
        not required.}
      @entry[:static-name]{The string used as name when constructing the
        parameter is guaranteed to remain valid and unmodified for the lifetime
        of the parameter.}
      @entry[:static-nick]{The string used as nick when constructing the
        parameter is guaranteed to remain valid and unmmodified for the
        lifetime of the parameter.}
      @entry[:static-blurb]{The string used as blurb when constructing the
        parameter is guaranteed to remain valid and unmodified for the lifetime
        of the parameter.}
      @entry[:deprecated]{The parameter is deprecated and will be removed in a
        future version.}
    @end{table}
  @end{values}
  @begin{short}
    Through the @symbol{g:param-flags} flag values, certain aspects of
    parameters can be configured.
  @end{short}
  @see-symbol{g:param-spec}")

(export 'param-flags)

;;; ----------------------------------------------------------------------------
;;; GParamSpec
;;; ----------------------------------------------------------------------------

(cffi:defcstruct param-spec
  (:type-instance (:pointer (:struct type-instance)))
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:flags param-flags)
  (:value-type type-t)
  (:owner-type type-t))

#+liber-documentation
(setf (liber:alias-for-symbol 'param-spec)
      "CStruct"
      (liber:symbol-documentation 'param-spec)
 "@version{2024-12-22}
  @begin{declaration}
(cffi:defcstruct param-spec
  (:type-instance (:pointer (:struct type-instance)))
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:flags param-flags)
  (:value-type type-t)
  (:owner-type type-t))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:type-instance]{The private @symbol{g:type-instance} portion.}
      @entry[:name]{The name of this parameter, always an interned string.}
      @entry[:flags]{The @symbol{g:param-flags} value for this parameter.}
      @entry[:value-type]{The @class{g:type-t} type ID for the @symbol{g:value}
        instance for this parameter.}
      @entry[:owner-type]{The @class{g:type-t} type ID that uses this
        parameter.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{g:param-spec} structure is an object structure that encapsulates
    the metadata required to specify parameters, such as, for example
    @class{g:object} properties.
  @end{short}

  Parameter names need to start with a letter (a-z or A-Z). Subsequent
  characters can be letters, numbers or a '-'. All other characters are
  replaced by a '-' during construction. The result of this replacement is
  called the canonical name of the parameter.
  @see-symbol{g:type-instance}
  @see-symbol{g:param-flags}
  @see-symbol{g:value}
  @see-class{g:type-t}")

(export 'param-spec)

;;; ----------------------------------------------------------------------------

;; Corresponding Lisp structure describing a property of a GObject class.

;; TODO: %PARAM-SPEC is the structure to access a GParamSpec from the Lisp side.
;; Should this be exported and not the implementatin PARAM-SPEC which
;; corresponds to the C side?

(defstruct %param-spec
  name
  type
  readable
  writable
  constructor
  constructor-only
  owner-type)

(defmethod print-object ((instance %param-spec) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (instance stream)
        (format stream
                "PROPERTY ~A ~A . ~A (flags:~@[~* readable~]~@[~* writable~]~@[~* constructor~]~@[~* constructor-only~])"
                (glib:gtype-name (%param-spec-type instance))
                (%param-spec-owner-type instance)
                (%param-spec-name instance)
                (%param-spec-readable instance)
                (%param-spec-writable instance)
                (%param-spec-constructor instance)
                (%param-spec-constructor-only instance)))))

;; Transform a value of the C type GParamSpec to Lisp type %param-spec

(defun parse-param-spec (param)
  (assert (not (cffi:null-pointer-p param))
          nil
          "PARSE-PARAM-SPEC: argument is a NULL pointer")
  (let ((flags (cffi:foreign-slot-value param '(:struct param-spec) :flags)))
    (make-%param-spec
      :name (cffi:foreign-slot-value param '(:struct param-spec) :name)
      :type (cffi:foreign-slot-value param '(:struct param-spec) :value-type)
      :readable (not (null (member :readable flags)))
      :writable (not (null (member :writable flags)))
      :constructor (not (null (member :construct flags)))
      :constructor-only (not (null (member :construct-only flags)))
      :owner-type (cffi:foreign-slot-value param
                                           '(:struct param-spec) :owner-type))))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_PARAM
;;; ----------------------------------------------------------------------------

(defun type-is-param (gtype)
 #+liber-documentation
 "@version{2024-12-22}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Checks whether @arg{gtype} is a @code{\"GParam\"} type.
  @end{short}
  @see-symbol{g:param-spec}
  @see-class{g:type-t}"
  (eq (type-fundamental gtype) (glib:gtype "GParam")))

(export 'type-is-param)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC
;;; ----------------------------------------------------------------------------

(defun is-param-spec (pspec)
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a @symbol{g:param-spec} instance}
  @begin{short}
    Checks whether @arg{pspec} is a valid @symbol{g:param-spec} instance of
    @code{\"GParam\"} type or derived.
  @end{short}
  @see-symbol{g:param-spec}"
  (type-is-param (type-from-instance pspec)))

(export 'is-param-spec)

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_TYPE
;;; ----------------------------------------------------------------------------

(defun param-spec-type (pspec)
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @short{Retrieves the @class{g:type-t} type ID of @arg{pspec}.}
  @see-symbol{g:param-spec}
  @see-class{g:type-t}"
  (type-from-instance pspec))

(export 'param-spec-type)

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_TYPE_NAME
;;; ----------------------------------------------------------------------------

(defun param-spec-type-name (pspec)
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @short{Retrieves the @class{g:type-t} type ID name of @arg{pspec}.}
  @see-symbol{g:param-spec}
  @see-class{g:type-t}"
  (type-name (param-spec-type pspec)))

(export 'param-spec-type-name)

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_VALUE_TYPE
;;; ----------------------------------------------------------------------------

(defun param-spec-value-type (pspec)
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @short{Retrieves the @class{g:type-t} type ID to initialize a @symbol{g:value}
    instance for this parameter.}
  @see-symbol{g:param-spec}
  @see-class{g:type-t}
  @see-symbol{g:value}"
  (cffi:foreign-slot-value pspec '(:struct param-spec) :value-type))

(export 'param-spec-value-type)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_ref" param-spec-ref)
    (:pointer (:struct param-spec))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @return{The @symbol{g:param-spec} instance that was passed into this
    function.}
  @begin{short}
    Increments the reference count of @arg{pspec}.
  @end{short}
  @see-symbol{g:param-spec}"
  (pspec (:pointer (:struct param-spec))))

(export 'param-spec-ref)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_unref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_unref" param-spec-unref) :void
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @begin{short}
    Decrements the reference count of a pspec.
  @end{short}
  @see-symbol{g:param-spec}
  @see-function{g:param-spec-ref}"
  (pspec (:pointer (:struct param-spec))))

(export 'param-spec-unref)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_sink
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ref_sink
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_ref_sink" param-spec-ref-sink)
    (:pointer (:struct param-spec))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @return{The @symbol{g:param-spec} instance that was passed into this function}
  @begin{short}
    Convenience function to ref and sink a @symbol{g:param-spec} instance.
  @end{short}
  @see-symbol{g:param-spec}
  @see-symbol{g:param-spec-ref}"
  (pspec (:pointer (:struct param-spec))))

(export 'param-spec-ref-sink)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_default_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_get_default_value" param-spec-default-value)
    (:pointer (:struct value))
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @return{The @symbol{g:value} instance with the default value.}
  @begin{short}
    Gets the default value of @arg{pspec} as a @symbol{g:value} instance.
  @end{short}
  @see-symbol{g:param-spec}
  @see-symbol{g:value}"
  (pspec (:pointer (:struct param-spec))))

(export 'param-spec-default-value)

;;; ----------------------------------------------------------------------------
;;; g_param_value_set_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_value_set_default" param-value-set-default) :void
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @argument[value]{a @symbol{g:value} instance of correct type for @arg{pspec}}
  @short{Sets @arg{value} to its default value as specified in @arg{pspec}.}
  @see-symbol{g:param-spec}
  @see-symbol{g:value}"
  (pspec (:pointer (:struct param-spec)))
  (value (:pointer (:struct value))))

(export 'param-value-set-default)

;;; ----------------------------------------------------------------------------
;;; g_param_value_defaults
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_value_defaults" param-value-defaults) :boolean
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @argument[value]{a @symbol{g:value} instance of correct type for @arg{pspec}}
  @return{The boolean whether @arg{value} contains the canonical default for
     @arg{pspec}.}
  @begin{short}
    Checks whether @arg{value} contains the default value as specified in
    @arg{pspec}.
  @end{short}
  @see-symbol{g:param-spec}
  @see-symbol{g:value}"
  (pspec (:pointer (:struct param-spec)))
  (value (:pointer (:struct value))))

(export 'param-value-defaults)

;;; ----------------------------------------------------------------------------
;;; g_param_value_validate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_value_validate" param-value-validate) :boolean
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @argument[value]{a @symbol{g:value} instance of correct type for @arg{pspec}}
  @return{The boolean whether modifying @arg{value} was necessary to ensure
    validity.}
  @begin{short}
    Ensures that the contents of @arg{value} comply with the specifications set
    out by @arg{pspec}.
  @end{short}

  For example, a @symbol{g:param-spec-int} instance might require that integers
  stored in @arg{value} may not be smaller than -42 and not be greater than +42.
  If @arg{value} contains an integer outside of this range, it is modified
  accordingly, so the resulting value will fit into the range -42 ... +42.
  @see-symbol{g:param-spec}
  @see-symbol{g:value}
  @see-symbol{g:param-spec-int}"
  (pspec (:pointer (:struct param-spec)))
  (value (:pointer (:struct value))))

(export 'param-value-validate)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_get_name" param-spec-name) :string
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @return{The string with the name of @arg{pspec}.}
  @begin{short}
    Gets the name of a @symbol{g:param-spec} instance.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(mapcar #'g:param-spec-name
        (g:object-class-list-properties \"GtkApplication\"))
=> (\"application-id\" \"flags\" \"resource-base-path\" \"is-registered\"
    \"is-remote\" \"inactivity-timeout\" \"action-group\" \"is-busy\"
    \"register-session\" \"screensaver-active\" \"menubar\" \"active-window\")
    @end{pre}
  @end{dictionary}
  @see-symbol{g:param-spec}"
  (pspec (:pointer (:struct param-spec))))

(export 'param-spec-name)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_nick
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_get_nick" param-spec-nick) :string
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @return{The string with the nickname of @arg{pspec}.}
  @short{Get the nickname of a @symbol{g:param-spec} instance.}
  @see-symbol{g:param-spec}"
  (pspec (:pointer (:struct param-spec))))

(export 'param-spec-nick)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_blurb
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_get_blurb" param-spec-blurb) :string
 #+liber-documentation
 "@version{2024-12-22}
  @argument[pspec]{a valid @symbol{g:param-spec} instance}
  @return{The string with the short description of @arg{pspec}.}
  @short{Gets the short description of a @symbol{g:param-spec} instance.}
  @see-symbol{g:param-spec}"
  (pspec (:pointer (:struct param-spec))))

(export 'param-spec-blurb)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_internal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_param_spec_internal" param-spec-internal) :pointer
 #+liber-documentation
 "@version{2024-12-22}
  @argument[ptype]{a @class{g:type-t} type ID for the property, must be
    derived from the @code{\"GParam\"} type}
  @argument[name]{a string with the canonical name of the property}
  @argument[nick]{a string with the nickname of the property}
  @argument[blurb]{a string with a short description of the property}
  @argument[flags]{a combination of flags from the @symbol{g:param-flags}
    bitfield}
  @return{The newly allocated @symbol{g:param-spec} instance.}
  @begin{short}
    Creates a new parameter specification instance.
  @end{short}
  A property name consists of segments consisting of ASCII letters and digits,
  separated by either the '-' or '_' character. The first character of a
  property name must be a letter. Names which violate these rules lead to
  undefined behaviour.

  When creating and looking up a @symbol{g:param-spec} instance, either
  separator can be used, but they cannot be mixed. Using '-' is considerably
  more efficient and in fact required when using property names as detail
  strings for signals.

  Beyond @arg{name}, @symbol{g:param-spec} instances have two more descriptive
  strings associated with them, @arg{nick}, which should be suitable for use as
  a label for the property in a property editor, and @arg{blurb}, which should
  be a somewhat longer description, suitable, for example, for a tooltip.
  The @arg{nick} and @arg{blurb} values should ideally be localized.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:param-spec-internal \"GParamBoolean\"
                       \"Boolean\" \"Bool\" \"Doku\"
                       '(:readable :writable))
=> #.(SB-SYS:INT-SAP #X00933890)
(g:param-spec-type-name *)
=> \"GParamBoolean\"
    @end{pre}
  @end{dictionary}
  @see-symbol{g:param-spec}
  @see-symbol{g:param-flags}
  @see-class{g:type-t}"
  (ptype type-t)
  (name :string)
  (nick :string)
  (blurb :string)
  (flags param-flags))

(export 'param-spec-internal)

;;; --- End of file gobject.param-spec.lisp ------------------------------------
