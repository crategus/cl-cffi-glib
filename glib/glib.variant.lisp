;;; ----------------------------------------------------------------------------
;;; glib.variant.lisp
;;;
;;; The documentation in this file is taken from the GLIB Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GLIB library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; GVariant
;;;
;;;     Strongly typed value datatype
;;;
;;; Types and Values
;;;
;;;     GVariant
;;;     GVariantClass
;;;     GVariantDict
;;;     GVariantIter
;;;
;;; Functions
;;;
;;;     g_variant_unref
;;;     g_variant_ref
;;;     g_variant_ref_sink
;;;     g_variant_is_floating
;;;     g_variant_take_ref
;;;     g_variant_get_type
;;;     g_variant_get_type_string
;;;     g_variant_is_of_type
;;;     g_variant_is_container
;;;     g_variant_compare
;;;     g_variant_classify
;;;
;;;     g_variant_check_format_string
;;;     g_variant_get
;;;     g_variant_get_va
;;;
;;;     g_variant_new
;;;     g_variant_new_va
;;;     g_variant_new_boolean
;;;     g_variant_new_byte
;;;     g_variant_new_int16
;;;     g_variant_new_uint16
;;;     g_variant_new_int32
;;;     g_variant_new_uint32
;;;     g_variant_new_int64
;;;     g_variant_new_uint64
;;;     g_variant_new_handle
;;;     g_variant_new_double
;;;     g_variant_new_string
;;;
;;;     g_variant_new_take_string
;;;     g_variant_new_printf
;;;
;;;     g_variant_new_object_path
;;;     g_variant_is_object_path
;;;     g_variant_new_signature
;;;     g_variant_is_signature
;;;     g_variant_new_variant
;;;     g_variant_new_strv
;;;     g_variant_new_objv
;;;     g_variant_new_bytestring
;;;     g_variant_new_bytestring_array
;;;
;;;     g_variant_get_boolean
;;;     g_variant_get_byte
;;;     g_variant_get_int16
;;;     g_variant_get_uint16
;;;     g_variant_get_int32
;;;     g_variant_get_uint32
;;;     g_variant_get_int64
;;;     g_variant_get_uint64
;;;     g_variant_get_handle
;;;     g_variant_get_double
;;;     g_variant_get_string
;;;     g_variant_dup_string
;;;     g_variant_get_variant
;;;     g_variant_get_strv
;;;     g_variant_dup_strv
;;;     g_variant_get_objv
;;;     g_variant_dup_objv
;;;     g_variant_get_bytestring
;;;     g_variant_dup_bytestring
;;;     g_variant_get_bytestring_array
;;;     g_variant_dup_bytestring_array
;;;
;;;     g_variant_new_maybe
;;;     g_variant_new_array
;;;     g_variant_new_tuple
;;;     g_variant_new_dict_entry
;;;     g_variant_new_fixed_array
;;;
;;;     g_variant_get_maybe
;;;     g_variant_n_children
;;;     g_variant_get_child_value
;;;     g_variant_get_child
;;;     g_variant_lookup_value
;;;     g_variant_lookup
;;;     g_variant_get_fixed_array
;;;
;;;     g_variant_get_size
;;;     g_variant_get_data
;;;     g_variant_get_data_as_bytes
;;;     g_variant_store
;;;     g_variant_new_from_data
;;;     g_variant_new_from_bytes
;;;     g_variant_byteswap
;;;     g_variant_get_normal_form
;;;     g_variant_is_normal_form
;;;
;;;     g_variant_hash
;;;     g_variant_equal
;;;
;;;     g_variant_print
;;;     g_variant_print_string
;;;
;;;     g_variant_parse
;;;     g_variant_new_parsed_va
;;;     g_variant_new_parsed
;;;     g_variant_parse_error_print_context

;;;     G_VARIANT_DICT_INIT
;;;     g_variant_dict_unref
;;;     g_variant_dict_ref
;;;     g_variant_dict_new
;;;     g_variant_dict_init
;;;     g_variant_dict_clear
;;;     g_variant_dict_contains
;;;     g_variant_dict_lookup
;;;     g_variant_dict_lookup_value
;;;     g_variant_dict_insert
;;;     g_variant_dict_insert_value
;;;     g_variant_dict_remove
;;;     g_variant_dict_end
;;;
;;;     g_variant_iter_copy
;;;     g_variant_iter_free
;;;     g_variant_iter_init
;;;     g_variant_iter_n_children
;;;     g_variant_iter_new
;;;     g_variant_iter_next_value
;;;     g_variant_iter_next
;;;     g_variant_iter_loop
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GVariantClass
;;; ----------------------------------------------------------------------------

(cffi:defcenum variant-class
  (:boolean     #.(char-code #\b))
  (:byte        #.(char-code #\y))
  (:int16       #.(char-code #\n))
  (:uint16      #.(char-code #\q))
  (:int32       #.(char-code #\i))
  (:uint32      #.(char-code #\u))
  (:int64       #.(char-code #\x))
  (:uint64      #.(char-code #\t))
  (:handle      #.(char-code #\h))
  (:double      #.(char-code #\d))
  (:string      #.(char-code #\s))
  (:object-path #.(char-code #\o))
  (:signature   #.(char-code #\g))
  (:variant     #.(char-code #\v))
  (:maybe       #.(char-code #\m))
  (:array       #.(char-code #\a))
  (:tuple       #.(char-code #\())
  (:dict-entry  #.(char-code #\{)))

#+liber-documentation
(setf (liber:alias-for-symbol 'variant-class)
      "CEnum"
      (liber:symbol-documentation 'variant-class)
 "@version{2025-05-25}
  @begin{declaration}
(cffi:defcenum variant-class
  (:boolean     #.(char-code #\b))
  (:byte        #.(char-code #\y))
  (:int16       #.(char-code #\n))
  (:uint16      #.(char-code #\q))
  (:int32       #.(char-code #\i))
  (:uint32      #.(char-code #\u))
  (:int64       #.(char-code #\x))
  (:uint64      #.(char-code #\t))
  (:handle      #.(char-code #\h))
  (:double      #.(char-code #\d))
  (:string      #.(char-code #\s))
  (:object-path #.(char-code #\o))
  (:signature   #.(char-code #\g))
  (:variant     #.(char-code #\v))
  (:maybe       #.(char-code #\m))
  (:array       #.(char-code #\a))
  (:tuple       #.(char-code #\())
  (:dict-entry  #.(char-code #\{)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:boolean]{The instance is a boolean.}
      @entry[:byte]{The variant is a byte.}
      @entry[:int16]{The variant is a signed 16 bit integer.}
      @entry[:uint16]{The variant is an unsigned 16 bitinteger.}
      @entry[:int32]{The variant is a signed 32 bit integer.}
      @entry[:unit32]{The variant is an unsigned 32 bit integer.}
      @entry[:int64]{The variant is a signed 64 bit integer.}
      @entry[:uint64]{The variant is an unsigned 64 bit integer.}
      @entry[:handle]{The variant is a file handle index.}
      @entry[:double]{The variant is a double precision floating point value.}
      @entry[:string]{The variant is a normal string.}
      @entry[:object-path]{The variant is a D-Bus object path string.}
      @entry[:signature]{The variant is a D-Bus signature string.}
      @entry[:variant]{The variant is a variant.}
      @entry[:maybe]{The variant is a maybe-typed value.}
      @entry[:array]{The variant is an array.}
      @entry[:tuple]{The variant is a tuple.}
      @entry[:dict-entry]{The variant is a dictionary entry.}
    @end{table}
  @end{values}
  @short{The range of possible toplevel types of @symbol{g:variant} parameters.}
  @see-symbol{g:variant}")

(export 'variant-class)

;;; ----------------------------------------------------------------------------
;;; GVariant
;;; ----------------------------------------------------------------------------

;; TODO: The documentation for memory use is removed. Consider to add a link
;; to memory use.

(cffi:defcstruct variant)

#+liber-documentation
(setf (liber:alias-for-symbol 'variant)
      "CStruct"
      (liber:symbol-documentation 'variant)
 "@version{2025-05-25}
  @begin{declaration}
(cffi:defcstruct variant)
  @end{declaration}
  @begin{short}
    The @symbol{g:variant} structure is a variant datatype.
  @end{short}
  It stores a value along with information about the type of that value. The
  range of possible values is determined by the type. The type system used by
  @symbol{g:variant} instances is the @class{g:variant-type} type.

  @symbol{g:variant} instances always have a type and a value, which are given
  at construction time. The variant type and value of a @symbol{g:variant}
  instance can never change other than by the @symbol{g:variant} instance itself
  being destroyed. A @symbol{g:variant} instance cannot contain a pointer.

  A @symbol{g:variant} instance is reference counted using the
  @fun{g:variant-ref} and @fun{g:variant-unref} functions. The
  @symbol{g:variant} instance also has floating reference counts, see the
  @fun{g:variant-ref-sink} function.

  The @symbol{g:variant} structure is completely threadsafe. A
  @symbol{g:variant} instance can be concurrently accessed in any way from any
  number of threads without problems.

  The @symbol{g:variant} structure is heavily optimised for dealing with data
  in serialised form. It works particularly well with data located in memory
  mapped files. It can perform nearly all deserialisation operations in a small
  constant time, usually touching only a single memory page. Serialised
  @symbol{g:variant} data can also be sent over the network.

  The @symbol{g:variant} structure is largely compatible with D-Bus. Almost all
  types of @symbol{g:variant} instances can be sent over D-Bus. See the
  @class{g:variant-type} documentation for exceptions. However,
  @symbol{g:variant} structures serialisation format is not the same as the
  serialisation format of a D-Bus message body: use @code{GDBusMessage}, in the
  GIO library, for those.

  For space-efficiency, the @symbol{g:variant} serialisation format does not
  automatically include the variant's length, type or endianness, which must
  either be implied from context (such as knowledge that a particular file
  format always contains a little-endian \"v\" which occupies the whole length
  of the file) or supplied out-of-band (for instance, a length, type and/or
  endianness indicator could be placed at the beginning of a file, network
  message or network stream).

  A @symbol{g:variant} instance size is limited mainly by any lower level
  operating system constraints, such as the number of bits in @code{:size}.
  For example, it is reasonable to have a 2 GB file mapped into memory with
  @code{GMappedFile}, and call the @code{g_variant_new_from_data()} function
  on it.

  For convenience to C programmers, the @symbol{g:variant} values features
  powerful varargs-based value construction and destruction. This feature is
  designed to be embedded in other libraries.

  There is a Python-inspired text language for describing @symbol{g:variant}
  parameters. The @symbol{g:variant} structure includes a printer for this
  language and a parser with type inferencing.
  @see-class{g:variant-type}
  @see-function{g:variant-ref}
  @see-function{g:variant-unref}")

;; Register Lisp symbol for fundamental "GVariant" type
(glib-init:at-init ()
  (setf (glib:symbol-for-gtype "GVariant") 'variant))

(export 'variant)

;;; ----------------------------------------------------------------------------
;;; g:with-variant
;;; ----------------------------------------------------------------------------

(defmacro with-variant ((var vclass value) &body body)
 #+liber-documentation
 "@version{2025-05-26}
  @syntax{(g:with-variant (var vclass value) body) => result}
  @argument[var]{a @symbol{g:variant} instance to create and initialize}
  @argument[vclass]{a @symbol{g:variant-class} value}
  @argument[value]{a value corresponding to @arg{vclass} to set}
  @begin{short}
    The @fun{g:with-variant} macro allocates a new @symbol{g:variant} instance,
    initializes it with the given value and executes the body that uses
    @arg{var}.
  @end{short}
  This macro creates variants with a full reference using the
  @fun{g:variant-ref-sink} function.

  After execution of the body the @fun{g:variant-unref} function is called on
  @arg{var}. This clears the memory used by the variant.
  @begin[Notes]{dictionary}
    This macro can only be used to create and initialise variants for basic
    types that have a @symbol{g:variant-class} value. The valid basic type
    strings are @code{\"b\"}, @code{\"y\"}, @code{\"n\"}, @code{\"q\"},
    @code{\"i\"}, @code{\"u\"}, @code{\"x\"}, @code{\"t\"}, @code{\"h\"},
    @code{\"d\"}, @code{\"s\"}, @code{\"o\"}, and @code{\"g\"}.
  @end{dictionary}
  @see-symbol{g:variant}
  @see-symbol{g:variant-class}
  @see-function{g:variant-ref-sink}
  @see-function{g:variant-unref}"
  (cond ((eq :boolean vclass)
         `(let ((,var (variant-ref-sink (variant-new-boolean ,value))))
            (unwind-protect
              (progn ,@body))
              (variant-unref ,var)))
        ((eq :byte vclass)
         `(let ((,var (variant-ref-sink (variant-new-byte ,value))))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :int16 vclass)
         `(let ((,var (variant-new-int16 ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :uint16 vclass)
         `(let ((,var (variant-new-uint16 ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :int32 vclass)
         `(let ((,var (variant-new-int32 ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :uint32 vclass)
         `(let ((,var (variant-new-uint32 ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :int64 vclass)
         `(let ((,var (variant-new-int64 ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :uint64 vclass)
         `(let ((,var (variant-new-uint64 ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :handle vclass)
         `(let ((,var (variant-new-handle ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :double vclass)
         `(let ((,var (variant-new-double ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :string vclass)
         `(let ((,var (variant-new-string ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :object-path vclass)
         `(let ((,var (variant-new-object-path ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :variant vclass)
         `(let ((,var (variant-new-variant ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        ((eq :signature vclass)
         `(let ((,var (variant-new-signature ,value)))
            (unwind-protect
              (progn ,@body)
              (variant-unref ,var))))
        (t
         (cl:error "G:WITH-VARIANT: Unknown variant type"))))

(export 'with-variant)

;;; ----------------------------------------------------------------------------
;;; g_variant_ref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_ref" variant-ref) (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The same @symbol{g:variant} instance.}
  @short{Increases the reference count of @arg{value}.}
  @see-symbol{g:variant}
  @see-function{g:variant-unref}
  @see-function{g:variant-ref-sink}"
  (value (:pointer (:struct variant))))

(export 'variant-ref)

;;; ----------------------------------------------------------------------------
;;; g_variant_unref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_unref" variant-unref) :void
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @begin{short}
    Decreases the reference count of @arg{value}.
  @end{short}
  When its reference count drops to 0, the memory used by the variant is freed.
  @see-symbol{g:variant}
  @see-function{g:variant-ref}
  @see-function{g:variant-ref-sink}"
  (value (:pointer (:struct variant))))

(export 'variant-unref)

;;; ----------------------------------------------------------------------------
;;; g_variant_ref_sink
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_ref_sink" variant-ref-sink)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The same @symbol{g:variant} instance.}
  @begin{short}
    The @symbol{g:variant} structure uses a floating reference count system.
  @end{short}
  All functions with names starting with @code{g:variant-new-} return floating
  references.

  Calling the @fun{g:variant-ref-sink} function on a @symbol{g:variant} instance
  with a floating reference will convert the floating reference into a full
  reference. Calling the @fun{g:variant-ref-sink} function on a non-floating
  @symbol{g:variant} instance results in an additional normal reference being
  added.

  In other words, if @arg{value} is floating, then this call
  \"assumes ownership\" of the floating reference, converting it to a normal
  reference. If @arg{value} is not floating, then this call adds a new normal
  reference increasing the reference count by one.

  All calls that result in a @symbol{g:variant} instance being inserted into a
  container will call the @fun{g:variant-ref-sink} function on the instance.
  This means that if the value was just created (and has only its floating
  reference) then the container will assume sole ownership of the value at that
  point and the caller will not need to unreference it. This makes certain
  common styles of programming much easier while still maintaining normal
  refcounting semantics in situations where values are not floating.
  @see-symbol{g:variant}
  @see-function{g:variant-ref}
  @see-function{g:variant-unref}"
  (value (:pointer (:struct variant))))

(export 'variant-ref-sink)

;;; ----------------------------------------------------------------------------
;;; g_variant_take_ref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_take_ref" variant-take-ref) (boxed variant-type)
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The same @symbol{g:variant} instance.}
  @begin{short}
    If @arg{value} is floating, sink it. Otherwise, do nothing.
  @end{short}
  Typically you want to use the @fun{g:variant-ref-sink} function in order to
  automatically do the correct thing with respect to floating or non-floating
  references, but there is one specific scenario where this function is helpful.

  The situation where this function is helpful is when creating an API that
  allows the user to provide a callback function that returns a
  @symbol{g:variant} instance. We certainly want to allow the user the
  flexibility to return a non-floating reference from this callback for the
  case where the value that is being returned already exists.

  At the same time, the style of the @symbol{g:variant} API makes it likely that
  for newly created @symbol{g:variant} instances, the user can be saved some
  typing if they are allowed to return a @symbol{g:variant} instance with a
  floating reference.

  Using this function on the return value of the user's callback allows the
  user to do whichever is more convenient for them. The caller will alway
  receives exactly one full reference to the value: either the one that was
  returned in the first place, or a floating reference that has been converted
  to a full reference.

  This function has an odd interaction when combined with the
  @fun{g:variant-ref-sink} function running at the same time in another thread
  on the same @symbol{g:variant} instance. If the @fun{g:variant-ref-sink}
  function runs first then the result will be that the floating reference is
  converted to a hard reference. If the @fun{g:variant-take-ref} function runs
  first then the result will be that the floating reference is converted to a
  hard reference and an additional reference on top of that one is added. It is
  best to avoid this situation.
  @see-symbol{g:variant}
  @see-function{g:variant-ref-sink}"
  (value (:pointer (:struct variant))))

(export 'variant-take-ref)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_floating
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_is_floating" variant-is-floating) :boolean
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The boolean whether @arg{value} is floating.}
  @begin{short}
    Checks whether @arg{value} has a floating reference count.
  @end{short}
  This function should only ever be used to assert that a given variant is or
  is not floating, or for debug purposes. To acquire a reference to a variant
  that might be floating, always use the @fun{g:variant-ref-sink} or
  @fun{g:variant-take-ref} functions.

  See the @fun{g:variant-ref-sink} function for more information about floating
  reference counts.
  @see-symbol{g:variant}
  @see-function{g:variant-ref-sink}
  @see-function{g:variant-take-ref}"
  (value (:pointer (:struct variant))))

(export 'variant-is-floating)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_type" variant-type) (boxed variant-type)
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The @class{g:variant-type} instance.}
  @begin{short}
    Determines the variant type of @arg{value}.
  @end{short}
  The return value is valid for the lifetime of @arg{value}.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-type)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_type_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_type_string" variant-type-string) :string
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The variant type string for the variant type of @arg{value}.}
  @begin{short}
    Returns the variant type string of @arg{value}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:variant-type-string (g:variant-new-double 10.0d0)) => \"d\"
(g:variant-type-string (g:variant-new-string \"test\")) => \"s\"
    @end{pre}
  @end{dictionary}
  @see-symbol{g:variant}"
  (value (:pointer (:struct variant))))

(export 'variant-type-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_of_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_is_of_type" variant-is-of-type) :boolean
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{@em{True} if the variant type of @arg{value} matches @arg{vtype}.}
  @begin{short}
    Checks if a @arg{value} has a variant type matching the provided
    @arg{vtype}.
  @end{short}
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant)))
  (vtype (boxed variant-type)))

(export 'variant-is-of-type)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_container
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_is_container" variant-is-container) :boolean
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @return{@em{True} if @arg{value} is a container.}
  @begin{short}
    Checks if @arg{value} is a container.
  @end{short}
  @see-symbol{g:variant}"
  (value (:pointer (:struct variant))))

(export 'variant-is-container)

;;; ----------------------------------------------------------------------------
;;; g_variant_compare
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_compare" variant-compare) :int
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value1]{a basic-typed @symbol{g:variant} instance}
  @argument[value2]{a @symbol{g:variant} instance of the same type}
  @begin{return}
    The integer with a negative value if a < b, zero if a = b, positive value
    if a > b.
  @end{return}
  @begin{short}
    Compares @arg{value1} and @arg{value2}.
  @end{short}
  The variant types of @arg{value1} and @arg{value2} are @code{:pointer} only
  to allow use of this function with @code{GTree}, @code{GPtrArray}, etc. They
  must each be a @symbol{g:variant} instance.

  Comparison is only defined for basic types, that is booleans, numbers,
  strings. For booleans, @em{false} is less than @em{true}. Numbers are ordered
  in the usual way. Strings are in ASCII lexographical order.

  It is a programmer error to attempt to compare container values or two
  values that have types that are not exactly equal. For example, you cannot
  compare a 32-bit signed integer with a 32-bit unsigned integer. Also note
  that this function is not particularly well-behaved when it comes to
  comparison of doubles. In particular, the handling of incomparable values,
  like NaN, is undefined.

  If you only require an equality comparison, the @fun{g:variant-equal} function
  is more general.
  @see-symbol{g:variant}
  @see-function{g:variant-equal}"
  (value1 (:pointer (:struct variant)))
  (value2 (:pointer (:struct variant))))

(export 'variant-compare)

;;; ----------------------------------------------------------------------------
;;; g_variant_classify
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_classify" variant-classify) variant-class
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The @symbol{g:variant-class} value of @arg{value}.}
  @begin{short}
    Classifies @arg{value} according to its toplevel type.
  @end{short}
  @see-symbol{g:variant}
  @see-symbol{g:variant-class}"
  (value (:pointer (:struct variant))))

(export 'variant-classify)

;;; ----------------------------------------------------------------------------
;;; g_variant_check_format_string ()
;;;
;;; gboolean g_variant_check_format_string (GVariant *value,
;;;                                         const gchar *format_string,
;;;                                         gboolean copy_only);
;;;
;;; Checks if calling g_variant_get() with format_string on value would be valid
;;; from a type-compatibility standpoint. format_string is assumed to be a valid
;;; format string (from a syntactic standpoint).
;;;
;;; If copy_only is TRUE then this function additionally checks that it would
;;; be safe to call g_variant_unref() on value immediately after the call to
;;; g_variant_get() without invalidating the result. This is only possible if
;;; deep copies are made (ie: there are no pointers to the data inside of the
;;; soon-to-be-freed GVariant instance). If this check fails then a g_critical()
;;; is printed and FALSE is returned.
;;;
;;; This function is meant to be used by functions that wish to provide varargs
;;; accessors to GVariant values of uncertain values (eg: g_variant_lookup() or
;;; g_menu_model_get_item_attribute()).
;;;
;;; value :
;;;     a GVariant
;;;
;;; format_string :
;;;     a valid GVariant format string
;;;
;;; copy_only :
;;;     TRUE to ensure the format string makes deep copies
;;;
;;; Returns :
;;;     TRUE if format_string is safe to use
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get
;;; ----------------------------------------------------------------------------

(defun variant-get (value)
 #+liber-documentation
 "@version{2025-05-27}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The value for the @symbol{g:variant} instance.}
  @begin{short}
    Returns the value for the variant.
  @end{short}
  @begin[Notes]{dictionary}
    This function does not provide a general implementation for all variant
    types. Instead, it only allows basic types that have a
    @symbol{g:variant-class} value. An error condition is thrown for all other
    variant types.
  @end{dictionary}
  @see-symbol{g:variant}
  @see-symbol{g:variant-class}"
  (let ((vclass (variant-classify value)))
    (cond ((eq :boolean vclass)
           (variant-boolean value))
          ((eq :byte vclass)
           (variant-byte value))
          ((eq :int16 vclass)
           (variant-int16 value))
          ((eq :uint16 vclass)
           (variant-uint16 value))
          ((eq :int32 vclass)
           (variant-int32 value))
          ((eq :uint32 vclass)
           (variant-uint32 value))
          ((eq :int64 vclass)
           (variant-int64 value))
          ((eq :uint64 vclass)
           (variant-uint64 value))
          ((eq :handle vclass)
           (variant-handle value))
          ((eq :double vclass)
           (variant-double value))
          ((eq :string vclass)
           (variant-string value))
          ((eq :object-path vclass)
           (variant-string value))
          ((eq :signature vclass)
           (variant-string value))
          ((eq :variant vclass)
           (variant-variant value))
          (t
           (cl:error "G:VARIANT-GET: Unknown variant type")))))

(export 'variant-get)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_va ()
;;;
;;; void g_variant_get_va (GVariant *value,
;;;                        const gchar *format_string,
;;;                        const gchar **endptr,
;;;                        va_list *app);
;;;
;;; This function is intended to be used by libraries based on GVariant that
;;; want to provide g_variant_get()-like functionality to their users.
;;;
;;; The API is more general than g_variant_get() to allow a wider range of
;;; possible uses.
;;;
;;; format_string must still point to a valid format string, but it only need
;;; to be nul-terminated if endptr is NULL. If endptr is non-NULL then it is
;;; updated to point to the first character past the end of the format string.
;;;
;;; app is a pointer to a va_list. The arguments, according to format_string,
;;; are collected from this va_list and the list is left pointing to the
;;; argument following the last.
;;;
;;; These two generalisations allow mixing of multiple calls to
;;; g_variant_new_va() and g_variant_get_va() within a single actual varargs
;;; call by the user.
;;;
;;; value :
;;;     a GVariant
;;;
;;; format_string :
;;;     a string that is prefixed with a format string
;;;
;;; endptr :
;;;     location to store the end pointer, or NULL
;;;
;;; app :
;;;     a pointer to a va_list
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new ()
;;;
;;; GVariant * g_variant_new (const gchar *format_string, ...);
;;;
;;; Creates a new GVariant instance.
;;;
;;; Think of this function as an analogue to g_strdup_printf().
;;;
;;; The type of the created instance and the arguments that are expected by this
;;; function are determined by format_string. See the section on GVariant Format
;;; Strings. Please note that the syntax of the format string is very likely to
;;; be extended in the future.
;;;
;;; The first character of the format string must not be '*' '?' '@' or 'r'; in
;;; essence, a new GVariant must always be constructed by this function (and not
;;; merely passed through it unmodified).
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     arguments, as per format_string
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_va ()
;;;
;;; GVariant * g_variant_new_va (const gchar *format_string,
;;;                              const gchar **endptr,
;;;                              va_list *app);
;;;
;;; This function is intended to be used by libraries based on GVariant that
;;; want to provide g_variant_new()-like functionality to their users.
;;;
;;; The API is more general than g_variant_new() to allow a wider range of
;;; possible uses.
;;;
;;; format_string must still point to a valid format string, but it only needs
;;; to be nul-terminated if endptr is NULL. If endptr is non-NULL then it is
;;; updated to point to the first character past the end of the format string.
;;;
;;; app is a pointer to a va_list. The arguments, according to format_string,
;;; are collected from this va_list and the list is left pointing to the
;;; argument following the last.
;;;
;;; These two generalisations allow mixing of multiple calls to
;;; g_variant_new_va() and g_variant_get_va() within a single actual varargs
;;; call by the user.
;;;
;;; The return value will be floating if it was a newly created GVariant
;;; instance (for example, if the format string was "(ii)"). In the case that
;;; the format_string was '*', '?', 'r', or a format starting with '@' then the
;;; collected GVariant pointer will be returned unmodified, without adding any
;;; additional references.
;;;
;;; In order to behave correctly in all cases it is necessary for the calling
;;; function to g_variant_ref_sink() the return result before returning control
;;; to the user that originally provided the pointer. At this point, the caller
;;; will have their own full reference to the result. This can also be done by
;;; adding the result to a container, or by passing it to another
;;; g_variant_new() call.
;;;
;;; format_string :
;;;     a string that is prefixed with a format string
;;;
;;; endptr :
;;;     location to store the end pointer, or NULL
;;;
;;; app :
;;;     a pointer to a va_list
;;;
;;; Returns :
;;;     a new, usually floating, GVariant
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_boolean
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_boolean" variant-new-boolean)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a boolean value}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @begin{short}
    Creates a new @symbol{g:variant} instance with the boolean value.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:variant-new-boolean nil) => #.(SB-SYS:INT-SAP #X5602D1E4F100)
(g:variant-boolean *) => NIL
(g:variant-new-boolean t) => #.(SB-SYS:INT-SAP #X5602D1E4F160)
(g:variant-boolean *) => T
    @end{pre}
  @end{dictionary}
  @see-symbol{g:variant}"
  (value :boolean))

(export 'variant-new-boolean)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_boolean
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_boolean" variant-boolean) :boolean
 #+liber-documentation
 "@version{2025-08-27}
  @argument[value]{a @sym{g:variant} instance for a boolean value}
  @return{The boolean values @em{true} or @em{false}.}
  @short{Returns the boolean value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"b\"} type string.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:variant-boolean (g:variant-new-boolean nil)) => NIL
(g:variant-boolean (g:variant-new-boolean t)) => T
    @end{pre}
  @end{dictionary}
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-boolean)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_byte
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_byte" variant-new-byte)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @code{:uchar} value}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @short{Creates a new @symbol{g:variant} instance with the @code{:uchar}
    value.}
  @see-symbol{g:variant}"
  (value :uchar))

(export 'variant-new-byte)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_byte
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_byte" variant-byte) :uchar
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance with a @code{:uchar} value}
  @return{The @code{:uchar} value.}
  @short{Returns the @code{:uchar} value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"y\"} type string.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-byte)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_int16
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_int16" variant-new-int16)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @code{:int16} value}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @short{Creates a new @symbol{g:variant} instance with the @code{:int16}
    value.}
  @see-symbol{g:variant}"
  (value :int16))

(export 'variant-new-int16)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_int16
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_int16" variant-int16) :int16
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance with a @code{:int16} value}
  @return{The @code{:int16} value.}
  @short{Returns the 16-bit signed integer of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"n\"} type string.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-int16)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_uint16
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_uint16" variant-new-uint16)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @code{:uint16} value}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @short{Creates a new @symbol{g:variant} instance with the @code{:uint16}
    value.}
  @see-symbol{g:variant}"
  (value :uint16))

(export 'variant-new-uint16)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_uint16
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_uint16" variant-uint16) :uint16
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance with a @code{:uint16} value}
  @return{The @code{:uint16} value.}
  @short{Returns the 16-bit unsigned integer of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"q\"} type string.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-uint16)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_int32
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_int32" variant-new-int32)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @code{:int32} value}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @short{Creates a new @symbol{g:variant} instance with the @code{:int32}
    value.}
  @see-symbol{g:variant}"
  (value :int32))

(export 'variant-new-int32)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_int32
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_int32" variant-int32) :int32
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance with a @code{:int32} value}
  @return{The @code{:int32} value.}
  @short{Returns the 32-bit signed integer of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"i\"} type string.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-int32)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_uint32
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_uint32" variant-new-uint32)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @code{:uint32} value}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @short{Creates a new @symbol{g:variant} instance with the @code{:uint32}
    value.}
  @see-symbol{g:variant}"
  (value :uint32))

(export 'variant-new-uint32)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_uint32
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_uint32" variant-uint32) :uint32
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance with a @code{:uint32} value}
  @return{The @code{:uint32} value.}
  @short{Returns the 32-bit unsigned integer of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"u\"} type string.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-uint32)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_int64
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_int64" variant-new-int64)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @code{:int64} value}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @begin{short}
    Creates a new @symbol{g:variant} instance with the @code{:int64} value.
  @end{short}
  @see-symbol{g:variant}"
  (value :int64))

(export 'variant-new-int64)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_int64
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_int64" variant-int64) :int64
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance with a @code{:int64} value}
  @return{The @code{:int64} value.}
  @begin{short}
    Returns the 64-bit signed integer of @arg{value}.
  @end{short}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"x\"} type string.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-int64)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_uint64
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_uint64" variant-new-uint64)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @code{:uint64} value}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @begin{short}
    Creates a new @symbol{g:variant} instance with the @code{:uint64} value.
  @end{short}
  @see-symbol{g:variant}"
  (value :uint64))

(export 'variant-new-uint64)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_uint64
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_uint64" variant-uint64) :uint64
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance with a @code{:uint64} value}
  @return{The @code{:uint64} value.}
  @begin{short}
    Returns the 64-bit unsigned integer of @arg{value}.
  @end{short}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"t\"} type string.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-uint64)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_handle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_handle" variant-new-handle)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @code{:int32} value}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @begin{short}
    Creates a new @symbol{g:variant} instance with a handle.
  @end{short}
  By convention, handles are indexes into an array of file descriptors that
  are sent alongside a D-Bus message. If you are not interacting with D-Bus,
  you probably do not need them.
  @see-symbol{g:variant}"
  (value :int32))

(export 'variant-new-handle)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_handle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_handle" variant-handle) :int32
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance with a handle}
  @return{The @code{:int32} value.}
  @begin{short}
    Returns the 32-bit signed integer of @arg{value}.
  @end{short}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"h\"} type string.

  By convention, handles are indexes into an array of file descriptors that
  are sent alongside a D-Bus message. If you are not interacting with D-Bus,
  you probably do not need them.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-handle)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_double
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_double" variant-new-double)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a double float}
  @return{The floating reference to a @symbol{g:variant} instance.}
  @begin{short}
    Creates a new @symbol{g:variant} instance with a double float.
  @end{short}
  @see-symbol{g:variant}"
  (value :double))

(export 'variant-new-double)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_double
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_double" variant-double) :double
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance for a double float}
  @return{The double float.}
  @begin{short}
    Returns the double precision floating point value of @arg{value}.
  @end{short}
  It is an error to call this function with a value of any type other than
  a @class{g:variant-type} type with the @code{\"d\"} type string.
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (value (:pointer (:struct variant))))

(export 'variant-double)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_string" variant-new-string)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[string]{a normal UTF-8 string}
  @return{The floating reference to a @symbol{g:variant} instance.}
  @begin{short}
    Creates a @symbol{g:variant} instance with a string value.
  @end{short}
  The string must be valid UTF-8. Use the @fun{g:variant-string} function to
  retrieve the string.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:variant-new-string \"This is a string.\")
=> #.(SB-SYS:INT-SAP #X55EF04FDCE00)
(g:variant-string *) => \"This is a string.\"
    @end{pre}
  @end{dictionary}
  @see-symbol{g:variant}
  @see-function{g:variant-string}"
  (value :string))

(export 'variant-new-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_string" %variant-string) :string
  (value (:pointer (:struct variant)))
  (length :pointer))

(defun variant-string (value)
 #+liber-documentation
 "@version{2025-08-27}
  @argument[value]{a @sym{g:variant} instance for a string}
  @return{The constant string, UTF-8 encoded.}
  @begin{short}
    Returns the string value of @arg{value}.
  @end{short}
  This includes the @class{g:variant-type} types with the @code{\"s\"},
  @code{\"o\"}, and @code{\"g\"} type strings. The string will always be UTF-8
  encoded. It is an error to call this function with a value of any type other
  than those three. The return value remains valid as long as @arg{value}
  exists. See the @fun{g:variant-new-string} function for an example.
  @see-symbol{g:variant}
  @see-class{g:variant-type}
  @see-function{g:variant-new-string}"
  (%variant-string value (cffi:null-pointer)))

(export 'variant-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_take_string ()
;;;
;;; GVariant *
;;; g_variant_new_take_string (gchar *string);
;;;
;;; Creates a string GVariant with the contents of string .
;;;
;;; string must be valid UTF-8, and must not be NULL. To encode potentially-NULL
;;; strings, use this with g_variant_new_maybe().
;;;
;;; This function consumes string . g_free() will be called on string when it
;;; is no longer required.
;;;
;;; You must not modify or access string in any other way after passing it to
;;; this function. It is even possible that string is immediately freed.
;;;
;;; string :
;;;     a normal UTF-8 nul-terminated string
;;;
;;; Returns :
;;;     a floating reference to a new string GVariant instance.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_printf ()
;;;
;;; GVariant *
;;; g_variant_new_printf (const gchar *format_string,
;;;                       ...);
;;;
;;; Creates a string-type GVariant using printf formatting.
;;;
;;; This is similar to calling g_strdup_printf() and then g_variant_new_string()
;;; but it saves a temporary variable and an unnecessary copy.
;;;
;;; format_string :
;;;     a printf-style format string
;;;
;;; ... :
;;;     arguments for format_string
;;;
;;; Returns :
;;;     a floating reference to a new string GVariant instance.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_object_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_object_path" variant-new-object-path)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[path]{a string for a D-Bus object path}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @begin{short}
    Creates a @symbol{g:variant} instance with the D-Bus object path in
    @arg{string}.
  @end{short}
  The @arg{string} argument must be a valid D-Bus object path. Use the
  @fun{g:variant-is-object-path} function if you are not sure.
  @see-symbol{g:variant}
  @see-function{g:variant-is-object-path}"
  (path :string))

(export 'variant-new-object-path)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_object_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_is_object_path" variant-is-object-path) :boolean
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[string]{a string for a D-Bus object path}
  @return{@em{True} if @arg{string} is a D-Bus object path.}
  @begin{short}
    Determines if a given @arg{string} is a valid D-Bus object path.
  @end{short}
  You should ensure that @arg{string} is a valid D-Bus object path before
  passing it to the @fun{g:variant-new-object-path} function.

  A valid object path starts with '/' followed by zero or more sequences of
  characters separated by '/' characters. Each sequence must contain only the
  characters \"[A-Z][a-z][0-9]_\". No sequence (including the one following the
  final '/' character) may be empty.
  @see-symbol{g:variant}
  @see-function{g:variant-new-object-path}"
  (string :string))

(export 'variant-is-object-path)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_signature
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_signature" variant-new-signature)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[signature]{a string for a signature}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @begin{short}
    Creates a @symbol{g:variant} instance with a D-Bus type signature in
    @arg{string}.
  @end{short}
  The @arg{string} argument must be a valid D-Bus type signature. Use the
  @fun{g:variant-is-signature} function if you are not sure.
  @see-symbol{g:variant}
  @see-function{g:variant-is-signature}"
  (signature :string))

(export 'variant-new-signature)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_signature
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_is_signature" variant-is-signature) :boolean
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[string]{a normal C nul-terminated string}
  @return{@em{True} if @arg{string} is a D-Bus type signature.}
  @begin{short}
    Determines if a given @arg{string} is a valid D-Bus type signature.
  @end{short}
  You should ensure that a string is a valid D-Bus type signature before passing
  it to the @fun{g:variant-new-signature} function.

  D-Bus type signatures consist of zero or more definite @class{g:variant-type}
  strings in sequence.
  @see-symbol{g:variant}
  @see-class{g:variant-type}
  @see-function{g:variant-new-signature}"
  (string :string))

(export 'variant-is-signature)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_variant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_variant" variant-new-variant)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The floating reference to a new @symbol{g:variant} instance.}
  @begin{short}
    The result is a @symbol{g:variant} instance representing a variant
    containing the original value.
  @end{short}
  If child is a floating reference, see the @fun{g:variant-ref-sink} function,
  the new instance takes ownership of child.
  @see-symbol{g:variant}
  @see-function{g:variant-ref-sink}"
  (value (:pointer (:struct variant))))

(export 'variant-new-variant)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_strv ()
;;;
;;; GVariant * g_variant_new_strv (const gchar * const *strv, gssize length);
;;;
;;; Constructs an array of strings GVariant from the given array of strings.
;;;
;;; If length is -1 then strv is NULL-terminated.
;;;
;;; strv :
;;;     an array of strings
;;;
;;; length :
;;;     the length of strv, or -1
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_objv ()
;;;
;;; GVariant * g_variant_new_objv (const gchar * const *strv, gssize length);
;;;
;;; Constructs an array of object paths GVariant from the given array of
;;; strings.
;;;
;;; Each string must be a valid GVariant object path; see
;;; g_variant_is_object_path().
;;;
;;; If length is -1 then strv is NULL-terminated.
;;;
;;; strv :
;;;     an array of strings
;;;
;;; length :
;;;     the length of strv, or -1
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_bytestring ()
;;;
;;; GVariant * g_variant_new_bytestring (const gchar *string);
;;;
;;; Creates an array-of-bytes GVariant with the contents of string. This
;;; function is just like g_variant_new_string() except that the string need not
;;; be valid utf8.
;;;
;;; The nul terminator character at the end of the string is stored in the
;;; array.
;;;
;;; string :
;;;     a normal nul-terminated string in no particular encoding
;;;
;;; Returns :
;;;     a floating reference to a new bytestring GVariant instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_bytestring_array ()
;;;
;;; GVariant * g_variant_new_bytestring_array (const gchar * const *strv,
;;;                                            gssize length);
;;;
;;; Constructs an array of bytestring GVariant from the given array of strings.
;;;
;;; If length is -1 then strv is NULL-terminated.
;;;
;;; strv :
;;;     an array of strings
;;;
;;; length :
;;;     the length of strv, or -1
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_string ()
;;;
;;; gchar * g_variant_dup_string (GVariant *value, gsize *length);
;;;
;;; Similar to g_variant_get_string() except that instead of returning a
;;; constant string, the string is duplicated.
;;;
;;; The string will always be utf8 encoded.
;;;
;;; The return value must be freed using g_free().
;;;
;;; value :
;;;     a string GVariant instance
;;;
;;; length :
;;;     a pointer to a gsize, to store the length
;;;
;;; Returns :
;;;     a newly allocated string, utf8 encoded
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_variant ()
;;;
;;; GVariant * g_variant_get_variant (GVariant *value);
;;;
;;; Unboxes value. The result is the GVariant instance that was contained in
;;; value.
;;;
;;; value :
;;;     a variant GVariant instance
;;;
;;; Returns :
;;;     the item contained in the variant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_get_variant" variant-variant)
    (:pointer (:struct variant))
  (value (:pointer (:struct variant))))

(export 'variant-variant)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_strv ()
;;;
;;; const gchar ** g_variant_get_strv (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of strings GVariant. This call makes a shallow
;;; copy; the return result should be released with g_free(), but the individual
;;; strings must not be modified.
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of strings GVariant
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of constant strings
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_strv ()
;;;
;;; gchar ** g_variant_dup_strv (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of strings GVariant. This call makes a deep
;;; copy; the return result should be released with g_strfreev().
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of strings GVariant
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of strings
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_objv ()
;;;
;;; const gchar ** g_variant_get_objv (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of object paths GVariant. This call makes a
;;; shallow copy; the return result should be released with g_free(), but the
;;; individual strings must not be modified.
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of object paths GVariant
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of constant strings
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_objv ()
;;;
;;; gchar ** g_variant_dup_objv (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of object paths GVariant. This call makes a
;;; deep copy; the return result should be released with g_strfreev().
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of object paths GVariant
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of strings
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_bytestring ()
;;;
;;; const gchar * g_variant_get_bytestring (GVariant *value);
;;;
;;; Returns the string value of a GVariant instance with an array-of-bytes type.
;;; The string has no particular encoding.
;;;
;;; If the array does not end with a nul terminator character, the empty string
;;; is returned. For this reason, you can always trust that a non-NULL
;;; nul-terminated string will be returned by this function.
;;;
;;; If the array contains a nul terminator character somewhere other than the
;;; last byte then the returned string is the string, up to the first such nul
;;; character.
;;;
;;; It is an error to call this function with a value that is not an array of
;;; bytes.
;;;
;;; The return value remains valid as long as value exists.
;;;
;;; value :
;;;     an array-of-bytes GVariant instance
;;;
;;; Returns :
;;;     the constant string
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_bytestring ()
;;;
;;; gchar * g_variant_dup_bytestring (GVariant *value, gsize *length);
;;;
;;; Similar to g_variant_get_bytestring() except that instead of returning a
;;; constant string, the string is duplicated.
;;;
;;; The return value must be freed using g_free().
;;;
;;; value :
;;;     an array-of-bytes GVariant instance
;;;
;;; length :
;;;     a pointer to a gsize, to store the length (not including the nul
;;;     terminator)
;;;
;;; Returns :
;;;     a newly allocated string
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_bytestring_array ()
;;;
;;; const gchar ** g_variant_get_bytestring_array (GVariant *value,
;;;                                                gsize *length);
;;;
;;; Gets the contents of an array of array of bytes GVariant. This call makes a
;;; shallow copy; the return result should be released with g_free(), but the
;;; individual strings must not be modified.
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of array of bytes GVariant ('aay')
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of constant strings
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_bytestring_array ()
;;;
;;; gchar ** g_variant_dup_bytestring_array (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of array of bytes GVariant. This call makes a
;;; deep copy; the return result should be released with g_strfreev().
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of array of bytes GVariant ('aay')
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of strings
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_maybe ()
;;;
;;; GVariant * g_variant_new_maybe (const GVariantType *child_type,
;;;                                 GVariant *child);
;;;
;;; Depending on if child is NULL, either wraps child inside of a maybe
;;; container or creates a Nothing instance for the given type.
;;;
;;; At least one of child_type and child must be non-NULL. If child_type is
;;; non-NULL then it must be a definite type. If they are both non-NULL then
;;; child_type must be the type of child.
;;;
;;; If child is a floating reference (see g_variant_ref_sink()), the new
;;; instance takes ownership of child.
;;;
;;; child_type :
;;;     the GVariantType of the child, or NULL
;;;
;;; child :
;;;     the child value, or NULL
;;;
;;; Returns :
;;;     a floating reference to a new GVariant maybe instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_array ()
;;;
;;; GVariant * g_variant_new_array (const GVariantType *child_type,
;;;                                 GVariant * const *children,
;;;                                 gsize n_children);
;;;
;;; Creates a new GVariant array from children.
;;;
;;; child_type must be non-NULL if n_children is zero. Otherwise, the child type
;;; is determined by inspecting the first element of the children array. If
;;; child_type is non-NULL then it must be a definite type.
;;;
;;; The items of the array are taken from the children array. No entry in the
;;; children array may be NULL.
;;;
;;; All items in the array must have the same type, which must be the same as
;;; child_type, if given.
;;;
;;; If the children are floating references (see g_variant_ref_sink()), the new
;;; instance takes ownership of them as if via g_variant_ref_sink().
;;;
;;; child_type :
;;;     the element type of the new array
;;;
;;; children :
;;;     an array of GVariant pointers, the children
;;;
;;; n_children :
;;;     the length of children
;;;
;;; Returns :
;;;     a floating reference to a new GVariant array
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_tuple
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_tuple" %variant-new-tuple) :pointer
  (children :pointer)
  (n-children :size))

(defun variant-new-tuple (&rest items)
 #+liber-documentation
 "@version{2025-05-25}
  @argument[items]{@symbol{g:variant} instances with the items to make the tuple
    out of}
  @return{The new @symbol{g:variant} instance with the tuple.}
  @begin{short}
    Creates a new tuple @symbol{g:variant} instance out of the items.
  @end{short}
  The type is determined from the types of @arg{items}.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:variant-new-tuple (g:variant-new-int16 10) (g:variant-new-int16 20))
=> #.(SB-SYS:INT-SAP #X6195B5F9ED70)
(g:variant-print *)
=> \"(10, 20)\"
    @end{pre}
  @end{dictionary}
  @see-symbol{g:variant}
  @see-function{g:variant-type-new-tuple}"
  (let ((n (length items)))
    (cffi:with-foreign-object (children :pointer n)
      (iter (for item in items)
            (for i from 0)
            (setf (cffi:mem-aref children :pointer i) item))
      (%variant-new-tuple children n))))

(export 'variant-new-tuple)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_dict_entry ()
;;;
;;; GVariant * g_variant_new_dict_entry (GVariant *key, GVariant *value);
;;;
;;; Creates a new dictionary entry GVariant. key and value must be non-NULL. key
;;; must be a value of a basic type (ie: not a container).
;;;
;;; If the key or value are floating references (see g_variant_ref_sink()), the
;;; new instance takes ownership of them as if via g_variant_ref_sink().
;;;
;;; key :
;;;     a basic GVariant, the key
;;;
;;; value :
;;;     a GVariant, the value
;;;
;;; Returns :
;;;     a floating reference to a new dictionary entry GVariant
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_fixed_array ()
;;;
;;; GVariant * g_variant_new_fixed_array (const GVariantType *element_type,
;;;                                       gconstpointer elements,
;;;                                       gsize n_elements,
;;;                                       gsize element_size);
;;;
;;; Provides access to the serialised data for an array of fixed-sized items.
;;;
;;; value must be an array with fixed-sized elements. Numeric types are
;;; fixed-size as are tuples containing only other fixed-sized types.
;;;
;;; element_size must be the size of a single element in the array. For example,
;;; if calling this function for an array of 32 bit integers, you might say
;;; sizeof (gint32). This value isn't used except for the purpose of a
;;; double-check that the form of the serialised data matches the caller's
;;; expectation.
;;;
;;; n_elements, which must be non-NULL is set equal to the number of items in
;;; the array.
;;;
;;; element_type :
;;;     the GVariantType of each element
;;;
;;; elements :
;;;     a pointer to the fixed array of contiguous elements
;;;
;;; n_elements :
;;;     the number of elements
;;;
;;; element_size :
;;;     the size of each element
;;;
;;; Returns :
;;;     a floating reference to a new array GVariant instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_maybe ()
;;;
;;; GVariant * g_variant_get_maybe (GVariant *value);
;;;
;;; Given a maybe-typed GVariant instance, extract its value. If the value is
;;; Nothing, then this function returns NULL.
;;;
;;; value :
;;;     a maybe-typed value
;;;
;;; Returns :
;;;     the contents of value, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_n_children ()
;;;
;;; gsize g_variant_n_children (GVariant *value);
;;;
;;; Determines the number of children in a container GVariant instance. This
;;; includes variants, maybes, arrays, tuples and dictionary entries. It is an
;;; error to call this function on any other type of GVariant.
;;;
;;; For variants, the return value is always 1. For values with maybe types, it
;;; is always zero or one. For arrays, it is the length of the array. For tuples
;;; it is the number of tuple items (which depends only on the type). For
;;; dictionary entries, it is always 2
;;;
;;; This function is O(1).
;;;
;;; value :
;;;     a container GVariant
;;;
;;; Returns :
;;;     the number of children in the container
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_child_value ()
;;;
;;; GVariant * g_variant_get_child_value (GVariant *value, gsize index_);
;;;
;;; Reads a child item out of a container GVariant instance. This includes
;;; variants, maybes, arrays, tuples and dictionary entries. It is an error to
;;; call this function on any other type of GVariant.
;;;
;;; It is an error if index_ is greater than the number of child items in the
;;; container. See g_variant_n_children().
;;;
;;; The returned value is never floating. You should free it with
;;; g_variant_unref() when you're done with it.
;;;
;;; This function is O(1).
;;;
;;; value :
;;;     a container GVariant
;;;
;;; index_ :
;;;     the index of the child to fetch
;;;
;;; Returns :
;;;     the child at the specified index
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_child ()
;;;
;;; void g_variant_get_child (GVariant *value,
;;;                           gsize index_,
;;;                           const gchar *format_string,
;;;                           ...);
;;;
;;; Reads a child item out of a container GVariant instance and deconstructs it
;;; according to format_string. This call is essentially a combination of
;;; g_variant_get_child_value() and g_variant_get().
;;;
;;; value :
;;;     a container GVariant
;;;
;;; index_ :
;;;     the index of the child to deconstruct
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     arguments, as per format_string
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_lookup_value ()
;;;
;;; GVariant * g_variant_lookup_value (GVariant *dictionary,
;;;                                    const gchar *key,
;;;                                    const GVariantType *expected_type);
;;;
;;; Looks up a value in a dictionary GVariant.
;;;
;;; This function works with dictionaries of the type a{s*} (and equally well
;;; with type a{o*}, but we only further discuss the string case for sake of
;;; clarity).
;;;
;;; In the event that dictionary has the type a{sv}, the expected_type string
;;; specifies what type of value is expected to be inside of the variant. If the
;;; value inside the variant has a different type then NULL is returned. In the
;;; event that dictionary has a value type other than v then expected_type must
;;; directly match the key type and it is used to unpack the value directly or
;;; an error occurs.
;;;
;;; In either case, if key is not found in dictionary, NULL is returned.
;;;
;;; If the key is found and the value has the correct type, it is returned. If
;;; expected_type was specified then any non-NULL return value will have this
;;; type.
;;;
;;; dictionary :
;;;     a dictionary GVariant
;;;
;;; key :
;;;     the key to lookup in the dictionary
;;;
;;; expected_type :
;;;     a GVariantType, or NULL
;;;
;;; Returns :
;;;     the value of the dictionary key, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_lookup ()
;;;
;;; gboolean g_variant_lookup (GVariant *dictionary,
;;;                            const gchar *key,
;;;                            const gchar *format_string,
;;;                            ...);
;;;
;;; Looks up a value in a dictionary GVariant.
;;;
;;; This function is a wrapper around g_variant_lookup_value() and
;;; g_variant_get(). In the case that NULL would have been returned, this
;;; function returns FALSE. Otherwise, it unpacks the returned value and returns
;;; TRUE.
;;;
;;; See g_variant_get() for information about format_string.
;;;
;;; dictionary :
;;;     a dictionary GVariant
;;;
;;; key :
;;;     the key to lookup in the dictionary
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     the arguments to unpack the value into
;;;
;;; Returns :
;;;     TRUE if a value was unpacked
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_fixed_array ()
;;;
;;; gconstpointer g_variant_get_fixed_array (GVariant *value,
;;;                                          gsize *n_elements,
;;;                                          gsize element_size);
;;;
;;; Provides access to the serialised data for an array of fixed-sized items.
;;;
;;; value must be an array with fixed-sized elements. Numeric types are
;;; fixed-size, as are tuples containing only other fixed-sized types.
;;;
;;; element_size must be the size of a single element in the array, as given by
;;; the section on Serialised Data Memory.
;;;
;;; In particular, arrays of these fixed-sized types can be interpreted as an
;;; array of the given C type, with element_size set to sizeof the appropriate
;;; type:
;;;
;;; element type                    C type
;;; G_VARIANT_TYPE_INT16 (etc.)     gint16 (etc.)
;;; G_VARIANT_TYPE_BOOLEAN          guchar (not gboolean!)
;;; G_VARIANT_TYPE_BYTE             guchar
;;; G_VARIANT_TYPE_HANDLE           guint32
;;; G_VARIANT_TYPE_DOUBLE           gdouble
;;;
;;; For example, if calling this function for an array of 32 bit integers, you
;;; might say sizeof (gint32). This value isn't used except for the purpose of
;;; a double-check that the form of the serialised data matches the caller's
;;; expectation.
;;;
;;; n_elements, which must be non-NULL is set equal to the number of items in
;;; the array.
;;;
;;; value :
;;;     a GVariant array with fixed-sized elements
;;;
;;; n_elements :
;;;     a pointer to the location to store the number of items
;;;
;;; element_size :
;;;     the size of each element
;;;
;;; Returns :
;;;     a pointer to the fixed array
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_size ()
;;;
;;; gsize g_variant_get_size (GVariant *value);
;;;
;;; Determines the number of bytes that would be required to store value with
;;; g_variant_store().
;;;
;;; If value has a fixed-sized type then this function always returned that
;;; fixed size.
;;;
;;; In the case that value is already in serialised form or the size has already
;;; been calculated (ie: this function has been called before) then this
;;; function is O(1). Otherwise, the size is calculated, an operation which is
;;; approximately O(n) in the number of values involved.
;;;
;;; value :
;;;     a GVariant instance
;;;
;;; Returns :
;;;     the serialised size of value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_data ()
;;;
;;; gconstpointer g_variant_get_data (GVariant *value);
;;;
;;; Returns a pointer to the serialised form of a GVariant instance. The
;;; returned data may not be in fully-normalised form if read from an untrusted
;;; source. The returned data must not be freed; it remains valid for as long
;;; as value exists.
;;;
;;; If value is a fixed-sized value that was deserialised from a corrupted
;;; serialised container then NULL may be returned. In this case, the proper
;;; thing to do is typically to use the appropriate number of nul bytes in place
;;; of value. If value is not fixed-sized then NULL is never returned.
;;;
;;; In the case that value is already in serialised form, this function is O(1).
;;; If the value is not already in serialised form, serialisation occurs
;;; implicitly and is approximately O(n) in the size of the result.
;;;
;;; To deserialise the data returned by this function, in addition to the
;;; serialised data, you must know the type of the GVariant, and (if the machine
;;; might be different) the endianness of the machine that stored it. As a
;;; result, file formats or network messages that incorporate serialised
;;; GVariants must include this information either implicitly (for instance "the
;;; file always contains a G_VARIANT_TYPE_VARIANT and it is always in
;;; little-endian order") or explicitly (by storing the type and/or endianness
;;; in addition to the serialised data).
;;;
;;; value :
;;;     a GVariant instance
;;;
;;; Returns :
;;;     the serialised form of value, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_data_as_bytes ()
;;;
;;; GBytes * g_variant_get_data_as_bytes (GVariant *value);
;;;
;;; Returns a pointer to the serialised form of a GVariant instance. The
;;; semantics of this function are exactly the same as g_variant_get_data(),
;;; except that the returned GBytes holds a reference to the variant data.
;;;
;;; value :
;;;     a GVariant
;;;
;;; Returns :
;;;     A new GBytes representing the variant data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_store ()
;;;
;;; void g_variant_store (GVariant *value, gpointer data);
;;;
;;; Stores the serialised form of value at data. data should be large enough.
;;; See g_variant_get_size().
;;;
;;; The stored data is in machine native byte order but may not be in
;;; fully-normalised form if read from an untrusted source. See
;;; g_variant_get_normal_form() for a solution.
;;;
;;; As with g_variant_get_data(), to be able to deserialise the serialised
;;; variant successfully, its type and (if the destination machine might be
;;; different) its endianness must also be available.
;;;
;;; This function is approximately O(n) in the size of data.
;;;
;;; value :
;;;     the GVariant to store
;;;
;;; data :
;;;     the location to store the serialised data at
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_from_data ()
;;;
;;; GVariant * g_variant_new_from_data (const GVariantType *type,
;;;                                     gconstpointer data,
;;;                                     gsize size,
;;;                                     gboolean trusted,
;;;                                     GDestroyNotify notify,
;;;                                     gpointer user_data);
;;;
;;; Creates a new GVariant instance from serialised data.
;;;
;;; type is the type of GVariant instance that will be constructed. The
;;; interpretation of data depends on knowing the type.
;;;
;;; data is not modified by this function and must remain valid with an
;;; unchanging value until such a time as notify is called with user_data. If
;;; the contents of data change before that time then the result is undefined.
;;;
;;; If data is trusted to be serialised data in normal form then trusted should
;;; be TRUE. This applies to serialised data created within this process or read
;;; from a trusted location on the disk (such as a file installed in /usr/lib
;;; alongside your application). You should set trusted to FALSE if data is read
;;; from the network, a file in the user's home directory, etc.
;;;
;;; If data was not stored in this machine's native endianness, any multi-byte
;;; numeric values in the returned variant will also be in non-native
;;; endianness. g_variant_byteswap() can be used to recover the original values.
;;;
;;; notify will be called with user_data when data is no longer needed. The
;;; exact time of this call is unspecified and might even be before this
;;; function returns.
;;;
;;; Note: data must be backed by memory that is aligned appropriately for the
;;; type being loaded. Otherwise this function will internally create a copy of
;;; the memory (since GLib 2.60) or (in older versions) fail and exit the
;;; process.
;;;
;;; type :
;;;     a definite GVariantType
;;;
;;; data :
;;;     the serialised data
;;;
;;; size :
;;;     the size of data
;;;
;;; trusted :
;;;     TRUE if data is definitely in normal form
;;;
;;; notify :
;;;     function to call when data is no longer needed
;;;
;;; user_data :
;;;     data for notify
;;;
;;; Returns :
;;;     a new floating GVariant of type type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_from_bytes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_new_from_bytes" %variant-new-from-bytes)
    (:pointer (:struct variant))
  (vtype (boxed variant-type))
  (bytes (boxed bytes))
  (trusted :boolean))

(defun variant-new-from-bytes (vtype bytes &optional trusted)
 #+liber-documentation
 "@version{2025-05-25}
  @argument[vtype]{a @class{g:variant-type} instance, or a valid type string}
  @argument[bytes]{a @class{g:bytes} instance}
  @argument[trusted]{an optional boolean whether the contents of @arg{bytes}
    are trusted, or the default @em{false} value}
  @return{The new @symbol{g:variant} instance with a floating reference.}
  @begin{short}
    Constructs a new serialised-mode @symbol{g:variant} instance.
  @end{short}
  This is the inner interface for creation of new serialised values that gets
  called from various functions in the C library. A reference is taken on bytes.

  The data in @arg{bytes} must be aligned appropriately for the type being
  loaded. Otherwise this function will internally create a copy of the memory.
  @see-symbol{g:variant}
  @see-class{g:variant-type}
  @see-class{g:bytes}"
  (let ((vtype (if (stringp vtype)
                   (variant-type-new vtype)
                   vtype)))
    (%variant-new-from-bytes vtype bytes trusted)))

(export 'variant-new-from-bytes)

;;; ----------------------------------------------------------------------------
;;; g_variant_byteswap ()
;;;
;;; GVariant * g_variant_byteswap (GVariant *value);
;;;
;;; Performs a byteswapping operation on the contents of value. The result is
;;; that all multi-byte numeric data contained in value is byteswapped. That
;;; includes 16, 32, and 64bit signed and unsigned integers as well as file
;;; handles and double precision floating point values.
;;;
;;; This function is an identity mapping on any value that does not contain
;;; multi-byte numeric data. That include strings, booleans, bytes and
;;; containers containing only these things (recursively).
;;;
;;; The returned value is always in normal form and is marked as trusted.
;;;
;;; value :
;;;     a GVariant
;;;
;;; Returns :
;;;     the byteswapped form of value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_normal_form ()
;;;
;;; GVariant * g_variant_get_normal_form (GVariant *value);
;;;
;;; Gets a GVariant instance that has the same value as value and is trusted to
;;; be in normal form.
;;;
;;; If value is already trusted to be in normal form then a new reference to
;;; value is returned.
;;;
;;; If value is not already trusted, then it is scanned to check if it is in
;;; normal form. If it is found to be in normal form then it is marked as
;;; trusted and a new reference to it is returned.
;;;
;;; If value is found not to be in normal form then a new trusted GVariant is
;;; created with the same value as value.
;;;
;;; It makes sense to call this function if you've received GVariant data from
;;; untrusted sources and you want to ensure your serialised output is
;;; definitely in normal form.
;;;
;;; value :
;;;     a GVariant
;;;
;;; Returns :
;;;     a trusted GVariant
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_is_normal_form ()
;;;
;;; gboolean g_variant_is_normal_form (GVariant *value);
;;;
;;; Checks if value is in normal form.
;;;
;;; The main reason to do this is to detect if a given chunk of serialised data
;;; is in normal form: load the data into a GVariant using
;;; g_variant_new_from_data() and then use this function to check.
;;;
;;; If value is found to be in normal form then it will be marked as being
;;; trusted. If the value was already marked as being trusted then this function
;;; will immediately return TRUE.
;;;
;;; value :
;;;     a GVariant instance
;;;
;;; Returns :
;;;     TRUE if value is in normal form
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_hash ()
;;;
;;; guint g_variant_hash (gconstpointer value);
;;;
;;; Generates a hash value for a GVariant instance.
;;;
;;; The output of this function is guaranteed to be the same for a given value
;;; only per-process. It may change between different processor architectures or
;;; even different versions of GLib. Do not use this function as a basis for
;;; building protocols or file formats.
;;;
;;; The type of value is gconstpointer only to allow use of this function with
;;; GHashTable. value must be a GVariant.
;;;
;;; value :
;;;     a basic GVariant value as a gconstpointer
;;;
;;; Returns :
;;;     a hash value corresponding to value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_equal" variant-equal) :boolean
 #+liber-documentation
 "@version{#2025-05-25}
  @argument[value1]{a @symbol{g:variant} instance}
  @argument[value2]{a @symbol{g:variant} instance}
  @return{@em{True} if @arg{value} and @arg{value} are equal.}
  @begin{short}
    Checks if @arg{value} and @arg{value} have the same type and value.
  @end{short}
  @see-symbol{g:variant}"
  (value1 (:pointer (:struct variant)))
  (value2 (:pointer (:struct variant))))

(export 'variant-equal)

;;; ----------------------------------------------------------------------------
;;; g_variant_print
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_print" %variant-print) (:string :free-from-foreign t)
  (value (:pointer (:struct variant)))
  (annotate :boolean))

(defun variant-print (value &optional (annotate nil))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[value]{a @symbol{g:variant} instance}
  @argument[annotate]{@em{true} if type information should be included in the
    output}
  @return{The string holding the result.}
  @begin{short}
    Pretty-prints @arg{value} in the format understood by the
    @fun{g:variant-parse} function.
  @end{short}
  If the @arg{annotate} argument is @em{true}, then type information is included
  in the output.
  @see-symbol{g:variant}
  @see-function{g:variant-parse}"
  (%variant-print value annotate))

(export 'variant-print)

;;; ----------------------------------------------------------------------------
;;; g_variant_print_string ()
;;;
;;; GString * g_variant_print_string (GVariant *value,
;;;                                   GString *string,
;;;                                   gboolean type_annotate);
;;;
;;; Behaves as g_variant_print(), but operates on a GString.
;;;
;;; If string is non-NULL then it is appended to and returned. Else, a new empty
;;; GString is allocated and it is returned.
;;;
;;; value :
;;;     a GVariant
;;;
;;; string :
;;;     a GString, or NULL
;;;
;;; type_annotate :
;;;     TRUE if type information should be included in the output
;;;
;;; Returns :
;;;     a GString containing the string
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_parse
;;; ----------------------------------------------------------------------------

;; FIXME: Does not work for an argument non-nil for VTYPE

(cffi:defcfun ("g_variant_parse" %variant-parse-1) (:pointer (:struct variant))
  (vtype :pointer) ; must be the type :pointer
  (text :string)
  (limit :pointer)
  (endptr :pointer)
  (err :pointer))

(cffi:defcfun ("g_variant_parse" %variant-parse-2) (:pointer (:struct variant))
  (vtype (boxed variant-type))
  (text :string)
  (limit :pointer)
  (endptr :pointer)
  (err :pointer))

(defun variant-parse (vtype text)
 #+liber-documentation
 "@version{2025-05-25}
  @argument[vtype]{a @class{g:variant-type} instance, or a valid type string}
  @argument[text]{a string containing a @symbol{g:variant} instance in text
    form}
  @return{The @symbol{g:variant} instance.}
  @begin{short}
    Parses a @symbol{g:variant} instance from a text representation.
  @end{short}
  If @arg{vtype} is non-@code{nil} then the value will be parsed to have that
  type. This may result in additional parse errors, in the case that the parsed
  value does not fit the variant type, but may also result in fewer errors, in
  the case that the variant type would have been ambiguous, such as with empty
  arrays.

  In the event that the parsing is successful, the resulting @symbol{g:variant}
  instance is returned. In case of any error, @code{nil} will be returned.

  Officially, the language understood by the parser is any string produced by
  the @fun{g:variant-print} function.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:variant-parse (g:variant-type-new \"b\") \"true\")
=> #.(SB-SYS:INT-SAP #X7F99C4012440)
(g:variant-print *) => \"true\"
(g:variant-parse \"b\" \"false\")
=> #.(SB-SYS:INT-SAP #X564C855E8690)
(g:variant-print *) => \"false\"
(g:variant-parse (g:variant-type-new \"i\") \"100\")
=> #.(SB-SYS:INT-SAP #X7F99C4012CF0)
(g:variant-print * nil) => \"100\"
(g:variant-parse \"d\" \"100\")
=> #.(SB-SYS:INT-SAP #X564C855F9900)
(g:variant-print *) => \"100.0\"
    @end{pre}
  @end{dictionary}
  @see-symbol{g:variant}
  @see-function{g:variant-print}"
  (with-error (err)
    (cond ((stringp vtype)
           (let ((vtype1 (variant-type-new vtype)))
               (%variant-parse-2 vtype1
                                 text
                                 (cffi:null-pointer)
                                 (cffi:null-pointer)
                                 err)))
          ((typep vtype 'variant-type)
           (%variant-parse-2 vtype
                             text
                             (cffi:null-pointer)
                             (cffi:null-pointer)
                             err))
          (t
           (%variant-parse-1 (cffi:null-pointer)
                             text
                             (cffi:null-pointer)
                             (cffi:null-pointer)
                             err)))))

(export 'variant-parse)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_parsed_va ()
;;;
;;; GVariant * g_variant_new_parsed_va (const gchar *format, va_list *app);
;;;
;;; Parses format and returns the result.
;;;
;;; This is the version of g_variant_new_parsed() intended to be used from
;;; libraries.
;;;
;;; The return value will be floating if it was a newly created GVariant
;;; instance. In the case that format simply specified the collection of a
;;; GVariant pointer (eg: format was "%*") then the collected GVariant pointer
;;; will be returned unmodified, without adding any additional references.
;;;
;;; In order to behave correctly in all cases it is necessary for the calling
;;; function to g_variant_ref_sink() the return result before returning control
;;; to the user that originally provided the pointer. At this point, the caller
;;; will have their own full reference to the result. This can also be done by
;;; adding the result to a container, or by passing it to another
;;; g_variant_new() call.
;;;
;;; format :
;;;     a text format GVariant
;;;
;;; app :
;;;     a pointer to a va_list
;;;
;;; Returns :
;;;     a new, usually floating, GVariant
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_parsed ()
;;;
;;; GVariant * g_variant_new_parsed (const gchar *format, ...);
;;;
;;; Parses format and returns the result.
;;;
;;; format must be a text format GVariant with one extension: at any point that
;;; a value may appear in the text, a '%' character followed by a GVariant
;;; format string (as per g_variant_new()) may appear. In that case, the same
;;; arguments are collected from the argument list as g_variant_new() would
;;; have collected.
;;;
;;; Consider this simple example:
;;;
;;; g_variant_new_parsed ("[('one', 1), ('two', %i), (%s, 3)]", 2, "three");
;;;
;;; In the example, the variable argument parameters are collected and filled
;;; in as if they were part of the original string to produce the result of
;;; [('one', 1), ('two', 2), ('three', 3)].
;;;
;;; This function is intended only to be used with format as a string literal.
;;; Any parse error is fatal to the calling process. If you want to parse data
;;; from untrusted sources, use g_variant_parse().
;;;
;;; You may not use this function to return, unmodified, a single GVariant
;;; pointer from the argument list. ie: format may not solely be anything along
;;; the lines of "%*", "%?", "%r", or anything starting with "%@".
;;;
;;; format :
;;;     a text format GVariant
;;;
;;; ... :
;;;     arguments as per format
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_parse_error_print_context ()
;;;
;;; gchar *
;;; g_variant_parse_error_print_context (GError *error,
;;;                                      const gchar *source_str);
;;;
;;; Pretty-prints a message showing the context of a GVariant parse error
;;; within the string for which parsing was attempted.
;;;
;;; The resulting string is suitable for output to the console or other
;;; monospace media where newlines are treated in the usual way.
;;;
;;; The message will typically look something like one of the following:
;;;
;;; unterminated string constant:
;;;   (1, 2, 3, 'abc
;;;             ^^^^
;;; or
;;;
;;; unable to find a common type:
;;;   [1, 2, 3, 'str']
;;;    ^        ^^^^^
;;;
;;; The format of the message may change in a future version.
;;;
;;; error must have come from a failed attempt to g_variant_parse() and
;;; source_str must be exactly the same string that caused the error. If
;;; source_str was not nul-terminated when you passed it to g_variant_parse()
;;; then you must add nul termination before using this function.
;;;
;;; error :
;;;     a GError from the GVariantParseError domain
;;;
;;; source_str :
;;;     the string that was given to the parser
;;;
;;; Returns :
;;;     the printed message.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GVariantDict
;;; ----------------------------------------------------------------------------

(define-gboxed-opaque variant-dict "GVariantDict"
  :export t
  :type-initializer "g_variant_dict_get_type"
  :alloc (%variant-dict-new (cffi:null-pointer)))

#+liber-documentation
(setf (liber:alias-for-class 'variant-dict)
      "GBoxed"
      (documentation 'variant-dict 'type)
 "@version{2025-05-25}
  @begin{declaration}
(define-gboxed-opaque variant-dict \"GVariantDict\"
  :export t
  :type-initializer \"g_variant_dict_get_type\"
  :alloc (%variant-dict-new (cffi:null-pointer)))
  @end{declaration}
  @begin{short}
    The @class{g:variant-dict} structure is a mutable interface to
    @symbol{g:variant} dictionaries.
  @end{short}
  It can be used for doing a sequence of dictionary lookups in an efficient
  way on an existing @symbol{g:variant} dictionary or it can be used to
  construct new dictionaries with a hashtable-like interface. It can also be
  used for taking existing dictionaries and modifying them in order to create
  new ones. The @class{g:variant-dict} structure can only be used with
  @code{a(sv)} dictionaries.

  You allocate a @class{g:variant-dict} instance with the
  @fun{g:variant-dict-new} function. The @fun{g:variant-dict-end} function is
  used to convert the @class{g:variant-dict} instance back into a
  @symbol{g:variant} dictionary type. This also implicitly frees all associated
  memory.
  @begin[Examples]{dictionary}
    Using a  @class{g:variant-dict} instance:
    @begin{pre}
(defun add-to-count (orig)
  (let* ((dict (g:variant-dict-new orig))
         (variant (g:variant-dict-lookup-value dict \"count\")))
    (when variant
      (let ((value (1+ (g:variant-int32 variant))))
        (g:variant-dict-insert-value dict \"count\" (g:variant-new-int32 value))
        (g:variant-dict-end dict)))))
    @end{pre}
  @end{dictionary}
  @see-constructor{g:variant-dict-new}
  @see-symbol{g:variant}")

(export 'variant-dict)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_DICT_INIT()
;;;
;;; #define G_VARIANT_DICT_INIT(asv) { { { asv, 3488698669u, { 0, } } } }
;;;
;;; A stack-allocated GVariantDict must be initialized if it is used together
;;; with g_auto() to avoid warnings or crashes if function returns before
;;; g_variant_dict_init() is called on the builder. This macro can be used as
;;; initializer instead of an explicit zeroing a variable when declaring it and
;;; a following g_variant_dict_init(), but it cannot be assigned to a variable.
;;;
;;; The passed asv has to live long enough for GVariantDict to gather the
;;; entries from, as the gathering does not happen in the G_VARIANT_DICT_INIT()
;;; call, but rather in functions that make sure that GVariantDict is valid. In
;;; context where the initialization value has to be a constant expression, the
;;; only possible value of asv is NULL. It is still possible to call
;;; g_variant_dict_init() safely with a different asv right after the variable
;;; was initialized with G_VARIANT_DICT_INIT().
;;;
;;; g_autoptr(GVariant) variant = get_asv_variant ();
;;; g_auto(GVariantDict) dict = G_VARIANT_DICT_INIT (variant);
;;;
;;; asv :
;;;     a GVariant*
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_unref ()
;;;
;;; void
;;; g_variant_dict_unref (GVariantDict *dict);
;;;
;;; Decreases the reference count on dict .
;;;
;;; In the event that there are no more references, releases all memory
;;; associated with the GVariantDict.
;;;
;;; Don't call this on stack-allocated GVariantDict instances or bad things
;;; will happen.
;;;
;;; dict :
;;;     a heap-allocated GVariantDict
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_ref ()
;;;
;;; GVariantDict *
;;; g_variant_dict_ref (GVariantDict *dict);
;;;
;;; Increases the reference count on dict .
;;;
;;; Don't call this on stack-allocated GVariantDict instances or bad things
;;; will happen.
;;;
;;; dict :
;;;     a heap-allocated GVariantDict
;;;
;;; Returns :
;;;     a new reference to dict
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_dict_new" %variant-dict-new) :pointer
  (from-asv (:pointer (:struct variant))))

(cffi:defcfun ("g_variant_dict_new" variant-dict-new)
    (boxed variant-dict :return)
 #+liber-documentation
 "@version{2025-05-25}
  @argument[from-asv]{a @symbol{g:variant} instance for initialization the
    dictionary}
  @return{The newly created @class{g:variant-dict} instance.}
  @begin{short}
    Allocates and initialises a new @class{g:variant-dict} instance.
  @end{short}
  @see-class{g:variant-dict}
  @see-symbol{g:variant}"
  (from-asv (:pointer (:struct variant))))

(export 'variant-dict-new)

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_init ()                                 not needed
;;;
;;; void
;;; g_variant_dict_init (GVariantDict *dict,
;;;                      GVariant *from_asv);
;;;
;;; Initialises a GVariantDict structure.
;;;
;;; If from_asv is given, it is used to initialise the dictionary.
;;;
;;; This function completely ignores the previous contents of dict . On one
;;; hand this means that it is valid to pass in completely uninitialised memory.
;;; On the other hand, this means that if you are initialising over top of an
;;; existing GVariantDict you need to first call g_variant_dict_clear() in
;;; order to avoid leaking memory.
;;;
;;; You must not call g_variant_dict_ref() or g_variant_dict_unref() on a
;;; GVariantDict that was initialised with this function. If you ever pass a
;;; reference to a GVariantDict outside of the control of your own code then
;;; you should assume that the person receiving that reference may try to use
;;; reference counting; you should use g_variant_dict_new() instead of this
;;; function.
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; from_asv :
;;;     the initial value for dict
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_clear ()                                not needed
;;;
;;; void
;;; g_variant_dict_clear (GVariantDict *dict);
;;;
;;; Releases all memory associated with a GVariantDict without freeing the
;;; GVariantDict structure itself.
;;;
;;; It typically only makes sense to do this on a stack-allocated GVariantDict
;;; if you want to abort building the value part-way through. This function
;;; need not be called if you call g_variant_dict_end() and it also does not
;;; need to be called on dicts allocated with g_variant_dict_new
;;; (see g_variant_dict_unref() for that).
;;;
;;; It is valid to call this function on either an initialised GVariantDict or
;;; one that was previously cleared by an earlier call to g_variant_dict_clear()
;;; but it is not valid to call this function on uninitialised memory.
;;;
;;; dict :
;;;     a GVariantDict
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_contains
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_dict_contains" variant-dict-contains) :boolean
 #+liber-documentation
 "@version{2025-05-25}
  @argument[dict]{a @class{g:variant-dict} instance}
  @argument[key]{a string for the key to look up in the dictionary}
  @return{@em{True} if @arg{key} is in the dictionary.}
  @begin{short}
    Checks if the key exists in the dictionary.
  @end{short}
  @see-class{g:variant-dict}"
  (dict (boxed variant-dict))
  (key :string))

(export 'variant-dict-contains)

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_lookup ()
;;;
;;; gboolean
;;; g_variant_dict_lookup (GVariantDict *dict,
;;;                        const gchar *key,
;;;                        const gchar *format_string,
;;;                        ...);
;;;
;;; Looks up a value in a GVariantDict.
;;;
;;; This function is a wrapper around g_variant_dict_lookup_value() and
;;; g_variant_get(). In the case that NULL would have been returned, this
;;; function returns FALSE. Otherwise, it unpacks the returned value and
;;; returns TRUE.
;;;
;;; format_string determines the C types that are used for unpacking the values
;;; and also determines if the values are copied or borrowed, see the section
;;; on GVariant format strings.
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; key :
;;;     the key to look up in the dictionary
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     the arguments to unpack the value into
;;;
;;; Returns :
;;;     TRUE if a value was unpacked
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_lookup_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_dict_lookup_value" %variant-dict-lookup-value)
    (:pointer (:struct variant))
  (dict (boxed variant-dict))
  (key :string)
  (vtype (boxed variant-type)))

(defun variant-dict-lookup-value (dict key &optional vtype)
 #+liber-documentation
 "@version{2025-05-25}
  @argument[dict]{a @class{g:variant-dict} instance}
  @argument[key]{a string for the key to look up in the dictionary}
  @argument[vtype]{a @class{g:variant-type} instance for the expected type}
  @begin{short}
    Looks up a value in a @class{g:variant-dict} instance.
  @end{short}
  If @arg{key} is not found in the dictionary, @code{nil} is returned.

  The @arg{vtype} string specifies what type of value is expected. If the value
  associated with @arg{key} has a different type then @code{nil} is returned.

  If the key is found and the value has the correct type, it is returned. If
  @arg{vtype} was specified then any non-@code{nil} return value will have this
  type.
  @see-class{g:variant-dict}
  @see-class{g:variant-type}"
  (let ((vtype (if (stringp vtype)
                   (variant-type-new vtype)
                   vtype)))
    (%variant-dict-lookup-value dict key vtype)))

(export 'variant-dict-lookup-value)

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_insert ()
;;;
;;; void
;;; g_variant_dict_insert (GVariantDict *dict,
;;;                        const gchar *key,
;;;                        const gchar *format_string,
;;;                        ...);
;;;
;;; Inserts a value into a GVariantDict.
;;;
;;; This call is a convenience wrapper that is exactly equivalent to calling
;;; g_variant_new() followed by g_variant_dict_insert_value().
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; key :
;;;     the key to insert a value for
;;;
;;; format_string :
;;;     a GVariant varargs format string
;;;
;;; ... :
;;;     arguments, as per format_string
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_insert_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_dict_insert_value" variant-dict-insert-value) :void
 #+liber-documentation
 "@version{2025-05-25}
  @argument[dict]{a @class{g:variant-dict} instance}
  @argument[key]{a string for the key to insert a value for}
  @argument[value]{a @symbol{g:variant} instance for the value to insert}
  @begin{short}
    Inserts, or replaces, a key in a @class{g:variant-dict} instance.
  @end{short}
  @see-class{g:variant-dict}
  @see-symbol{g:variant}"
  (dict (boxed variant-dict))
  (key :string)
  (value (:pointer (:struct variant))))

(export 'variant-dict-insert-value)

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_dict_remove" variant-dict-remove) :boolean
 #+liber-documentation
 "@version{2025-05-25}
  @argument[dict]{a @class{g:variant-dict} instance}
  @argument[key]{a string for the key to remove}
  @return{@em{True} if the key was found and removed.}
  @begin{short}
    Removes a key and its associated value from a @class{g:variant-dict}
    instance.
  @end{short}
  @see-class{g:variant-dict}"
  (dict (boxed variant-dict))
  (key :string))

(export 'variant-dict-remove)

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_dict_end" variant-dict-end)
    (:pointer (:struct variant))
 #+liber-documentation
 "@version{2025-05-25}
  @argument[dict]{a @class{g:variant-dict} instance}
  @return{The new @symbol{g:variant} instance.}
  @begin{short}
    Returns the current value of @arg{dict} as a @symbol{g:variant} instance,
    clearing it in the process.
  @end{short}

  It is not permissible to use @arg{dict} in any way after this call except for
  reference counting operations.
  @see-class{g:variant-dict}
  @see-symbol{g:variant}"
  (dict (boxed variant-dict)))

(export 'variant-dict-end)

;;; ----------------------------------------------------------------------------
;;; GVariantIter
;;; ----------------------------------------------------------------------------

(cffi:defcstruct variant-iter)

#+liber-documentation
(setf (liber:alias-for-symbol 'variant-iter)
      "CStruct"
      (liber:symbol-documentation 'variant-iter)
 "@version{2025-05-25}
  @begin{declaration}
(cffi:defcstruct variant-iter)
  @end{declaration}
  @begin{short}
    The @symbol{g:variant-iter} structure is an opaque data structure and can
    only be accessed using the following functions.
  @end{short}
  @see-function{g:variant-iter-new}
  @see-function{g:variant-iter-next-value}")

(export 'variant-iter)

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_iter_copy" variant-iter-copy)
    (:pointer (:struct variant-iter))
 #+liber-documentation
 "@version{#2025-05-04}
  @argument[iter]{a @symbol{g:variant-iter} instance}
  @return{The new heap-allocated @symbol{g:variant-iter} instance.}
  @begin{short}
    Creates a new heap-allocated @symbol{g:variant-iter} instance to iterate
    over the container that was being iterated over by @arg{iter}.
  @end{short}
  Iteration begins on the new iterator from the current position of the old
  iterator but the two copies are independent past that point.

  Use the @fun{g:variant-iter-free} function to free the return value when you
  no longer need it.

  A reference is taken to the container that @arg{iter} is iterating over and
  will be released only when the @fun{g:variant-iter-free} function is called.
  @see-symbol{g:variant-iter}
  @see-function{g:variant-iter-free}"
  (iter (:pointer (:struct variant-iter))))

(export 'variant-iter-copy)

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_iter_free" variant-iter-free) :void
 #+liber-documentation
 "@version{2025-05-04}
  @argument[iter]{a heap-allocated @symbol{g:variant-iter} instance}
  @begin{short}
    Frees a heap-allocated @symbol{g:variant-iter} instance.
  @end{short}
  Only call this function on iterators that were returned by the
  @fun{g:variant-iter-new} or @fun{g:variant-iter-copy} functions.
  @see-symbol{g:variant-iter}
  @see-function{g:variant-iter-new}
  @see-function{g:variant-iter-free}"
  (iter (:pointer (:struct variant-iter))))

(export 'variant-iter-free)

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_init ()
;;;
;;; gsize g_variant_iter_init (GVariantIter *iter, GVariant *value);
;;;
;;; Initialises (without allocating) a GVariantIter. iter may be completely
;;; uninitialised prior to this call; its old value is ignored.
;;;
;;; The iterator remains valid for as long as value exists, and need not be
;;; freed in any way.
;;;
;;; iter :
;;;     a pointer to a GVariantIter
;;;
;;; value :
;;;     a container GVariant
;;;
;;; Returns :
;;;     the number of items in value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_n_children ()
;;;
;;; gsize g_variant_iter_n_children (GVariantIter *iter);
;;;
;;; Queries the number of child items in the container that we are iterating
;;; over. This is the total number of items -- not the number of items
;;; remaining.
;;;
;;; This function might be useful for preallocation of arrays.
;;;
;;; iter :
;;;     a GVariantIter
;;;
;;; Returns :
;;;     the number of children in the container
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_iter_new" variant-iter-new)
    (:pointer (:struct variant-iter))
 #+liber-documentation
 "@version{2025-05-04}
  @argument[value]{a @symbol{g:variant} instance}
  @return{The new heap-allocated @symbol{g:variant-iter} instance.}
  @begin{short}
    Creates a heap-allocated @symbol{g:variant-iter} instance for iterating
    over the items in @arg{value}.
  @end{short}
  Use the @fun{g:variant-iter-free} function to free the return value when you
  no longer need it.

  A reference is taken to @arg{value} and will be released only when the
  @fun{g:variant-iter-free} function is called.
  @see-symbol{g:variant-iter}
  @see-function{g:variant-iter-free}"
  (value (:pointer (:struct variant))))

(export 'variant-iter-new)

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_next_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_iter_next_value" %variant-iter-next-value)
    (:pointer (:struct variant))
  (iter (:pointer (:struct variant-iter))))

(defun variant-iter-next-value (iter)
 #+liber-documentation
 "@version{2025-05-04}
  @argument[iter]{a @symbol{g:variant-iter} instance}
  @return{The @symbol{g:variant} instance, or @code{nil}.}
  @begin{short}
    Gets the next item in the container.
  @end{short}
  If no more items remain then @code{nil} is returned.

  Use the @fun{g:variant-unref} function to drop your reference on the return
  value when you no longer need it.
  @begin[Examples]{dictionary}
    Iterating with the @fun{g:variant-iter-next-value} function.
    @begin{pre}
(defun iterate-container (container)
  (let ((iter (g:variant-iter-new container)))
    (iter (for value = (g:variant-iter-next-value iter))
          (while value)
          ;; Do something with VALUE
          ...
          (g:variant-unref value))
    (g:variant-iter-free iter)))
    @end{pre}
  @end{dictionary}
  @see-symbol{g:variant-iter}
  @see-symbol{g:variant}
  @see-function{g:variant-unref}"
  (let ((variant (%variant-iter-next-value iter)))
    (unless (cffi:null-pointer-p variant)
      variant)))

(export 'variant-iter-next-value)

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_next ()
;;;
;;; gboolean g_variant_iter_next (GVariantIter *iter,
;;;                               const gchar *format_string,
;;;                               ...);
;;;
;;; Gets the next item in the container and unpacks it into the variable
;;; argument list according to format_string, returning TRUE.
;;;
;;; If no more items remain then FALSE is returned.
;;;
;;; All of the pointers given on the variable arguments list of this function
;;; are assumed to point at uninitialised memory. It is the responsibility of
;;; the caller to free all of the values returned by the unpacking process.
;;;
;;; See the section on GVariant Format Strings.
;;;
;;; Example 19. Memory management with g_variant_iter_next()
;;;
;;; /* Iterates a dictionary of type 'a{sv}' */
;;; void
;;; iterate_dictionary (GVariant *dictionary)
;;; {
;;;   GVariantIter iter;
;;;   GVariant *value;
;;;   gchar *key;
;;;
;;;   g_variant_iter_init (&iter, dictionary);
;;;   while (g_variant_iter_next (&iter, "{sv}", &key, &value))
;;;     {
;;;       g_print ("Item '%s' has type '%s'\n", key,
;;;                g_variant_get_type_string (value));
;;;
;;;       /* must free data for ourselves */
;;;       g_variant_unref (value);
;;;       g_free (key);
;;;     }
;;; }
;;;
;;;
;;; For a solution that is likely to be more convenient to C programmers when
;;; dealing with loops, see g_variant_iter_loop().
;;;
;;; iter :
;;;     a GVariantIter
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     the arguments to unpack the value into
;;;
;;; Returns :
;;;     TRUE if a value was unpacked, or FALSE if there as no value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_loop ()
;;;
;;; gboolean g_variant_iter_loop (GVariantIter *iter,
;;;                               const gchar *format_string,
;;;                               ...);
;;;
;;; Gets the next item in the container and unpacks it into the variable
;;; argument list according to format_string, returning TRUE.
;;;
;;; If no more items remain then FALSE is returned.
;;;
;;; On the first call to this function, the pointers appearing on the variable
;;; argument list are assumed to point at uninitialised memory. On the second
;;; and later calls, it is assumed that the same pointers will be given and that
;;; they will point to the memory as set by the previous call to this function.
;;; This allows the previous values to be freed, as appropriate.
;;;
;;; This function is intended to be used with a while loop as demonstrated in
;;; the following example. This function can only be used when iterating over
;;; an array. It is only valid to call this function with a string constant for
;;; the format string and the same string constant must be used each time.
;;; Mixing calls to this function and g_variant_iter_next() or
;;; g_variant_iter_next_value() on the same iterator causes undefined behavior.
;;;
;;; If you break out of a such a while loop using g_variant_iter_loop() then you
;;; must free or unreference all the unpacked values as you would with
;;; g_variant_get(). Failure to do so will cause a memory leak.
;;;
;;; See the section on GVariant Format Strings.
;;;
;;; Example 20. Memory management with g_variant_iter_loop()
;;;
;;; /* Iterates a dictionary of type 'a{sv}' */
;;; void
;;; iterate_dictionary (GVariant *dictionary)
;;; {
;;;   GVariantIter iter;
;;;   GVariant *value;
;;;   gchar *key;
;;;
;;;   g_variant_iter_init (&iter, dictionary);
;;;   while (g_variant_iter_loop (&iter, "{sv}", &key, &value))
;;;     {
;;;       g_print ("Item '%s' has type '%s'\n", key,
;;;                g_variant_get_type_string (value));
;;;
;;;       /* no need to free 'key' and 'value' here */
;;;       /* unless breaking out of this loop */
;;;     }
;;; }
;;;
;;;
;;; For most cases you should use g_variant_iter_next().
;;;
;;; This function is really only useful when unpacking into GVariant or
;;; GVariantIter in order to allow you to skip the call to g_variant_unref() or
;;; g_variant_iter_free().
;;;
;;; For example, if you are only looping over simple integer and string types,
;;; g_variant_iter_next() is definitely preferred. For string types, use the '&'
;;; prefix to avoid allocating any memory at all (and thereby avoiding the need
;;; to free anything as well).
;;;
;;; iter :
;;;     a GVariantIter
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     the arguments to unpack the value into
;;;
;;; Returns :
;;;     TRUE if a value was unpacked, or FALSE if there was no value
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.variant.lisp ------------------------------------------
