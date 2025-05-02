;;; ----------------------------------------------------------------------------
;;; glib.variant-type.lisp
;;;
;;; The documentation in this file is taken from the GLib Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GLib library,
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
;;; GVariantType
;;;
;;;     Introduction to the GVariant type system
;;;
;;; Types and Values
;;;
;;;     GVariantType
;;;
;;; Functions
;;;
;;;     g_variant_type_free                                 not implemented
;;;     g_variant_type_new
;;;     g_variant_type_copy
;;;
;;;     g_variant_type_string_is_valid
;;;     g_variant_type_string_scan                          not implemented
;;;     g_variant_type_get_string_length                    not implemented
;;;     g_variant_type_peek_string                          not implemented
;;;     g_variant_type_dup_string
;;;
;;;     g_variant_type_is_definite
;;;     g_variant_type_is_container
;;;     g_variant_type_is_basic
;;;     g_variant_type_is_maybe
;;;     g_variant_type_is_array
;;;     g_variant_type_is_tuple
;;;     g_variant_type_is_dict_entry
;;;     g_variant_type_is_variant
;;;
;;;     g_variant_type_hash
;;;     g_variant_type_equal
;;;     g_variant_type_is_subtype_of
;;;
;;;     g_variant_type_new_maybe
;;;     g_variant_type_new_array
;;;     g_variant_type_new_tuple
;;;     g_variant_type_new_dict_entry
;;;
;;;     g_variant_type_element
;;;     g_variant_type_n_items
;;;     g_variant_type_first
;;;     g_variant_type_next
;;;     g_variant_type_key
;;;     g_variant_type_value
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GVariantType
;;; ----------------------------------------------------------------------------

(define-gboxed-opaque variant-type "GVariantType"
  :export t
  :type-initializer "g_variant_type_get_gtype"
  :alloc (cl:error "GVariantType cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'variant-type)
      "GBoxed"
      (documentation 'variant-type 'type)
 "@version{2024-11-20}
  @begin{short}
    The GVariant type system is based, in large part, on the D-Bus type system,
    with two major changes and some minor lifting of restrictions.
  @end{short}
  The D-Bus specification, therefore, provides a significant amount of
  information that is useful when working with @symbol{g:variant} parameters.

  The first major change with respect to the D-Bus type system is the
  introduction of maybe (or \"nullable\") types. Any type in the GVariant type
  system can be converted to a maybe type, in which case, \"nothing\" (or
  \"null\") becomes a valid value. Maybe types have been added by introducing
  the character @code{\"m\"} to type strings.

  The second major change is that the GVariant type system supports the concept
  of \"indefinite types\" - types that are less specific than the normal types
  found in D-Bus. For example, it is possible to speak of \"an array of any
  type\" in the GVariant type system, where the D-Bus type system would require
  you to speak of \"an array of integers\" or \"an array of strings\".
  Indefinite types have been added by introducing the characters @code{\"*\"},
  @code{\"?\"} and @code{\"r\"} to type strings.

  Finally, all arbitrary restrictions relating to the complexity of types are
  lifted along with the restriction that dictionary entries may only appear
  nested inside of arrays.

  Just as in D-Bus, GVariant types are described with type strings. Subject to
  the differences mentioned above, these strings are of the same form as those
  found in D-Bus. Note, however: D-Bus always works in terms of messages and
  therefore individual type strings appear nowhere in its interface. Instead,
  \"signatures\" are a concatenation of the strings of the type of each argument
  in a message. The GVariant type system deals with single values directly so
  GVariant type strings always describe the type of exactly one value. This
  means that a D-Bus signature string is generally not a valid GVariant type
  string - except in the case that it is the signature of a message containing
  exactly one argument.

  An indefinite type is similar in spirit to what may be called an abstract
  type in other type systems. No value can exist that has an indefinite type
  as its type, but values can exist that have types that are subtypes of
  indefinite types. That is to say, the @fun{g:variant-type} function will never
  return an indefinite type, but calling the @fun{g:variant-is-of-type} function
  with an indefinite type may return @em{true}. For example, you cannot have a
  value that represents \"an array of no particular type\", but you can have an
  \"array of integers\" which certainly matches the type of \"an array of no
  particular type\", since \"array of integers\" is a subtype of \"array of no
  particular type\".

  This is similar to how instances of abstract classes may not directly exist
  in other type systems, but instances of their non-abstract subtypes may. For
  example, in GTK, no object that has the @code{GtkWidget} type can exist, since
  the @code{GtkWidget} class is an abstract class, but a @code{GtkButton} object
  can certainly be instantiated, and you would say that the @code{GtkButton} is
  a @code{GtkWidget} object, since the @code{GtkButton} class is a subclass of
  the @code{GtkWidget} class.

  Two types may not be compared by value. Use the @fun{g:variant-type-equal}
  or @fun{g:variant-type-is-subtype-of} functions.

  @subheading{GVariant Type Strings}
  The GVariant type string can be any of the following:
  @begin{itemize}
    @item{any basic type string (listed below)}
    @item{@code{\"v\"}, @code{\"r\"} or @code{\"*\"}}
    @item{one of the characters @code{\"a\"} or @code{\"m\"}, followed by
      another type string}
    @item{the character @code{\"(\"}, followed by a concatenation of zero or
      more other type strings, followed by the character @code{\")\"}}
    @item{the character @code{\"{\"}, followed by a basic type string (see
      below), followed by another type string, followed by the character
      @code{\"@}\"}}
  @end{itemize}
  A basic type string describes a basic type as per the
  @fun{g:variant-type-is-basic} function and is always a single character in
  length. The valid basic type strings are @code{\"b\"}, @code{\"y\"},
  @code{\"n\"}, @code{\"q\"}, @code{\"i\"}, @code{\"u\"}, @code{\"x\"},
  @code{\"t\"}, @code{\"h\"}, @code{\"d\"}, @code{\"s\"}, @code{\"o\"},
  @code{\"g\"} and @code{\"?\"}.

  The above definition is recursive to arbitrary depth. The @code{\"aaaaai\"}
  and @code{\"(ui(nq((y)))s)\"} type strings are both valid type strings, as is
  the @code{\"a(aa(ui)(qna{ya(yd)@}))\"} type string.

  The meaning of each of the characters is as follows:
  @begin[code]{table}
     @entry[b]{The type string of a boolean value.}
     @entry[y]{The type string of a byte.}
     @entry[n]{The type string of a signed 16 bit integer.}
     @entry[q]{The type string of an unsigned 16 bit integer.}
     @entry[i]{The type string of a signed 32 bit integer.}
     @entry[u]{The type string of an unsigned 32 bit integer.}
     @entry[x]{The type string of a signed 64 bit integer.}
     @entry[t]{The type string of an unsigned 64 bit integer.}
     @entry[h]{The type string of a signed 32 bit value that, by convention,
       is used as an index into an array of file descriptors that are sent
       alongside a D-Bus message.}
     @entry[d]{The type string of a double precision floating point value.}
     @entry[s]{The type string of a string.}
     @entry[o]{The type string of a string in the form of a D-Bus object path.}
     @entry[g]{The type string of a string in the form of a D-Bus type
       signature.}
     @entry[?]{The type string of an indefinite type that is a supertype of any
       of the basic types.}
     @entry[v]{The type string of a container type that contain any other type
       of value.}
     @entry[a]{Used as a prefix on another type string to mean an array of
       that type. The @code{\"ai\"} type string, for example, is the type of an
       array of 32 bit signed integers.}
     @entry[m]{Used as a prefix on another type string to mean a \"maybe\", or
       \"nullable\", version of that type. The @code{\"ms\"} type string, for
       example, is the type of a value that maybe contains a string, or maybe
       contains nothing.}
     @entry[()]{Used to enclose zero or more other concatenated type strings
       to create a tuple type. The @code{\"(is)\"} type string, for example, is
       the type of a pair of an integer and a string.}
     @entry[r]{The type string of an indefinite type that is a supertype of any
       tuple type, regardless of the number of items.}
     @entry[{}]{Used to enclose a basic type string concatenated with another
       type string to create a dictionary entry type, which usually appears
       inside of an array to form a dictionary. The @code{\"a{sd@}\"} type
       string, for example, is the type of a dictionary that maps strings to
       double precision floating point values. The first type (the basic type)
       is the key type and the second type is the value type. The reason that
       the first type is restricted to being a basic type is so that it can
       easily be hashed.}
     @entry[*]{The type string of the indefinite type that is a supertype of all
       types. Note that, as with all type strings, this character represents
       exactly one type. It cannot be used inside of tuples to mean \"any number
       of items\".}
  @end{table}
  Any type string of a container that contains an indefinite type is, itself,
  an indefinite type. For example, the @code{\"a*\"} type string (corresponding
  to an array) is an indefinite type that is a supertype of every array type.
  The @code{\"(*s)\"} type string is a supertype of all tuples that contain
  exactly two items where the second item is a string.

  The @code{\"a{?*@}\"} type string is an indefinite type that is a supertype of
  all arrays containing dictionary entries where the key is any basic type and
  the value is any type at all. This is, by definition, a dictionary. Note that,
  due to the restriction that the key of a dictionary entry must be a basic
  type. The @code{\"{**@}\"} type string is not a valid type string.

  Two types may not be compared by value. Use the @fun{g:variant-type-equal} or
  @fun{g:variant-type-is-subtype-of} functions. May be copied using the
  @fun{g:variant-type-copy} function.
  @see-constructor{g:variant-type-new}
  @see-constrcutor{g:variant-type-copy}
  @see-constructor{g:variant-type-new-array}
  @see-constructor{g:variant-type-new-dict-entry}
  @see-constructor{g:variant-type-new-maybe}
  @see-constructor{g:variant-type-new-tuple}
  @see-symbol{g:variant}
  @see-function{g:variant-type}
  @see-function{g:variant-is-of-type}
  @see-function{g:variant-type-is-basic}
  @see-function{g:variant-type-equal}
  @see-function{g:variant-type-is-subtype-of}
  @see-function{g:variant-type-copy}")

(export 'variant-type)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE -> variant-type-checked                 not exported
;;; ----------------------------------------------------------------------------

;; We call the private C function g_variant_type_checked_ to define a Lisp
;; function which returns a GVariantType from a valid type string.

(cffi:defcfun ("g_variant_type_checked_" variant-type-checked)
    (boxed variant-type)
 #+liber-documentation
 "@version{#2021-7-31}
  @argument[string]{a well-formed @class{variant-type} type string}
  @return{The @class{variant-type} instance.}
  @begin{short}
    Converts a string to a @class{variant-type} instance.
  @end{short}
  Depending on the current debugging level, this function may perform a runtime
  check to ensure that the type string is a valid GVariant type string.

  It is always a programmer error to use this function with an invalid type
  string. If in doubt, use the @fun{variant-type-string-is-valid} function to
  check if the string is valid.
  @see-class{variant-type}
  @see-function{variant-type-string-is-valid}"
  (string :string))

;;; ----------------------------------------------------------------------------
;;; g_variant_type_free                                     not needed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_free" variant-type-free) :void
 #+liber-documentation
 "@version{#2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @begin{short}
    Frees a @class{g:variant-type} instance that was allocated with the
    @fun{g:variant-type-copy} or @fun{g:variant-type-new} functions or one of
    the container type constructor functions.
  @end{short}
  @see-class{g:variant-type}
  @see-function{g:variant-type-copy}
  @see-function{g:variant-type-new}"
  (vtype (boxed variant-type)))

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_new" variant-type-new)
    (boxed variant-type :return)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[string]{a valid @class{g:variant-type} type string}
  @return{The new @class{g:variant-type} instance.}
  @begin{short}
    Creates a new variant type corresponding to the type string given by
    @arg{string}.
  @end{short}
  It is a programmer error to call this function with an invalid type string.
  Use the @fun{g:variant-type-string-is-valid} function if you are unsure.
  @begin[Examples]{dictionary}
    @begin{pre}
;; A string variant type
(g:variant-type-new \"s\") => #<GLIB:VARIANT-TYPE {100FEBF233@}>
;; An integer variant type
(g:variant-type-new \"i\") => #<GLIB:VARIANT-TYPE {100FEBF613@}>
    @end{pre}
  @end{dictionary}
  @see-class{g:variant-type}
  @see-function{g:variant-type-string-is-valid}"
  (string :string))

(export 'variant-type-new)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_copy" variant-type-copy)
    (boxed variant-type :return)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{The new @class{g:variant-type} instance.}
  @begin{short}
    Makes a copy of a variant type.
  @end{short}
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-copy)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_string_is_valid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_string_is_valid" variant-type-string-is-valid)
    :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[string]{a @class{g:variant-type} type string}
  @return{@em{True} if @arg{string} is exactly one valid type string.}
  @begin{short}
    Checks if @arg{string} is a valid variant type string.
  @end{short}
  @see-class{g:variant-type}"
  (string :string))

(export 'variant-type-string-is-valid)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_string_scan ()
;;;
;;; gboolean g_variant_type_string_scan (const gchar *string,
;;;                                      const gchar *limit,
;;;                                      const gchar **endptr);
;;;
;;; Scan for a single complete and valid GVariant type string in string. The
;;; memory pointed to by limit (or bytes beyond it) is never accessed.
;;;
;;; If a valid type string is found, endptr is updated to point to the first
;;; character past the end of the string that was found and TRUE is returned.
;;;
;;; If there is no valid type string starting at string, or if the type string
;;; does not end before limit then FALSE is returned.
;;;
;;; For the simple case of checking if a string is a valid type string, see
;;; g_variant_type_string_is_valid().
;;;
;;; string :
;;;     a pointer to any string
;;;
;;; limit :
;;;     the end of string, or NULL
;;;
;;; endptr :
;;;     location to store the end pointer, or NULL
;;;
;;; Returns :
;;;     TRUE if a valid type string was found
;;; ----------------------------------------------------------------------------

;; not implemented, is equivalent to g-variant-type-string-is-valid

;;; ----------------------------------------------------------------------------
;;; g_variant_type_get_string_length                        not implemented
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_variant_type_get_string_length" variant-type-string-length)
    :size
 #+liber-documentation
 "@version{#2020-11-29}
  @argument[variant-type]{a @class{variant-type} instance}
  @return{The length of the corresponding type string.}
  @begin{short}
    Returns the length of the type string corresponding to the given type.
  @end{short}
  This function must be used to determine the valid extent of the memory region
  returned by the @fun{variant-type-peek-string} function.
  @see-class{variant-type}
  @see-function{variant-type-peek-string}"
  (variant-type (boxed variant-type)))

;;; ----------------------------------------------------------------------------
;;; g_variant_type_peek_string                              not implemented
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_peek_string" variant-type-peek-string) :string
 #+liber-documentation
 "@version{#2020-11-29}
  @argument[variant-type]{a @class{variant-type} instance}
  @return{The corresponding type string.}
  @begin{short}
    Returns the type string corresponding to the given @arg{variant-type}.
  @end{short}
  The result is not nul-terminated. In order to determine its length you must
  call the @fun{variant-type-string-length} function.

  To get a nul-terminated string, see the @fun{variant-type-dup-string}
  function.
  @see-class{variant-type}"
  (variant-type (boxed variant-type)))

;;; ----------------------------------------------------------------------------
;;; g_variant_type_dup_string
;;; ----------------------------------------------------------------------------

;; TODO: Is this function needed?

(cffi:defcfun ("g_variant_type_dup_string" variant-type-dup-string)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{The corresponding variant type string.}
  @begin{short}
    Returns a newly allocated copy of the variant type string corresponding to
    @arg{vtype}.
  @end{short}
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-dup-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_definite
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_is_definite" variant-type-is-definite) :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{@em{True} if @arg{vtype} is definite.}
  @begin{short}
    Determines if the given variant type is definite, that is not indefinite.
  @end{short}
  A type is definite if its variant type string does not contain any indefinite
  type characters like @code{\"*\"}, @code{\"?\"}, or @code{\"r\"}. A variant
  type may not have an indefinite type, so calling this function on the result
  of the @fun{g:variant-type-new} function will always result in @em{true} being
  returned. Calling this function on an indefinite type like @code{\"a*\"},
  however, will result in @em{false} being returned.
  @see-class{g:variant-type}
  @see-function{g:variant-type-new}"
  (vtype (boxed variant-type)))

(export 'variant-type-is-definite)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_container
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_is_container" variant-type-is-container) :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[type]{a @class{g:variant-type} instance}
  @return{@em{True} if @arg{vtype} is a container type.}
  @begin{short}
    Determines if the given variant type is a container type.
  @end{short}
  Container types are any array, maybe, tuple, or dictionary entry types plus
  the variant type. This function returns @em{true} for any indefinite type for
  which every definite subtype is a container, the @code{\"a*\"} type string,
  for example.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-is-container)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_basic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_is_basic" variant-type-is-basic) :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{@em{True} if @arg{vtype} is a basic type.}
  @begin{short}
    Determines if the given variant type is a basic type.
  @end{short}
  Basic types are booleans, bytes, integers, doubles, strings, object paths
  and signatures. Only a basic variant type may be used as the key of a
  dictionary entry. This function returns @em{false} for all indefinite types
  except the @code{\"?\"} type string.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-is-basic)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_maybe
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_is_maybe" variant-type-is-maybe) :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{@em{True} if @arg{vtype} is a maybe type.}
  @begin{short}
    Determines if the given variant type is a maybe type.
  @end{short}
  This is @em{true} if the type string for @arg{vtype} starts with a
  @code{\"m\"} type string. This function returns @em{true} for any indefinite
  type for which every definite subtype is a maybe type, the @code{\"m*\"} type
  string, for example.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-is-maybe)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_array
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_is_array" variant-type-is-array) :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{@em{True} if @arg{vtype} is an array type.}
  @begin{short}
    Determines if the given variant type is an array type.
  @end{short}
  This is @em{true} if the variant type string for type starts with a
  @code{\"a\"} type string. This function returns @em{true} for any indefinite
  type for which every definite subtype is an array type, @code{\"a*\"} type
  string, for example.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-is-array)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_tuple
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_is_tuple" variant-type-is-tuple) :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{@em{True} if @arg{vtype} is a tuple type.}
  @begin{short}
    Determines if the given variant type is a tuple type.
  @end{short}
  This is @em{true} if the variant type string for type starts with a @code{(}
  type string or if type is a @code{r} type string. This function returns
  @em{true} for any indefinite type for which every definite subtype is a tuple
  type, the @code{\"r\"} type string, for example.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-is-tuple)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_dict_entry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_is_dict_entry" variant-type-is-dict-entry)
    :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{@em{True} if @arg{vtype} is a dictionary entry type.}
  @begin{short}
    Determines if the given variant type is a dictionary entry type.
  @end{short}
  This is @em{true} if the type string for type starts with a @code{{} type
  string. This function returns @em{true} for any indefinite type for which
  every definite subtype is a dictionary entry type, the @code{\"{?*@}\"} type
  string, for example.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-is-dict-entry)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_variant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_is_variant" variant-type-is-variant) :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{@em{True} if @arg{vtype} is the variant type.}
  @begin{short}
    Determines if the given variant type is the variant type.
  @end{short}
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-is-variant)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_hash
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_hash" variant-type-hash) :uint
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{The unsigned integer with the hash value.}
  @begin{short}
    The has value of the given variant type.
  @end{short}
  A valid @class{g:variant-type} instance must be provided.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-hash)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_equal" variant-type-equal) :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype1]{a @class{g:variant-type} instance}
  @argument[vtype2]{a @class{g:variant-type} instance}
  @begin{return}
    @em{True} if @arg{vtype1} and @arg{vtype2} are exactly equal.
  @end{return}
  @begin{short}
    Compares two variant types for equality.
  @end{short}
  Only returns @em{true} if the types are exactly equal. Even if one type is an
  indefinite type and the other is a subtype of it, @em{false} will be returned
  if they are not exactly equal. If you want to check for subtypes, use the
  @fun{g:variant-type-is-subtype-of} function. For both arguments, a valid
  @class{g:variant-type} instance must be provided.
  @see-class{g:variant-type}
  @see-function{g:variant-type-is-subtype-of}"
  (vtype1 (boxed variant-type))
  (vtype2 (boxed variant-type)))

(export 'variant-type-equal)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_subtype_of
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_is_subtype_of" variant-type-is-subtype-of)
    :boolean
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @argument[supertype]{a @class{g:variant-type} instance}
  @return{@em{True} if @arg{vtype} is a subtype of @arg{supertype}.}
  @begin{short}
    Checks if @arg{vtype} is a subtype of @arg{supertype}.
  @end{short}
  This function returns @em{true} if @arg{vtype} is a subtype of
  @arg{supertype}. All types are considered to be subtypes of themselves. Aside
  from that, only indefinite types can have subtypes.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type))
  (supertype (boxed variant-type)))

(export 'variant-type-is-subtype-of)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_maybe
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_new_maybe" variant-type-new-maybe)
    (boxed variant-type :return)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{The new maybe @class{g:variant-type} instance.}
  @begin{short}
    Constructs the variant type corresponding to a maybe instance containing
    @arg{vtype} or nothing.
  @end{short}
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-new-maybe)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_array
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_new_array" variant-type-new-array)
    (boxed variant-type :return)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance}
  @return{The new array @class{g:variant-type} instance.}
  @begin{short}
    Constructs the variant type corresponding to an array of elements of the
    @arg{vtype} type.
  @end{short}
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-new-array)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_tuple
;;; ----------------------------------------------------------------------------

;; TODO: Improve the implementation for using type strings

(cffi:defcfun ("g_variant_type_new_tuple" %variant-type-new-tuple)
    (boxed variant-type :return)
  (items :pointer)
  (length :int))

(defun variant-type-new-tuple (&rest items)
 #+liber-documentation
 "@version{2025-2-2}
  @argument[items]{@class{g:variant-type} instances, one for each item}
  @return{The new tuple @class{g:variant-type} instance.}
  @begin{short}
    Constructs a new tuple type, from @arg{items}.
  @end{short}
  @see-class{g:variant-type}
  @see-function{g:variant-new-tuple}"
  (let ((n-items (length items)))
    (cffi:with-foreign-object (items-ar :pointer n-items)
      (iter (for i from 0 below n-items)
            (for item in items)
            (setf (cffi:mem-aref items-ar :pointer i)
                  (cffi:convert-to-foreign item
                                           '(boxed variant-type))))
      (%variant-type-new-tuple items-ar n-items))))

(export 'variant-type-new-tuple)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_dict_entry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_new_dict_entry" variant-type-new-dict-entry)
    (boxed variant-type :return)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[key]{a basic @class{g:variant-type} instance}
  @argument[value]{a @class{g:variant-type} instance}
  @return{The new dictionary entry @class{g:variant-type} instance.}
  @begin{short}
    Constructs the variant type corresponding to a dictionary entry with a key
    of @arg{key} type and a value of @arg{value} type.
  @end{short}
  @see-class{g:variant-type}"
  (key (boxed variant-type))
  (value (boxed variant-type)))

(export 'variant-type-new-dict-entry)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_element
;;; ----------------------------------------------------------------------------

;; TODO: Check the return value, do we have to free it?

(cffi:defcfun ("g_variant_type_element" variant-type-element)
    (boxed variant-type)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{an array or maybe @class{g:variant-type} instance}
  @return{The element type of @arg{vtype}.}
  @begin{short}
    Determines the element type of an array or maybe type.
  @end{short}
  This function may only be used with array or maybe types.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-element)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_n_items
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_n_items" variant-type-n-items) :size
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a tuple or dictionary entry @class{g:variant-type} instance}
  @return{The number of items in @arg{vtype}.}
  @begin{short}
    Determines the number of items contained in a tuple or dictionary entry
    @arg{vtype}.
  @end{short}
  This function may only be used with tuple or dictionary entry types, but
  must not be used with the generic @code{r} tuple type. In the case of a
  dictionary entry type, this function will always return 2.
  @see-class{g:variant-type}"
  (vtype (boxed variant-type)))

(export 'variant-type-n-items)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_first
;;; ----------------------------------------------------------------------------

;; TODO: Check the NIL return value. Is this correct?

(cffi:defcfun ("g_variant_type_first" variant-type-first)
    (boxed variant-type)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a tuple or dictionary entry @class{g:variant-type} instance}
  @return{The first item type of @arg{vtype}.}
  @begin{short}
    Determines the first item type of a tuple or dictionary entry type.
  @end{short}
  This function may only be used with tuple or dictionary entry types, but
  must not be used with the generic @code{r} tuple type. In the case of a
  dictionary entry type, this returns the type of the key. The @code{nil} value
  is returned in case of type being the @code{\"()\"} type string. This call,
  together with the @fun{g:variant-type-next} function provides an iterator
  interface over tuple and dictionary entry types.
  @see-class{g:variant-type}
  @see-function{g:variant-type-next}"
  (vtype (boxed variant-type)))

(export 'variant-type-first)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_next
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_next" variant-type-next) (boxed variant-type)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a @class{g:variant-type} instance from a previous call}
  @return{The next @class{g:variant-type} instance after type, or @code{nil}.}
  @begin{short}
    Determines the next item type of a tuple or dictionary entry type.
  @end{short}
  The @arg{vtype} argument must be the result of a previous call to the
  @fun{g:variant-type-first} or @fun{g:variant-type-next} functions. If called
  on the key type of a dictionary entry then this call returns the value type.
  If called on the value type of a dictionary entry then this call returns
  @code{nil}. For tuples, @code{nil} is returned when @arg{vtype} is the last
  item in a tuple.
  @see-class{g:variant-type}
  @see-function{g:variant-type-first}"
  (vtype (boxed variant-type)))

(export 'variant-type-next)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_key
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_key" variant-type-key) (boxed variant-type)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a dictionary entry @class{g:variant-type} instance}
  @return{The key type of the dictionary entry.}
  @begin{short}
    Determines the key type of a dictionary entry type.
  @end{short}
  This function may only be used with a dictionary entry type. Other than the
  additional restriction, this call is equivalent to the
  @fun{g:variant-type-first} function.
  @see-class{g:variant-type}
  @see-function{g:variant-type-first}
  @see-function{g:variant-type-value}"
  (vtype (boxed variant-type)))

(export 'variant-type-key)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_variant_type_value" variant-type-value)
    (boxed variant-type)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[vtype]{a dictionary entry @class{g:variant-type} instance}
  @return{The value type of the dictionary entry.}
  @begin{short}
    Determines the value type of a dictionary entry type.
  @end{short}
  This function may only be used with a dictionary entry type.
  @see-class{g:variant-type}
  @see-function{g:variant-type-key}"
  (vtype (boxed variant-type)))

(export 'variant-type-value)

;;; --- End of file glib.variant-type.lisp -------------------------------------
