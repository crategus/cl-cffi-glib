;;; ----------------------------------------------------------------------------
;;; glib.bytes.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.76 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2021 -2023 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GBytes
;;;
;;; Functions
;;;
;;;     g_bytes_new
;;;     g_bytes_new_take
;;;     g_bytes_new_static
;;;     g_bytes_new_with_free_func
;;;     g_bytes_new_from_bytes
;;;     g_bytes_get_data
;;;     g_bytes_get_size
;;;     g_bytes_hash
;;;     g_bytes_equal
;;;     g_bytes_compare
;;;     g_bytes_ref
;;;     g_bytes_unref
;;;     g_bytes_unref_to_data
;;;     g_bytes_unref_to_array
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GBytes
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque bytes "GBytes"
  :type-initializer "g_bytes_get_type"
  :alloc (%bytes-new (cffi:null-pointer) 0))

#+liber-documentation
(setf (liber:alias-for-class 'bytes)
      "GBoxed"
      (documentation 'bytes 'type)
 "@version{2023-8-12}
  @begin{pre}
(define-g-boxed-opaque bytes \"GBytes\"
  :type-initializer \"g_bytes_get_type\"
  :alloc (%bytes-new (cffi:null-pointer) 0))
  @end{pre}
  @begin{short}
    The @class{g:bytes} structure is a simple refcounted data type representing
    an immutable sequence of zero or more bytes from an unspecified origin.
  @end{short}
  The purpose of a @class{g:bytes} instance is to keep the memory region that
  it holds alive for as long as anyone holds a reference to the bytes. When the
  last reference count is dropped, the memory is released. Multiple unrelated
  callers can use byte data in the @class{g:bytes} instance without coordinating
  their activities, resting assured that the byte data will not change or move
  while they hold a reference.

  A @class{g:bytes} instance can come from many different origins that may have
  different procedures for freeing the memory region. Examples are memory from
  the @fun{g:malloc} function.
  @begin[Examples]{dictionary}
    Usage of a @class{g:bytes} instance for a Lisp string as byte data.
    @begin{pre}
(multiple-value-bind (data len)
    (foreign-string-alloc \"A test string.\")
  (defvar bytes (g:bytes-new data len)))
=> BYTES
(g:bytes-size bytes)
=> 15
(g:bytes-data bytes)
=> #.(SB-SYS:INT-SAP #X55EBB2C1DB70)
=> 15
(foreign-string-to-lisp (g:bytes-data bytes))
=> \"A test string.\"
=> 14
    @end{pre}
  @end{dictionary}
  @see-constructor{g:bytes-new}")

(export 'bytes)

;;; ----------------------------------------------------------------------------
;;; g_bytes_new () -> bytes-new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_bytes_new" %bytes-new) :pointer
  (data :pointer)
  (size :size))

(cffi:defcfun ("g_bytes_new" bytes-new) (boxed bytes :return)
 #+liber-documentation
 "@version{2022-11-22}
  @argument[data]{a pointer to the data to be used for the bytes}
  @argument[size]{an integer with the size of @arg{data}}
  @return{A new @class{g:bytes} instance.}
  @short{Creates a new @class{g:bytes} instance from @arg{data}.}
  @see-class{g:bytes}"
  (data :pointer)
  (size :size))

(export 'bytes-new)

;;; ----------------------------------------------------------------------------
;;; g_bytes_new_take ()
;;;
;;; GBytes *
;;; g_bytes_new_take (gpointer data,
;;;                   gsize size);
;;;
;;; Creates a new GBytes from data .
;;;
;;; After this call, data belongs to the bytes and may no longer be modified by
;;; the caller. g_free() will be called on data when the bytes is no longer in
;;; use. Because of this data must have been created by a call to g_malloc(),
;;; g_malloc0() or g_realloc() or by one of the many functions that wrap these
;;; calls (such as g_new(), g_strdup(), etc).
;;;
;;; For creating GBytes with memory from other allocators, see
;;; g_bytes_new_with_free_func().
;;;
;;; data may be NULL if size is 0.
;;;
;;; data :
;;;     the data to be used for the bytes.
;;;
;;; size :
;;;     the size of data
;;;
;;; Returns :
;;;     a new GBytes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_new_static ()
;;;
;;; GBytes *
;;; g_bytes_new_static (gconstpointer data,
;;;                     gsize size);
;;;
;;; Creates a new GBytes from static data.
;;;
;;; data must be static (ie: never modified or freed). It may be NULL if size
;;; is 0.
;;;
;;; data :
;;;     the data to be used for the bytes.
;;;
;;; size :
;;;     the size of data
;;;
;;; Returns :
;;;     a new GBytes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_new_with_free_func ()
;;;
;;; GBytes *
;;; g_bytes_new_with_free_func (gconstpointer data,
;;;                             gsize size,
;;;                             GDestroyNotify free_func,
;;;                             gpointer user_data);
;;;
;;; Creates a GBytes from data .
;;;
;;; When the last reference is dropped, free_func will be called with the
;;; user_data argument.
;;;
;;; data must not be modified after this call is made until free_func has been
;;; called to indicate that the bytes is no longer in use.
;;;
;;; data may be NULL if size is 0.
;;;
;;; data :
;;;     the data to be used for the bytes.
;;;
;;; size :
;;;     the size of data
;;;
;;; free_func :
;;;     the function to call to release the data
;;;
;;; user_data :
;;;     data to pass to free_func
;;;
;;; Returns :
;;;     a new GBytes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_new_from_bytes ()
;;;
;;; GBytes *
;;; g_bytes_new_from_bytes (GBytes *bytes,
;;;                         gsize offset,
;;;                         gsize length);
;;;
;;; Creates a GBytes which is a subsection of another GBytes. The offset +
;;; length may not be longer than the size of bytes .
;;;
;;; A reference to bytes will be held by the newly created GBytes until the byte
;;; data is no longer needed.
;;;
;;; Since 2.56, if offset is 0 and length matches the size of bytes , then bytes
;;; will be returned with the reference count incremented by 1. If bytes is a
;;; slice of another GBytes, then the resulting GBytes will reference the same
;;; GBytes instead of bytes . This allows consumers to simplify the usage of
;;; GBytes when asynchronously writing to streams.
;;;
;;; bytes :
;;;     a GBytes
;;;
;;; offset :
;;;     offset which subsection starts at
;;;
;;; length :
;;;     length of subsection
;;;
;;; Returns :
;;;     a new GBytes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_get_data ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_bytes_get_data" %bytes-data) :pointer
  (bytes (boxed bytes))
  (size (:pointer :size)))

(defun bytes-data (bytes)
 #+liber-documentation
 "@version{2023-1-6}
  @argument[bytes]{a @class{g:bytes} instance}
  @begin{return}
    @arg{data} -- a pointer to the byte data, or a @code{null-pointer} value
    @br{}
    @arg{size} -- an integer with the size of the byte data
  @end{return}
  @begin{short}
    Get the byte data in the @class{g:bytes} instance.
  @end{short}
  This function will always return the same pointer for a given @class{g:bytes}
  instance.

  A @code{null-pointer} value may be returned if the @arg{size} value is 0. This
  is not guaranteed, as the @class{g:bytes} instance may represent an empty
  string with @arg{data} not a @code{null-pointer} value and the @arg{size}
  value as 0. A @code{null-pointer} value will not be returned if the @arg{size}
  value is non-zero.
  @see-class{g:bytes}"
  (cffi:with-foreign-object (size :size)
    (let ((ptr (%bytes-data bytes size)))
      (values ptr (cffi:mem-ref size :size)))))

(export 'bytes-data)

;;; ----------------------------------------------------------------------------
;;; g_bytes_get_size () -> bytes-size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_bytes_get_size" bytes-size) :size
 #+liber-documentation
 "@version{2022-11-22}
  @argument[bytes]{a @class{g:bytes} instance}
  @return{An integer with the size of the byte data.}
  @begin{short}
    Get the size of the byte data in the @class{g:bytes} instance.
  @end{short}
  This function will always return the same value for a given @class{g:bytes}
  instance.
  @see-class{g:bytes}"
  (bytes (boxed bytes)))

(export 'bytes-size)

;;; ----------------------------------------------------------------------------
;;; g_bytes_hash ()
;;;
;;; guint
;;; g_bytes_hash (gconstpointer bytes);
;;;
;;; Creates an integer hash code for the byte data in the GBytes.
;;;
;;; This function can be passed to g_hash_table_new() as the key_hash_func
;;; parameter, when using non-NULL GBytes pointers as keys in a GHashTable.
;;;
;;; bytes :
;;;     a pointer to a GBytes key.
;;;
;;; Returns :
;;;     a hash value corresponding to the key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_equal ()
;;;
;;; gboolean
;;; g_bytes_equal (gconstpointer bytes1,
;;;                gconstpointer bytes2);
;;;
;;; Compares the two GBytes values being pointed to and returns TRUE if they are
;;; equal.
;;;
;;; This function can be passed to g_hash_table_new() as the key_equal_func
;;; parameter, when using non-NULL GBytes pointers as keys in a GHashTable.
;;;
;;; bytes1 :
;;;     a pointer to a GBytes.
;;;
;;; bytes2 :
;;;     a pointer to a GBytes to compare with bytes1 .
;;;
;;; Returns :
;;;     TRUE if the two keys match.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_compare ()
;;;
;;; gint
;;; g_bytes_compare (gconstpointer bytes1,
;;;                  gconstpointer bytes2);
;;;
;;; Compares the two GBytes values.
;;;
;;; This function can be used to sort GBytes instances in lexicographical order.
;;;
;;; If bytes1 and bytes2 have different length but the shorter one is a prefix
;;; of the longer one then the shorter one is considered to be less than the
;;; longer one. Otherwise the first byte where both differ is used for
;;; comparison. If bytes1 has a smaller value at that position it is considered
;;; less, otherwise greater than bytes2 .
;;;
;;; bytes1 :
;;;     a pointer to a GBytes.
;;;
;;; bytes2 :
;;;     a pointer to a GBytes to compare with bytes1 .
;;;
;;; Returns :
;;;     a negative value if bytes1 is less than bytes2 , a positive value if
;;;     bytes1 is greater than bytes2 , and zero if bytes1 is equal to bytes2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_ref ()
;;;
;;; GBytes *
;;; g_bytes_ref (GBytes *bytes);
;;;
;;; Increase the reference count on bytes .
;;;
;;; bytes :
;;;     a GBytes
;;;
;;; Returns :
;;;     the GBytes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_unref ()
;;;
;;; void
;;; g_bytes_unref (GBytes *bytes);
;;;
;;; Releases a reference on bytes . This may result in the bytes being freed. If
;;; bytes is NULL, it will return immediately.
;;;
;;; bytes :
;;;     a GBytes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_unref_to_data ()
;;;
;;; gpointer
;;; g_bytes_unref_to_data (GBytes *bytes,
;;;                        gsize *size);
;;;
;;; Unreferences the bytes, and returns a pointer the same byte data contents.
;;;
;;; As an optimization, the byte data is returned without copying if this was
;;; the last reference to bytes and bytes was created with g_bytes_new(),
;;; g_bytes_new_take() or g_byte_array_free_to_bytes(). In all other cases the
;;; data is copied.
;;;
;;; bytes :
;;;     a GBytes.
;;;
;;; size :
;;;     location to place the length of the returned data.
;;;
;;; Returns :
;;;     a pointer to the same byte data, which should be freed with g_free()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bytes_unref_to_array ()
;;;
;;; GByteArray *
;;; g_bytes_unref_to_array (GBytes *bytes);
;;;
;;; Unreferences the bytes, and returns a new mutable GByteArray containing the
;;; same byte data.
;;;
;;; As an optimization, the byte data is transferred to the array without
;;; copying if this was the last reference to bytes and bytes was created with
;;; g_bytes_new(), g_bytes_new_take() or g_byte_array_free_to_bytes(). In all
;;; other cases the data is copied.
;;;
;;; Do not use it if bytes contains more than G_MAXUINT bytes. GByteArray stores
;;; the length of its data in guint, which may be shorter than gsize, that bytes
;;; is using.
;;;
;;; bytes :
;;;     a GBytes.
;;;
;;; Returns .
;;;     a new mutable GByteArray containing the same byte data.
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.bytes.lisp --------------------------------------------
