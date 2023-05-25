;;; ----------------------------------------------------------------------------
;;; glib.misc.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.76 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; This file contains several type definitions and functions, which are
;;; needed for the implemenation of the GTK library.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Basic Types
;;;
;;;     Standard GLib types, defined for ease-of-use and portability
;;;
;;; Only the following types are implemented:
;;;
;;;     gsize
;;;     gssize
;;;     goffset
;;; ----------------------------------------------------------------------------
;;;
;;; Memory Allocation
;;;
;;;  The following functions for general memory handling are implemented:
;;;
;;;     g_malloc
;;;     g_free
;;; ----------------------------------------------------------------------------
;;;
;;; String Utility Functions
;;;
;;;     Various string-related functions
;;;
;;; Implemented is:
;;;
;;;     GString
;;;     GStrv
;;;
;;;     g_strdup                                           not exported
;;; ----------------------------------------------------------------------------
;;;
;;; Doubly-Linked Lists
;;;
;;;     Linked lists containing integer values or pointers to data, with the
;;;     ability to iterate over the list in both directions
;;;
;;; Implemented is:
;;;
;;;     GList
;;;
;;;     g_list_free                                        not exported
;;;     g_list_next                                        not exported
;;;     g_list_append                                      not exported
;;; ----------------------------------------------------------------------------
;;;
;;; Singly-Linked Lists
;;;
;;;     Linked lists containing integer values or pointers to data, limited to
;;;     iterating over the list in one direction
;;;
;;; Implemented is:
;;;
;;;     GSList
;;;
;;;     g_slist_alloc                                      not exported
;;;     g_slist_free                                       not exported
;;;     g_slist_next                                       not exported
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; gsize -> size-t                                        not exported
;;; ----------------------------------------------------------------------------

;; TODO: This type is replaced with the CFFI :SIZE type.

#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond ((cffi-features:cffi-feature-p :x86-64) (defctype size-t :uint64))
        ((cffi-features:cffi-feature-p :x86)    (defctype size-t :ulong))
        ((cffi-features:cffi-feature-p :ppc32)  (defctype size-t :uint32))
        ((cffi-features:cffi-feature-p :ppc64)  (defctype size-t :uint64))
        (t
         (cl:error "Can not define 'size-t', unknown CPU architecture ~
                   (known are x86 and x86-64)"))))

#+nil
(setf (documentation 'size-t 'type)
 "@version{#2022-10-25}
  @begin{short}
    An unsigned integer type corresponding to the @code{size_t} type defined in
    C99.
  @end{short}
  This type is wide enough to hold the numeric value of a pointer, so it is
  usually 64 bit wide on a 64 bit platform. Values of this type can range from
  0 to @code{G_MAXSIZE}.
  @see-type{ssize-t}
  @see-type{offset-t}")

;;; ----------------------------------------------------------------------------
;;; gssize -> ssize-t                                      not exported
;;; ----------------------------------------------------------------------------

;; TODO: Not needed, use the CFFI :SSIZE type

#+nil
(defctype ssize-t :long)

#+nil
(setf (documentation 'ssize-t 'type)
 "@version{#2022-10-25}
  @begin{short}
    A signed variant of the @type{size-t} type, corresponding to the
    @code{ssize_t} type defined on most platforms.
  @end{short}
  Values of this type can range from @code{G_MINSSIZE} to @code{G_MAXSSIZE}.
  @see-type{size-t}
  @see-type{offset-t}")

;;; ----------------------------------------------------------------------------
;;; goffset                                                not exported
;;; ----------------------------------------------------------------------------

;; TODO: Not needed, use the CFFI :OFFSET type

#+nil
(defctype offset-t :uint64)

#+nil
(setf (documentation 'offset-t 'type)
 "@version{#2022-10-25}
  @begin{short}
    A signed integer type that is used for file offsets, corresponding to the
    C99 @code{off64_t} type.
  @end{short}
  Values of this type can range from @code{G_MINOFFSET} to @code{G_MAXOFFSET}.
  @see-type{size-t}
  @see-type{ssize-t}")

;;; ----------------------------------------------------------------------------
;;; g_malloc () -> malloc
;;; ----------------------------------------------------------------------------

(defcfun ("g_malloc" malloc) :pointer
 #+liber-documentation
 "@version{2022-11-21}
  @argument[nbytes]{an integer of @code{:size} type with the number of bytes
  to allocate}
  @return{A foreign pointer to the allocated memory.}
  @begin{short}
    Allocates @arg{nbytes} bytes of memory.
  @end{short}
  If the @arg{nbytes} argument is 0 the @sym{malloc} function returns a foreign
  @code{null-pointer} value.
  @begin[Examples]{dictionary}
    @begin{pre}
(glib:malloc 100)
=> #.(SB-SYS:INT-SAP #X559FB7283340)
(glib:malloc 0)
=> #.(SB-SYS:INT-SAP #X00000000)
(cffi:null-pointer-p (glib:malloc 0))
=> T
    @end{pre}
  @end{dictionary}
  @see-function{glib:free}"
  (nbytes :size))

(export 'malloc)

;;; ----------------------------------------------------------------------------
;;; g_free () -> free
;;; ----------------------------------------------------------------------------

(defcfun ("g_free" free) :void
 #+liber-documentation
 "@version{2022-11-21}
  @argument[mem]{a foreign pointer to the memory to free}
  @begin{short}
    Frees the memory pointed to by the @arg{mem} foreign pointer.
  @end{short}
  If the @arg{mem} argument is a foreign @code{null-pointer} the
  @sym{glib:free} function simply returns.
  @see-function{glib:malloc}"
  (mem :pointer))

(export 'free)

;;; ----------------------------------------------------------------------------
;;; GString -> string-t                                    not exported
;;; ----------------------------------------------------------------------------

;; TODO: Not needed, use the CFFI :string type

(define-foreign-type string-type ()
  ((free-from-foreign :initarg :fff
                      :reader string-type-fff
                      :initform nil)
   (free-to-foreign :initarg :ftf
                    :reader string-type-ftf
                    :initform t))
  (:actual-type :pointer))

(define-parse-method string-t (&key (free-from-foreign nil) (free-to-foreign t))
  (make-instance 'string-type
                 :fff free-from-foreign
                 :ftf free-to-foreign))

(defmethod cffi:translate-to-foreign (value (type string-type))
  (cffi:foreign-funcall "g_strdup"
                        (:string :free-to-foreign (strv-type-ftf type)) value
                        :pointer))

(defmethod cffi:translate-from-foreign (value (type string-type))
  (prog1
    (cffi:convert-from-foreign value '(:string :free-from-foreign nil))
    (when (string-type-fff type)
      (free value))))

#+liber-documentation
(setf (documentation 'string-t 'type)
 "@version{#2022-10-25}
  @begin{short}
    A type that is almost like the foreign CFFI @code{:string} type but uses
    the GLib @fun{malloc} and @fun{free} functions to allocate and free
    memory.
  @end{short}

  The @sym{string-t} type performs automatic conversion between Lisp and C
  strings. Note that, in the case of functions the converted C string will
  have dynamic extent, i.e. it will be automatically freed after the foreign
  function returns.

  In addition to Lisp strings, this type will accept foreign pointers and pass
  them unmodified.
  @see-type{strv-t}
  @see-function{malloc}
  @see-function{free}")

;;; ----------------------------------------------------------------------------
;;; GStrv
;;; ----------------------------------------------------------------------------

(define-foreign-type strv-type ()
  ((free-from-foreign :initarg :free-from-foreign
                      :initform t
                      :reader strv-type-fff)
   (free-to-foreign :initarg :free-to-foreign
                    :initform t
                    :reader strv-type-ftf))
  (:actual-type :pointer))

(define-parse-method strv-t (&key (free-from-foreign t) (free-to-foreign t))
  (make-instance 'strv-type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod cffi:translate-from-foreign (value (type strv-type))
  (unless (cffi:null-pointer-p value)
    (prog1
      (loop for i from 0
            for str-ptr = (cffi:mem-aref value :pointer i)
            until (cffi:null-pointer-p str-ptr)
            collect
              (cffi:convert-from-foreign str-ptr
                                         '(:string :free-from-foreign nil))
            do (when (strv-type-fff type) (free str-ptr)))
      (when (strv-type-fff type)
        (free value)))))

(defmethod cffi:translate-to-foreign (values (type strv-type))
  (let* ((n (length values))
         (result (malloc (* (1+ n) (cffi:foreign-type-size :pointer)))))
    (loop for i from 0
          for str in values
          do (setf (cffi:mem-aref result :pointer i)
                   (cffi:foreign-funcall "g_strdup"
                                         (:string
                                          :free-to-foreign (strv-type-ftf type))
                                         str
                                         :pointer)))
    (setf (cffi:mem-aref result :pointer n) (cffi:null-pointer))
    result))

#+liber-documentation
(setf (documentation 'strv-t 'type)
 "@version{2022-11-21}
  @begin{short}
    The @sym{g:strv-t} type represents and performs automatic conversion between
    a list of Lisp strings and an array of C strings of the CFFI @code{:string}
    type.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(setq str (cffi:convert-to-foreign (list \"Hello\" \"World\") 'g:strv-t))
=> #.(SB-SYS:INT-SAP #X01541998)
(cffi:convert-from-foreign str 'g:strv-t)
=> (\"Hello\" \"World\")
    @end{pre}
  @end{dictionary}")

(export 'strv-t)

;;; ----------------------------------------------------------------------------
;;; GList
;;; ----------------------------------------------------------------------------

;; TODO: Check the memory management in more detail.

(defcstruct %list-t
  (data :pointer)
  (next :pointer)
  (prev :pointer))

(define-foreign-type list-type ()
  ((type :reader list-type-type
         :initarg :type
         :initform :pointer)
   (free-from-foreign :reader list-type-free-from-foreign
                      :initarg :free-from-foreign
                      :initform t)
   (free-to-foreign :reader list-type-free-to-foreign
                    :initarg :free-to-foreign
                    :initform t))
  (:actual-type :pointer))

(define-parse-method list-t (type &key (free-from-foreign t)
                                       (free-to-foreign t))
  (make-instance 'list-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod cffi:translate-from-foreign (ptr (type list-type))
  (prog1
    (loop for data = ptr then (%list-next data)
          until (cffi:null-pointer-p data)
          collect (cffi:convert-from-foreign
                      (cffi:foreign-slot-value data '(:struct %list-t) 'data)
                      (list-type-type type)))
    (when (list-type-free-from-foreign type)
      (%list-free ptr))))

(defmethod cffi:translate-to-foreign (lst (type list-type))
  (let ((nlst (cffi:null-pointer)))
    (loop for data in lst
          do (setf nlst
                   (%list-append nlst
                       (cffi:convert-to-foreign data (list-type-type type)))))
    nlst))

#+liber-documentation
(setf (documentation 'list-t 'type)
 "@version{2022-11-21}
  @begin{short}
    The @sym{g:list-t} type represents a C doubly linked list with elements of
    the @code{GList} structure.
  @end{short}
  The @sym{g:list-t} type performs automatic conversion from a C list to a Lisp
  list. The elements of the list can be pointers, strings or GObjects.
  @begin[Examples]{dictionary}
    @begin{pre}
(cffi:convert-to-foreign (list \"a\" \"b\" \"c\") '(g:list-t :string))
=> #.(SB-SYS:INT-SAP #X03B92220)
(cffi:convert-from-foreign * '(g:list-t :string))
=> (\"a\" \"b\" \"c\")
    @end{pre}
  @end{dictionary}
  @see-type{g:slist-t}")

(export 'list-t)

;;; ----------------------------------------------------------------------------
;;; g_list_free ()                                         not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_free" %list-free) :void
  (lst (:pointer (:struct %list-t))))

;;; ----------------------------------------------------------------------------
;;; g_list_next ()                                         not exported
;;; ----------------------------------------------------------------------------

(defun %list-next (lst)
  (if (cffi:null-pointer-p lst)
      (cffi:null-pointer)
      (cffi:foreign-slot-value lst '(:struct %list-t) 'next)))

;;; ----------------------------------------------------------------------------
;;; g_list_append ()                                       not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_append" %list-append) (:pointer (:struct %list-t))
  (lst (:pointer (:struct %list-t)))
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; GSList
;;; ----------------------------------------------------------------------------

;; TODO: Check the memory management in more detail.

(defcstruct %slist-t
  (data :pointer)
  (next :pointer))

(define-foreign-type slist-type ()
  ((type :reader slist-type-type :initarg :type :initform :pointer)
   (free-from-foreign :reader slist-type-free-from-foreign
                      :initarg :free-from-foreign
                      :initform t)
   (free-to-foreign :reader slist-type-free-to-foreign
                    :initarg :free-to-foreign
                    :initform t))
  (:actual-type :pointer))

(define-parse-method slist-t (type &key (free-from-foreign t)
                                        (free-to-foreign t))
  (make-instance 'slist-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod cffi:translate-from-foreign (ptr (type slist-type))
  (prog1
    (loop for item = ptr then (%slist-next item)
          until (cffi:null-pointer-p item)
          collect (cffi:convert-from-foreign
                      (cffi:foreign-slot-value item '(:struct %slist-t) 'data)
                      (slist-type-type type)))
    (when (slist-type-free-from-foreign type)
      (%slist-free ptr))))

(defmethod cffi:translate-to-foreign (lst (type slist-type))
  (let ((result (cffi:null-pointer))
        (first-iteration-p t)
        (last nil))
    (loop for item in lst
          for n = (%slist-alloc)
          for ptr = (cffi:convert-to-foreign item (slist-type-type type))
          do (setf (cffi:foreign-slot-value n '(:struct %slist-t) 'data) ptr)
             (setf (cffi:foreign-slot-value n '(:struct %slist-t) 'next)
                   (cffi:null-pointer))
             (when last
               (setf (cffi:foreign-slot-value last '(:struct %slist-t) 'next)
                     n))
             (setf last n)
             (when first-iteration-p
               (setf first-iteration-p nil)
               (setf result n)))
    result))

#+liber-documentation
(setf (documentation 'slist-t 'type)
 "@version{2022-11-21}
  @begin{short}
    The @sym{g:slist-t} type represents a C singly linked list with elements of
    the @code{GSList} structure.
  @end{short}
  The @sym{g:slist-t} type performs automatic conversion from a C list to a Lisp
  list. The elements of the list can be pointers, strings or GObjects.
  @begin[Examples]{dictionary}
    @begin{pre}
(cffi:convert-to-foreign (list \"a\" \"b\" \"c\") '(g:slist-t :string))
=> #.(SB-SYS:INT-SAP #X03B92220)
(cffi:convert-from-foreign * '(g:slist-t :string))
=> (\"a\" \"b\" \"c\")
    @end{pre}
  @end{dictionary}
  @see-type{g:list-t}")

(export 'slist-t)

;;; ----------------------------------------------------------------------------
;;; g_slist_alloc ()                                       not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_slist_alloc" %slist-alloc) (:pointer (:struct %slist-t)))

;;; ----------------------------------------------------------------------------
;;; g_slist_free ()                                        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_slist_free" %slist-free) :void
  (lst (:pointer (:struct %slist-t))))

;;; ----------------------------------------------------------------------------
;;; g_slist_next ()                                        not exported
;;; ----------------------------------------------------------------------------

(defun %slist-next (lst)
  (if (cffi:null-pointer-p lst)
      (cffi:null-pointer)
      (cffi:foreign-slot-value lst '(:struct %slist-t) 'next)))

;;; ----------------------------------------------------------------------------
;;; GDateTime
;;; ----------------------------------------------------------------------------

(define-foreign-type date-time-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser date-time))

(let ((offset (encode-universal-time 0 0 0 1 1 1970)))
  (defmethod cffi:translate-to-foreign
      (value (type date-time-type))
    (cffi:foreign-funcall "g_date_time_new_from_unix_utc"
                          :int64 (- value offset)
                          :pointer))

  (defmethod cffi:translate-from-foreign
      (value (type date-time-type))
    (+ offset
       (cffi:foreign-funcall "g_date_time_to_unix"
                             :pointer value
                             :int64))))

#+liber-documentation
(setf (liber:alias-for-class 'date-time)
      "Type"
      (documentation 'date-time 'type)
 "@version{2023-5-22}
  @begin{short}
    The @sym{g:date-time} type represents the C @code{GDateTime} type which
    represents a date and time, including a time one.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(cffi:convert-to-foreign 0 'g:date-time)
=> #.(SB-SYS:INT-SAP #X558E3298CE40)
(cffi:convert-from-foreign * 'g:date-time) => 0
    @end{pre}
  @end{dictionary}")

(export 'date-time)

;;; ----------------------------------------------------------------------------
;;; gunichar
;;; ----------------------------------------------------------------------------

(define-foreign-type unichar ()
  ()
  (:actual-type :uint32)
  (:simple-parser unichar))

(defmethod cffi:translate-from-foreign (value (type unichar))
  (code-char value))

(defmethod cffi:translate-to-foreign (value (type unichar))
  (if (integerp value) value (char-code value)))

#+liber-documentation
(setf (liber:alias-for-class 'unichar)
      "Type"
      (documentation 'unichar 'type)
 "@version{2023-5-22}
  @begin{short}
    The @sym{g:unichar} type represents the C @code{gunichar} type which can
    hold any UCS-4 character code.
  @end{short}
  The @sym{g:unichar} type performs automatic conversion from the C 
  @code{gunichar} type to a Lisp character and a Lisp character to the C type.
  An integer value as argument to the @sym{cffi:convert-to-foreign} function
  is passed through.
  @begin[Examples]{dictionary}
    Convert a Lisp character to a C @code{gunichar} value:
    @begin{pre}
(cffi:convert-to-foreign #\A 'g:unichar) => 65
(cffi:convert-to-foreign #\0 'g:unichar) => 48
(cffi:convert-to-foreign #\return 'g:unichar) => 13
(cffi:convert-to-foreign #\space 'g:unichar) => 32
    @end{pre}
    Convert a C @code{gunichar} value to a Lisp character:
    @begin{pre}
(cffi:convert-from-foreign 65 'g:unichar) => #\A
(cffi:convert-from-foreign 48 'g:unichar) => #\0
(cffi:convert-from-foreign 13 'g:unichar) => #\return
(cffi:convert-from-foreign 32 'g:unichar) => #\space
    @end{pre}
    An integer value argument is passed through:
    @begin{pre}
(cffi:convert-to-foreign 65 'g:unichar) => 65
    @end{pre}
  @end{dictionary}")

(export 'unichar)

;;; --- End of file glib.misc.lisp ---------------------------------------------
