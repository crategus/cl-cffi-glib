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
;;; Memory Allocation
;;;
;;;     g_malloc
;;;     g_free
;;;
;;; Various types
;;;
;;;     GStrv
;;;     GList
;;;     GSList
;;;     GDateTime
;;;     gunichar
;;;
;;; Utility functions
;;;
;;;     g_get_application_name
;;;     g_set_application_name
;;;     g_get_prgname
;;;     g_set_prgname
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; g_malloc () -> malloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_malloc" malloc) :pointer
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

(cffi:defcfun ("g_free" free) :void
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
;;; GStrv
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type strv-type ()
  ((free-from-foreign :initarg :free-from-foreign
                      :initform t
                      :reader strv-type-fff)
   (free-to-foreign :initarg :free-to-foreign
                    :initform t
                    :reader strv-type-ftf))
  (:actual-type :pointer))

(cffi:define-parse-method strv-t (&key (free-from-foreign t)
                                       (free-to-foreign t))
  (make-instance 'strv-type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod cffi:translate-from-foreign (value (type strv-type))
  (unless (cffi:null-pointer-p value)
    (prog1
      (iter (for i from 0)
            (for str-ptr = (cffi:mem-aref value :pointer i))
            (until (cffi:null-pointer-p str-ptr))
            (collect
                (cffi:convert-from-foreign str-ptr
                                           '(:string :free-from-foreign nil)))
            (when (strv-type-fff type) (free str-ptr)))
      (when (strv-type-fff type)
        (free value)))))

(defmethod cffi:translate-to-foreign (values (type strv-type))
  (let* ((n (length values))
         (result (malloc (* (1+ n) (cffi:foreign-type-size :pointer)))))
    (iter (for i from 0)
          (for str in values)
          (setf (cffi:mem-aref result :pointer i)
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

(cffi:defcstruct %list-t
  (data :pointer)
  (next :pointer)
  (prev :pointer))

(cffi:define-foreign-type list-type ()
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

(cffi:define-parse-method list-t (type &key (free-from-foreign t)
                                            (free-to-foreign t))
  (make-instance 'list-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod cffi:translate-from-foreign (ptr (type list-type))
  (prog1
    (iter (for data initially ptr then (%list-next data))
          (until (cffi:null-pointer-p data))
          (collect (cffi:convert-from-foreign
                       (cffi:foreign-slot-value data '(:struct %list-t) 'data)
                       (list-type-type type))))
    (when (list-type-free-from-foreign type)
      (%list-free ptr))))

(defmethod cffi:translate-to-foreign (lst (type list-type))
  (let ((nlst (cffi:null-pointer)))
    (iter (for data in lst)
          (setf nlst
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

(cffi:defcfun ("g_list_free" %list-free) :void
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

(cffi:defcfun ("g_list_append" %list-append) (:pointer (:struct %list-t))
  (lst (:pointer (:struct %list-t)))
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; GSList
;;; ----------------------------------------------------------------------------

;; TODO: Check the memory management in more detail.

(cffi:defcstruct %slist-t
  (data :pointer)
  (next :pointer))

(cffi:define-foreign-type slist-type ()
  ((type :reader slist-type-type :initarg :type :initform :pointer)
   (free-from-foreign :reader slist-type-free-from-foreign
                      :initarg :free-from-foreign
                      :initform t)
   (free-to-foreign :reader slist-type-free-to-foreign
                    :initarg :free-to-foreign
                    :initform t))
  (:actual-type :pointer))

(cffi:define-parse-method slist-t (type &key (free-from-foreign t)
                                             (free-to-foreign t))
  (make-instance 'slist-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod cffi:translate-from-foreign (ptr (type slist-type))
  (prog1
    (iter (for item initially ptr then (%slist-next item))
          (until (cffi:null-pointer-p item))
          (collect (cffi:convert-from-foreign
                       (cffi:foreign-slot-value item '(:struct %slist-t) 'data)
                       (slist-type-type type))))
    (when (slist-type-free-from-foreign type)
      (%slist-free ptr))))

(defmethod cffi:translate-to-foreign (lst (type slist-type))
  (let ((result (cffi:null-pointer))
        (first-iteration-p t)
        (last nil))
    (iter (for n = (%slist-alloc))
          (for item in lst)
          (for ptr = (cffi:convert-to-foreign item (slist-type-type type)))
          (progn
            (setf (cffi:foreign-slot-value n '(:struct %slist-t) 'data) ptr)
            (setf (cffi:foreign-slot-value n '(:struct %slist-t) 'next)
                  (cffi:null-pointer))
            (when last
              (setf (cffi:foreign-slot-value last '(:struct %slist-t) 'next)
                    n))
            (setf last n)
            (when first-iteration-p
              (setf first-iteration-p nil)
              (setf result n))))
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

(cffi:defcfun ("g_slist_alloc" %slist-alloc) (:pointer (:struct %slist-t)))

;;; ----------------------------------------------------------------------------
;;; g_slist_free ()                                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_slist_free" %slist-free) :void
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

(cffi:define-foreign-type date-time-type ()
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

(cffi:define-foreign-type unichar ()
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

;;; ----------------------------------------------------------------------------
;;; g_get_application_name ()
;;; g_set_application_name () -> application-name
;;; ----------------------------------------------------------------------------

(defun (setf application-name) (name)
  (cffi:foreign-funcall "g_set_application_name"
                        :string name
                        :void)
  name)

(cffi:defcfun ("g_get_application_name" application-name) :string
 #+liber-documentation
 "@version{2022-11-23}
  @syntax[]{(g:application-name) => name}
  @syntax[]{(setf (g:application-name) name)}
  @argument[name]{a string with the localized name of the application}
  @begin{short}
    Accessor of a human readable name for the application.
  @end{short}
  This name should be localized if possible, and is intended for display to the
  user. Contrast with the @fun{g:prgname} function, which gets a non-localized
  name. If the @sym{(setf g:application-name)} function has not been called,
  returns the result of the @fun{g:prgname} function, which may be @code{nil}
  if the @sym{(setf g:prgname)} function has also not been called.

  The @fun{g:prgname} function will be called automatically by
  @code{gtk_init()}, but the @sym{g:application-name} function will not. Note
  that for thread safety reasons, this function can only be called once.

  The application name will be used in contexts such as error messages, or
  when displaying the name of an application in the task list.
  @see-function{g:prgname}")

(export 'application-name)

;;; ----------------------------------------------------------------------------
;;; g_get_prgname ()
;;; g_set_prgname () -> prgname
;;; ----------------------------------------------------------------------------

(defun (setf prgname) (prgname)
  (cffi:foreign-funcall "g_set_prgname"
                        :string prgname
                        :void)
  prgname)

(cffi:defcfun ("g_get_prgname" prgname) :string
 #+liber-documentation
 "@version{2022-11-23}
  @syntax[]{(g:prgname) => prgname}
  @syntax[]{(setf (g:prgname) prgname)}
  @argument[prgname]{a string with the name of the program}
  @begin{short}
    Accessor of the name of the program.
  @end{short}
  This name should not be localized, contrast with the @fun{g:application-name}
  function. If you are using GDK or GTK the program name is set in the
  @code{gdk_init()} function, which is called by the @code{gtk_init()} function.
  The program name is found by taking the last component of the first command
  line argument.

  Note that for thread-safety reasons this function can only be called once.
  @see-function{g:application-name}")

(export 'prgname)

;;; --- End of file glib.misc.lisp ---------------------------------------------
