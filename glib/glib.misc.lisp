;;; ----------------------------------------------------------------------------
;;; glib.misc.lisp
;;;
;;; The documentation in this file is taken from the GLib Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GLib library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;; This file contains several type definitions and functions, which are
;;; needed for the implementation of the GTK library.
;;;
;;; Various types
;;;
;;;     GStrv
;;;     GList
;;;     GSList
;;;     GQuark
;;;     GDateTime
;;;     gunichar
;;;
;;; Memory Allocation
;;;
;;;     g_malloc
;;;     g_free
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
 "@version{2025-05-19}
  @begin{short}
    The @type{g:strv-t} type specifier performs automatic conversion between a
    list of Lisp strings and an array of C strings of the CFFI @code{:string}
    type.
  @end{short}
  @begin[Examples]{dictionary}
    Convert a list of strings to and from a C array of strings.
    @begin{pre}
(setq str (cffi:convert-to-foreign (list \"Hello\" \"World\") 'g:strv-t))
=> #.(SB-SYS:INT-SAP #X01541998)
(cffi:convert-from-foreign str 'g:strv-t)
=> (\"Hello\" \"World\")
    @end{pre}
    The @slot[g:themed-icon]{names} property of the @class{g:themed-icon} class
    is an example for the use of the @class{g:strv-t} type specifier. The
    @fun{g:themed-icon-new-from-names} function creates a themed icon from
    strings. The accessor for the @slot[g:themed-icon]{names} property returns
    the names of the themed icon as a list of strings. Internally, the list
    list of strings is converted to and from a C array of strings via the
    @class{g:strv-t} type specifier.
    @begin{pre}
(let ((icon (g:themed-icon-new-from-names \"gnome-dev-cdrom-audio\"
                                          \"gnome-dev-cdrom\"
                                          \"gnome-dev\"
                                          \"gnome\")))
  (g:themed-icon-names icon))
=> (\"gnome-dev-cdrom-audio\" \"gnome-dev-cdrom\" \"gnome-dev\" \"gnome\")
    @end{pre}
  @end{dictionary}
  @see-class{g:themed-icon}
  @see-function{g:themed-icon-new-from-names}
  @see-function{g:themed-icon-names}")

(export 'strv-t)

;;; ----------------------------------------------------------------------------
;;; GList
;;; ----------------------------------------------------------------------------

;; TODO: Check the memory management in more detail

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
 "@version{2025-05-19}
  @begin{short}
    The @type{g:list-t} type specifier performs automatic conversion from a C
    @code{GList} instance to a Lisp list.
  @end{short}
 The elements of the list can be pointers, strings or GObjects.
  @begin[Examples]{dictionary}
    Convert a list of strings to and from a C @code{GList} instance.
    @begin{pre}
(cffi:convert-to-foreign (list \"a\" \"b\" \"c\") '(g:list-t :string))
=> #.(SB-SYS:INT-SAP #X03B92220)
(cffi:convert-from-foreign * '(g:list-t :string))
=> (\"a\" \"b\" \"c\")
    @end{pre}
    The @fun{g:content-types-registered} function is an example for the use of
    the @class{g:list-t} type specifier in the Lisp implementation. It returns
    a list of strings which is converted from a C @code{GList} instance.
    @begin{pre}
(g:content-types-registered)
=> (\"application/x-sc\" \"application/vnd.squashfs\" ... )
    @end{pre}
  @end{dictionary}
  @see-type{g:slist-t}
  @see-function{g:content-types-registered}")

(export 'list-t)

;;; ----------------------------------------------------------------------------
;;; g_list_free                                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_free" %list-free) :void
  (lst (:pointer (:struct %list-t))))

;;; ----------------------------------------------------------------------------
;;; g_list_next                                             not exported
;;; ----------------------------------------------------------------------------

(defun %list-next (lst)
  (if (cffi:null-pointer-p lst)
      (cffi:null-pointer)
      (cffi:foreign-slot-value lst '(:struct %list-t) 'next)))

;;; ----------------------------------------------------------------------------
;;; g_list_append                                           not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_append" %list-append) (:pointer (:struct %list-t))
  (lst (:pointer (:struct %list-t)))
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; GSList
;;; ----------------------------------------------------------------------------

;; TODO: Check the memory management in more detail

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
 "@version{2025-05-19}
  @begin{short}
    The @type{g:slist-t} type specifier performs automatic conversion from a C
    @code{GSList} instance to a Lisp list.
  @end{short}
  The elements of the list can be pointers, strings or GObjects.

  The @fun{gtk:builder-objects} and @fun{gtk:size-group-widgets} functions are
  examples for the use of the @class{g:slist-t} type specifier in the Lisp
  implementation. These functions return a list of @class{g:object} instances
  which are converted from C @code{GSList} instances.
  @begin[Examples]{dictionary}
    @begin{pre}
(cffi:convert-to-foreign (list \"a\" \"b\" \"c\") '(g:slist-t :string))
=> #.(SB-SYS:INT-SAP #X03B92220)
(cffi:convert-from-foreign * '(g:slist-t :string))
=> (\"a\" \"b\" \"c\")
    @end{pre}
  @end{dictionary}
  @see-type{g:list-t}
  @see-function{gtk:builder-objects}
  @see-function{gtk:size-group-widgets}")

(export 'slist-t)

;;; ----------------------------------------------------------------------------
;;; g_slist_alloc                                           not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_slist_alloc" %slist-alloc) (:pointer (:struct %slist-t)))

;;; ----------------------------------------------------------------------------
;;; g_slist_free                                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_slist_free" %slist-free) :void
  (lst (:pointer (:struct %slist-t))))

;;; ----------------------------------------------------------------------------
;;; g_slist_next                                            not exported
;;; ----------------------------------------------------------------------------

(defun %slist-next (lst)
  (if (cffi:null-pointer-p lst)
      (cffi:null-pointer)
      (cffi:foreign-slot-value lst '(:struct %slist-t) 'next)))

;;; ----------------------------------------------------------------------------
;;; GQuark
;;; ----------------------------------------------------------------------------

;; A GQuark is implemented in the C Library as guint32.

(cffi:define-foreign-type quark-type ()
  ()
  (:actual-type :uint32)
  (:simple-parser quark-as-string))

(defmethod cffi:translate-to-foreign (value (type quark-type))
  (cffi:foreign-funcall "g_quark_from_string"
                        :string (or value (cffi:null-pointer))
                        :uint32))

(defmethod cffi:translate-from-foreign (value (type quark-type))
  (cffi:foreign-funcall "g_quark_to_string"
                        :uint32 value
                        :string))

#+liber-documentation
(setf (documentation 'quark-as-string 'type)
 "@version{2025-05-19}
  @begin{short}
    Quarks are associations between strings and integer identifiers.
  @end{short}
  Given either the string or the @code{GQuark} identifier it is possible to
  retrieve the other.
  @begin[Lisp binding]{dictionary}
    In the Lisp binding the @type{g:quark-as-string} type specifier translates
    a string argument to the corresponding @code{GQuark} identifier and a
    @code{GQuark} return value is translated to the corresponding Lisp string.
    No further functions are implemented for the @type{g:quark-as-string}
    type specifier.

    If the Lisp string does not currently has an associated @code{GQuark}, a
    new @code{GQuark} is created. A @code{GQuark} value of zero is associated
    to @code{nil} in Lisp.

    See the @fun{g:type-qdata} function for attaching a @code{GQuark}
    identifier to a @class{g:type-t} type ID.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Translate a Lisp String to a @code{GQuark} identifier:
    @begin{pre}
(cffi:convert-to-foreign \"GtkWidget\" 'g:quark-as-string) => 91
(cffi:convert-to-foreign \"gboolean\" 'g:quark-as-string) => 9
(cffi:convert-to-foreign nil 'g:quark-as-string) => 0
     @end{pre}
     Translate a @code{GQuark} identifier to a Lisp string:
     @begin{pre}
(cffi:convert-from-foreign 91 'g:quark-as-string) => \"GtkWidget\"
(cffi:convert-from-foreign 9 'g:quark-as-string) => \"gboolean\"
(cffi:convert-from-foreign 0 'g:quark-as-string) => NIL
     @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:type-qdata}")

(export 'quark-as-string)

;;; ----------------------------------------------------------------------------
;;; GDateTime
;;; ----------------------------------------------------------------------------

;; TODO: Improve the documentation

(cffi:define-foreign-type date-time-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser date-time))

(let ((offset (encode-universal-time 0 0 0 1 1 1970)))
  (defmethod cffi:translate-to-foreign (value (type date-time-type))
    (cffi:foreign-funcall "g_date_time_new_from_unix_utc"
                          :int64 (- value offset)
                          :pointer))

  (defmethod cffi:translate-from-foreign (value (type date-time-type))
    (+ offset
       (cffi:foreign-funcall "g_date_time_to_unix"
                             :pointer value
                             :int64))))

#+liber-documentation
(setf (liber:alias-for-class 'date-time)
      "Type"
      (documentation 'date-time 'type)
 "@version{2025-05-19}
  @begin{short}
    The @type{g:date-time} type specifier performs automatic conversion between
    the @code{GDateTime} time representation and the Lisp universal time, that
    is measured as an offset from the beginning of the year 1900.
  @end{short}
  @begin[Notes]{dictionary}
    The Lisp implementation uses the @code{g_date_time_new_from_unix_utc()} and
    @code{g_date_time_to_unix()} functions for conversion. The
    @class{g:date-time} type specifier takes into account the offset between
    the Unix time that is the number of seconds that have elapsed since
    1970-01-01 00:00:00 UTC and the Lisp universal time that is counted from
    1900-01-01 00:00:00 UTC.
  @end{dictionary}
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

(cffi:define-foreign-type unichar-type ()
  ()
  (:actual-type :uint32)
  (:simple-parser unichar))

(defmethod cffi:translate-from-foreign (value (type unichar-type))
  (code-char value))

(defmethod cffi:translate-to-foreign (value (type unichar-type))
  (if (integerp value) value (char-code value)))

#+liber-documentation
(setf (liber:alias-for-class 'unichar)
      "Type"
      (documentation 'unichar 'type)
 "@version{2025-05-19}
  @begin{short}
    The @type{g:unichar} type specifier represents the C @code{gunichar} type
    which can hold any UCS-4 character code.
  @end{short}
  The @type{g:unichar} type specifier performs automatic conversion from the C
  @code{gunichar} type to a Lisp character and a Lisp character to the C type.
  An integer as argument to the @sym{cffi:convert-to-foreign} function is
  passed through.
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
    An integer argument is passed through:
    @begin{pre}
(cffi:convert-to-foreign 65 'g:unichar) => 65
    @end{pre}
  @end{dictionary}")

(export 'unichar)

;;; ----------------------------------------------------------------------------
;;; g_malloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_malloc" malloc) :pointer
 #+liber-documentation
 "@version{2025-05-19}
  @argument[nbytes]{an integer of @code{:size} type for the number of bytes
    to allocate}
  @return{The foreign pointer to the allocated memory.}
  @begin{short}
    Allocates @arg{nbytes} bytes of memory.
  @end{short}
  If the @arg{nbytes} argument is 0 the @fun{g:malloc} function returns a
  foreign @code{cffi:null-pointer} value.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:malloc 100)
=> #.(SB-SYS:INT-SAP #X559FB7283340)
(g:malloc 0)
=> #.(SB-SYS:INT-SAP #X00000000)
(cffi:null-pointer-p (g:malloc 0))
=> T
    @end{pre}
  @end{dictionary}
  @see-function{g:free}"
  (nbytes :size))

(export 'malloc)

;;; ----------------------------------------------------------------------------
;;; g_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_free" free) :void
 #+liber-documentation
 "@version{2025-05-19}
  @argument[mem]{a foreign pointer to the memory to free}
  @begin{short}
    Frees the memory pointed to by the @arg{mem} foreign pointer.
  @end{short}
  If the @arg{mem} argument is a foreign @code{cffi:null-pointer} the
  @fun{g:free} function simply returns.
  @see-function{g:malloc}"
  (mem :pointer))

(export 'free)

;;; ----------------------------------------------------------------------------
;;; g_get_application_name
;;; g_set_application_name
;;; ----------------------------------------------------------------------------

(defun (setf application-name) (name)
  (cffi:foreign-funcall "g_set_application_name" :string name :void)
  name)

(cffi:defcfun ("g_get_application_name" application-name) :string
 #+liber-documentation
 "@version{2025-05-19}
  @syntax{(g:application-name) => name}
  @syntax{(setf (g:application-name) name)}
  @argument[name]{a string for the localized name of the application}
  @begin{short}
    Accessor of a human readable name for the application.
  @end{short}
  This name should be localized if possible, and is intended for display to the
  user. Contrast with the @fun{g:prgname} function, which gets a non-localized
  name. If the @setf{g:application-name} function has not been called, returns
  the result of the @fun{g:prgname} function, which may be @code{nil} if the
  @setf{g:prgname} function has also not been called.

  The @fun{g:prgname} function will be called automatically by
  @code{gtk_init()}, but the @fun{g:application-name} function will not. Note
  that for thread safety reasons, this function can only be called once.

  The application name will be used in contexts such as error messages, or
  when displaying the name of an application in the task list.
  @see-function{g:prgname}")

(export 'application-name)

;;; ----------------------------------------------------------------------------
;;; g_get_prgname
;;; g_set_prgname
;;; ----------------------------------------------------------------------------

(defun (setf prgname) (prgname)
  (cffi:foreign-funcall "g_set_prgname" :string prgname :void)
  prgname)

(cffi:defcfun ("g_get_prgname" prgname) :string
 #+liber-documentation
 "@version{2025-05-19}
  @syntax{(g:prgname) => prgname}
  @syntax{(setf (g:prgname) prgname)}
  @argument[prgname]{a string for the name of the program}
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
