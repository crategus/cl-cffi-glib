;;; ----------------------------------------------------------------------------
;;; glib.gtype.lisp
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

(in-package :glib)

;;; ----------------------------------------------------------------------------

(defvar *warn-unknown-gtype* t)

(defun warn-unknown-gtype (name)
  (when *warn-unknown-gtype*
    (let ((msg (if (stringp name)
                   name
                   (format nil "~a" name))))
      (warn "~a is not known to the GType system" msg))))

;;; ----------------------------------------------------------------------------

;; We need two functions from the GObject library. These functions are later
;; implemented a second time for the GObject API, but do not use the :size
;; type, but the Lisp GTYPE type.

(cffi:defcfun ("g_type_name" %g-type-name) :string
  (gtype :size))

(cffi:defcfun ("g_type_from_name" %g-type-from-name) :size
  (name :string))

;;; ----------------------------------------------------------------------------

;; GTYPE is a Lisp representation of a foreign GType

(defstruct gtype
  name
  %id)

;;; ----------------------------------------------------------------------------

;; Global hash tables to store names and ids of foreign GTypes

(defvar *gtype-lock* (bt:make-lock "gtype lock"))

(let ((name-to-gtype-table (make-hash-table :test 'equal))
      (id-to-gtype-table (make-hash-table)))

  (defun name-to-gtype (name)
    (gethash name name-to-gtype-table))
  (defun (setf name-to-gtype) (value name)
    (setf (gethash name name-to-gtype-table) value))
  (defun id-to-gtype (id)
    (gethash id id-to-gtype-table))
  (defun (setf id-to-gtype) (value id)
    (setf (gethash id id-to-gtype-table) value))

  (defun invalidate-gtypes ()
    (bt:with-lock-held (*gtype-lock*)
      (clrhash id-to-gtype-table)
      (iter (for (nil gtype) in-hashtable name-to-gtype-table)
            (setf (gtype-%id gtype) nil))))

  ;; For debugging purposes
  (defun get-name-to-gtypes ()
    (iter (for (name nil) in-hashtable name-to-gtype-table)
          (collect name)))
  (defun get-id-to-gtypes ()
    (iter (for (id gtype) in-hashtable id-to-gtype-table)
          (collect (list id (gtype-name gtype))))))

(glib-init:at-finalize () (invalidate-gtypes))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-symbol 'gtype)
      "Struct"
      (liber:symbol-documentation 'gtype)
 "@version{2024-12-08}
  @begin{short}
    The @symbol{g:gtype} structure represents the unique identifier of a
    registered foreign @code{GType} type on the Lisp side.
  @end{short}
  In the C library a @code{GType} type is represented as an integer. The
  @class{g:type-t} type specifier automatically converts between the Lisp
  @symbol{g:gtype} representation and the C integer.
  @see-constructor{g:gtype}
  @begin[Examples]{dictionary}
    Create a @symbol{g:gtype} instance from a @code{GType} name:
    @begin{pre}
(g:gtype \"GApplication\")
=> #<GTYPE :name \"GApplication\" :id 94607290994400>
(g:gtype-name *) => \"GApplication\"
(g:gtype-id **) => 94607290994400
    @end{pre}
  @end{dictionary}
  @see-slot{g:gtype-name}
  @see-slot{g:gtype-id}
  @see-class{g:type-t}")

(defmethod print-object ((instance gtype) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (instance stream)
        (format stream "GTYPE :name \"~A\" :id ~D"
                       (gtype-name instance)
                       (gtype-%id instance)))))

#+liber-documentation
(setf (documentation 'gtype-name 'function)
 "@version{2025-09-27}
  @argument[instance]{a @symbol{g:gtype} instance}
  @return{The string for the name of the @code{GType} type.}
  @begin{short}
    Returns the name of the @code{GType} type.
  @end{short}
  The @arg{instance} argument must be a valid @symbol{g:gtype} instance.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:gtype-name (g:gtype \"gboolean\")) => \"gboolean\"
(g:gtype-name (g:gtype \"GObject\")) => \"GObject\"
(g:gtype-name (g:gtype \"GSimpleAction\")) => \"GSimpleAction\"
    @end{pre}
  @end{dictionary}
  @see-symbol{g:gtype}
  @see-function{g:gtype-id}")

;;; ----------------------------------------------------------------------------

;; GTYPE-ID replaces the accessor GTYPE-%ID

(defun gtype-id (instance)
 #+liber-documentation
 "@version{2024-12-08}
  @argument[instance]{a @symbol{g:gtype} instance}
  @return{The integer for the unique identifier of the @code{GType} type.}
  @begin{short}
    Returns the unique identifier for the @code{GType} type.
  @end{short}
  The function returns 0 for a @code{nil} value, representing an invalid
  @code{GType} type.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:gtype-id (g:gtype \"gboolean\")) => 20
(g:gtype-id (g:gtype \"GObject\")) => 80
(g:gtype-id (g:gtype \"GAction\")) => 95376336284736
(g:gtype-id (g:gtype \"GSimpleAction\")) => 95376336285744
(g:gtype-id nil) => 0
    @end{pre}
  @end{dictionary}
  @see-symbol{g:gtype}
  @see-function{g:gtype-name}"
  (cond ((null instance) 0) ; for an invalid type
        ((gtype-%id instance) (gtype-%id instance))
        (t
         (bt:with-lock-held (*gtype-lock*)
           (let ((id (%g-type-from-name (gtype-name instance))))
             (if (zerop id)
                 ;; No valid ID, print a warning
                 (warn-unknown-gtype (gtype-name instance))
                 ;; Store the valid ID
                 (setf (gtype-%id instance) id
                       (id-to-gtype id) instance))
             id)))))

;;; ----------------------------------------------------------------------------

;; Make a Lisp GTYPE representation from a NAME or ID

;; TODO: We have changed the implementation so that we do not store unknown
;; types in the hash tables. Because we have only valid types in the hash
;; tables, we migth further simplify the implmentation.

(defun gtype-from-name (name)
  (when name
    (bt:with-lock-held (*gtype-lock*)
      (let ((gtype (name-to-gtype name)))
        (when gtype
          (unless (gtype-%id gtype)
            (let ((id (%g-type-from-name name)))
              (if (zerop id)
                  ;; No valid ID, print a warning
                  (warn-unknown-gtype name)
                  ;; Store the valid ID
                  (setf (gtype-%id gtype) id
                        (id-to-gtype id) gtype))))
          (return-from gtype-from-name gtype)))
      (let ((id (%g-type-from-name name)))
        (if (zerop id)
            ;; No valid ID, print a warning
            (warn-unknown-gtype name)
            ;; Generate and store a GTYPE
            (let ((gtype (make-gtype :name (copy-seq name) :%id id)))
              (setf (id-to-gtype id) gtype)
              (setf (name-to-gtype name) gtype)
              (return-from gtype-from-name gtype)))))))

(defun gtype-from-id (id)
  (unless (zerop id)
    (bt:with-lock-held (*gtype-lock*)
      (let ((gtype (id-to-gtype id)))
        (if gtype
            gtype
            (let (;; FIXME: This might cause a bug, because %g-type-name expects
                  ;; a valid ID. If the ID is not a valid ID the programm might
                  ;; crash. See the documentation of g_type_name. Can we expect
                  ;; that the ID will always be a valid ID? In this case the
                  ;; check for the return value is unnecessary.
                  (name (%g-type-name id)))
              (unless name
                (warn-unknown-gtype id)
                (return-from gtype-from-id nil))
              (let ((gtype (name-to-gtype name)))
                (when gtype
                  (setf (gtype-%id gtype) id)
                  (setf (id-to-gtype id) gtype)
                  (return-from gtype-from-id gtype))
                (let ((gtype (make-gtype :name name :%id id)))
                  (setf (id-to-gtype id) gtype)
                  (setf (name-to-gtype name) gtype)
                  (return-from gtype-from-id gtype)))))))))

;;; ----------------------------------------------------------------------------

;; The function GTYPE converts an integer or a string representation of
;; a foreign GType to a Lisp GTYPE.

(defun gtype1 (thing)
  (etypecase thing
    (null nil)
    (gtype thing)
    (string (gtype-from-name thing))
    (integer (gtype-from-id thing))))

(defun gtype (thing)
  (gtype1 thing))

(define-compiler-macro gtype (&whole whole thing)
  (if (constantp thing)
      `(load-time-value (gtype1 ,thing))
      whole))

#+liber-documentation
(setf (documentation 'gtype 'function)
 "@version{2024-12-08}
  @argument[thing]{a string, an integer, or an @symbol{g:gtype} instance}
  @return{The @symbol{g:gtype} instance for @arg{thing}.}
  @begin{short}
    Returns a @symbol{g:gtype} instance for @arg{thing}.
  @end{short}
  If the string or the integer does not represent a valid @code{GType} type the
  @code{nil} value is returned and a warning is printed.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:gtype 20) => #<GTYPE :name \"gboolean\" :id 20>
(g:gtype \"gboolean\") => #<GTYPE :name \"gboolean\" :id 20>
(g:gtype (g:gtype 20)) => #<GTYPE :name \"gboolean\" :id 20>
(g:gtype \"unknown\")
=> WARNING: unknown is not known to the GType system
=> NIL
    @end{pre}
  @end{dictionary}
  @see-symbol{g:gtype}")

(export 'gtype)

;; -----------------------------------------------------------------------------

;; Global hash table for storing the corresponding symbol names for GType names
;; for example, "GtkButton" -> 'gtk:button

(let ((symbol-for-gtype (make-hash-table :test 'equal)))

  (defun symbol-for-gtype (name-or-gtype)
    #+liber-documentation
    "@version{2024-12-08}
     @argument[name-or-gtype]{a string or a @symbol{g:gtype} instance
       representing a valid @code{GType} type}
     @return{The Lisp symbol representing the @code{GType} type.}
     @begin{short}
       Returns the Lisp symbol for the given @arg{name-or-gtype} argument.
     @end{short}

     The Lisp symbol is used with the @code{make-instance} method to create
     an instance for a @code{GType} type from the Lisp side.
     @begin[Examples]{dictionary}
       @begin{pre}
(g:symbol-for-gtype \"GApplication\")
=> GIO:APPLICATION
=> T
(g:symbol-for-gtype (g:gtype \"GSimpleAction\"))
=> GIO:SiMPLE-ACTION
=> T
(make-instance *)
=> #<GIO:SIMPLE-ACTION {1007481A53@}>
       @end{pre}
     @end{dictionary}
     @see-symbol{g:gtype}"
    (let ((key (if (stringp name-or-gtype)
                   name-or-gtype
                   (gtype-name (gtype name-or-gtype)))))
    (gethash key symbol-for-gtype)))

  (defun (setf symbol-for-gtype) (symbol name-or-gtype)
    (let ((key (if (stringp name-or-gtype)
                   name-or-gtype
                   (gtype-name (gtype name-or-gtype)))))
    (setf (gethash key symbol-for-gtype) symbol)))

  ;; For debugging purposes
  (defun list-symbol-for-gtype ()
    (iter (for (nil symbol) in-hashtable symbol-for-gtype)
          (collect symbol))))

;;; --- End of file glib.gtype.lisp --------------------------------------------
