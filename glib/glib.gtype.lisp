;;; ----------------------------------------------------------------------------
;;; glib.gtype.lisp
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

(in-package :glib)

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

#+liber-documentation
(setf (liber:alias-for-symbol 'gtype)
      "Struct"
      (liber:symbol-documentation 'gtype)
 "@version{2024-6-11}
  @begin{short}
    The @symbol{g:gtype} structure represents the unique identifier of a
    registered @code{GType} type.
  @end{short}
  In the C library a @code{GType} type is represented as an integer.
  @see-constructor{g:gtype}
  @see-slot{g:gtype-name}
  @see-slot{g:gtype-id}
  @see-class{g:type-t}")

#+liber-documentation
(setf (documentation 'gtype-name 'function)
 "@version{2024-6-11}
  @argument[instance]{a @symbol{g:gtype} instance}
  @return{The string with the name of the @code{GType} type.}
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

(defmethod print-object ((instance gtype) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (instance stream)
        (format stream "GTYPE :name \"~A\" :id ~D"
                       (gtype-name instance)
                       (gtype-%id instance)))))

;;; ----------------------------------------------------------------------------

;; Global hash tables to store names and ids of foreign GTypes

(defvar *gtype-lock* (bt:make-lock "gtype lock"))
(defvar *name-to-gtype* (make-hash-table :test 'equal))
(defvar *id-to-gtype* (make-hash-table))

(defun invalidate-gtypes ()
  (bt:with-lock-held (*gtype-lock*)
    (clrhash *id-to-gtype*)
    (iter (for (name gtype) in-hashtable *name-to-gtype*)
          (setf (gtype-%id gtype) nil))))

(defun get-name-to-gtypes ()
  (iter (for (name gtype) in-hashtable *name-to-gtype*)
        (collect name)))

(defun get-id-to-gtypes ()
  (iter (for (id gtype) in-hashtable *id-to-gtype*)
        (collect (list id (gtype-name gtype)))))

(glib-init:at-finalize () (invalidate-gtypes))

;;; ----------------------------------------------------------------------------

(defvar *warn-unknown-gtype* t)

(defun warn-unknown-gtype (name)
  (when *warn-unknown-gtype*
    (let ((msg (if (stringp name)
                   name
                   (format nil "~a" name))))
      (warn "~a is not known to the GType system" msg))))

;;; ----------------------------------------------------------------------------

;; GTYPE-ID replaces the accessor GTYPE-%ID

(defun gtype-id (instance)
 #+liber-documentation
 "@version{2024-6-11}
  @argument[instance]{a @symbol{g:gtype} instance}
  @return{The integer for the unique identifier of the @code{GType} type.}
  @begin{short}
    Returns the unique identifiert for the @code{GType} type.
  @end{short}
  The function returns 0 for a @code{nil} value. This represents an invalid
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
                       (gethash id *id-to-gtype*) instance))
             id)))))

;;; ----------------------------------------------------------------------------

;; Make a Lisp GTYPE representation from a NAME or an ID

;; TODO: We have changed the implementation so that we do not store unknown
;; types in the hash tables. Because we have only valid types in the hash
;; tables, we migth further simplify the implmentation.

(defun gtype-from-name (name)
  (when name
    (bt:with-lock-held (*gtype-lock*)
      (let ((gtype (gethash name *name-to-gtype*)))
        (when gtype
          (unless (gtype-%id gtype)
            (let ((id (%g-type-from-name name)))
              (if (zerop id)
                  ;; No valid ID, print a warning
                  (warn-unknown-gtype name)
                  ;; Store the valid ID
                  (setf (gtype-%id gtype) id
                        (gethash id *id-to-gtype*) gtype))))
          (return-from gtype-from-name gtype)))
      (let ((id (%g-type-from-name name)))
        (if (zerop id)
            ;; No valid ID, print a warning
            (warn-unknown-gtype name)
            ;; Generate and store a GTYPE
            (let ((gtype (make-gtype :name (copy-seq name) :%id id)))
              (setf (gethash id *id-to-gtype*) gtype
                    (gethash name *name-to-gtype*) gtype)
              (return-from gtype-from-name gtype)))))))

(defun gtype-from-id (id)
  (unless (zerop id)
    (bt:with-lock-held (*gtype-lock*)
      (let ((gtype (gethash id *id-to-gtype*)))
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
              (let ((gtype (gethash name *name-to-gtype*)))
                (when gtype
                  (setf (gtype-%id gtype) id
                        (gethash id *id-to-gtype*) gtype)
                  (return-from gtype-from-id gtype))
                (let ((gtype (make-gtype :name name :%id id)))
                  (setf (gethash id *id-to-gtype*) gtype
                        (gethash name *name-to-gtype*) gtype)
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
 "@version{2024-6-11}
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
      (iter (for (key symbol) in-hashtable symbol-for-gtype)
            (collect symbol))))

;;; --- End of file glib.gtype.lisp --------------------------------------------
