;;; ----------------------------------------------------------------------------
;;; glib.gtype.lisp
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

(in-package :glib)

;;; ----------------------------------------------------------------------------

;; We need two functions from the GObject library. These functions are later
;; implemented a second time for the GObject API, but do not use the :size
;; type, but the Lisp gtype.

(defcfun ("g_type_name" %g-type-name) :string
  (gtype :size))

(defcfun ("g_type_from_name" %g-type-from-name) :size
  (name :string))

;;; ----------------------------------------------------------------------------

;; gtype is a Lisp representation of a foreign GType

(defstruct (gtype
             ;; a print function to get nice output
             (:print-function
               (lambda (struct stream depth)
                 (declare (ignore depth))
                 (print-unreadable-object (struct stream)
                   (format stream "GTYPE :name \"~A\" :id ~D"
                                  (gtype-name struct)
                                  (gtype-%id struct))))))
  name
  %id)

;;; ----------------------------------------------------------------------------

;; Global hash tables to store names and ids of foreign GTypes

(defvar *name-to-gtype* (make-hash-table :test 'equal))
(defvar *id-to-gtype* (make-hash-table))
(defvar *gtype-lock* (bt:make-lock "gtype lock"))

(defun invalidate-gtypes ()
  (bt:with-lock-held (*gtype-lock*)
    (clrhash *id-to-gtype*)
    (iter (for (name gtype) in-hashtable *name-to-gtype*)
          (setf (gtype-%id gtype) nil))))

(glib-init:at-finalize () (invalidate-gtypes))

;;; ----------------------------------------------------------------------------

(defvar *warn-unknown-gtype* t)

(defun warn-unknown-gtype (name)
  ;; Do not print a warning for types which are not derived from GObject
  ;; FIXME: This is a hack.
  (when (and *warn-unknown-gtype*
             (or (numberp name)
                 (not (member name '("AtkImplementorIface"
                                     "LispArrayListStore"
                                     "LispTreeStore")
                              :test #'string=))))
    (let ((msg (if (stringp name)
                   (string-upcase name)
                   (format nil "ID:~a" name))))
      (warn "GType ~A is not known to the GType system." msg))))

;;; ----------------------------------------------------------------------------

;; GTYPE-ID replaces the accessor GTYPE-%ID

(defun gtype-id (gtype)
  (cond ((null gtype) 0) ; for +g-type-invalid+
        ((gtype-%id gtype) (gtype-%id gtype))
        (t
         (bt:with-lock-held (*gtype-lock*)
           (let ((id (%g-type-from-name (gtype-name gtype))))
             (if (zerop id)
                 (warn-unknown-gtype (gtype-name gtype))
                 (setf (gtype-%id gtype) id
                       (gethash id *id-to-gtype*) gtype))
             id)))))

;;; ----------------------------------------------------------------------------

;; Make a Lisp GTYPE representation from a NAME or an ID

(defun gtype-from-name (name)
  (when name
    (bt:with-lock-held (*gtype-lock*)
      (let ((gtype (gethash name *name-to-gtype*)))
        (when gtype
          (unless (gtype-%id gtype)
            (let ((id (%g-type-from-name name)))
              (if (zerop id)
                  (warn-unknown-gtype name)
                  (setf (gtype-%id gtype) id
                        (gethash id *id-to-gtype*) gtype))))
          (return-from gtype-from-name gtype)))
      (let ((id (%g-type-from-name name)))
        (when (zerop id)
          (warn-unknown-gtype name)
          (setf id nil))
        (let ((gtype (make-gtype :name (copy-seq name) :%id id)))
          (setf (gethash id *id-to-gtype*) gtype
                (gethash name *name-to-gtype*) gtype)
          (return-from gtype-from-name gtype))))))

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

(export 'gtype)

;; -----------------------------------------------------------------------------

;; Global hash table for storing the corresponding symbol names for GType names
;; e.g. "GtkButton" -> 'button

(defvar *symbol-for-gtypes* (make-hash-table :test 'equal))

(defun symbol-for-gtype (name-or-gtype)
  (let ((key (if (stringp name-or-gtype)
                 name-or-gtype
                 (gtype-name (gtype name-or-gtype)))))
  (gethash key *symbol-for-gtypes*)))

(defun (setf symbol-for-gtype) (symbol name-or-gtype)
  (let ((key (if (stringp name-or-gtype)
                 name-or-gtype
                 (gtype-name (gtype name-or-gtype)))))
  (setf (gethash key *symbol-for-gtypes*) symbol)))

;;; --- End of file glib.gtype.lisp --------------------------------------------
