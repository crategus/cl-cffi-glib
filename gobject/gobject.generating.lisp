;;; ----------------------------------------------------------------------------
;;; gobject.generating.lisp
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

(in-package :gobject)

(defvar *known-interfaces* (make-hash-table :test 'equal))

;;; ----------------------------------------------------------------------------

;; A list of Gtypes with the corresponding Lisp symbol which do not follow
;; the general rule of building the symbol name from a Gtype name.
;; Only use in GTK3 for e.g. "GtkHBox" -> gtk:hbox and not gtk:h-box

;; FIXME: The entries in the list are duplicated. Why?

(defvar *lisp-name-exceptions* nil)

(defun get-lisp-name-exception (name)
  (second (assoc name *lisp-name-exceptions* :test 'equal)))

(defun (setf get-lisp-name-exception) (symbol name)
  (setf *lisp-name-exceptions*
        (append (list (list name symbol)) *lisp-name-exceptions*)))

;;; ----------------------------------------------------------------------------

(defstruct property
  name
  accessor
  readable
  writable)

(defstruct (cl-property (:include property))
  args)

(defstruct (gobject-property (:include property))
  gname
  gtype)

(defstruct (cffi-property (:include property))
  ctype
  reader
  writer)

;;; ----------------------------------------------------------------------------

(defmethod make-load-form ((object cl-property) &optional env)
  (declare (ignore env))
  `(make-cl-property :name `,(property-name object)
                     :args `,(cl-property-args)))

(defmethod make-load-form ((object gobject-property) &optional env)
  (declare (ignore env))
  `(make-gobject-property :name ',(property-name object)
                          :accessor ',(property-accessor object)
                          :readable ',(property-readable object)
                          :writable ',(property-writable object)
                          :gname ',(gobject-property-gname object)
                          :gtype ',(gobject-property-gtype object)))

(defmethod make-load-form ((object cffi-property) &optional env)
  (declare (ignore env))
  `(make-cffi-property :name ',(property-name object)
                       :accessor ',(property-accessor object)
                       :readable ',(property-readable object)
                       :writable ',(property-writable object)
                       :ctype ',(cffi-property-ctype object)
                       :reader ',(cffi-property-reader object)
                       :writer ',(cffi-property-writer object)))

;;; ----------------------------------------------------------------------------

;; Needed for the macros define-g-object-class and define-g-interface

(defun parse-property (spec)
  (cond ((eq :cffi (first spec))
         (parse-cffi-property (rest spec)))
        ((eq :cl (first spec))
         (parse-cl-property (rest spec)))
        (t
         (parse-gobject-property spec))))

(defun parse-cl-property (spec)
  (destructuring-bind (name &rest args) spec
    (make-cl-property :name name
                      :args args)))

(defun parse-gobject-property (spec)
  (destructuring-bind (name accessor gname gtype readable writable) spec
    (make-gobject-property :name name
                           :accessor accessor
                           :gname gname
                           :gtype gtype
                           :readable readable
                           :writable writable)))

(defun parse-cffi-property (spec)
  (destructuring-bind (name accessor ctype reader writer) spec
    (make-cffi-property :name name
                        :accessor accessor
                        :ctype ctype
                        :reader reader
                        :writer writer
                        :readable (not (null reader))
                        :writable (not (null writer)))))

;;; ----------------------------------------------------------------------------

;; Are these functions in use?

(defun property->method-arg (property)
  (when (or (gobject-property-p property)
            (and (cffi-property-p property)
                 (property-writable property)))
    (let ((name (property-name property)))
      `(,name nil ,(name->supplied-p name)))))

(defun gobject-property->arg-push (property)
  (assert (typep property 'gobject-property))
  (with-slots (name gtype gname) property
    `(when ,(name->supplied-p name)
       (push ,gname arg-names)
       (push ,gtype arg-types)
       (push ,name arg-values))))

(defun cffi-property->initarg (property)
  (assert (typep property 'cffi-property))
  (when (property-writable property)
    (with-slots (accessor name type writer) property
      `(when ,(name->supplied-p name)
         (setf (,accessor object) ,name)))))

(defun name->supplied-p (name)
  (make-symbol (format nil "~A-SUPPLIED-P" (symbol-name name))))

;;; ----------------------------------------------------------------------------

(defgeneric property->reader (class property))

(defmethod property->reader (class (property gobject-property))
  (with-slots (accessor gtype gname) property
   `(defmethod ,accessor ((object ,class))
      (object-property object ,gname ,gtype))))

(defmethod property->reader (class (property cffi-property))
  (with-slots (accessor type reader) property
    (etypecase reader
      (string `(defmethod ,accessor ((object ,class))
                 (cffi:foreign-funcall ,reader object object ,type)))
      (symbol `(defmethod ,accessor ((object ,class))
                 (funcall ',reader object))))))

;;; ----------------------------------------------------------------------------

(defgeneric property->writer (class property))

(defmethod property->writer (class (property gobject-property))
  (with-slots (accessor gtype gname) property
    `(defmethod (setf ,accessor) (new-value (object ,class))
       (setf (object-property object ,gname ,gtype) new-value))))

(defmethod property->writer (class (property cffi-property))
  (with-slots (accessor type writer) property
    (etypecase writer
      (string
        `(defmethod (setf ,accessor) (new-value (object ,class))
           (cffi:foreign-funcall ,writer object object ,type new-value :void)
           new-value))
      (symbol
        `(defmethod (setf ,accessor) (new-value (object ,class))
           (funcall ',writer object new-value)
           new-value)))))

;;; ----------------------------------------------------------------------------

(defun property->accessors (class property export)
  (append (when (property-readable property)
            (list (property->reader class property)))
          (when (property-writable property)
            (list (property->writer class property)))
          (when export
            (list `(export ',(property-accessor property)
                           (find-package
                             ,(package-name
                                (symbol-package
                                 (property-accessor property)))))))))

;;; ----------------------------------------------------------------------------

(defun interface->lisp-class-name (interface)
  (etypecase interface
    (symbol interface)
    (string (or (gethash interface *known-interfaces*)
                (error "Unknown interface ~A" interface)))))

;;; ----------------------------------------------------------------------------

(defun property->slot (class property)
  (declare (ignorable class))
  (cond ((gobject-property-p property)
         `(,(property-name property)
           :allocation :gobject-property
           :g-property-type ,(gobject-property-gtype property)
           :accessor ,(property-accessor property)
           :initarg ,(intern (string-upcase (property-name property))
                             (find-package :keyword))
           :g-property-name ,(gobject-property-gname property)))
        ((cffi-property-p property)
         `(,(property-name property)
           :allocation :gobject-fn
           :g-property-type ,(cffi-property-ctype property)
           :accessor ,(property-accessor property)
           ,@(when (not (null (cffi-property-writer property)))
               `(:initarg
                 ,(intern (string-upcase (property-name property))
                          (find-package :keyword))))
           :g-getter ,(cffi-property-reader property)
           :g-setter ,(cffi-property-writer property)))
        ((cl-property-p property)
         `(,(property-name property)
           ,@(cl-property-args property)))))

;;; ----------------------------------------------------------------------------

(defmacro define-g-object-class (g-type-name name
                                 (&key (superclass 'object)
                                       (export t)
                                       interfaces
                                       type-initializer)
                                 (&rest properties))
  (setf properties (mapcar #'parse-property properties))
  `(progn
     (defclass ,name (,@(when (and superclass
                                   (not (eq superclass 'object)))
                          (list superclass))
                      ,@(mapcar #'interface->lisp-class-name interfaces))
       (,@(mapcar (lambda (property)
                     (property->slot name property))
                   properties))
       (:gname . ,g-type-name)
       ,@(when type-initializer
           (list `(:initializer . ,type-initializer)))
       (:metaclass gobject-class))
     ,@(when export
         (cons `(export ',name
                        (find-package
                          ,(package-name (symbol-package name))))
               (mapcar (lambda (property)
                         `(export ',(intern (format nil "~A-~A"
                                                    (symbol-name name)
                                                    (property-name property))
                                            (symbol-package name))
                                  (find-package
                                    ,(package-name (symbol-package name)))))
                       properties)))))

(defmacro define-gobject (g-type-name name
                          (&key (superclass 'object)
                                (export t)
                                interfaces
                                type-initializer)
                          (&rest properties))
  (setf properties (mapcar #'parse-property properties))
  `(progn
     (defclass ,name (,@(when (and superclass
                                   (not (eq superclass 'object)))
                          (list superclass))
                      ,@(mapcar #'interface->lisp-class-name interfaces))
       (,@(mapcar (lambda (property)
                     (property->slot name property))
                   properties))
       (:gname . ,g-type-name)
       ,@(when type-initializer
           (list `(:initializer . ,type-initializer)))
       (:metaclass gobject-class))
     ,@(when export
         (cons `(export ',name
                        (find-package
                          ,(package-name (symbol-package name))))
               (mapcar (lambda (property)
                         `(export ',(intern (format nil "~A-~A"
                                                    (symbol-name name)
                                                    (property-name property))
                                            (symbol-package name))
                                  (find-package
                                    ,(package-name (symbol-package name)))))
                       properties)))))

;;; ----------------------------------------------------------------------------

;; TODO: We have added code to allow other than g:object to be a prerequiste
;; of an interface. We make GdkDragSurface a prerequiste of GdkSurface- But now
;; the test gdk-cairo-context-cairo-create fails. The function
;; translate-to-foreign for an object no longer regcognizes GdkWaylandSurface
;; to be a GdkSurface. What is wrong?

(defmacro define-g-interface (gtype-name name
                              (&key (superclass 'object)
                                    (export t)
                                    type-initializer)
                              (&rest properties))
  (setf properties (mapcar #'parse-property properties))
  `(progn
     (defclass ,name (,@(when (and superclass
                                   (not (eq superclass 'object)))
                          (list superclass)))
       (,@(mapcar (lambda (property)
                    (property->slot name property))
                  properties))
       (:gname . ,gtype-name)
       ,@(when type-initializer
           (list `(:initializer . ,type-initializer)))
       (:interface-p . t)
       (:metaclass gobject-class))
     ,@(when export
         (cons `(export ',name
                        (find-package ,(package-name (symbol-package name))))
               (mapcar (lambda (property)
                         `(export ',(intern (format nil "~A-~A"
                                                    (symbol-name name)
                                                    (property-name property))
                                            (symbol-package name))
                                  (find-package
                                    ,(package-name (symbol-package name)))))
                       properties)))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,gtype-name *known-interfaces*) ',name))))

(defmacro define-ginterface (gtype-name name
                             (&key (superclass 'object)
                                   (export t)
                                   type-initializer)
                             (&rest properties))
  (setf properties (mapcar #'parse-property properties))
  `(progn
     (defclass ,name (,@(when (and superclass
                                   (not (eq superclass 'object)))
                          (list superclass)))
       (,@(mapcar (lambda (property)
                    (property->slot name property))
                  properties))
       (:gname . ,gtype-name)
       ,@(when type-initializer
           (list `(:initializer . ,type-initializer)))
       (:interface-p . t)
       (:metaclass gobject-class))
     ,@(when export
         (cons `(export ',name
                        (find-package ,(package-name (symbol-package name))))
               (mapcar (lambda (property)
                         `(export ',(intern (format nil "~A-~A"
                                                    (symbol-name name)
                                                    (property-name property))
                                            (symbol-package name))
                                  (find-package
                                    ,(package-name (symbol-package name)))))
                       properties)))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,gtype-name *known-interfaces*) ',name))))

;;; --- gobject.generating.lisp ------------------------------------------------
