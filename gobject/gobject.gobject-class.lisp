;;; ----------------------------------------------------------------------------
;;; gobject.gobject-class.lisp
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

(in-package :gobject)

;;; ----------------------------------------------------------------------------

(define-condition property-access-error (error)
  ((property-name :initarg :property-name
                  :reader property-access-error-property-name)
   (class-name :initarg :class-name
               :reader property-access-error-class-name)
   (message :initarg :message :reader property-access-error-message))
  (:report (lambda (condition stream)
             (format stream "Error accessing property '~A' on class '~A': ~A"
                     (property-access-error-property-name condition)
                     (property-access-error-class-name condition)
                     (property-access-error-message condition)))))

(define-condition property-unreadable-error (property-access-error)
  ()
  (:default-initargs :message "property is not readable"))

(define-condition property-unwritable-error (property-access-error)
  ()
  (:default-initargs :message "property is not writable"))

;;; ----------------------------------------------------------------------------

(defclass gobject-class (standard-class)
  ((gname :initform nil
          :accessor gobject-class-gname)
   (direct-gname :initform nil
                 :initarg :gname
                 :accessor gobject-class-direct-gname)
   (initializer :initform nil
                :initarg :initializer
                :reader gobject-class-initializer)
   (interface-p :initform nil
                :initarg :interface-p
                :reader gobject-class-interface-p))
  (:documentation "Metaclass for GObject based classes."))

(export 'gobject-class)
(export 'gobject-class-gname)
(export 'gobject-class-direct-gname)
(export 'gobject-class-initializer)
(export 'gobject-class-interface-p)

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :around ((class gobject-class) &rest initargs)
  (apply #'call-next-method class
         (compute-new-initargs-for-metaclass initargs 'object)))

(defmethod initialize-instance :after
           ((class gobject-class) &key &allow-other-keys)
  (log-for :subclass
           ":subclass INITIALIZE-INSTANCE :after for class ~a ~a ~a~%"
           class (gobject-class-direct-gname class) (class-name class))
  (when (gobject-class-direct-gname class)
    (setf (glib:symbol-for-gtype (gobject-class-direct-gname class))
          (class-name class))
    (glib-init:at-init (class)
        (initialize-gobject-class-g-type class))))

(defmethod reinitialize-instance :around ((class gobject-class)
                                          &rest initargs
                                          &key (direct-superclasses nil d-s-p)
                                          &allow-other-keys)
  (declare (ignore direct-superclasses))
  (if d-s-p
      (apply #'call-next-method
             class
             (compute-new-initargs-for-metaclass initargs 'object))
      (call-next-method)))

;;; ----------------------------------------------------------------------------

;;      standard-direct-slot-definition
;;      ╰── gobject-direct-slot-definition
;;          ├── gobject-property-direct-slot-definition
;;          ╰── gobject-fn-direct-slot-definition

(defclass gobject-direct-slot-definition (standard-direct-slot-definition)
  ((g-property-type :initform nil
                    :initarg :g-property-type
                    :reader gobject-direct-slot-definition-g-property-type)))

(defclass gobject-property-direct-slot-definition
          (gobject-direct-slot-definition)
  ((g-property-name :initform nil
                    :initarg :g-property-name
                    :reader
                    gobject-property-direct-slot-definition-g-property-name)))

(defclass gobject-fn-direct-slot-definition (gobject-direct-slot-definition)
  ((g-getter-name :initform nil
                  :initarg :g-getter
                  :reader gobject-fn-direct-slot-definition-g-getter-name)
   (g-setter-name :initform nil
                  :initarg :g-setter
                  :reader gobject-fn-direct-slot-definition-g-setter-name)))

;;      standard-effective-slot-definition
;;      ╰── gobject-effective-slot-definition
;;          ├── gobject-property-effective-slot-definition
;;          ╰── gobject-fn-effective-slot-definition

(defclass gobject-effective-slot-definition (standard-effective-slot-definition)
  ((g-property-type :initform nil
                    :initarg :g-property-type
                    :accessor
                    gobject-effective-slot-definition-g-property-type)))

(defclass gobject-property-effective-slot-definition
          (gobject-effective-slot-definition)
  ((g-property-name :initform nil
                    :initarg :g-property-name
                    :accessor
                    gobject-property-effective-slot-definition-g-property-name)))

(defclass gobject-fn-effective-slot-definition
          (gobject-effective-slot-definition)
  ((g-getter-name :initform nil
                  :initarg :g-getter
                  :accessor gobject-fn-effective-slot-definition-g-getter-name)
   (g-setter-name :initform nil
                  :initarg :g-setter
                  :accessor gobject-fn-effective-slot-definition-g-setter-name)
   (g-getter-fn :initform nil
                :accessor gobject-fn-effective-slot-definition-g-getter-fn)
   (g-setter-fn :initform nil
                :accessor gobject-fn-effective-slot-definition-g-setter-fn)))

;;; ----------------------------------------------------------------------------

(defun compute-new-initargs-for-metaclass (initargs base-class)
  (if (initargs-have-base-in-superclass initargs base-class)
      initargs
      ;; At this point we have an interface definition, we add BASE-CLASS,
      ;; that is G:OBJECT, to the list of superclasses.
      (append (filter-from-initargs initargs :direct-superclasses)
              (list :direct-superclasses
                    (append (getf initargs :direct-superclasses)
                            (list (find-class base-class)))))))

;; Check if the class is derived from BASE-CLASS, that is G:OBJECT. This is
;; not true for an interface definition.
(defun initargs-have-base-in-superclass (initargs base-class)
  (let ((d-s (getf initargs :direct-superclasses)))
    (loop for class in d-s
          thereis (subtypep class base-class))))

;; Remove a key and value from the list of initargs.
(defun filter-from-initargs (initargs removed-key)
  (loop for (key value) on initargs by #'cddr
        unless (eq key removed-key)
        collect key and collect value))

(defun filter-initargs-by-class (class initargs)
  (iter (with slots = (class-slots class))
        (for (arg-name arg-value) on initargs by #'cddr)
        (for slot = (find arg-name slots :key #'slot-definition-initargs
                                         :test 'member))
        (unless (and slot (typep slot 'gobject-effective-slot-definition))
          (nconcing (list arg-name arg-value)))))

;;; ----------------------------------------------------------------------------

;; Called from the method initialize-instance for a gobject-class.
;; This functions calls the type iniatilizer of a GTK class and does
;; a lot of checks. Consider to simplify the code.

(defun initialize-gobject-class-g-type (class)
  ;; We get warnings from the second part of the if-statement for
  ;; objects which we have subclassed. We switch off the warnings.
  (let ((*warn-unknown-gtype* nil))
    (log-for :subclass
             ":subclass INITIALIZE-GOBJECT-CLASS-G-TYPE for class ~a ~a~%"
             class
             (glib:gtype (gobject-class-direct-gname class)))
    (if (gobject-class-initializer class)
        ;; We have a g-type-initializer function
        (let* ((initializer-fn-ptr (cffi:foreign-symbol-pointer
                                     (gobject-class-initializer class)))
               (gtype (when initializer-fn-ptr
                        (cffi:foreign-funcall-pointer initializer-fn-ptr
                                                      nil
                                                      type-t))))
          (if (null initializer-fn-ptr)
              (warn "Type initializer for class '~A' (GType '~A') is invalid: ~
                     foreign symbol '~A'"
                    (gobject-class-direct-gname class)
                    (class-name class)
                    (gobject-class-initializer class))
              (progn
                (unless gtype
                  (warn "Declared GType name '~A' for class '~A' is invalid ~
                        ('~A' returned 0)"
                        (gobject-class-direct-gname class)
                        (class-name class)
                        (gobject-class-initializer class)))
                (unless (eq gtype
                            (glib:gtype (gobject-class-direct-gname class)))
                  (warn "Declared GType name '~A' for class '~A' does not ~
                         match actual GType name '~A'"
                        (gobject-class-direct-gname class)
                        (class-name class)
                        (glib:gtype-name gtype))))))
        ;; We have no g-type-initializer function. This code prints warnings
        ;; for subclasses. Consider to remove the code.
        (when (zerop (glib:gtype-id
                         (glib:gtype (gobject-class-direct-gname class))))
          ;; This is a hack to avoid a warning when loading the library.
          (when (and *warn-unknown-gtype* ; a hack we never get a warning
                     (not (string= "AtkImplementorIface"
                                   (gobject-class-direct-gname class))))
            (warn "Declared GType name '~A' for class '~A' is invalid."
                  (gobject-class-direct-gname class)
                  (class-name class)))))))

;;; ----------------------------------------------------------------------------

(defmethod finalize-inheritance :after ((class gobject-class))
  (iter (for superclass in (class-direct-superclasses class))
        (unless (class-finalized-p superclass)
          (finalize-inheritance superclass)))
  (setf (gobject-class-gname class)
        (or (gobject-class-direct-gname class)
            (let ((gobject-superclass
                   (iter (for superclass in (class-direct-superclasses class))
                         (finding superclass
                                  such-that (typep superclass
                                                    'gobject-class)))))
              (assert gobject-superclass)
              (gobject-class-gname gobject-superclass)))))

;;; ----------------------------------------------------------------------------

(defmethod validate-superclass ((class gobject-class)
                                (superclass standard-class))
  t)

(defmethod direct-slot-definition-class ((class gobject-class) &rest initargs
                                         &key allocation &allow-other-keys)
  (declare (ignore initargs))
  (case allocation
    (:gobject-property (find-class 'gobject-property-direct-slot-definition))
    (:gobject-fn (find-class 'gobject-fn-direct-slot-definition))
    (otherwise (call-next-method))))

(defvar *e-s-d* nil)

(defmethod effective-slot-definition-class ((class gobject-class) &rest initargs)
  (declare (ignore initargs))
  (or *e-s-d* (call-next-method)))

(defmethod compute-effective-slot-definition ((class gobject-class)
                                              name
                                              direct-slots)
  (let ((effective-slot
         (let ((*e-s-d*
                (loop
                  for slot in direct-slots
                  when (typep slot 'gobject-direct-slot-definition)
                  return (etypecase slot
                         (gobject-property-direct-slot-definition
                          (find-class 'gobject-property-effective-slot-definition))
                         (gobject-fn-direct-slot-definition
                          (find-class 'gobject-fn-effective-slot-definition))))))
           (call-next-method))))
    (when (typep effective-slot 'gobject-effective-slot-definition)
      (let ((allocation
             (loop
               for direct-slot in direct-slots
               when (slot-definition-allocation direct-slot)
               return (slot-definition-allocation direct-slot)))
            (property-name
             (loop
               for direct-slot in direct-slots
               when (and (typep direct-slot
                                'gobject-property-direct-slot-definition)
                         (gobject-property-direct-slot-definition-g-property-name direct-slot))
               return (gobject-property-direct-slot-definition-g-property-name direct-slot)))
            (property-type
             (loop
               for direct-slot in direct-slots
               when (gobject-direct-slot-definition-g-property-type direct-slot)
               return (gobject-direct-slot-definition-g-property-type direct-slot)))
            (property-getter
             (loop
               for direct-slot in direct-slots
               when (and (typep direct-slot 'gobject-fn-direct-slot-definition)
                         (gobject-fn-direct-slot-definition-g-getter-name direct-slot))
               return (gobject-fn-direct-slot-definition-g-getter-name direct-slot)))
            (property-setter
             (loop
               for direct-slot in direct-slots
               when (and (typep direct-slot 'gobject-fn-direct-slot-definition)
                         (gobject-fn-direct-slot-definition-g-setter-name direct-slot))
               return (gobject-fn-direct-slot-definition-g-setter-name direct-slot))))
        (setf (gobject-effective-slot-definition-g-property-type effective-slot)
              (gobject-effective-slot-definition-g-property-type effective-slot))
        (ecase allocation
          (:gobject-property
           (assert property-name
                   nil
                   "G-PROPERTY-NAME for slot ~A on class ~A must be specified"
                   name
                   (class-name class))
           (setf (gobject-property-effective-slot-definition-g-property-name effective-slot)
                 property-name))
          (:gobject-fn
           (assert (or property-getter property-setter)
                   nil
                   "At least one of G-PROPERTY-GETTER or G-PROPERTY-SETTER ~
                    for slot ~A on class ~A must be specified"
                   name
                   (class-name class))
           (when (or (and property-getter
                          (stringp property-getter))
                     (and property-setter
                          (stringp property-setter)))
             (assert property-type
                     nil
                     "G-PROPERTY-TYPE for slot ~A on class ~A must be ~
                      specified because at least one of accessor is specified ~
                      as a foreign function"
                     name
                     (class-name class)))
           (setf (gobject-fn-effective-slot-definition-g-getter-name effective-slot)
                 property-getter
                 (gobject-fn-effective-slot-definition-g-setter-name effective-slot)
                 property-setter
                 (gobject-fn-effective-slot-definition-g-getter-fn effective-slot)
                 (and property-getter
                      (if (stringp property-getter)
                          (compile nil
                                   (if (cffi:foreign-symbol-pointer property-getter)
                                       `(lambda (object)
                                          (cffi:foreign-funcall ,property-getter
                                                                object object
                                                                ,property-type))
                                       `(lambda (object)
                                          (declare (ignore object))
                                          (error "Property getter ~A is not available"
                                                 ,property-getter))))
                          property-getter))
                 (gobject-fn-effective-slot-definition-g-setter-fn effective-slot)
                 (and property-setter
                      (if (stringp property-setter)
                          (compile nil
                                   (if (cffi:foreign-symbol-pointer property-setter)
                                       `(lambda (object new-value)
                                          (cffi:foreign-funcall ,property-setter
                                                                object
                                                                object
                                                                ,property-type
                                                                new-value
                                                                :void))
                                       `(lambda (object)
                                          (declare (ignore object))
                                          (error "Property setter ~A is not avaiable"
                                                 ,property-setter))))
                                                 property-setter)))))))
    effective-slot))

;;; ----------------------------------------------------------------------------

(defmethod slot-boundp-using-class
    ((class gobject-class) object
     (slot gobject-property-effective-slot-definition))
  (handler-case
    (and (slot-boundp object 'pointer)
         (object-pointer object)
         (progn
           (class-property-type
               (type-from-instance (object-pointer object))
               (gobject-property-effective-slot-definition-g-property-name slot)
               :assert-readable t)
             t))
    (property-unreadable-error () nil)))

(defmethod slot-boundp-using-class ((class gobject-class) object
                                    (slot gobject-fn-effective-slot-definition))
  (and (slot-boundp object 'pointer)
       (object-pointer object)
       (not (null (gobject-fn-effective-slot-definition-g-getter-fn slot)))))

(defmethod slot-boundp-using-class ((class gobject-class) object
                                    (slot gobject-effective-slot-definition))
  (slot-boundp object 'pointer))


(defmethod slot-value-using-class ((class gobject-class) object
                                   (slot gobject-property-effective-slot-definition))
  (object-property
               (object-pointer object)
               (gobject-property-effective-slot-definition-g-property-name slot)
               (gobject-effective-slot-definition-g-property-type slot)))

(defmethod (setf slot-value-using-class)
           (new-value (class gobject-class)
            object (slot gobject-property-effective-slot-definition))
  (setf (object-property
            (object-pointer object)
            (gobject-property-effective-slot-definition-g-property-name slot)
            (gobject-effective-slot-definition-g-property-type slot))
        new-value))

(defmethod slot-value-using-class ((class gobject-class) object
                                   (slot gobject-fn-effective-slot-definition))
  (let ((fn (gobject-fn-effective-slot-definition-g-getter-fn slot)))
    (funcall fn object)))

(defmethod (setf slot-value-using-class)
           (new-value (class gobject-class)
            object (slot gobject-fn-effective-slot-definition))
  (funcall (gobject-fn-effective-slot-definition-g-setter-fn slot)
           object
           new-value)
  new-value)

(defmethod slot-makunbound-using-class ((class gobject-class) object
                                        (slot gobject-effective-slot-definition))
  (declare (ignore object))
  nil)

;;; --- End of file gobject.gobject-class.lisp ---------------------------------
