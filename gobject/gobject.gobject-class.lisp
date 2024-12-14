;;; ----------------------------------------------------------------------------
;;; gobject.gobject-class.lisp
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

;;; ----------------------------------------------------------------------------

;;      standard-direct-slot-definition
;;      ╰── gobject-direct-slot-definition
;;          ├── gobject-prop-direct-slot-definition
;;          ╰── gobject-func-direct-slot-definition

(defclass gobject-direct-slot-definition (standard-direct-slot-definition)
  ((prop-type :initform nil
              :initarg :prop-type
              :reader gobject-direct-slot-definition-prop-type)))

(defclass gobject-prop-direct-slot-definition (gobject-direct-slot-definition)
  ((prop-name :initform nil
              :initarg :prop-name
              :reader gobject-prop-direct-slot-definition-prop-name)))

(defclass gobject-func-direct-slot-definition (gobject-direct-slot-definition)
  ((getter-name :initform nil
                :initarg :getter
                :reader gobject-func-direct-slot-definition-getter-name)
   (setter-name :initform nil
                :initarg :setter
                :reader gobject-func-direct-slot-definition-setter-name)))

;;      standard-effective-slot-definition
;;      ╰── gobject-effective-slot-definition
;;          ├── gobject-prop-effective-slot-definition
;;          ╰── gobject-func-effective-slot-definition

(defclass gobject-effective-slot-definition (standard-effective-slot-definition)
  ((prop-type :initform nil
              :initarg :prop-type
              :accessor gobject-effective-slot-definition-prop-type)))

(defclass gobject-prop-effective-slot-definition
          (gobject-effective-slot-definition)
  ((prop-name :initform nil
              :initarg :prop-name
              :accessor gobject-prop-effective-slot-definition-prop-name)))

(defclass gobject-func-effective-slot-definition
          (gobject-effective-slot-definition)
  ((getter-name :initform nil
                :initarg :getter
                :accessor gobject-func-effective-slot-definition-getter-name)
   (setter-name :initform nil
                :initarg :setter
                :accessor gobject-func-effective-slot-definition-setter-name)
   (getter-func :initform nil
                :accessor gobject-func-effective-slot-definition-getter-func)
   (setter-func :initform nil
                :accessor gobject-func-effective-slot-definition-setter-func)))

;;; ----------------------------------------------------------------------------

;; Called from INITIALIZE-INSTANCE for an object
(defun filter-initargs-by-class (class initargs)
  (iter (with slots = (class-slots class))
        (for (arg-name arg-value) on initargs by #'cddr)
        (for slot = (find arg-name slots :key #'slot-definition-initargs
                                         :test 'member))
        (unless (and slot (typep slot 'gobject-effective-slot-definition))
          (nconcing (list arg-name arg-value)))))

;; Check if BASE-CLASS or a subclass of BASE-CLASS is on the list of
;; direct superclasses of INITARGS. This is not true for an interface
;; definition
(defun initargs-have-base-in-superclass (initargs base-class)
  (let ((superclasses (getf initargs :direct-superclasses)))
    (iter (for class in superclasses)
          (thereis (subtypep class base-class)))))

;; Remove key and value for REMOVE-KEY from the list of INITARGS
(defun filter-from-initargs (initargs remove-key)
  (iter (for (key value) on initargs by #'cddr)
        (unless (eq key remove-key)
          (collect key into new-initargs)
          (collect value into new-initargs))
        (finally (return new-initargs))))

(defun compute-new-initargs-for-metaclass (initargs base-class)
  (if (initargs-have-base-in-superclass initargs base-class)
      initargs
      ;; At this point we have an interface definition, we add BASE-CLASS,
      ;; that is G:OBJECT, to the list of direct superclasses
      (append (filter-from-initargs initargs :direct-superclasses)
              (list :direct-superclasses
                    (append (getf initargs :direct-superclasses)
                            (list (find-class base-class)))))))

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :around
           ((class gobject-class) &rest initargs)
  (apply #'call-next-method
         class
         (compute-new-initargs-for-metaclass initargs 'object)))

(defmethod initialize-instance :after
           ((class gobject-class) &key &allow-other-keys)
  (when (gobject-class-direct-gname class)
    (setf (glib:symbol-for-gtype (gobject-class-direct-gname class))
          (class-name class))
    (glib-init:at-init (class)
        (initialize-gobject-class-g-type class))))

(defmethod reinitialize-instance :around
           ((class gobject-class) &rest initargs
                                  &key (direct-superclasses nil superclasses-p)
                                  &allow-other-keys)
  (declare (ignore direct-superclasses))
  (if superclasses-p
      (apply #'call-next-method
             class
             (compute-new-initargs-for-metaclass initargs 'object))
      (call-next-method)))

;;; ----------------------------------------------------------------------------

(defmethod finalize-inheritance :after ((class gobject-class))
  (iter (for superclass in (class-direct-superclasses class))
        (unless (class-finalized-p superclass)
          (finalize-inheritance superclass)))
  (setf (gobject-class-gname class)
        (or (gobject-class-direct-gname class)
            (let ((gobject-superclass
                    (iter (for superclass in (class-direct-superclasses class))
                          (finding superclass such-that
                                   (typep superclass 'gobject-class)))))
              (assert gobject-superclass)
              (gobject-class-gname gobject-superclass)))))

;;; ----------------------------------------------------------------------------

(defmethod validate-superclass ((class gobject-class)
                                (superclass standard-class))
  t)

;;; ----------------------------------------------------------------------------

;; Called from the method initialize-instance for a gobject-class.
;; This functions calls the type iniatilizer of a GTK class and does
;; a lot of checks. Consider to simplify the code.

(defun initialize-gobject-class-g-type (class)
  ;; We get warnings from the second part of the if-statement for
  ;; objects which we have subclassed. We switch off the warnings.
  (let ((*warn-unknown-gtype* nil))
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

(defmethod direct-slot-definition-class
           ((class gobject-class) &rest initargs
                                  &key allocation &allow-other-keys)
  (declare (ignore initargs))
  (case allocation
    (:gobject-property (find-class 'gobject-prop-direct-slot-definition))
    (:gobject-fn (find-class 'gobject-func-direct-slot-definition))
    (otherwise (call-next-method))))

(defvar *effective-slot-def* nil)

(defmethod effective-slot-definition-class
           ((class gobject-class) &rest initargs)
  (declare (ignore initargs))
  (or *effective-slot-def*
      (call-next-method)))

(defmethod compute-effective-slot-definition
           ((class gobject-class) name direct-slots)
  (let ((effective-slot
         (let ((*effective-slot-def*
                (iter (for slot in direct-slots)
                      (when (typep slot 'gobject-direct-slot-definition)
                        (return
                          (etypecase slot
                            (gobject-prop-direct-slot-definition
                              (find-class 'gobject-prop-effective-slot-definition))
                            (gobject-func-direct-slot-definition
                              (find-class 'gobject-func-effective-slot-definition))))))))
           (call-next-method))))
    (when (typep effective-slot 'gobject-effective-slot-definition)
      (let ((allocation
             (iter (for direct-slot in direct-slots)
                   (when (slot-definition-allocation direct-slot)
                     (return (slot-definition-allocation direct-slot)))))
            (property-name
             (iter (for direct-slot in direct-slots)
                   (when (and (typep direct-slot 'gobject-prop-direct-slot-definition)
                              (gobject-prop-direct-slot-definition-prop-name direct-slot))
                     (return
                       (gobject-prop-direct-slot-definition-prop-name direct-slot)))))
            (property-type
             (iter (for direct-slot in direct-slots)
                   (when (gobject-direct-slot-definition-prop-type direct-slot)
                     (return
                       (gobject-direct-slot-definition-prop-type direct-slot)))))
            (property-getter
             (iter (for direct-slot in direct-slots)
                   (when (and (typep direct-slot 'gobject-func-direct-slot-definition)
                              (gobject-func-direct-slot-definition-getter-name direct-slot))
                     (return
                       (gobject-func-direct-slot-definition-getter-name direct-slot)))))
            (property-setter
             (iter (for direct-slot in direct-slots)
                   (when (and (typep direct-slot 'gobject-func-direct-slot-definition)
                              (gobject-func-direct-slot-definition-setter-name direct-slot))
                     (return
                       (gobject-func-direct-slot-definition-setter-name direct-slot))))))
        (ecase allocation
          (:gobject-property
           (assert property-name
                   nil
                   "PROP-NAME for slot ~A on class ~A must be specified"
                   name
                   (class-name class))
           (setf (gobject-prop-effective-slot-definition-prop-name effective-slot)
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
                     "PROP-TYPE for slot ~A on class ~A must be  specified ~
                      because at least one of accessor is specified ~
                      as a foreign function"
                     name
                     (class-name class)))
           (setf (gobject-func-effective-slot-definition-getter-name effective-slot)
                 property-getter
                 (gobject-func-effective-slot-definition-setter-name effective-slot)
                 property-setter

                 (gobject-func-effective-slot-definition-getter-func effective-slot)
                 (and property-getter
                      (when (stringp property-getter)
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

                 (gobject-func-effective-slot-definition-setter-func effective-slot)
                 (and property-setter
                      (when (stringp property-setter)
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
            (slot gobject-prop-effective-slot-definition))
  (handler-case
    (and (slot-boundp object 'pointer)
         (object-pointer object)
         (progn
           (class-property-type
               (type-from-instance (object-pointer object))
               (gobject-prop-effective-slot-definition-prop-name slot)
               :assert-readable t)
           t))
    (property-unreadable-error () nil)))

(defmethod slot-boundp-using-class
           ((class gobject-class) object
            (slot gobject-func-effective-slot-definition))
  (and (slot-boundp object 'pointer)
       (object-pointer object)
       (not (null (gobject-func-effective-slot-definition-getter-func slot)))))

(defmethod slot-boundp-using-class
           ((class gobject-class) object
            (slot gobject-effective-slot-definition))
  (slot-boundp object 'pointer))

;; This method calls G:OBJECT-PROPERTY to get the slot value from the C object
(defmethod slot-value-using-class
           ((class gobject-class)
            object
            (slot gobject-prop-effective-slot-definition))
  (object-property (object-pointer object)
                   (gobject-prop-effective-slot-definition-prop-name slot)
                   (gobject-effective-slot-definition-prop-type slot)))

;; This method calls (SETF G:OBJECT-PROPERTY) to set the slot of the C object
(defmethod (setf slot-value-using-class)
           (new-value
            (class gobject-class)
            object
            (slot gobject-prop-effective-slot-definition))
  (setf (object-property (object-pointer object)
                         (gobject-prop-effective-slot-definition-prop-name slot)
                         (gobject-effective-slot-definition-prop-type slot))
        new-value))

(defmethod slot-value-using-class
           ((class gobject-class)
            object
            (slot gobject-func-effective-slot-definition))
  (let ((func (gobject-func-effective-slot-definition-getter-func slot)))
    (funcall func object)))

(defmethod (setf slot-value-using-class)
           (new-value
            (class gobject-class)
            object
            (slot gobject-func-effective-slot-definition))
  (funcall (gobject-func-effective-slot-definition-setter-func slot)
           object
           new-value)
  new-value)

(defmethod slot-makunbound-using-class
           ((class gobject-class)
            object
            (slot gobject-effective-slot-definition))
  (declare (ignore object))
  nil)

;;; --- End of file gobject.gobject-class.lisp ---------------------------------
