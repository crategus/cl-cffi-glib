;;; ----------------------------------------------------------------------------
;;; gobject.base.lisp
;;;
;;; The documentation in this file is taken from the GObject Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GObject
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;; GObject
;;;
;;;     The base object type
;;;
;;; Types and Values
;;;
;;;     GObject
;;;     GObjectClass
;;;     GObjectConstructParam                              not exported
;;;     GParameter                                         not exported
;;;     GInitiallyUnowned
;;;
;;; Functions
;;;
;;;     GCallback
;;;
;;;     G_TYPE_IS_OBJECT
;;;     G_IS_OBJECT
;;;
;;;     g_object_class_install_property                     not exported
;;;     g_object_class_install_properties                   not implemented
;;;     g_object_class_find_property
;;;     g_object_class_list_properties
;;;     g_object_class_override_property                    not implemented
;;;
;;;     g_object_interface_install_property                 not exported
;;;     g_object_interface_find_property
;;;     g_object_interface_list_properties
;;;
;;;     g_object_new
;;;     g_object_new_with_properties
;;;     g_object_newv                                       Deprecated 2.54
;;;     g_object_ref
;;;     g:object-ref-count
;;;     g_object_unref
;;;     g_object_ref_sink                                   not exported
;;;     g_set_object                                        not implemented
;;;     g_clear_object                                      not implemented
;;;     g_object_is_floating                                not implemented
;;;     g_object_force_floating                             not implemented
;;;
;;;     GToggleNotify
;;;     g_object_add_toggle_ref                             not exported
;;;     g_object_remove_toggle_ref                          not exported
;;;
;;;     g_object_notify
;;;     g_object_notify_by_pspec                            not implemented
;;;     g_object_freeze_notify
;;;     g_object_thaw_notify
;;;
;;;     g_object_get_data
;;;     g_object_set_data
;;;     g_object_set_data_full
;;;
;;;     g_object_set_property
;;;     g_object_get_property
;;;
;;; Signals
;;;
;;;     notify
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GBinding
;;;     ├── GInitiallyUnowned
;;;     ╰── GTypeModule
;;; ----------------------------------------------------------------------------

(in-package :gobject)

(defvar *current-creating-object* nil)
(defvar *current-object-from-pointer* nil)
(defvar *currently-making-object-p* nil)

(let ((gobjects-strong-table (make-hash-table :test 'equal))
      (gobjects-weak-table (tg:make-weak-hash-table :test 'equal
                                                    :weakness :value)))

  (glib-init:at-finalize ()
    (clrhash gobjects-weak-table)
    (clrhash gobjects-strong-table)
    (setf *current-creating-object* nil
          *current-object-from-pointer* nil
          *currently-making-object-p* nil))

  ;; Access the hashtables with functions

  (defun get-gobject-for-pointer-strong (ptr)
    (gethash (cffi:pointer-address ptr) gobjects-strong-table))
  (defun (setf get-gobject-for-pointer-strong) (value ptr)
    (setf (gethash (cffi:pointer-address ptr) gobjects-strong-table) value))
  (defun rem-gobject-for-pointer-strong (ptr)
    (remhash (cffi:pointer-address ptr) gobjects-strong-table))

  (defun list-gobject-for-pointer-strong ()
    (iter (for (key value) in-hashtable gobjects-strong-table)
          (collect value)))

  (defun get-gobject-for-pointer-weak (ptr)
    (gethash (cffi:pointer-address ptr) gobjects-weak-table))
  (defun (setf get-gobject-for-pointer-weak) (value ptr)
    (setf (gethash (cffi:pointer-address ptr) gobjects-weak-table) value))
  (defun rem-gobject-for-pointer-weak (ptr)
    (remhash (cffi:pointer-address ptr) gobjects-weak-table))

  (defun list-gobject-for-pointer-weak ()
    (iter (for (key value) in-hashtable gobjects-weak-table)
          (collect value)))

  (defun get-gobject-for-pointer (ptr)
    (or (gethash (cffi:pointer-address ptr) gobjects-strong-table)
        (gethash (cffi:pointer-address ptr) gobjects-weak-table)))

  (defun list-gobject-for-pointer ()
    (let ((weak (list-gobject-for-pointer-weak))
          (strong (list-gobject-for-pointer-strong)))
      (values (append strong weak)
              (length strong)
              (length weak)))))

;;; ----------------------------------------------------------------------------
;;; GParameter                                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %parameter
  (name (:string :free-from-foreign nil :free-to-foreign nil))
  (value (:struct value)))

;;; ----------------------------------------------------------------------------
;;; GObject
;;; ----------------------------------------------------------------------------

(defclass object ()
  ((pointer
    :type (or null cffi:foreign-pointer)
    :initarg :pointer
    :accessor object-pointer
    :initform nil)
   (has-reference
    :type boolean
    :accessor object-has-reference
    :initform nil)))

(export 'object)
(export 'object-pointer)
(export 'object-has-reference)

;; Add OBJECT to the global hash table for registered types
(glib-init:at-init ()
  (setf (glib:symbol-for-gtype "GObject") 'object))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation 'object 'type)
 "@version{2024-12-14}
  @begin{short}
    The @class{g:object} class is the fundamental type providing the common
    attributes and methods for all object types in GTK, Pango and other
    libraries.
  @end{short}
  The @class{g:object} class provides methods for object construction and
  destruction, property access methods, and signal support.
  @begin[Lisp Implementation]{dictionary}
    In the Lisp implementation two slots are added. The @slot[g:object]{pointer}
    slot holds the foreign pointer to the C instance of the object. The
    @slot[g:object]{has-reference} slot is initialized to the @em{true} value
    during creation of an object. See the slot access functions for examples.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"notify\" signal}
    @begin{pre}
lambda (object pspec)    :no-hooks
    @end{pre}
    @begin[code]{table}
      @entry[object]{The @class{g:object} instance which received the signal.}
      @entry[pspec]{The @symbol{g:param-spec} instance of the property which
        changed.}
    @end{table}
    The signal is emitted on an object when one of its properties has been
    changed. Note that getting this signal does not guarantee that the
    value of the property has actually changed, it may also be emitted when
    the setter for the property is called to reinstate the previous value.
    This signal is typically used to obtain change notification for a single
    property, by specifying the property name as a detail in the
    @fun{g:signal-connect} function call, like this:
    @begin{pre}
(g:signal-connect switch \"notify::active\"
   (lambda (widget pspec)
     (declare (ignore pspec))
     (if (gtk:switch-active widget)
         (setf (gtk:label-label label) \"The Switch is ON\")
         (setf (gtk:label-label label) \"The Switch is OFF\"))))
    @end{pre}
    It is important to note that you must use canonical parameter names as
    detail strings for the notify signal.
  @end{dictionary}
  @see-constructor{g:object-new}
  @see-slot{g:object-has-reference}
  @see-slot{g:object-pointer}
  @see-symbol{g:param-spec}
  @see-function{g:signal-connect}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor details
;;; ----------------------------------------------------------------------------

;;; --- g:object-has-reference -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-reference" 'object) t)
 "Holds the @em{true} value when the instance is successfully registered.")

#+liber-documentation
(setf (liber:alias-for-function 'object-has-reference)
      "Accessor"
      (documentation 'object-has-reference 'function)
 "@version{2024-12-14}
  @syntax{(g:object-has-reference object) => has-reference}
  @argument[object]{a @class{g:object} instance}
  @argument[has-reference]{@em{true} when registering @arg{object}}
  @begin{short}
    Accessor of the @slot[g:object]{has-reference} slot of the @class{g:object}
    class.
  @end{short}
  The slot is set to @em{true} when registering an object during creation.
  @see-class{g:object}")

;;; --- g:object-pointer -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pointer" 'object) t)
 "Holds a foreign pointer to the C instance of a GObject.")

#+liber-documentation
(setf (liber:alias-for-function 'object-pointer)
      "Accessor"
      (documentation 'object-pointer 'function)
 "@version{2024-12-14}
  @syntax{(g:object-pointer object) => pointer}
  @argument[object]{a @class{g:object} instance}
  @argument[pointer]{a foreign pointer to the C instance of @arg{object}}
  @begin{short}
    Accessor of the @slot[g:object]{pointer} slot of the @class{g:object} class.
  @end{short}
  The @fun{g:object-pointer} function gets the foreign C pointer of a
  @class{g:object} instance.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq label (make-instance 'gtk:label)) => #<GTK:LABEL {E2DB181@}>
(g:object-pointer label) => #.(SB-SYS:INT-SAP #X081BDAE0)
    @end{pre}
  @end{dictionary}
  @see-class{g:object}")

;; Abbreviation POINTER for the OBJECT-POINTER slot access function
(defmethod glib:pointer ((instance object))
  (object-pointer instance))

;;; ----------------------------------------------------------------------------
;;; GInitiallyUnowned
;;; ----------------------------------------------------------------------------

(defclass initially-unowned (object)
  ()
  (:metaclass gobject-class)
  (:gname . "GInitiallyUnowned")
  (:initializer . "g_initially_unowned_get_type")
  (:documentation "Base class that has initial 'floating' reference."))

(export 'initially-unowned)

;;; ----------------------------------------------------------------------------

;; GC for weak pointers

(let ((gobject-gc-hooks nil)
      (gobject-gc-hooks-lock (bt:make-recursive-lock "gobject-gc-hooks-lock")))

  (defun dispose-carefully (pointer)
    (handler-case
      (register-gobject-for-gc pointer)
      (error (err)
        (format t "Error in DISPOSE-CAREFULLY: ~a~%" err))))

  (defmethod release ((obj object))
    (tg:cancel-finalization obj)
    (let ((ptr (object-pointer obj)))
      (setf (object-pointer obj) nil)
      (dispose-carefully ptr)))

  (defun activate-gc-hooks ()
    (bt:with-recursive-lock-held (gobject-gc-hooks-lock)
      (when gobject-gc-hooks
        (iter (for pointer in gobject-gc-hooks)
              (%object-remove-toggle-ref pointer
                                         (cffi:callback toggle-notify)
                                         (cffi:null-pointer)))
        (setf gobject-gc-hooks nil)))
    nil)

  (defun register-gobject-for-gc (pointer)
    (bt:with-recursive-lock-held (gobject-gc-hooks-lock)
      (let ((locks-were-present (not (null gobject-gc-hooks))))
        (push pointer gobject-gc-hooks)
        (unless locks-were-present
          (glib:idle-add #'activate-gc-hooks)))))

  (defun get-gobject-gc-hooks ()
    gobject-gc-hooks))

;;; ----------------------------------------------------------------------------

;; If object was not created from lisp-side, we should ref it
;; If an object is regular object, we should not ref-sink it
;; If an object is GInitiallyUnowned, then it is created with a floating
;; reference, we should ref-sink it
;; A special case is GtkWindow: we should ref-sink it anyway

(defun should-ref-sink-at-creation (object)
  (let ((result (cond ;; not new objects should be ref_sunk
                      ((equal *current-object-from-pointer*
                              (object-pointer object))
                       t)
                      ;; g_object_new returns objects with ref=1,
                      ;; we should save this ref
                      ((eq object *current-creating-object*)
                       ;; but GInitiallyUnowned objects should be ref_sunk
                       (typep object 'initially-unowned))
                      (t t))))
    result))

(defun register-gobject (object)
  (let ((pointer (object-pointer object)))
    (when (should-ref-sink-at-creation object)
      (%object-ref-sink pointer))
    (setf (object-has-reference object) t)
    (setf (get-gobject-for-pointer-strong pointer) object)
    (%object-add-toggle-ref pointer
                            (cffi:callback toggle-notify)
                            (cffi:null-pointer))
    (object-unref pointer)))

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :around ((obj object) &key)
  (when *currently-making-object-p*
    (setf *currently-making-object-p* t))
  (let ((*current-creating-object* obj))
    (call-next-method)))

(defmethod initialize-instance :after ((obj object) &key &allow-other-keys)
  (unless (slot-boundp obj 'pointer)
    (error "INITIALIZE-INIT: Pointer slot is not initialized for ~a" obj))
  (let* ((pointer (object-pointer obj))
         (s (format nil "~a" obj)))
    (tg:finalize obj
                 (lambda ()
                   (handler-case
                     (dispose-carefully pointer)
                     (error (err)
                       (format t "Error in finalizer for ~a: ~a~%" s err))))))
  (register-gobject obj)
  (activate-gc-hooks))

;;; ----------------------------------------------------------------------------

;; TODO: This function is not used.

(cffi:defcallback gobject-weak-ref-finalized :void
    ((data :pointer) (pointer :pointer))
  (declare (ignore data))
  (rem-gobject-for-pointer-weak pointer)
  (when (get-gobject-for-pointer-strong pointer)
    (warn "GObject at ~A was weak-ref-finalized while still holding lisp-side ~
           strong reference to it"
          pointer))
   (rem-gobject-for-pointer-strong pointer))

;;; ----------------------------------------------------------------------------

;; Translate a pointer to the corresponding Lisp object. If a Lisp object does
;; not exist, create the Lisp object.
(defun get-or-create-gobject-for-pointer (pointer)
  (unless (cffi:null-pointer-p pointer)
    (or (get-gobject-for-pointer pointer)
        (create-gobject-from-pointer pointer))))

;;; ----------------------------------------------------------------------------

;; Create a Lisp object from a C pointer to an existing C object.

(defun create-gobject-from-pointer (pointer)
  (flet (;; Get the corresponing lisp type for a GType
         (get-gobject-lisp-type (gtype)
            (iter (while (not (null gtype)))
                  (for lisp-type = (glib:symbol-for-gtype
                                       (glib:gtype-name gtype)))
                  (when lisp-type (return lisp-type))
                  (setf gtype (type-parent gtype)))))
    (let* ((gtype (type-from-instance pointer))
           (lisp-type (get-gobject-lisp-type gtype)))
      (unless lisp-type
        (error "Type ~a is not registered with REGISTER-OBJECT-TYPE"
               (glib:gtype-name gtype)))
      (let ((*current-object-from-pointer* pointer))
        (make-instance lisp-type :pointer pointer)))))

;;; ----------------------------------------------------------------------------

;; Define the type foreign-gobject-type and the type transformation rules.

(cffi:define-foreign-type foreign-gobject-type ()
  ((sub-type :reader sub-type
             :initarg :sub-type
             :initform 'object)
   (returnp :reader foreign-gobject-type-returnp
            :initarg :returnp
            :initform nil))
  (:actual-type :pointer))

(cffi:define-parse-method object (&rest args)
  (let* ((sub-type (first (remove-if #'keywordp args)))
         (flags (remove-if-not #'keywordp args))
         (returnp (not (null (find :return flags)))))
    (make-instance 'foreign-gobject-type
                   :sub-type sub-type
                   :returnp returnp)))

(defmethod cffi:translate-to-foreign (object (type foreign-gobject-type))
  (let ((pointer nil))
    (cond ((null object)
           (cffi:null-pointer))
          ((cffi:pointerp object)
           object)
          ((null (setf pointer (object-pointer object)))
           (error "Object ~a has been disposed" object))
          ((typep object 'object)
           (when (sub-type type)
             (assert (typep object (sub-type type))
                      nil
                      "Object ~a is not a subtype of ~a" object (sub-type type)))
           pointer)
          (t (error "Object ~a is not translatable as GObject*" object)))))

(defmethod cffi:translate-from-foreign (pointer (type foreign-gobject-type))
  (let ((object (get-or-create-gobject-for-pointer pointer)))
    (when (and object
               (foreign-gobject-type-returnp type))
      (object-unref (object-pointer object)))
    object))

;;; ----------------------------------------------------------------------------

;; Get the definition of a property for the GObject type. Both arguments are of
;; type string, for example (class-property-info "GtkLabel" "label")

;; TODO: Duplicates the implementation of OBJECT-CLASS-FIND-PROPERTY. But
;; we return a %PARAM-SPEC instance which is the Lisp side of GParamSpec.
;; Improve the implementation of GParamSpec!?

(defun class-property-pspec (gtype name)
  (let ((class (type-class-ref gtype)))
    (when class
      (unwind-protect
        (let ((pspec (%object-class-find-property class name)))
          (unless (cffi:null-pointer-p pspec)
            (parse-param-spec pspec)))
        (type-class-unref class)))))

;; Get the type of a property NAME for a class of type GTYPE
;; Checks if the properties are readable or writeable

(defun class-property-type (gtype name &key assert-readable assert-writable)
  (let ((pspec (class-property-pspec gtype name)))
    (assert pspec
            nil
            "CLASS-PROPERTY-TYPE: Property ~a not registered for ~a object"
            name
            gtype)
    (when (and assert-readable
               (not (%param-spec-readable pspec)))
      (error 'property-unreadable-error
             :property-name name
             :class-name (type-name gtype)))
    (when (and assert-writable
               (not (%param-spec-writable pspec)))
      (error 'property-unwritable-error
             :property-name name
             :class-name (type-name gtype)))
    (%param-spec-type pspec)))

;;; ----------------------------------------------------------------------------

(defmethod make-instance ((class gobject-class) &rest initargs &key pointer)
  (ensure-finalized class)
  (let ((*currently-making-object-p* t))
    (if pointer
        (progn
          (assert (= (length initargs) 2)
                  nil
                  "POINTER can not be combined with other initargs (~A)"
                  initargs)
          (call-next-method))
        (let* ((default-initargs
                (iter (for (arg value) in (class-default-initargs class))
                      (nconcing (list arg value))))
               (effective-initargs (append initargs default-initargs))
               (pointer (create-gobject-from-class class effective-initargs)))
          (apply #'call-next-method class
                 :pointer pointer
                 effective-initargs)))))

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance ((instance object) &rest initargs
                                                  &key &allow-other-keys)
  (let ((filtered-initargs (filter-initargs-by-class (class-of instance)
                                                     initargs)))
    (apply #'call-next-method instance filtered-initargs)))

;;; ----------------------------------------------------------------------------

(defun create-gobject-from-class (class initargs)
  (when (gobject-class-interface-p class)
    (error "Trying to create instance of GInterface '~a' (class '~a')"
           (gobject-class-gname class)
           (class-name class)))
  (let (arg-names arg-values arg-types nc-setters nc-arg-values)
    (declare (dynamic-extent arg-names arg-values arg-types
                             nc-setters nc-arg-values))
    (iter (for (arg-name arg-value) on initargs by #'cddr)
          (for slot = (find arg-name
                            (class-slots class)
                            :key 'slot-definition-initargs
                            :test 'member))
          (when (and slot (typep slot 'gobject-effective-slot-definition)))
          (typecase slot
           (gobject-prop-effective-slot-definition
            (push (gobject-prop-effective-slot-definition-prop-name slot)
                  arg-names)
            (push arg-value arg-values)
            ;; TODO: The list is always filled with NIL values. Why?
            (push (gobject-effective-slot-definition-prop-type slot)
                  arg-types))
           (gobject-func-effective-slot-definition
            (push (gobject-func-effective-slot-definition-setter-func slot)
                  nc-setters)
            (push arg-value nc-arg-values))))
    (let ((object (call-gobject-constructor (gobject-class-gname class)
                                            arg-names
                                            arg-values
                                            arg-types)))
      (iter (for fn in nc-setters)
            (for value in nc-arg-values)
            (funcall fn object value))
      object)))

;;; ----------------------------------------------------------------------------

(defun call-gobject-constructor (gtype names values &optional types)
  (unless types
    (setf types
          (mapcar (lambda (name)
                    (class-property-type gtype name))
                  names)))
  (let ((count (length names)))
    (cffi:with-foreign-object (aptr-values '(:struct value) count)
      (iter (for i from 0 below count)
            (for name in names)
            (for value in values)
            (for type in types)
            (for type1 = (or type
                             (class-property-type gtype name)))
            (for gvalue = (cffi:mem-aptr aptr-values '(:struct value) i))
             (set-gvalue gvalue value type1))
      (unwind-protect
        (glib-sys:with-foreign-string-array (aptr-names names)
          (%object-new-with-properties gtype
                                       count
                                       aptr-names
                                       aptr-values))
        (iter (for i from 0 below count)
              (for gvalue = (cffi:mem-aptr aptr-values '(:struct value) i))
              (value-unset gvalue))))))

;;; ----------------------------------------------------------------------------
;;; GObjectClass                                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct object-class
  (:type-class (:pointer (:struct type-class)))
  (:construct-properties :pointer)
  (:constructor :pointer)
  (:set-property :pointer)
  (:get-property :pointer)
  (:dispose :pointer)
  (:finalize :pointer)
  (:dispatch-properties-changed :pointer)
  (:notify :pointer)
  (:constructed :pointer)
  (:pdummy :pointer :count 7))

;; Accessors for the slots of the GObjectClass structure
(defun object-class-get-property (class)
  (cffi:foreign-slot-value class '(:struct object-class) :get-property))

(defun (setf object-class-get-property) (value class)
  (setf (cffi:foreign-slot-value class '(:struct object-class) :get-property)
        value))

(defun object-class-set-property (class)
  (cffi:foreign-slot-value class '(:struct object-class) :set-property))

(defun (setf object-class-set-property) (value class)
  (setf (cffi:foreign-slot-value class '(:struct object-class) :set-property)
        value))

;;; ----------------------------------------------------------------------------
;;; GObjectConstructParam                                   not exported
;;; ----------------------------------------------------------------------------

;; This structure is not needed in the implementation of the Lisp library
;; and is not exported.

(cffi:defcstruct object-construct-param
  (:pspec (:pointer (:struct param-spec)))
  (:value (:pointer (:struct value))))

;;; ----------------------------------------------------------------------------
;;; GCallback
;;; ----------------------------------------------------------------------------

(cffi:defcallback callback :void () )

#+liber-documentation
(setf (liber:alias-for-symbol 'callback)
      "Callback"
      (liber:symbol-documentation 'callback)
 "@version{#2025-05-27}
  @begin{declaration}
lambda ()
  @end{declaration}
  @begin{short}
    The type used for callback functions in structure definitions and function
    signatures.
  @end{short}
  This does not mean that all callback functions must take no parameters and
  return void. The required signature of a callback function is determined by
  the context in which is used, for example, the signal to which it is
  connected.")

(export 'callback)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_OBJECT
;;; ----------------------------------------------------------------------------

(defun type-is-object (gtype)
 #+liber-documentation
 "@version{2024-12-14}
  @argument[gtype]{a @class{g:type-t} type ID to check}
  @begin{return}
    @em{False} or @em{true}, indicating whether the @arg{gtype} argument is a
    @code{\"GObject\"} type.
  @end{return}
  @begin{short}
    Checks if the passed in type ID is a @code{\"GObject\"} type or derived
    from it.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:type-is-object \"GtkLabel\") => T
(g:type-is-object \"GtkActionable\") => NIL
(g:type-is-object \"gboolean\") => NIL
(g:type-is-object \"unknown\") => NIL
(g:type-is-object nil) => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}"
  (and (not (type-is-a gtype (glib:gtype "GInterface")))
       (type-is-a gtype (glib:gtype "GObject"))))

(export 'type-is-object)

;;; ----------------------------------------------------------------------------
;;; G_IS_OBJECT
;;; ----------------------------------------------------------------------------

(defun is-object (object)
 #+liber-documentation
 "@version{2024-12-14}
  @argument[object]{a valid @symbol{g:type-instance} instance to check}
  @begin{short}
    Checks whether the @arg{object} argument is of @code{\"GObject\"} type
    or derived from it.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:is-object (make-instance 'gtk-button)) => T
    @end{pre}
  @end{dictionary}
  @see-class{g:object}
  @see-symbol{g:type-instance}"
  (type-check-instance-type object (glib:gtype "GObject")))

(export 'is-object)

;;; ----------------------------------------------------------------------------
;;; g_object_class_install_property                         not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_class_install_property" %object-class-install-property)
    :void
  (class (:pointer (:struct object-class)))
  (property-id :uint)
  (pspec (:pointer (:struct param-spec))))

;;; ----------------------------------------------------------------------------
;;; g_object_class_install_properties
;;;
;;; Installs new properties from an array of GParamSpecs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_class_find_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_class_find_property" %object-class-find-property)
    (:pointer (:struct param-spec))
  (class (:pointer (:struct object-class)))
  (name :string))

(defun object-class-find-property (gtype name)
 #+liber-documentation
 "@version{2024-12-14}
  @argument[gtype]{a @class{g:type-t} type ID for an object class type}
  @argument[name]{a string with the name of the property to look up}
  @begin{return}
    The @symbol{g:param-spec} instance for the property, or @code{nil} if
    the object class does not have a property of that name.
  @end{return}
  @begin{short}
    Looks up the @symbol{g:param-spec} instance for a property of an object
    class type.
  @end{short}
  Signals an error if the @arg{gtype} type ID is not a @code{\"GObject\"} type.
  @begin[Examples]{dictionary}
    Get the @symbol{g:param-spec} instance for the @slot[g:simple-action]{name}
    property of the @class{g:simple-action} object.
    @begin{pre}
(setq pspec (g:object-class-find-property \"GSimpleAction\" \"name\"))
=> #.(SB-SYS:INT-SAP #X560E17A46220)
(g:param-spec-name pspec) => \"name\"
(g:param-spec-type pspec) => #<GTYPE :name \"GParamString\" :id 94618525293072>
(g:param-spec-value-type pspec) => #<GTYPE :name \"gchararray\" :id 64>
    @end{pre}
  @end{dictionary}
  @see-class{g:object}
  @see-class{g:type-t}
  @see-symbol{g:param-spec}
  @see-function{g:object-class-list-properties}"
  (assert (type-is-object gtype)
          nil
          "G:OBJECT-CLASS-FIND-PROPERTY: ~a is not a GObject" gtype)
  (let ((class (type-class-ref gtype)))
    (unwind-protect
      (let ((pspec (%object-class-find-property class name)))
        (when (not (cffi:null-pointer-p pspec)) pspec))
      (type-class-unref class))))

(export 'object-class-find-property)

;;; ----------------------------------------------------------------------------
;;; g_object_class_list_properties
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_class_list_properties" %object-class-list-properties)
    (:pointer (:pointer (:struct param-spec)))
  (class (:pointer (:struct object-class)))
  (n-props (:pointer :uint)))

(defun object-class-list-properties (gtype)
 #+liber-documentation
 "@version{2024-12-14}
  @argument[gtype]{a @class{g:type-t} type ID of an object class}
  @return{The list of @symbol{g:param-spec} instances.}
  @begin{short}
    Gets a list of @symbol{g:param-spec} instances for all properties of an
    object class type.
  @end{short}
  Signals an error if the @arg{gtype} type ID is not a @code{\"GObject\"} type.
  @begin[Examples]{dictionary}
    @begin{pre}
(mapcar #'g:param-spec-name
        (g:object-class-list-properties \"GApplication\"))
=> (\"application-id\" \"flags\" \"resource-base-path\" \"is-registered\"
    \"is-remote\" \"inactivity-timeout\" \"action-group\" \"is-busy\")
    @end{pre}
  @end{dictionary}
  @see-class{g:object}
  @see-class{g:type-t}
  @see-symbol{g:param-spec}
  @see-function{g:object-class-find-property}"
  (assert (type-is-object gtype)
          nil
          "G:OBJECT-CLASS-LIST-PROPERTIES: ~a is not a GObject" gtype)
  (let ((class (type-class-ref gtype)))
    (unwind-protect
      (cffi:with-foreign-object (n-props :uint)
        (let ((pspecs (%object-class-list-properties class n-props)))
          (unwind-protect
            (iter (for count from 0 below (cffi:mem-ref n-props :uint))
                  (for pspec = (cffi:mem-aref pspecs :pointer count))
                  (collect pspec))
          (glib:free pspecs))))
      (type-class-unref class))))

(export 'object-class-list-properties)

;;; ----------------------------------------------------------------------------
;;; g_object_class_override_property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_interface_install_property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_interface_find_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_interface_find_property"
               %object-interface-find-property) (:pointer (:struct param-spec))
  (iface (:pointer (:struct type-interface)))
  (name :string))

(defun object-interface-find-property (gtype name)
 #+liber-documentation
 "@version{2024-12-14}
  @argument[gtype]{a @class{g:type-t} type ID for an interface type}
  @argument[name]{a string with the name of a property to lookup}
  @begin{return}
    The @symbol{g:param-spec} instance for the property of the interface type
    with, or @code{nil} if no such property exists.
  @end{return}
  @begin{short}
    Find the @symbol{g:param-spec} instance with the given property name for
    an interface type.
  @end{short}
  Signals an error if the @arg{gtype} type ID is not an @code{\"GInterface\"}
  type.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:object-interface-find-property \"GAction\" \"name\")
=> #.(SB-SYS:INT-SAP #X55A6D24988C0)
(g:param-spec-name *)
=> \"name\"
(g:object-interface-find-property \"GAction\" \"unknown\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:param-spec}"
  (assert (type-is-interface gtype)
          nil
          "G:OBJECT-INTERFACE-FIND-PROPERTY: ~a is not a GInterface type"
          gtype)
  (let ((iface (type-default-interface-ref gtype)))
    (unwind-protect
      (let ((pspec (%object-interface-find-property iface name)))
        (when (not (cffi:null-pointer-p pspec)) pspec))
      (type-default-interface-unref iface))))

(export 'object-interface-find-property)

;;; ----------------------------------------------------------------------------
;;; g_object_interface_list_properties
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_interface_list_properties"
               %object-interface-list-properties)
    (:pointer (:struct param-spec))
  (iface (:pointer (:struct type-interface)))
  (n-props (:pointer :uint)))

(defun object-interface-list-properties (gtype)
 #+liber-documentation
 "@version{2024-12-14}
  @argument[gtype]{a @class{g:type-t} type ID of an interface type}
  @return{The list of @symbol{g:param-spec} instances for all properties of an
    interface type.}
  @begin{short}
    Lists the properties of an interface type.
  @end{short}
  Signals an error if the @arg{gtype} type ID is not an @code{\"GInterface\"}
  type.
  @begin[Examples]{dictionary}
    @begin{pre}
(mapcar #'g:param-spec-name
        (g:object-interface-list-properties \"GAction\"))
=> (\"enabled\" \"name\" \"parameter-type\" \"state\" \"state-type\")
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-symbol{g:param-spec}"
  (assert (type-is-interface gtype)
          nil
          "G:OBJECT-INTERFACE-LIST-PROPERTIES: ~a is not a GInterface type"
          gtype)
  (let ((iface (type-default-interface-ref gtype)))
    (unwind-protect
      (cffi:with-foreign-object (n-props :uint)
        (let ((pspecs (%object-interface-list-properties iface n-props)))
          (unwind-protect
            (iter (for count from 0 below (cffi:mem-ref n-props :uint))
                  (for pspec = (cffi:mem-aref pspecs :pointer count))
                  (collect pspec))
            (glib:free pspecs))))
      (type-default-interface-unref iface))))

(export 'object-interface-list-properties)

;;; ----------------------------------------------------------------------------
;;; g_object_new
;;; ----------------------------------------------------------------------------

(defun object-new (gtype &rest args)
 #+liber-documentation
 "@version{2024-12-14}
  @argument[gtype]{a @class{g:type-t} type ID of the @class{g:object} subtype
    to instantiate}
  @argument[args]{pairs of a property keyword and value}
  @begin{short}
    Creates a new instance of a @class{g:object} subtype and sets its
    properties.
  @end{short}
  Construction parameters which are not explicitly specified are set to their
  default values.
  @begin[Note]{dictionary}
    In the Lisp implementation this function calls the @code{make-instance}
    method to create the new instance.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:object-new \"GtkButton\" :label \"text\" :margin 6)
=> #<GTK:BUTTON {D941381@}>
    @end{pre}
    This is equivalent to:
    @begin{pre}
(make-instance 'gtk:button :label \"text\" :margin 6)
=> #<GTK:BUTTON {D947381@}>
    @end{pre}
  @end{dictionary}
  @see-class{g:object}
  @see-class{g:type-t}"
  (let ((symbol (glib:symbol-for-gtype gtype)))
    (apply 'make-instance symbol args)))

(export 'object-new)

;;; ----------------------------------------------------------------------------
;;; g_object_new_with_properties                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_new_with_properties" %object-new-with-properties)
    :pointer
  (gtype type-t)
  (n-properties :uint)
  (names :pointer)
  (values :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_newv                                           Deprecated 2.54
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_ref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_ref" object-ref) object
 #+liber-documentation
 "@version{2024-12-14}
  @argument[object]{a @class{g:object} instance}
  @return{The same @class{g:object} instance.}
  @short{Increases the reference count of @arg{object}.}
  @see-class{g:object}
  @see-function{g:object-ref-count}
  @see-function{g:object-unref}"
  (object object))

(export 'object-ref)

;;; ----------------------------------------------------------------------------
;;; g:object-ref-count
;;; ----------------------------------------------------------------------------

;; Only used for the g:object-ref-count implementation
(cffi:defcstruct %object
  (:type-instance (:pointer (:struct type-instance)))
  (:ref-count :uint)
  (:data :pointer))

(defun object-ref-count (object)
 #+liber-documentation
 "@version{2024-12-14}
  @argument[object]{a @class{g:object} instance}
  @return{The integer with the reference count of @arg{object}.}
  @short{Returns the reference count of the object.}
  @begin[Notes]{dictionary}
    This function is a Lisp extension. It can be used in test files to check
    the number of references for objects.
  @end{dictionary}
  @see-class{g:object}
  @see-function{g:object-ref}"
  (let ((pointer (if (cffi:pointerp object)
                     object
                     (object-pointer object))))
    (cffi:foreign-slot-value pointer '(:struct %object) :ref-count)))

(export 'object-ref-count)

;;; ----------------------------------------------------------------------------
;;; g_object_unref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_unref" object-unref) :void
 #+liber-documentation
 "@version{2024-12-14}
  @argument[object]{a @class{g:object} instance}
  @begin{short}
    Decreases the reference count of @arg{object}.
  @end{short}
  When its reference count drops to 0, the object is finalized.
  @see-class{g:object}
  @see-function{g:object-ref}"
  (object object))

(export 'object-unref)

;;; ----------------------------------------------------------------------------
;;; g_object_ref_sink                                       not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_ref_sink" %object-ref-sink) :pointer
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; GToggleNotify                                           not exported
;;; ----------------------------------------------------------------------------

(cffi:defcallback toggle-notify :void
    ((data :pointer)
     (object :pointer)
     (is-last-ref :boolean))
  (declare (ignore data))
  (if is-last-ref
      (let ((obj (get-gobject-for-pointer-strong object)))
        (if obj
            (progn
              (rem-gobject-for-pointer-strong object)
              (setf (get-gobject-for-pointer-weak object) obj))
            (warn "TOGGLE-NOTIFY: ~a at ~a has no Lisp side (strong) reference"
                  (type-name (type-from-instance object))
                  object)))
      (let ((obj (get-gobject-for-pointer-weak object)))
        (unless obj
          (warn "TOGGLE-NOTIFY: ~a at ~a has no Lisp side (weak) reference"
                (type-name (type-from-instance object))
                object))
        (rem-gobject-for-pointer-weak object)
        (setf (get-gobject-for-pointer-strong object) obj))))

;;; ----------------------------------------------------------------------------
;;; g_object_add_toggle_ref                                 not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_add_toggle_ref" %object-add-toggle-ref) :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_remove_toggle_ref                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_remove_toggle_ref" %object-remove-toggle-ref) :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_notify
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_notify" object-notify) :void
 #+liber-documentation
 "@version{2025-05-09}
  @argument[object]{a @class{g:object} instance}
  @argument[name]{a string for the name of a property installed on the class
    of @arg{object}}
  @begin{short}
    Emits a @code{\"notify\"} signal for the property on the object.
  @end{short}
  @see-class{g:object}"
  (object object)
  (name :string))

(export 'object-notify)

;;; ----------------------------------------------------------------------------
;;; g_object_notify_by_pspec
;;;
;;; Emits a "notify" signal for the property specified by pspec on object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_freeze_notify
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_freeze_notify" object-freeze-notify) :void
 #+liber-documentation
 "@version{2024-12-14}
  @argument[object]{a @class{g:object} instance}
  @begin{short}
    Increases the freeze count on the object.
  @end{short}
  If the freeze count is non-zero, the emission of @code{\"notify\"} signals on
  the object is stopped. The signals are queued until the freeze count is
  decreased to zero. This is necessary for accessors that modify multiple
  properties to prevent premature notification while the object is still being
  modified.
  @see-class{g:object}
  @see-function{g:object-thaw-notify}"
  (object object))

(export 'object-freeze-notify)

;;; ----------------------------------------------------------------------------
;;; g_object_thaw_notify
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_thaw_notify" object-thaw-notify) :void
 #+liber-documentation
 "@version{2024-12-14}
  @argument[object]{a @class{g:object} instance}
  @begin{short}
    Reverts the effect of a previous call to the @fun{g:object-freeze-notify}
    function.
  @end{short}
  The freeze count is decreased on the object and when it reaches zero, all
  queued @code{\"notify\"} signals are emitted. It is an error to call this
  function when the freeze count is zero.
  @see-class{g:object}
  @see-function{g:object-freeze-notify}"
  (object object))

(export 'object-thaw-notify)

;;; ----------------------------------------------------------------------------
;;; g_object_get_data
;;; g_object_set_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_set_data" %object-set-data) :void
  (object object)
  (key :string)
  (data :pointer))

(defun (setf object-data) (data object key)
  (let ((ptr (%object-get-data object key)))
    (cond ((null data)
           ;; Remove data and free the stable-poiner
           (%object-set-data object key (cffi:null-pointer))
           (when (not (cffi:null-pointer-p ptr))
             (glib:free-stable-pointer ptr)))
          ((cffi:null-pointer-p ptr)
           (setf ptr (glib:allocate-stable-pointer data))
           (%object-set-data object key ptr))
          (t
           (setf (glib:get-stable-pointer-value ptr) data)
           (%object-set-data object key ptr)))
    data))

(cffi:defcfun ("g_object_get_data" %object-get-data) :pointer
  (object object)
  (key :string))

(defun object-data (object key)
 #+liber-documentation
 "@version{2024-12-14}
  @syntax{(g:object-data object key) => data}
  @syntax{(setf (g:object-data object key) data)}
  @argument[object]{a @class{g:object} instance containing the associations}
  @argument[key]{a string with the name of the key}
  @argument[data]{any Lisp object as data to associate with that key}
  @begin{short}
    Each object carries around a table of associations from strings to pointers.
  @end{short}
  The @fun{g:object-data} function gets a named field from the objects table of
  associations. The @setf{g:object-data} function sets an association. If the
  object already had an association with that name, the old association will be
  destroyed.
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar item (make-instance 'g:menu-item)) => ITEM
;; Set an integer
(setf (g:object-data item \"prop\") 123) => 123
(g:object-data item \"prop\") => 123
;; Set a Lisp list
(setf (g:object-data item \"prop\") '(a b c)) => (A B C)
(g:object-data item \"prop\") => (A B C)
;; Set a GObject
(setf (g:object-data item \"prop\") (make-instance 'g:menu-item))
=> #<GIO:MENU-ITEM {1001AAA263@}>
(g:object-data item \"prop\")
=> #<GIO:MENU-ITEM {1001AAA263@}>
;; Clear the association
(setf (g:object-data item \"prop\") nil) => nil
(g:object-data item \"prop\") => nil
    @end{pre}
  @end{dictionary}
  @see-class{g:object}"
  (let ((ptr (%object-get-data object key)))
    (when (not (cffi:null-pointer-p ptr))
      (glib:get-stable-pointer-value ptr))))

(export 'object-data)

;;; ----------------------------------------------------------------------------
;;; GDestroyNotify
;;; ----------------------------------------------------------------------------

(cffi:defcallback destroy-notify :void
    ((data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (unwind-protect
      (funcall func)
      (glib:free-stable-pointer data))))

#+liber-documentation
(setf (liber:alias-for-symbol 'destroy-notify)
      "Callback"
      (liber:symbol-documentation 'destroy-notify)
 "@version{2024-12-14}
  @syntax{lambda ()}
  @begin{short}
    Specifies the type of function which is called when a data element is
    destroyed.
  @end{short}
  The callback function takes no argument and has no return value.
  See the @fun{g:object-set-data-full} function for an example.
  @see-function{g:object-set-data-full}")

(export 'destroy-notify)

;;; ----------------------------------------------------------------------------
;;; g_object_set_data_full
;;; ----------------------------------------------------------------------------

;; TODO: Improve the documentation. We dot not store data, but a pointer
;; to a callback function.

(cffi:defcfun ("g_object_set_data_full" %object-set-data-full) :void
  (object object)
  (key :string)
  (data :pointer)
  (destroy :pointer))

(defun object-set-data-full (object key func)
 #+liber-documentation
 "@version{2025-3-1}
  @argument[object]{a @class{g:object} instance containing the associations}
  @argument[key]{a string for the name of the key}
  @argument[func]{a @symbol{g:destroy-notify} callback function}
  @begin{short}
    Like the @fun{g:object-data} function except it adds notification for
    when the association is destroyed, either by setting it to a different
    value or when the object is destroyed.
  @end{short}
  @begin[Examples]{dictionary}
    Set a destroy notify callback function for a window. This function is
    called when the window is destroyed or when the data is set to @code{nil}.
    @begin{pre}
(g:object-set-data-full window \"about\"
                        (lambda ()
                          (gtk:window-destroy about)))
    @end{pre}
  @end{dictionary}
  @see-class{g:object}
  @see-symbol{g:destroy-notify}
  @see-function{g:object-data}"
  (%object-set-data-full object
                         key
                         (glib:allocate-stable-pointer func)
                         (cffi:callback destroy-notify)))

(export 'object-set-data-full)

;;; ----------------------------------------------------------------------------
;;; g_object_set_property
;;; g_object_get_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_set_property" %object-set-property) :void
  (object object)
  (name :string)
  (value (:pointer (:struct value))))

(defun (setf object-property) (value object name &optional gtype)
  (unless gtype
    (setf gtype
          (class-property-type (type-from-instance object)
                               name
                               :assert-writable t)))
  (with-value (gvalue gtype value)
    (%object-set-property object name gvalue))
  value)

(cffi:defcfun ("g_object_get_property" %object-get-property) :void
  (object object)
  (name :string)
  (value (:pointer (:struct value))))

(defun object-property (object name &optional gtype)
 #+liber-documentation
 "@version{2024-12-14}
  @syntax{(g:object-property object name gtype) => value}
  @syntax{(setf (g:object-property object name gtype) value)}
  @argument[object]{a @class{g:object} instance}
  @argument[name]{a string with the name of the property}
  @argument[gtype]{an optional @class{g:type-t} type ID of the property}
  @argument[value]{a value for the property}
  @short{Accessor of the property of an object.}
  @begin[Examples]{dictionary}
    Setting and retrieving the
    @slot[gtk:settings]{gtk-application-prefer-dark-theme} setting.
    @begin{pre}
(defvar settings (gtk:settings-default))
=> SETTINGS
(setf (g:object-property settings \"gtk-application-prefer-dark-theme\") t)
=> T
(g:object-property settings \"gtk-application-prefer-dark-theme\")
=> T
    @end{pre}
  @end{dictionary}
  @see-class{g:object}
  @see-class{g:type-t}"
  (restart-case
    (unless gtype
      (setf gtype
            (class-property-type (type-from-instance object)
                                 name
                                 :assert-readable t)))
    (return-nil () (return-from object-property nil)))
  (with-value (gvalue gtype)
    (%object-get-property object name gvalue)
    (get-gvalue gvalue gtype)))

(export 'object-property)

;;; --- End of file gobject.base.lisp ------------------------------------------
