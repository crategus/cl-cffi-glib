;;; ----------------------------------------------------------------------------
;;; gobject.base.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;     G_TYPE_IS_OBJECT
;;;     G_IS_OBJECT
;;;
;;;     g_object_class_install_property                     not exported
;;;     g_object_class_install_properties                   not exported
;;;     g_object_class_find_property
;;;     g_object_class_list_properties
;;;     g_object_class_override_property                    not exported
;;;
;;;     g_object_interface_install_property                 not exported
;;;     g_object_interface_find_property
;;;     g_object_interface_list_properties
;;;
;;;     g_object_new
;;;     g_object_new_with_properties                        Since 2.54
;;;     g_object_newv                                       Deprecated 2.54
;;;     g_object_ref                                        not exported
;;;     g_object_unref                                      not exported
;;;     g_object_ref_sink                                   not exported
;;;     g_set_object                                        not implemented
;;;     g_clear_object                                      not implemented
;;;     g_object_is_floating                                not exported
;;;     g_object_force_floating                             not exported
;;;
;;;     GWeakNotify
;;;     g_object_weak_ref
;;;     g_object_weak_unref                                 not exported
;;;     g_object_add_weak_pointer                           not exported
;;;     g_object_remove_weak_pointer
;;;     g_set_weak_pointer
;;;     g_clear_weak_pointer
;;;
;;;     GToggleNotify
;;;     g_object_add_toggle_ref                             not exported
;;;     g_object_remove_toggle_ref                          not exported
;;;     g_object_connect
;;;     g_object_disconnect
;;;
;;;     g_object_set
;;;     g_object_setv
;;;     g_object_get
;;;     g_object_getv
;;;
;;;     g_object_notify
;;;     g_object_notify_by_pspec
;;;     g_object_freeze_notify
;;;     g_object_thaw_notify
;;;
;;;     g_object_get_data
;;;     g_object_set_data
;;;     g_object_set_data_full
;;;     g_object_steal_data
;;;     g_object_dup_data
;;;     g_object_replace_data
;;;     g_object_get_qdata
;;;     g_object_set_qdata
;;;     g_object_set_qdata_full
;;;     g_object_steal_qdata
;;;     g_object_dup_qdata
;;;     g_object_replace-qdata
;;;     g_object_set_property
;;;     g_object_get_property
;;;     g_object_new_valist
;;;     g_object_set_valist
;;;     g_object_get_valist
;;;     g_object_watch_closure
;;;     g_object_run_dispose
;;;
;;;     g_weak_ref_init
;;;     g_weak_ref_clear
;;;     g_weak_ref_get
;;;     g_weak_ref_set
;;;     g_assert_finalize_object
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

(let ((gobjects-weak-table (tg:make-weak-hash-table :test 'equal
                                                    :weakness :value))
      (gobjects-strong-table (make-hash-table :test 'equal)))

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
;;; GObject                                                 not exported
;;; ----------------------------------------------------------------------------

;; %object is not needed in the implementation. It is defined to access the
;; slot REF-COUNT with the G:OBJECT-REF-COUNT function for debugging the code.

(cffi:defcstruct %object
  (:type-instance (:pointer (:struct type-instance)))
  (:ref-count :uint)
  (:data :pointer))

;;; ----------------------------------------------------------------------------

;; Define the base class object

(defclass object ()
  ((pointer
    :type (or null cffi:foreign-pointer)
    :initarg :pointer
    :accessor object-pointer
    :initform nil)
   (has-reference
    :type boolean
    :accessor object-has-reference
    :initform nil)
   ;; TODO: Remove this slot, it is no longer needed. The handlers are stored
   ;; in a hash table.
   (signal-handlers
    :type (array t *)
    :initform (make-array 0 :adjustable t :fill-pointer t)
    :reader object-signal-handlers)))

(export 'object)
(export 'object-pointer)
(export 'object-has-reference)
(export 'object-signal-handlers)

;; Add object to the global Hash table for registered types
(glib-init:at-init ()
  (setf (glib:symbol-for-gtype "GObject") 'object))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation 'object 'type)
 "@version{2023-12-1}
  @begin{short}
    The @class{g:object} class is the fundamental type providing the common
    attributes and methods for all object types in GTK, Pango and other
    libraries.
  @end{short}
  The @class{g:object} class provides methods for object construction and
  destruction, property access methods, and signal support.
  @begin[Lisp Implementation]{dictionary}
    In the Lisp implementation three slots are added. The
    @slot[g:object]{pointer} slot holds the foreign pointer to the C instance
    of the object. The @slot[g:object]{signal-handlers} slot stores the Lisp
    functions which are connected to an instance with the @fun{g:signal-connect}
    function. The @slot[g:object]{has-reference} slot is initialized to the
    value @em{true} during creation of an object. See the slot access functions
    for examples.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"notify\" signal}
    @begin{pre}
lambda (object pspec)    :no-hooks
    @end{pre}
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
    @begin[code]{table}
      @entry[object]{The @class{g:object} instance which received the signal.}
      @entry[pspec]{The @symbol{g:param-spec} instance of the property which
        changed.}
    @end{table}
  @end{dictionary}
  @see-constructor{g:object-new}
  @see-slot{g:object-has-reference}
  @see-slot{g:object-pointer}
  @see-slot{g:object-signal-handlers}
  @see-symbol{g:param-spec}
  @see-function{g:signal-connect}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor details
;;; ----------------------------------------------------------------------------

;;; --- g:object-has-reference -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-reference" 'object) t)
 "Holds the value @em{true} when the instance is successfully registered.")

#+liber-documentation
(setf (liber:alias-for-function 'object-has-reference)
      "Accessor"
      (documentation 'object-has-reference 'function)
 "@version{2023-12-1}
  @syntax{(g:object-has-reference object) => has-reference}
  @argument[object]{a @class{g:object} instance}
  @argument[has-reference]{@em{true} when registering @arg{object}}
  @begin{short}
    Accessor of the @code{has-reference} slot of the @class{g:object} class.
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
 "@version{2023-12-1}
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

(defmethod (setf glib:pointer) (value (instance object))
  (setf (object-pointer instance) value))

;;; --- g:object-signal-handlers -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "signal-handlers" 'object) t)
 "An array of Lisp signals handlers which are connected to the instance.")

#+liber-documentation
(setf (liber:alias-for-function 'object-signal-handlers)
      "Accessor"
      (documentation 'object-signal-handlers 'function)
 "@version{2023-12-1}
  @argument[object]{a @class{g:object} instance}
  @argument[handlers]{an array with the signal handlers connected to
    @arg{object}}
  @begin{short}
    Returns the array of Lisp signal handlers which are connected with the
    @fun{g:signal-connect} function to a @class{g:object} instance.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(setq button (make-instance 'gtk:button))
=> #<GTK-BUTTON {E319359@}>
(g:signal-connect button \"clicked\" (lambda () ))
=> 27
(g:object-signal-handlers button)
=> #(#<FUNCTION (LAMBDA #) {E324855@}>)
(g:signal-connect button \"destroy\" (lambda () ))
=> 28
(g:object-signal-handlers button)
=> #(#<FUNCTION (LAMBDA #) {E324855@}> #<FUNCTION (LAMBDA #) {E336EDD@}>)
    @end{pre}
  @end{dictionary}
  @see-class{g:object}
  @see-function{g:signal-connect}")

;;; ----------------------------------------------------------------------------
;;; GInitiallyUnowned
;;; ----------------------------------------------------------------------------

;; This class is unexported for the cl-cffi-gtk documentation.

(defclass initially-unowned (object)
  ()
  (:metaclass gobject-class)
  (:gname . "GInitiallyUnowned")
  (:initializer . "g_initially_unowned_get_type")
  (:documentation "Base class that has initial 'floating' reference."))

(export 'initially-unowned)

;;; ----------------------------------------------------------------------------

;; GC for weak pointers

(defvar *gobject-gc-hooks-lock*
        (bt:make-recursive-lock "gobject-gc-hooks-lock"))
(defvar *gobject-gc-hooks* nil) ; pointers to objects to be freed

;;; ----------------------------------------------------------------------------

(defun dispose-carefully (pointer)
  (handler-case
    (register-gobject-for-gc pointer)
    (error (e)
      (format t "Error in DISPOSE-CAREFULLY: ~A~%" e))))

(defmethod release ((obj object))
  (tg:cancel-finalization obj)
  (let ((ptr (object-pointer obj)))
    (setf (object-pointer obj) nil)
    (dispose-carefully ptr)))

(defun activate-gc-hooks ()
  (bt:with-recursive-lock-held (*gobject-gc-hooks-lock*)
    (when *gobject-gc-hooks*
      (iter (for pointer in *gobject-gc-hooks*)
            (%object-remove-toggle-ref pointer
                                       (cffi:callback toggle-notify)
                                       (cffi:null-pointer)))
      (setf *gobject-gc-hooks* nil)))
  nil)

(defun register-gobject-for-gc (pointer)
  (bt:with-recursive-lock-held (*gobject-gc-hooks-lock*)
    (let ((locks-were-present (not (null *gobject-gc-hooks*))))
      (push pointer *gobject-gc-hooks*)
      (unless locks-were-present
        (glib:idle-add #'activate-gc-hooks)))))

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
    (%object-unref pointer)))

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
                     (error (e)
                       (format t "Error in finalizer for ~A: ~A~%" s e))))))
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
        (error "Type ~A is not registered with REGISTER-OBJECT-TYPE"
               (glib:gtype-name gtype)))
      (let ((*current-object-from-pointer* pointer))
        (make-instance lisp-type :pointer pointer)))))

;;; ----------------------------------------------------------------------------

;; Define the type foreign-g-object-type and the type transformation rules.

(cffi:define-foreign-type foreign-g-object-type ()
  ((sub-type :reader sub-type
             :initarg :sub-type
             :initform 'object)
   (already-referenced :reader foreign-g-object-type-already-referenced
                       :initarg :already-referenced
                       :initform nil))
  (:actual-type :pointer))

(cffi:define-parse-method object (&rest args)
  (let* ((sub-type (first (remove-if #'keywordp args)))
         (flags (remove-if-not #'keywordp args))
         (already-referenced (not (null (find :already-referenced flags)))))
    (make-instance 'foreign-g-object-type
                   :sub-type sub-type
                   :already-referenced already-referenced)))

(defmethod cffi:translate-to-foreign (object (type foreign-g-object-type))
  (let ((pointer nil))
    (cond ((null object)
           (cffi:null-pointer))
          ((cffi:pointerp object)
           object)
          ((null (setf pointer (object-pointer object)))
           (error "Object ~A has been disposed" object))
          ((typep object 'object)
           (when (sub-type type)
             (assert (typep object (sub-type type))
                      nil
                      "Object ~A is not a subtype of ~A" object (sub-type type)))
           pointer)
          (t (error "Object ~A is not translatable as GObject*" object)))))

(defmethod cffi:translate-from-foreign (pointer (type foreign-g-object-type))
  (let ((object (get-or-create-gobject-for-pointer pointer)))
    (when (and object
               (foreign-g-object-type-already-referenced type))
      (%object-unref (object-pointer object)))
    object))

;;; ----------------------------------------------------------------------------

;; Get the definition of a property for the GObject type. Both arguments are of
;; type string, e.g. (class-property-info "GtkLabel" "label")

;; TODO: Duplicates the implementation of OBJECT-CLASS-FIND-PROPERTY. But
;; we return a %PARAM-SPEC instance which is the Lisp side of a GParamSpec
;; instance. Improve the implementation of GParamSpec!?

(defun class-property-pspec (gtype name)
  (let ((class (type-class-ref gtype)))
    (when class
      (unwind-protect
        (let ((pspec (%object-class-find-property class name)))
          (unless (cffi:null-pointer-p pspec)
            (parse-g-param-spec pspec)))
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
    (error "Trying to create instance of GInterface '~A' (class '~A')"
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
            (set-g-value gvalue
                         value
                         type1
                         :zero-gvalue t))
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
;;; G_TYPE_IS_OBJECT
;;; ----------------------------------------------------------------------------

(defun type-is-object (gtype)
 #+liber-documentation
 "@version{2023-12-1}
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
 "@version{2023-12-1}
  @argument[object]{a valid @symbol{g:type-instance} instance to check}
  @begin{short}
    Checks whether the @arg{object} argument is of @code{\"GObject\"} type
    or derived from it.
  @end{short}
  @begin[Example]{dictionary}
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

;; For internal use and not exported

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
 "@version{2023-12-1}
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
  @begin[Example]{dictionary}
    Get the @symbol{g:param-spec} instance for the @slot[g:simple-action]{name}
    property of the @class{g:simple-action} object is looked up.
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
 "@version{2023-12-1}
  @argument[gtype]{a @class{g:type-t} type ID of an object class}
  @return{The list of @symbol{g:param-spec} instances.}
  @begin{short}
    Gets a list of @symbol{g:param-spec} instances for all properties of an
    object class type.
  @end{short}
  Signals an error if the @arg{gtype} type ID is not a @code{\"GObject\"} type.
  @begin[Example]{dictionary}
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
;;; g_object_class_override_property                        not exported
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_object_class_override_property"
                %object-class-override-property) :void
 #+liber-documentation
 "@version{#2020-2-17}
  @argument[class]{a @symbol{object-class} structure}
  @argument[property-id]{the new property ID}
  @argument[name]{the name of a property registered in a parent class or in an
    interface of this class}
  @begin{short}
    Registers @arg{property-id} as referring to a property with the name
    @arg{name} in a parent class or in an interface implemented by @arg{class}.
  @end{short}
  This allows this class to override a property implementation in a parent class
  or to provide the implementation of a property from an interface.
  @begin[Note]{dictionary}
    Internally, overriding is implemented by creating a property of type
    @code{GParamSpecOverride}; generally operations that query the properties of
    the object class, such as the functions @fun{object-class-find-property}
    or @fun{object-class-list-properties} will return the overridden property.
    However, in one case, the @code{construct_properties} argument of the
    constructor virtual function, the @code{GParamSpecOverride} is passed
    instead, so that the @code{param_id} field of the @symbol{param-spec}
    instance will be correct. For virtually all uses, this makes no difference.
    If you need to get the overridden property, you can call the
    @fun{param-spec-get-redirect-target} function.
  @end{dictionary}
  @see-class{g:object}
  @see-symbol{g:object-class}
  @see-function{g:object-class-find-property}
  @see-function{g:object-class-list-properties}
  @see-function{g:param-spec-get-redirect-target}"
  (class (:pointer (:struct object-class)))
  (property-id :uint)
  (name :string))

;;; ----------------------------------------------------------------------------
;;; g_object_interface_install_property                     not exported
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_object_interface_install_property"
               %object-interface-install-property) :void
 #+liber-documentation
 "@version{#2020-2-17}
  @argument[iface]{any interface vtable for the interface, or the default
    vtable for the interface}
  @argument[pspec]{a @symbol{param-spec} instance for the new property}
  @begin{short}
    Add a property to an interface; this is only useful for interfaces that are
    added to GObject-derived types.
  @end{short}
  Adding a property to an interface forces all objects classes with that
  interface to have a compatible property. The compatible property could be a
  newly created @symbol{param-spec} instance, but normally the
  @fun{object-class-override-property} function will be used so that the
  object class only needs to provide an implementation and inherits the property
  description, default value, bounds, and so forth from the interface
  property.

  This function is meant to be called from the interface's default vtable
  initialization function (the @code{class_init} member of
  @symbol{type-info}.) It must not be called after after @code{class_init} has
  been called for any object types implementing this interface.
  @see-class{g:object}
  @see-symbol{g:param-spec}
  @see-symbol{g:type-info}
  @see-function{g:object-class-override-property}"
  (iface :pointer)
  (pspec (:pointer (:struct param-spec))))

;;; ----------------------------------------------------------------------------
;;; g_object_interface_find_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_interface_find_property"
               %object-interface-find-property) (:pointer (:struct param-spec))
  (iface (:pointer (:struct type-interface)))
  (name :string))

(defun object-interface-find-property (gtype name)
 #+liber-documentation
 "@version{2023-12-1}
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
 "@version{2023-12-1}
  @argument[gtype]{a @class{g:type-t} type ID of an interface type}
  @return{The list of @symbol{g:param-spec} instances for all properties of an
    interface type.}
  @begin{short}
    Lists the properties of an interface type.
  @end{short}
  Signals an error if the @arg{gtype} type ID is not an @code{\"GInterface\"}
  type.
  @begin[Example]{dictionary}
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
 "@version{2023-12-1}
  @argument[gtype]{a @class{g:type-t} type ID of the @class{g:object} subtype
    to instantiate}
  @argument[args]{pairs of the property keyword and value}
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
;;; g_object_new_with_properties ()
;;;
;;; GObject *
;;; g_object_new_with_properties (GType object_type,
;;;                               guint n_properties,
;;;                               const char *names[],
;;;                               const GValue values[]);
;;;
;;; Creates a new instance of a GObject subtype and sets its properties using
;;; the provided arrays. Both arrays must have exactly n_properties elements,
;;; and the names and values correspond by index.
;;;
;;; Construction parameters (see G_PARAM_CONSTRUCT, G_PARAM_CONSTRUCT_ONLY)
;;; which are not explicitly specified are set to their default values.
;;;
;;; object_type :
;;;     the object type to instantiate
;;;
;;; n_properties :
;;;     the number of properties
;;;
;;; names :
;;;     the names of each property to be set.
;;;
;;; values :
;;;     the values of each property to be set.
;;;
;;; Returns :
;;;     a new instance of object_type .
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_new_with_properties" %object-new-with-properties)
    :pointer
  (gtype type-t)
  (n-properties :uint)
  (names :pointer)
  (values :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_newv                                           not exported
;;; ----------------------------------------------------------------------------

;; This function is called internally in the Lisp library to create an object
;; and is not exported.

;; TODO: Replace this function with g_object_new_with_properties

(cffi:defcfun ("g_object_newv" %object-newv) :pointer
  (gtype type-t)
  (n-parameter :uint)
  (parameters :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_ref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_ref" %object-ref) :pointer
  (object :pointer))

(defun object-ref (object)
 #+liber-documentation
 "@version{2024-10-12}
  @argument[object]{a @class{g:object} instance}
  @return{The same @arg{object} instance.}
  @short{Increases the reference count of the object.}
  @see-class{g:object}
  @see-function{g:object-ref-count}
  @see-function{g:object-unref}"
  (cffi:convert-from-foreign (%object-ref (object-pointer object)) 'object))

(export 'object-ref)

;;; ----------------------------------------------------------------------------
;;; g:object-ref-count
;;; ----------------------------------------------------------------------------

(defun object-ref-count (object)
 #+liber-documentation
 "@version{2024-12-12}
  @argument[object]{a @class{g:object} instance}
  @return{The integer with the reference count of @arg{object}.}
  @short{Returns the reference count of the object.}
  @see-class{g:object}
  @see-function{g:object-ref}"
  (cffi:foreign-slot-value (if (cffi:pointerp object)
                               object
                               (object-pointer object))
                           '(:struct %object) :ref-count))

(export 'object-ref-count)

;;; ----------------------------------------------------------------------------
;;; g_object_unref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_unref" %object-unref) :void
  (object :pointer))

(defun object-unref (object)
 #+liber-documentation
 "@version{2024-10-12}
  @argument[object]{a @class{g:object} instance}
  @begin{short}
    Decreases the reference count of @arg{object}.
  @end{short}
  When its reference count drops to 0, the object is finalized.
  @see-class{g:object}
  @see-function{g:object-ref}"
  (%object-unref (object-pointer object)))

(export 'object-unref)

;;; ----------------------------------------------------------------------------
;;; g_object_ref_sink                                       not exported
;;; ----------------------------------------------------------------------------

;; For internal use

(cffi:defcfun ("g_object_ref_sink" %object-ref-sink) :pointer
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_set_object
;;;
;;; Updates a GObject pointer to refer to new_object
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_clear_object
;;;
;;; Clears a reference to a GObject.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_is_floating                                    not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(cffi:defcfun ("g_object_is_floating" %object-is-floating) :boolean
 #+liber-documentation
 "@version{#2020-2-17}
  @argument[object]{a @class{g:object} instance}
  @return{@em{True} if @arg{object} has a floating reference.}
  @begin{short}
    Checks whether @arg{object} has a floating reference.
  @end{short}
  @see-class{g:object}"
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_force_floating                                 not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(cffi:defcfun ("g_object_force_floating" %object-force-floating) :void
 #+liber-documentation
 "@version{#2020-2-17}
  @argument[object]{a @class{g:object} instance}
  @begin{short}
    This function is intended for @class{g:object} implementations to re-enforce
    a floating object reference.
  @end{short}
  Doing this is seldom required: all @class{g:initially-unowned}'s are created
  with a floating reference which usually just needs to be sunken by calling
  the function @fun{g:object-ref-sink}.
  @see-class{g:object}
  @see-class{g:initially-unowned}
  @see-function{g:object-ref-sink}"
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; GWeakNotify
;;;
;;; A GWeakNotify function can be added to an object as a callback that gets
;;; triggered when the object is finalized.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_weak_ref                                       not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

#+nil
(cffi:defcfun ("g_object_weak_ref" %object-weak-ref) :void
 #+liber-documentation
 "@version{#2020-2-17}
  @argument[object]{@class{g:object} instance to reference weakly}
  @argument[notify]{callback to invoke before the @arg{object} is freed}
  @argument[data]{extra data to pass to @arg{notify}}
  @begin{short}
    Adds a weak reference callback to an object.
  @end{short}
  Weak references are used for notification when an object is finalized. They
  are called \"weak references\" because they allow you to safely hold a pointer
  to an object without calling @fun{object-ref} (@fun{object-ref} adds a
  strong reference, that is, forces the object to stay alive).

  Note that the weak references created by this method are not thread-safe:
  they cannot safely be used in one thread if the object's last
  @fun{object-unref} might happen in another thread. Use @class{weak-ref} if
  thread-safety is required.
  @see-class{g:object}
  @see-function{object-ref}
  @see-function{object-unref}"
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_weak_unref                                     not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

#+nil
(cffi:defcfun ("g_object_weak_unref" %object-weak-unref) :void
 #+liber-documentation
 "@version{#2020-2-17}
  @argument[object]{@class{object} instance to remove a weak reference from}
  @argument[notify]{callback to search for}
  @argument[data]{data to search for}
  @begin{short}
    Removes a weak reference callback to an object.
  @end{short}
  @see-class{object}"
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_add_weak_pointer
;;;
;;; Adds a weak reference from weak_pointer to object to indicate that the
;;; pointer located at weak_pointer_location is only valid during the lifetime
;;; of object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_remove_weak_pointer
;;;
;;; Removes a weak reference from object that was previously added using
;;; g_object_add_weak_pointer().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_set_weak_pointer
;;;
;;; Updates a pointer to weakly refer to new_object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_clear_weak_pointer
;;;
;;; Clears a weak reference to a GObject.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GToggleNotify                                           not exported
;;;
;;; A callback function used for notification when the state of a toggle
;;; reference changes.
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
            (warn "TOGGLE-NOTIFY: ~a at ~a has no lisp-side (strong) reference"
                  (type-name (type-from-instance object))
                  object)))
      (let ((obj (get-gobject-for-pointer-weak object)))
        (unless obj
          (warn "TOGGLE-NOTIFY: ~a at ~a has no lisp-side (weak) reference"
                (type-name (type-from-instance object))
                object))
        (rem-gobject-for-pointer-weak object)
        (setf (get-gobject-for-pointer-strong object) obj))))

;;; ----------------------------------------------------------------------------
;;; g_object_add_toggle_ref                                 not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(cffi:defcfun ("g_object_add_toggle_ref" %object-add-toggle-ref) :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_remove_toggle_ref                              not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(cffi:defcfun ("g_object_remove_toggle_ref" %object-remove-toggle-ref) :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_connect
;;;
;;; A convenience function to connect multiple signals at once.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_disconnect
;;;
;;; A convenience function to disconnect multiple signals at once.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set
;;;
;;; Sets properties on an object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_setv
;;;
;;; Sets n_properties properties for an object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_get
;;;
;;; Gets properties of an object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_getv
;;;
;;; Gets n_properties properties for an object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_notify
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_notify" object-notify) :void
 #+liber-documentation
 "@version{2023-12-1}
  @argument[object]{a @class{g:object} instance}
  @argument[name]{a string with the name of a property installed on the class
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
 "@version{2023-12-1}
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
 "@version{2023-12-1}
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
 "@version{2024-5-13}
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
 "@version{2024-5-13}
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

(cffi:defcfun ("g_object_set_data_full" %object-set-data-full) :void
  (object object)
  (key :string)
  (data :pointer)
  (destroy :pointer))

(defun object-set-data-full (object key func)
 #+liber-documentation
 "@version{2024-5-13}
  @argument[object]{a @class{g:object} instance containing the associations}
  @argument[key]{a string with the name of the key}
  @argument[func]{a @symbol{g:destroy-notify} callback function}
  @begin{short}
    Like the @fun{g:object-data} function except it adds notification for
    when the association is destroyed, either by setting it to a different
    value or when the object is destroyed.
  @end{short}
  Note that the @arg{func} callback function is not called if the @arg{data}
  argument is @code{nil}.
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
;;; g_object_steal_data                                     not exported
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_object_steal_data" object-steal-data) :pointer
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[object]{a @class{g:object} instance containing the associations}
  @argument[key]{a string with the name of the key}
  @return{The data as a pointer if found, or @code{nil} if no such data exists.}
  @begin{short}
    Remove a specified datum from the data associations of the object, without
    invoking the destroy handler of the association.
  @end{short}
  @see-class{g:object}
  @see-function{g:object-data}
  @see-function{g:object-set-data-full}"
  (object object)
  (key :string))

;;;-----------------------------------------------------------------------------
;;; g_object_dup_data
;;;
;;; This is a variant of g_object_get_data() which returns a 'duplicate' of the
;;; value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_replace_data
;;;
;;; Compares the user data for the key key on object with oldval, and if they
;;; are the same, replaces oldval with newval.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_get_qdata
;;;
;;; This function gets back user data pointers stored via g_object_set_qdata().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set_qdata ()
;;;
;;; This sets an opaque, named pointer on an object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set_qdata_full
;;;
;;; This function works like g_object_set_qdata(), but in addition, a
;;; void (*destroy) (gpointer) function may be specified which is called with
;;; data as argument when the object is finalized, or the data is being
;;; overwritten by a call to g_object_set_qdata() with the same quark.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_steal_qdata
;;;
;;; This function gets back user data pointers stored via g_object_set_qdata()
;;; and removes the data from object without invoking its destroy() function (if
;;; any was set).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_dup_qdata
;;;
;;; This is a variant of g_object_get_qdata() which returns a 'duplicate' of the
;;; value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_replace_qdata
;;;
;;; Compares the user data for the key quark on object with oldval, and if they
;;; are the same, replaces oldval with newval.
;;; ----------------------------------------------------------------------------

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
  (cffi:with-foreign-object (new-value '(:struct value))
    (unwind-protect
      (progn
        (set-g-value new-value value gtype :zero-gvalue t)
        (%object-set-property object name new-value))
      (value-unset new-value)))
  value)

(cffi:defcfun ("g_object_get_property" %object-get-property) :void
  (object object)
  (name :string)
  (value (:pointer (:struct value))))

(defun object-property (object name &optional gtype)
 #+liber-documentation
 "@version{2023-7-27}
  @syntax{(g:object-property object name gtype) => value}
  @syntax{(setf (g:object-property object name gtype) value)}
  @argument[object]{a @class{g:object} instance}
  @argument[name]{a string with the name of the property}
  @argument[gtype]{an optional @class{g:type-t} type ID of the property}
  @argument[value]{a value for the property}
  @short{Accessor of the property of an object.}
  @begin[Example]{dictionary}
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
  (cffi:with-foreign-object (value '(:struct value))
    (unwind-protect
      (progn
        (value-init value gtype)
        (%object-get-property object name value)
        (parse-g-value value))
      (value-unset value))))

(export 'object-property)

;;; ----------------------------------------------------------------------------
;;; g_object_new_valist
;;;
;;; Creates a new instance of a GObject subtype and sets its properties.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set_valist
;;;
;;; Sets properties on an object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_get_valist
;;;
;;; Gets properties of an object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_watch_closure ()
;;;
;;; This function essentially limits the life time of the closure to the life
;;; time of the object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_run_dispose
;;;
;;; Releases all references to other objects.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GWeakRef
;;;
;;; A structure containing a weak reference to a GObject.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_weak_ref_init
;;;
;;; Initialise a non-statically-allocated GWeakRef.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_weak_ref_clear
;;;
;;; Frees resources associated with a non-statically-allocated GWeakRef. After
;;; this call, the GWeakRef is left in an undefined state.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_weak_ref_get
;;;
;;; If weak_ref is not empty, atomically acquire a strong reference to the
;;; object it points to, and return that reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_weak_ref_set
;;;
;;; Change the object to which weak_ref points, or set it to NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_assert_finalize_object
;;;
;;; Assert that object is non-NULL, then release one reference to it with
;;; g_object_unref() and assert that it has been finalized (i.e. that there are
;;; no more references).
;;;
;;; Since 2.62
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.base.lisp ------------------------------------------
