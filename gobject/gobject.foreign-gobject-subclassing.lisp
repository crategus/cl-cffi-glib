;;; ----------------------------------------------------------------------------
;;; gobject.foreign-gobject-subclassing.lisp
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

;; Structure to store the definition of a subclass
(defstruct subclass-info
  gname
  class
  parent
  interfaces
  properties)

;; Stores the subclasses as SUBCLASS-INFO instances
(let ((subclass-infos (make-hash-table :test 'equal)))

  (defun get-subclass-info (gtype)
    (let ((gname (if (stringp gtype)
                     gtype
                     (glib:gtype-name (glib:gtype gtype)))))
      (gethash gname subclass-infos)))

  (defun (setf get-subclass-info) (value gtype)
    (let ((gname (if (stringp gtype)
                     gtype
                     (glib:gtype-name (glib:gtype gtype)))))
      (setf (gethash gname subclass-infos) value)))

  (defun print-subclass-infos ()
    (maphash (lambda (key value) (format t "~a ~a~%" key value))
             subclass-infos)))

;;; ----------------------------------------------------------------------------

;; Definition of a vtable for subclassing an interface

(defstruct vtable-info
  gtype-name
  cstruct-name
  methods)

(defstruct vtable-method-info
  slot-name
  name
  return-type
  args
  callback-name
  impl-call)

;; Stores the subclasses as SUBCLASS-INFO instances
(let ((vtable-infos (make-hash-table :test 'equal)))

  (defun get-vtable-info (gtype)
    (let ((gname (if (stringp gtype)
                     gtype
                     (glib:gtype-name (glib:gtype gtype)))))
      (gethash gname vtable-infos)))

  (defun (setf get-vtable-info) (value gtype)
    (let ((gname (if (stringp gtype)
                     gtype
                     (glib:gtype-name (glib:gtype gtype)))))
      (setf (gethash gname vtable-infos) value)))

  (defun print-vtable-infos ()
    (maphash (lambda (key value) (format t "~a ~a~%" key value))
             vtable-infos)))

;;; ----------------------------------------------------------------------------

(defmethod make-load-form ((object vtable-method-info) &optional environment)
  (declare (ignore environment))
  `(make-vtable-method-info :slot-name ',(vtable-method-info-slot-name object)
                            :name ',(vtable-method-info-name object)
                            :return-type
                            ',(vtable-method-info-return-type object)
                            :args ',(vtable-method-info-args object)
                            :callback-name
                            ',(vtable-method-info-callback-name object)))

(defun vtable-methods (iface-name items)
  (iter (for item in items)
        (when (eq :skip (first item)) (next-iteration))
        (destructuring-bind (name (return-type &rest args) &key impl-call) item
          (for method-name = (intern (format nil "~A-~A-IMPL"
                                             (symbol-name iface-name)
                                             (symbol-name name))))
          (for callback-name = (intern (format nil "~A-~A-CALLBACK"
                                               (symbol-name iface-name)
                                               (symbol-name name))))
          (collect (make-vtable-method-info :slot-name name
                                            :name method-name
                                            :return-type return-type
                                            :args args
                                            :callback-name callback-name
                                            :impl-call impl-call)))))

;;; ----------------------------------------------------------------------------

(defun interface-init (iface data)
  (destructuring-bind (class-name interface-name)
      (prog1
        (glib:get-stable-pointer-value data)
        (glib:free-stable-pointer data))
    (declare (ignorable class-name))
    (let* ((vtable (get-vtable-info interface-name))
           (vtable-cstruct (when vtable (vtable-info-cstruct-name vtable))))
      (when vtable
        (iter (for method in (vtable-info-methods vtable))
              (for cb = (cffi:get-callback
                            (vtable-method-info-callback-name method)))
              (for slot-name = (vtable-method-info-slot-name method))
              (setf (cffi:foreign-slot-value iface
                                            `(:struct ,vtable-cstruct)
                                             slot-name)
                    cb))))))

(cffi:defcallback c-interface-init :void
    ((iface :pointer) (data :pointer))
  (interface-init iface data))

;;; ----------------------------------------------------------------------------

(defun add-interface (name interface)
  (let* ((interface-info (list name interface))
         (interface-info-ptr (glib:allocate-stable-pointer interface-info)))
    (cffi:with-foreign-object (info '(:struct interface-info))
      (setf (cffi:foreign-slot-value info
                                     '(:struct interface-info) :interface-init)
            (cffi:callback c-interface-init)
            (cffi:foreign-slot-value info
                                     '(:struct interface-info) :interface-data)
            interface-info-ptr)
      (type-add-interface-static (glib:gtype name)
                                 (glib:gtype interface)
                                 info))))

(defun add-interfaces (name)
  (let* ((subclass-info (get-subclass-info name))
         (interfaces (subclass-info-interfaces subclass-info)))
    (iter (for interface in interfaces)
          (add-interface name interface))))

;;; ----------------------------------------------------------------------------

(defun wrap-body-with-boxed-translations (args body)
  (if (null args)
      body
      (let ((arg (first args)))
        (destructuring-bind (arg-name arg-type) arg
          (if (and (listp arg-type) (eq 'boxed (first arg-type)))
              (let ((var (gensym))
                    (cffi-type (cffi::parse-type arg-type)))
               `((let ((,var ,arg-name)
                       (,arg-name (cffi:translate-from-foreign ,arg-name
                                                               ,cffi-type)))
                   (unwind-protect
                     (progn
                       ,@(wrap-body-with-boxed-translations (rest args) body))
                     ;; Method for GBoxed types to free resources
                     (glib:cleanup-translated-object-for-callback ,cffi-type
                                                                  ,arg-name
                                                                  ,var)))))
              (wrap-body-with-boxed-translations (rest args) body))))))

(defmacro glib-defcallback (name-and-options return-type args &body body)
  (let* ((c-args (iter (for arg in args)
                       (for (name type) = arg)
                       (if (and (listp type) (eq 'glib:boxed (first type)))
                           (collect `(,name :pointer))
                           (collect arg))))
         (c-body (wrap-body-with-boxed-translations args body)))
   `(cffi:defcallback ,name-and-options ,return-type ,c-args ,@c-body)))

;;; ----------------------------------------------------------------------------

(defun vtable-item->cstruct-item (item)
  (if (eq :skip (first item))
      (rest item)
      (list (first item) :pointer)))

(defmacro define-vtable ((gtype-name name) &body items)
  (let ((cstruct-name (intern (format nil "~A-VTABLE" (symbol-name name))))
        (methods (vtable-methods name items)))
   `(progn
      (cffi:defcstruct ,cstruct-name
                       ,@(mapcar #'vtable-item->cstruct-item items))
      (setf (get-vtable-info ,gtype-name)
            (make-vtable-info :gtype-name ,gtype-name
                              :cstruct-name ',cstruct-name
                              :methods
                              (list ,@(mapcar #'make-load-form methods))))
    ,@(iter (for method in methods)
            (for args =
                 (if (vtable-method-info-impl-call method)
                     (first (vtable-method-info-impl-call method))
                     (mapcar #'first (vtable-method-info-args method))))
            (collect `(defgeneric ,(vtable-method-info-name method) (,@args)))
            (collect `(glib-defcallback
                       ,(vtable-method-info-callback-name method)
                       ,(vtable-method-info-return-type method)
                        (,@(vtable-method-info-args method))
                        (restart-case
                         ,(if (vtable-method-info-impl-call method)
                              `(progn
                                 ,@(rest (vtable-method-info-impl-call method)))
                              `(,(vtable-method-info-name method)
                                 ,@(mapcar #'first
                                           (vtable-method-info-args method))))
                          (return-from-interface-method-implementation
                            (v)
                            :interactive
                            (lambda () (list (eval (read)))) v))))))))

;;; ----------------------------------------------------------------------------

(defun instance-init (instance cclass)
  (unless (or *current-creating-object*
              *currently-making-object-p*
              (get-gobject-for-pointer instance))
    (let* ((gname (glib:gtype-name (type-from-class cclass)))
           (subclass (subclass-info-class (get-subclass-info gname))))
      (make-instance subclass :pointer instance))))

(cffi:defcallback instance-init-cb :void
    ((instance :pointer) (class :pointer))
  (instance-init instance class))

;;; ----------------------------------------------------------------------------

;; Helper functions for PROPERTY->PARAM-SPEC
(defun minimum-foreign-integer (type &optional (signed t))
  (if signed
      (- (ash 1 (1- (* 8 (cffi:foreign-type-size type)))))
      0))

(defun maximum-foreign-integer (type &optional (signed t))
  (if signed
      (1- (ash 1 (1- (* 8 (cffi:foreign-type-size type)))))
      (1- (ash 1 (* 8 (cffi:foreign-type-size type))))))

;;; ----------------------------------------------------------------------------

;; Create and return a GParamSpec for the property we wish to install
;; TODO: This must be extended whenever a new fundamental GType is introduced
(defun property->param-spec (property)
  (destructuring-bind (property-name
                       property-type
                       accessor
                       property-get-fn
                       property-set-fn)
      property
    (declare (ignore accessor))
    (let ((property-gtype (glib:gtype property-type))
          (flags (append (when property-get-fn (list :readable))
                         (when property-set-fn (list :writable)))))
      (ev-case (type-fundamental property-gtype)
        (nil
         (error "GValue is of invalid type ~A (~A)"
                property-gtype (glib:gtype-name property-gtype)))
        ((glib:gtype "void") nil)
        ((glib:gtype "gchar")
         (param-spec-char property-name
                          property-name
                          property-name
                          (minimum-foreign-integer :char)
                          (maximum-foreign-integer :char)
                          0
                          flags))
        ((glib:gtype "guchar")
         (param-spec-uchar property-name
                           property-name
                           property-name
                           (minimum-foreign-integer :uchar nil)
                           (maximum-foreign-integer :uchar nil)
                           0
                           flags))
        ((glib:gtype "gboolean")
         (param-spec-boolean property-name
                             property-name
                             property-name
                             nil
                             flags))
        ((glib:gtype "gint")
         (param-spec-int property-name
                         property-name
                         property-name
                         (minimum-foreign-integer :int)
                         (maximum-foreign-integer :int)
                         0
                         flags))
        ((glib:gtype "guint")
         (param-spec-uint property-name
                          property-name
                          property-name
                          (minimum-foreign-integer :uint nil)
                          (maximum-foreign-integer :uint nil)
                          0
                          flags))
        ((glib:gtype "glong")
         (param-spec-long property-name
                          property-name
                          property-name
                          (minimum-foreign-integer :long)
                          (maximum-foreign-integer :long)
                          0
                          flags))
        ((glib:gtype "gulong")
         (param-spec-ulong property-name
                           property-name
                           property-name
                           (minimum-foreign-integer :ulong nil)
                           (maximum-foreign-integer :ulong nil)
                           0
                           flags))
        ((glib:gtype "gint64")
         (param-spec-int64 property-name
                           property-name
                           property-name
                           (minimum-foreign-integer :int64)
                           (maximum-foreign-integer :int64)
                           0
                           flags))
        ((glib:gtype "guint64")
         (param-spec-uint64 property-name
                            property-name
                            property-name
                            (minimum-foreign-integer :uint64 nil)
                            (maximum-foreign-integer :uint64 t)
                            0
                            flags))
        ((glib:gtype "GEnum")
         (param-spec-enum property-name
                          property-name
                          property-name
                          property-gtype
                          (enum-item-value
                            (first (get-enum-items property-gtype)))
                          flags))
        ((glib:gtype "GFlags")
         (param-spec-enum property-name
                          property-name
                          property-name
                          property-gtype
                          (flags-item-value
                            (first (get-flags-items property-gtype)))
                          flags))
        ((glib:gtype "gfloat")
         (param-spec-float property-name
                           property-name
                           property-name
                           most-negative-single-float
                           most-positive-single-float
                           0.0
                           flags))
        ((glib:gtype "gdouble")
         (param-spec-double property-name
                            property-name
                            property-name
                            most-negative-double-float
                            most-positive-double-float
                            0.0d0
                            flags))
        ((glib:gtype "gchararray")
         (param-spec-string property-name
                            property-name
                            property-name
                            ""
                            flags))
        ((glib:gtype "gpointer")
         (param-spec-pointer property-name
                             property-name
                             property-name
                             flags))
        ((glib:gtype "GBoxed")
         (param-spec-boxed property-name
                           property-name
                           property-name
                           property-gtype
                           flags))
        ((glib:gtype "GObject")
         (param-spec-object property-name
                            property-name
                            property-name
                            property-gtype
                            flags))
        (t
         (error "Unknown type: ~A (~A)"
                property-gtype (glib:gtype-name property-gtype)))))))

;;; ----------------------------------------------------------------------------

(defun object-property-get (instance property-id g-value pspec)
  (declare (ignore property-id))
  (let* ((object (get-gobject-for-pointer instance))

         (property-name (cffi:foreign-slot-value pspec
                                                 '(:struct param-spec)
                                                 :name))

         (property-type (cffi:foreign-slot-value pspec
                                                 '(:struct param-spec)
                                                 :value-type))

         (gname (glib:gtype-name
                        (cffi:foreign-slot-value pspec
                                                 '(:struct param-spec)
                                                 :owner-type)))

         (subclass-info (get-subclass-info gname))

         (property-info (find property-name
                              (subclass-info-properties subclass-info)
                              :test 'string= :key 'first))

         (property-get-fn (third property-info)))

    (assert (fourth property-info))

    (let ((value (restart-case
                   (funcall property-get-fn object)
                   (return-from-property-getter (value)
                                                :interactive
                                                (lambda ()
                                                  (format t "Enter new value: ")
                                                  (list (eval (read))))
                                                value))))
      (set-gvalue g-value value property-type))))

(cffi:defcallback c-object-property-get :void
    ((instance :pointer)
     (property-id :uint)
     (value :pointer)
     (pspec :pointer))
  (object-property-get instance property-id value pspec))

;;; ----------------------------------------------------------------------------

(defun object-property-set (object property-id value pspec)
  (declare (ignore property-id))
  (let* ((lisp-object (get-gobject-for-pointer object))
         (property-name (cffi:foreign-slot-value pspec
                                                 '(:struct param-spec) :name))
         (type-name (glib:gtype-name
                        (cffi:foreign-slot-value pspec
                                                 '(:struct param-spec)
                                                 :owner-type)))
         (lisp-type-info (get-subclass-info type-name))
         (property-info (find property-name
                              (subclass-info-properties lisp-type-info)
                              :test 'string= :key 'first))
         (property-set-fn (third property-info))
         (new-value (value-get value)))
    (assert (fifth property-info))
    (restart-case
      (funcall (fdefinition (list 'setf property-set-fn)) new-value lisp-object)
      (return-without-error-from-property-setter () nil))))

(cffi:defcallback c-object-property-set :void
    ((object :pointer) (property-id :uint) (value :pointer) (pspec :pointer))
  (object-property-set object property-id value pspec))

;;; ----------------------------------------------------------------------------

#|
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
|#

#+nil
(define-vtable ("GObject" object)
  (:skip type-class :pointer)
  (:skip construct-properties :pointer)
  ;; Virtual functions for GObject class
  (:skip constructor :pointer)
  (:skip set-property :pointer)
  (:skip get-property :pointer)
  (dispose (:void (object object)))
  (finalize (:void (object object)))
  (:skip dispatch-properties-changed :pointer)
  (notify (:void (object object) (pspec :pointer)))
  (constructed (:void (object object)))
  (:skip dummy1 :pointer)
  (:skip dummy2 :pointer)
  (:skip dummy3 :pointer)
  (:skip dummy4 :pointer)
  (:skip dummy5 :pointer)
  (:skip dummy6 :pointer)
  (:skip dummy7 :pointer))

;;; ----------------------------------------------------------------------------

(defun class-init (cclass data)
  (declare (ignore data))
  (let* ((gname (glib:gtype-name (type-from-class cclass)))
         (subclass-info (get-subclass-info gname))
         (subclass (subclass-info-class subclass-info)))
    ;; Set SUBCLASS as the symbol for the GType
    ;; FIXME: This seems to be to late. Do this more early.
    (setf (glib:symbol-for-gtype gname) subclass)

    ;; Initialize the getter and setter methods for the object class
    ;; Consider to generalize this to allow overriding the virtual functions
    ;; of C classes.
    (setf (object-class-get-property cclass)
          (cffi:callback c-object-property-get)
          (object-class-set-property cclass)
          (cffi:callback c-object-property-set))

    ;; Install the properties for the object class
    (iter (for property in (subclass-info-properties subclass-info))
          (for pspec = (property->param-spec property))
          (for property-id from 123) ; FIXME: ???
          (%object-class-install-property cclass property-id pspec))))

(cffi:defcallback class-init-cb :void
    ((class :pointer) (data :pointer))
  (class-init class data))

;;; ----------------------------------------------------------------------------

(defmacro register-object-type-implementation (name
                                               class
                                               parent
                                               interfaces
                                               properties)
  (let ((*warn-unknown-gtype* nil))
    (unless (stringp parent)
      (setf parent (glib:gtype-name (glib:gtype parent)))))

  `(progn
     (setf (get-subclass-info ,name)
           (make-subclass-info :gname ,name
                               :class ',class
                               :parent ,parent
                               :interfaces ',interfaces
                               :properties ',properties))
     (glib-init:at-init (',class)
       (log-for :subclass
                "Registering GObject type implementation ~A for type ~A~%"
                ',class ,name)
       (cffi:with-foreign-object (query '(:struct type-query))
         (type-query (glib:gtype ,parent) query)
         (type-register-static-simple
             (glib:gtype ,parent)
             ,name
             (cffi:foreign-slot-value query '(:struct type-query) :class-size)
             (cffi:callback class-init-cb)
             (cffi:foreign-slot-value query '(:struct type-query) :instance-size)
             (cffi:callback instance-init-cb) nil))
       (add-interfaces ,name))
     (defmethod initialize-instance :before ((object ,class) &key pointer)
       (log-for :subclass
                ":subclass INITIAlIZE-INSTANCE :before ~A :pointer ~A~%"
                object pointer)
       (unless (or pointer
                   (and (slot-boundp object 'pointer)
                        (object-pointer object)))
         (log-for :subclass "calling g-object-constructor~%")
         (setf (object-pointer object)
               (call-gobject-constructor ,name nil nil)
               (object-has-reference object) t)))
     ,name))

;;; ----------------------------------------------------------------------------

(defun install-vtable (gname)

    (let* ((class (type-class-ref gname))
           (vtable (get-vtable-info gname))
           (vtable-cstruct (when vtable (vtable-info-cstruct-name vtable))))

      (format t "   gname : ~a~%" gname)
      (format t "   class : ~a~%" class)
      (format t "  vtable : ~a~%" vtable)
      (format t " cstruct : ~a~%" vtable-cstruct)

      (when vtable
        (iter (for method in (vtable-info-methods vtable))
              (for cb = (cffi:get-callback
                            (vtable-method-info-callback-name method)))
              (for slot-name = (vtable-method-info-slot-name method))

              (format t "  method : ~a~%" method)
              (format t "      cb : ~a~%" cb)
              (format t "    slot : ~a~%" slot-name)

              (setf (cffi:foreign-slot-value class
                                            `(:struct ,vtable-cstruct)
                                             slot-name)
                    cb)))))

;;; ----------------------------------------------------------------------------

;; Filter the properties that we will register
(defun filter-properties-to-register (properties)
  (iter (for property in properties)
        (when (or (eq :cl (first property))
                  (eq :cffi (first property)))
          (next-iteration))
        (collect (list (third property)
                       (fourth property)
                       (second property)
                       (fifth property)
                       (sixth property)))))

(defun subclass-property->slot (class property &optional (initform-p nil))
  (declare (ignorable class))
  (cond ((gobject-property-p property)
         `(,(property-name property)
           :accessor ,(property-accessor property)
           :initarg ,(intern (string-upcase (property-name property))
                             (find-package :keyword))
           ,@(when initform-p
               (let* (;; Get GParamSpec for the GType of the property
                      (pspec (property->param-spec
                                 (list (gobject-property-gname property)
                                       (gobject-property-gtype property)
                                       (gobject-property-accessor property)
                                       nil nil)))
                      ;; Get default value from GParamSpec for initialization
                      (default (value-get (param-spec-default-value pspec))))
                 `(:initform ,default)))))
        ;; Does this work? We do not use the code at this time, but check this
        ;; more carefully
        ((cffi-property-p property)
         `(,(property-name property)
           :accessor ,(property-accessor property)
           ,@(when (not (null (cffi-property-writer property)))
               `(:initarg
                 ,(intern (string-upcase (property-name property))
                          (find-package :keyword))))
           :reader ,(cffi-property-reader property)
           :writer ,(cffi-property-writer property)))
        ((cl-property-p property)
         `(,(property-name property)
           ,@(cl-property-args property)))))

(defmacro define-gobject-subclass (g-type-name name
                                    (&key (superclass 'object)
                                          (export t)
                                          interfaces)
                                    (&rest properties))

  (let ((props (mapcar #'parse-property properties))
        (parent (cond ((stringp superclass)
                        superclass)
                      ((eq 'object superclass)
                       "GObject")
                      (t
                       (gobject-class-gname (find-class superclass))))))

    (setf properties (filter-properties-to-register properties))

    `(progn
       ;; Store the definition as subclass-info
       (setf (get-subclass-info ,g-type-name)
             (make-subclass-info :gname ,g-type-name
                                 :class ',name
                                 :parent ,parent
                                 :interfaces ',interfaces
                                 :properties ',properties))
       ;; Declare the Lisp class
       (defclass ,name (,@(when (and superclass
                                     (not (eq superclass 'object)))
                            (list superclass))
                        ,@(mapcar #'interface->lisp-class-name interfaces))
         ;; Generate the slot definitions from the given properties
         (,@(mapcar (lambda (property)
                       (subclass-property->slot name property t))
                     props))
         (:gname . ,g-type-name)
         (:metaclass gobject-class))

       ;; Register the class
       (glib-init:at-init (',name)
         (log-for :subclass
                  "Debug subclass: Registering GObject type ~A for type ~A~%"
                  ',name ,g-type-name)
         (cffi:with-foreign-object (query '(:struct type-query))
           (type-query (glib:gtype ,parent) query)
           (type-register-static-simple
               (glib:gtype ,parent)
               ,g-type-name
               (cffi:foreign-slot-value query '(:struct type-query):class-size)
               (cffi:callback class-init-cb)
               (cffi:foreign-slot-value query '(:struct type-query) :instance-size)
               (cffi:callback instance-init-cb) nil))
         (add-interfaces ,g-type-name))

       ;; Initializer for the instance of the subclass
       (defmethod initialize-instance :before ((object ,name) &key pointer)
         (log-for :subclass
                  ":subclass INITIAlIZE-INSTANCE :before ~A :pointer ~A~%"
                  object pointer)
         (unless (or pointer
                     (and (slot-boundp object 'pointer)
                          (object-pointer object)))
           (log-for :subclass ":subclass calling g-object-constructor~%")
           (setf (object-pointer object)
                 (call-gobject-constructor ,g-type-name nil nil)
                 (object-has-reference object) t)))

       ;; Export the accessible symbols
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
                          props)))
)))

(export 'define-gobject-subclass)

;;; --- End of file gobject.foreign-gobject-subclassing.lisp -------------------
