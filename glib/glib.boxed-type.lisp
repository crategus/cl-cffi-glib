;;; ----------------------------------------------------------------------------
;;; glib.boxed-type.lisp
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

;; TODO: More work needed to rework the implementation of GBoxed.

;; TYPE-INITIALIZER is a string specifying a C function to initialize the
;; GType, or a function designator specifying a Lisp function.
;; TODO: This function is called by the macros that initialize a GBoxed type
;; or a GEnum and GFlags type. There is a second implementation
;; INITIAlIZE-GOBJECT-CLASS-G-TYPE that could use this function to avoid the
;; duplication of code and functionality.
(defun type-initializer-call (type-initializer)
  (etypecase type-initializer
    (string `(if (cffi:foreign-symbol-pointer ,type-initializer)
                 (cffi:foreign-funcall-pointer
                     (cffi:foreign-symbol-pointer ,type-initializer)
                     ()
                     :size)
                 (warn "Type initializer '~A' is not available"
                       ,type-initializer)))
    (symbol `(funcall ',type-initializer))))

;;; ----------------------------------------------------------------------------
;;; g_boxed_copy ()
;;; ----------------------------------------------------------------------------

;; Used internally for the implementation of a Lisp boxed type. This function
;; is not exported.

(cffi:defcfun ("g_boxed_copy" %boxed-copy) :pointer
  (gtype :size)
  (boxed :pointer))

(defun boxed-copy (gtype boxed)
  (unless (cffi:null-pointer-p boxed)
    (let ((gtype (if (integerp gtype) gtype (%g-type-from-name gtype))))
      (%boxed-copy gtype boxed))))

;;; ----------------------------------------------------------------------------
;;; g_boxed_free ()
;;; ----------------------------------------------------------------------------

;; Used internally for the implementation of a Lisp boxed type. This function
;; is not exported.

(cffi:defcfun ("g_boxed_free" %boxed-free) :void
  (gtype :size)
  (boxed :pointer))

(defun boxed-free (gtype boxed)
  (let ((gtype (if (integerp gtype) gtype (%g-type-from-name gtype))))
    (%boxed-free gtype boxed)))

;;; ----------------------------------------------------------------------------

(defstruct boxed-info
  name
  gtype)

;;; ----------------------------------------------------------------------------

;;; Garbage Collection for GBoxed opaque objects

(defvar *gboxed-gc-hooks* nil) ; pointers to objects to be freed
(defvar *gboxed-gc-hooks-lock* (bt:make-recursive-lock "gboxed-gc-hooks-lock"))
(defvar *gboxed-gc-hooks-counter* 0)

(defun activate-gboxed-gc-hooks ()
  (bt:with-recursive-lock-held (*gboxed-gc-hooks-lock*)
    (when *gboxed-gc-hooks*
      (iter (for (pointer info) in *gboxed-gc-hooks*)
            (decf *gboxed-gc-hooks-counter*)
            (boxed-free-fn info pointer))
      (assert (= 0 *gboxed-gc-hooks-counter*))
      (setf *gboxed-gc-hooks* nil))))

(defun register-gboxed-for-gc (pointer info)
  (bt:with-recursive-lock-held (*gboxed-gc-hooks-lock*)
    (let ((locks-were-present (not (null *gboxed-gc-hooks*))))
      (incf *gboxed-gc-hooks-counter*)
      (push (list pointer info) *gboxed-gc-hooks*)
      (unless locks-were-present
        (idle-add #'activate-gboxed-gc-hooks)))))

;;; ----------------------------------------------------------------------------

;; Hash table to store the Lisp infos for a boxed type.
;; The key is of type symbol. The accessors works for a symbol or a string
;; which represents a GType.

(defvar *boxed-infos* (make-hash-table :test 'eq))

(defun get-boxed-info (symbol-or-gtype)
  (let ((key (if (symbolp symbol-or-gtype)
                 symbol-or-gtype
                 (gtype-name (gtype symbol-or-gtype)))))
  (or (etypecase key
        (symbol (gethash key *boxed-infos*))
        (string (gethash (symbol-for-gtype key) *boxed-infos*)))
      (cl:error "GET-BOXED-INFO: Unknown GBoxed type '~A'"
                (gtype-name (gtype key))))))

(defun (setf get-boxed-info) (info symbol-or-gtype)
  (let ((key (if (symbolp symbol-or-gtype)
                 symbol-or-gtype
                 (gtype-name (gtype symbol-or-gtype)))))
  (etypecase key
    (symbol (setf (gethash key *boxed-infos*) info))
    (string (setf (gethash (symbol-for-gtype key) *boxed-infos*) info)))))

;;; ----------------------------------------------------------------------------

;; Define the base type boxed-type
;;
;; This type is specialized further to:
;;    boxed-opaque-type
;;    boxed-cstruct-type
;;    boxed-variant-cstruct-type

(cffi:define-foreign-type boxed-type ()
  ((info :initarg :info
         :accessor boxed-type-info
         :initform (cl:error "info must be specified"))
   (returnp :initarg :returnp
             :accessor boxed-type-returnp
             :initform nil))
  (:actual-type :pointer))

(defgeneric make-boxed-type (info &key returnp))

;;; ----------------------------------------------------------------------------

(cffi:define-parse-method boxed (name &rest options)
  (let ((info (get-boxed-info name)))
    (make-boxed-type info
                     :returnp (member :return options))))

(export 'boxed)

;;; ----------------------------------------------------------------------------

;;; A generic function which is implemented by the derived classes

(defgeneric cleanup-translated-object-for-callback (foreign-type
                                                    converted-object
                                                    native-object))

;;; This method is not further implemented and always returns TRUE

(defgeneric has-callback-cleanup (foreign-type))

(defmethod has-callback-cleanup ((type boxed-type))
  t)

;;; ----------------------------------------------------------------------------

;; Definition of generic functions to copy and to free a boxed object

(defgeneric boxed-copy-fn (info native)
  (:method (info native)
             (boxed-copy (boxed-info-gtype info) native)))

(defgeneric boxed-free-fn (info native)
  (:method (info native)
             (boxed-free (boxed-info-gtype info) native)))

;;; ----------------------------------------------------------------------------
;;; Imlementation of boxed-opaque-type
;;; ----------------------------------------------------------------------------

;; TODO: The alloc and free slots are unused in the implementation. Consider
;; to remove the slots.

(defstruct (boxed-opaque-info (:include boxed-info))
  alloc
  free)

(cffi:define-foreign-type boxed-opaque-type (boxed-type) ())

(defclass boxed-opaque ()
  ((pointer :initarg :pointer
            :initform nil
            :accessor boxed-opaque-pointer)))

(export 'boxed-opaque-pointer)

(defgeneric pointer (object))

(defmethod pointer ((object boxed-opaque))
  (boxed-opaque-pointer object))

(defmethod make-boxed-type ((info boxed-opaque-info) &key returnp)
  (make-instance 'boxed-opaque-type
                 :info info
                 :returnp returnp))

(defmethod cffi:translate-to-foreign (proxy (type boxed-opaque-type))
  (if (null proxy)
      (cffi:null-pointer)
      (prog1
        (boxed-opaque-pointer proxy)
        (when (boxed-type-returnp type)
          (tg:cancel-finalization proxy)
          (setf (boxed-opaque-pointer proxy) nil)))))

(defmethod cffi:free-translated-object (native (type boxed-opaque-type) param)
  (declare (ignore native type param)))

(defmethod cffi:translate-from-foreign (native (type boxed-opaque-type))
  (let* ((info (boxed-type-info type))
         ;; Changed 2021-8-2:
         ;; If NATIVE is NIL we return NIL. This handles the case of slots which
         ;; are initialized to a NULL pointer for the GBoxed opaque type.
         ;; Changed 2023-1-24:
         ;; In addition we do not return a boxed value for NULL-POINTER
         (proxy (when (and native (not (cffi:null-pointer-p native)))
                  (make-instance (boxed-info-name info) :pointer native))))
    (if (and proxy (boxed-type-returnp type))
        ;; Changed 2023-1-24:
        ;; Add a finalizer for return values of type :RETURN
        (tg:finalize proxy (make-boxed-free-finalizer native info))
        proxy)))

(defmethod cleanup-translated-object-for-callback
    ((type boxed-opaque-type) proxy native)
  (declare (ignore native))
  (tg:cancel-finalization proxy)
  (setf (boxed-opaque-pointer proxy) nil))

;;; ----------------------------------------------------------------------------

(defun make-boxed-free-finalizer (pointer info)
  (lambda ()
    (register-gboxed-for-gc pointer info)))

(defmacro define-g-boxed-opaque (name
                                 gtype
                                 &key (export t)
                                      type-initializer
                                      (alloc (cl:error "Alloc must be specified")))
  (let ((native-copy (gensym "NATIVE-COPY-"))
        (instance (gensym "INSTANCE-"))
        (info (gensym "INFO-")))
    `(progn
       (defclass ,name (boxed-opaque) ())
       (defmethod initialize-instance
                  :after ((,instance ,name) &key &allow-other-keys)
         (unless (boxed-opaque-pointer ,instance)
           (let ((,native-copy ,alloc)
                 (,info (get-boxed-info ',name)))
             (assert (cffi:pointerp ,native-copy))
             (setf (boxed-opaque-pointer ,instance) ,native-copy)
             (tg:finalize ,instance
                          (make-boxed-free-finalizer ,native-copy ,info)))))
       ;; Change 2023-1-24:
       ;; Add call to the type initializer, when available
       ,@(when type-initializer
           (list `(glib-init:at-init ()
;                     (eval-when (:compile-toplevel :load-toplevel :execute)
                       ,(type-initializer-call type-initializer))))
;                       )
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ;; Register the Lisp symbol NAME for GTYPE
         (setf (symbol-for-gtype ,gtype) ',name)
         ;; Store the structure in a hash table
         (setf (get-boxed-info ',name)
               (make-boxed-opaque-info :name ',name
                                       :gtype ,gtype)))
       ,@(when export
           (list `(export ',name
                          (find-package
                              ,(package-name (symbol-package name)))))))))

;; For replacing define-g-boxed-opaque
(defmacro define-gboxed-opaque (name gtype
                                &key (export t)
                                     type-initializer
                                     (alloc (cl:error "Alloc must be specified")))
  (let ((native-copy (gensym "NATIVE-COPY-"))
        (instance (gensym "INSTANCE-"))
        (info (gensym "INFO-")))
    `(progn
       (defclass ,name (boxed-opaque) ())
       (defmethod initialize-instance
                  :after ((,instance ,name) &key &allow-other-keys)
         (unless (boxed-opaque-pointer ,instance)
           (let ((,native-copy ,alloc)
                 (,info (get-boxed-info ',name)))
             (assert (cffi:pointerp ,native-copy))
             (setf (boxed-opaque-pointer ,instance) ,native-copy)
             (tg:finalize ,instance
                          (make-boxed-free-finalizer ,native-copy ,info)))))
       ;; Change 2023-1-24:
       ;; Add call to the type initializer, when available
       ,@(when type-initializer
           (list `(glib-init:at-init ()
                       ,(type-initializer-call type-initializer))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ;; Register the Lisp symbol NAME for GTYPE
         (setf (symbol-for-gtype ,gtype) ',name)
         ;; Store the structure in a hash table
         (setf (get-boxed-info ',name)
               (make-boxed-opaque-info :name ',name
                                       :gtype ,gtype)))
       ,@(when export
           (list `(export ',name
                          (find-package
                              ,(package-name (symbol-package name)))))))))

;;; ----------------------------------------------------------------------------
;;;
;;; Imlementation of boxed-cstruct-type
;;;
;;; ----------------------------------------------------------------------------

;; Helper function used by the method boxed-copy-fn

(defun memcpy (target source bytes)
  (iter (for i from 0 below bytes)
        (setf (cffi:mem-aref target :uchar i)
              (cffi:mem-aref source :uchar i))))

;;; ----------------------------------------------------------------------------

;; Helper functions to create an internal symbol

(defun generated-cstruct-name (symbol)
  (intern (format nil "~A-CSTRUCT" (symbol-name symbol))
          (symbol-package symbol)))

(defun generated-cunion-name (symbol)
  (intern (format nil "~A-CUNION" (symbol-name symbol))
          (symbol-package symbol)))

;;; ----------------------------------------------------------------------------

(defstruct cstruct-description
  name
  slots)

(defmethod make-load-form ((object cstruct-description) &optional environment)
  (make-load-form-saving-slots object :environment environment))

;;; ----------------------------------------------------------------------------

(defstruct (boxed-cstruct-info (:include boxed-info))
  cstruct-description)

;;; ----------------------------------------------------------------------------

(defstruct cstruct-slot-description
  name
  type
  count
  initform
  inline-p)

(defmethod make-load-form ((object cstruct-slot-description)
                           &optional environment)
  (make-load-form-saving-slots object :environment environment))

;;; ----------------------------------------------------------------------------

(defstruct (cstruct-inline-slot-description (:include cstruct-slot-description))
  boxed-type-name)

;;; ----------------------------------------------------------------------------

(defmethod make-load-form ((object cstruct-inline-slot-description)
                           &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defclass boxed-cstruct-type (boxed-type) ())

(defun parse-cstruct-slot (slot)
  (destructuring-bind (name type &key count initform inline) slot
    (if inline
        (make-cstruct-inline-slot-description :name name
                                              :type
                                              (list :union
                                                    (generated-cunion-name type))
                                              :count count
                                              :initform initform
                                              :inline-p inline
                                              :boxed-type-name type)
        (make-cstruct-inline-slot-description :name name
                                              :type type
                                              :count count
                                              :initform initform
                                              :inline-p inline))))

(defun parse-cstruct-definition (name slots)
  (make-cstruct-description :name name
                            :slots (mapcar #'parse-cstruct-slot slots)))

;;; ----------------------------------------------------------------------------

(defmacro define-g-boxed-cstruct (name gtype
                                  (&key (export t)
                                        type-initializer)
                                  &body slots)
  (let ((cstruct-description (parse-cstruct-definition name slots)))
    `(progn
       (defstruct ,name
         ,@(iter (for slot in (cstruct-description-slots cstruct-description))
                 (for name = (cstruct-slot-description-name slot))
                 (for initform = (cstruct-slot-description-initform slot))
                 (collect (list name initform))))
       (cffi:defcstruct ,(generated-cstruct-name name)
         ,@(iter (for slot in (cstruct-description-slots cstruct-description))
                 (for name = (cstruct-slot-description-name slot))
                 (for type = (cstruct-slot-description-type slot))
                 (for count = (cstruct-slot-description-count slot))
                 (collect `(,name ,type ,@(when count `(:count ,count))))))
       (cffi:defcunion ,(generated-cunion-name name)
         (,name (:struct ,(generated-cstruct-name name))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ;; Register the Lisp symbol NAME for GTYPE
         (setf (symbol-for-gtype ,gtype) ',name)
         ;; Store the structure in a hash table
         (setf (get-boxed-info ',name)
               (make-boxed-cstruct-info :name ',name
                                        :gtype ,gtype
                                        :cstruct-description
                                        ,cstruct-description))
         (setf (get ',name 'structure-constructor)
               ',(intern (format nil "MAKE-~A" (symbol-name name))
                         (symbol-package name))))
       ;; Change 2023-2-4:
       ;; Add call to the type initializer, when available,
       ;; and export the symbol
       ,@(when type-initializer
           (list `(glib-init:at-init ()
                     ,(type-initializer-call type-initializer))))
       ,@(when export
           (list `(export (boxed-related-symbols ',name)))))))

;; For replacing define-gboxed-cstruct
(defmacro define-gboxed-cstruct (name gtype
                                 (&key (export t)
                                       type-initializer)
                                 &body slots)
  (let ((cstruct-description (parse-cstruct-definition name slots)))
    `(progn
       (defstruct ,name
         ,@(iter (for slot in (cstruct-description-slots cstruct-description))
                 (for name = (cstruct-slot-description-name slot))
                 (for initform = (cstruct-slot-description-initform slot))
                 (collect (list name initform))))
       (cffi:defcstruct ,(generated-cstruct-name name)
         ,@(iter (for slot in (cstruct-description-slots cstruct-description))
                 (for name = (cstruct-slot-description-name slot))
                 (for type = (cstruct-slot-description-type slot))
                 (for count = (cstruct-slot-description-count slot))
                 (collect `(,name ,type ,@(when count `(:count ,count))))))
       (cffi:defcunion ,(generated-cunion-name name)
         (,name (:struct ,(generated-cstruct-name name))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ;; Register the Lisp symbol NAME for GTYPE
         (setf (symbol-for-gtype ,gtype) ',name)
         ;; Store the structure in a hash table
         (setf (get-boxed-info ',name)
               (make-boxed-cstruct-info :name ',name
                                        :gtype ,gtype
                                        :cstruct-description
                                        ,cstruct-description))
         (setf (get ',name 'structure-constructor)
               ',(intern (format nil "MAKE-~A" (symbol-name name))
                         (symbol-package name))))
       ;; Change 2023-2-4:
       ;; Add call to the type initializer, when available,
       ;; and export the symbol
       ,@(when type-initializer
           (list `(glib-init:at-init ()
                     ,(type-initializer-call type-initializer))))
       ,@(when export
           (list `(export (boxed-related-symbols ',name)))))))

;;; ----------------------------------------------------------------------------

(defmethod make-boxed-type ((info boxed-cstruct-info) &key returnp)
  (make-instance 'boxed-cstruct-type
                 :info info
                 :returnp returnp))

(defmethod boxed-copy-fn ((info boxed-cstruct-info) native)
  (if (boxed-info-gtype info)
      (boxed-copy (boxed-info-gtype info) native)
      (let ((copy
             (cffi:foreign-alloc
                 (generated-cstruct-name (boxed-info-name info)))))
        (memcpy copy
                native
                (cffi:foreign-type-size
                  (list :struct
                        (generated-cstruct-name (boxed-info-name info)))))
        copy)))

(defmethod boxed-free-fn ((info boxed-cstruct-info) native)
  (if (boxed-info-gtype info)
      (boxed-free (boxed-info-gtype info) native)
      (cffi:foreign-free native)))

;;; ----------------------------------------------------------------------------

(defun copy-slots-to-native (proxy native description)
  (iter (with cstruct-type = (generated-cstruct-name
                               (cstruct-description-name description)))
        (for slot in (cstruct-description-slots description))
        (for slot-name = (cstruct-slot-description-name slot))
        (cond
          ((cstruct-slot-description-count slot)
           (iter (with ptr = (cffi:foreign-slot-pointer native
                                                   (list :struct cstruct-type)
                                                   slot-name))
                 (with array = (slot-value proxy slot-name))
                 (for i from 0 below (cstruct-slot-description-count slot))
                 (setf (cffi:mem-aref ptr (cstruct-slot-description-type slot) i)
                       (aref array i))))
          ((cstruct-slot-description-inline-p slot)
           (let ((info
                  (get-boxed-info
                       (cstruct-inline-slot-description-boxed-type-name slot))))
             (copy-slots-to-native
                      (slot-value proxy slot-name)
                      (cffi:foreign-slot-pointer native
                                            (list :struct cstruct-type)
                                            slot-name)
                      (boxed-cstruct-info-cstruct-description info))))
          (t
           (setf (cffi:foreign-slot-value native
                                          (list :struct cstruct-type)
                                          slot-name)
                 (slot-value proxy slot-name))))))

(defun create-structure (structure-name)
  (let ((constructor (get structure-name 'structure-constructor)))
    (assert constructor nil "Don't know how to create structure of type ~A"
            structure-name)
    (funcall constructor)))

(defun copy-slots-to-proxy (proxy native description)
  (iter (with cstruct-type = (generated-cstruct-name
                               (cstruct-description-name description)))
        (for slot in (cstruct-description-slots description))
        (for slot-name = (cstruct-slot-description-name slot))
        (cond
          ((cstruct-slot-description-count slot)
           (setf (slot-value proxy slot-name)
                 (make-array (list (cstruct-slot-description-count slot))))
           (iter (with ptr = (cffi:foreign-slot-pointer native
                                                   (list :struct cstruct-type)
                                                   slot-name))
                 (with array = (slot-value proxy slot-name))
                 (for i from 0 below (cstruct-slot-description-count slot))
                 (setf (aref array i)
                       (cffi:mem-aref ptr (cstruct-slot-description-type slot) i))))
          ((cstruct-slot-description-inline-p slot)
           (let ((info (get-boxed-info
                         (cstruct-inline-slot-description-boxed-type-name slot))))
             (setf (slot-value proxy slot-name)
                   (create-structure
                     (cstruct-inline-slot-description-boxed-type-name slot)))
             (copy-slots-to-proxy
                      (slot-value proxy slot-name)
                      (cffi:foreign-slot-pointer native
                                                 (list :struct cstruct-type)
                                                 slot-name)
                      (boxed-cstruct-info-cstruct-description info))))
          (t (setf (slot-value proxy slot-name)
                   (cffi:foreign-slot-value native
                                            (list :struct cstruct-type)
                                            slot-name))))))

;;; ----------------------------------------------------------------------------

(defmethod cffi:translate-to-foreign (proxy (type boxed-cstruct-type))
  (if (null proxy)
      (cffi:null-pointer)
      (let* ((info (boxed-type-info type))
             (native-struct-type
               (generated-cstruct-name (boxed-info-name info))))
        (cffi:with-foreign-object (native-struct
                                      (list :struct native-struct-type))
          (copy-slots-to-native
                        proxy
                        native-struct
                        (boxed-cstruct-info-cstruct-description info))
          (values (boxed-copy-fn info native-struct)
                  proxy)))))

(defmethod cffi:free-translated-object (native-struct
                                   (type boxed-cstruct-type)
                                   proxy)
  (when proxy
    (let ((info (boxed-type-info type)))
      (copy-slots-to-proxy
                        proxy
                        native-struct
                        (boxed-cstruct-info-cstruct-description info))
      (boxed-free-fn info native-struct))))

(defmethod cffi:translate-from-foreign (native-struct (type boxed-cstruct-type))
  (unless (cffi:null-pointer-p native-struct)
    (let* ((info (boxed-type-info type))
           (proxy-struct-type (boxed-info-name info))
           (proxy (create-structure proxy-struct-type)))
      (copy-slots-to-proxy proxy
                           native-struct
                           (boxed-cstruct-info-cstruct-description info))
      (when (boxed-type-returnp type)
        (boxed-free-fn info native-struct))
      proxy)))

;;; ----------------------------------------------------------------------------

(defmethod cleanup-translated-object-for-callback
    ((type boxed-cstruct-type) proxy native-structure)
  (when proxy
    (let ((info (boxed-type-info type)))
      (copy-slots-to-native
                     proxy
                     native-structure
                     (boxed-cstruct-info-cstruct-description info)))))

;;; ----------------------------------------------------------------------------

(defstruct var-structure
  name
  parent
  slots
  discriminator-slot
  variants
  resulting-cstruct-description)

(defstruct var-structure-variant
  discriminating-values
  structure)

(defmethod make-load-form ((object var-structure) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod make-load-form ((object var-structure-variant) &optional env)
  (make-load-form-saving-slots object :environment env))

(defun var-struct-all-slots (struct)
  (when struct
    (append (var-struct-all-slots (var-structure-parent struct))
            (var-structure-slots struct))))

(defun all-structures (structure)
  (append (iter (for variant in (var-structure-variants structure))
                (appending (all-structures (var-structure-variant-structure variant))))
          (list structure)))

(defun parse-variant-structure-definition (name slots &optional parent)
  (iter (with result = (make-var-structure :name name
                                           :parent parent
                                           :slots nil
                                           :discriminator-slot nil
                                           :variants nil))
        (for slot in slots)
        (if (eq :variant (first slot))
            (progn
              (when (var-structure-discriminator-slot result)
                (cl:error "Structure has more than one discriminator slot"))
              (setf (var-structure-discriminator-slot result)
                    (second slot)
                    (var-structure-variants result)
                    (parse-variants result (nthcdr 2 slot))))
            (push (parse-cstruct-slot slot) (var-structure-slots result)))
        (finally (setf (var-structure-slots result)
                       (reverse (var-structure-slots result)))
                 (unless parent
                   (set-variant-result-structure result))
                 (return result))))

(defun set-variant-result-structure (var-structure)
  (setf (var-structure-resulting-cstruct-description var-structure)
        (make-cstruct-description
         :name
         (var-structure-name var-structure)
         :slots
         (append
          (when (var-structure-parent var-structure)
            (cstruct-description-slots
              (var-structure-resulting-cstruct-description
                (var-structure-parent var-structure))))
          (var-structure-slots var-structure))))
  (iter (for variant in (var-structure-variants var-structure))
        (for child-var-structure = (var-structure-variant-structure variant))
        (set-variant-result-structure child-var-structure)))

(defun ensure-list (thing)
  (if (listp thing)
      thing
      (list thing)))

(defun parse-variants (parent variants)
  (iter (for (options variant-name . slots) in variants)
        (for variant =
             (make-var-structure-variant
              :discriminating-values (ensure-list options)
              :structure
              (parse-variant-structure-definition variant-name slots parent)))
        (collect variant)))

(defun generate-cstruct-1 (struct)
  `(cffi:defcstruct ,(generated-cstruct-name (cstruct-description-name struct))
     ,@(iter (for slot in (cstruct-description-slots struct))
             (collect `(,(cstruct-slot-description-name slot)
                         ,(cstruct-slot-description-type slot)
                         ,@(when (cstruct-slot-description-count slot)
                             `(:count ,(cstruct-slot-description-count slot))))))))

(defun generate-c-structures (structure)
  (iter (for str in (all-structures structure))
        (for cstruct = (var-structure-resulting-cstruct-description str))
        (collect (generate-cstruct-1 cstruct))))

(defun generate-variant-union (struct)
  `(cffi:defcunion ,(generated-cunion-name (var-structure-name struct))
     ,@(iter (for str in (all-structures struct))
             (collect `(,(var-structure-name str)
                         (:struct ,(generated-cstruct-name (var-structure-name str))))))))

(defun generate-structure-1 (str)
  (let ((name (var-structure-name str)))
    `(progn
       (defstruct ,(if (var-structure-parent str)
                       `(,(var-structure-name str)
                          (:include
                            ,(var-structure-name (var-structure-parent str))
                            (,(var-structure-discriminator-slot (var-structure-parent str))
                              ,(first (var-structure-variant-discriminating-values
                                        (find str
                                              (var-structure-variants
                                                (var-structure-parent str))
                                              :key #'var-structure-variant-structure))))))
                       `,(var-structure-name str))
         ,@(iter (for slot in (var-structure-slots str))
                 (collect `(,(cstruct-slot-description-name slot)
                             ,(cstruct-slot-description-initform slot)))))
       (setf (get ',name 'structure-constructor)
             ',(intern (format nil "MAKE-~A" (symbol-name name))
                       (symbol-package name))))))

(defun generate-structures (str)
  (iter (for variant in (reverse (all-structures str)))
        (collect (generate-structure-1 variant))))

(defun generate-native-type-decision-procedure-1 (str proxy-var)
  (if (null (var-structure-discriminator-slot str))
      `(values ',(var-structure-resulting-cstruct-description str))
      `(typecase ,proxy-var
         ,@(iter (for variant in (var-structure-variants str))
                 (for v-str = (var-structure-variant-structure variant))
                 (collect `(,(var-structure-name v-str)
                             ,(generate-native-type-decision-procedure-1 v-str proxy-var))))
         (,(var-structure-name str)
          (values ',(var-structure-resulting-cstruct-description str))))))

(defun generate-proxy-type-decision-procedure-1 (str native-var)
  (if (null (var-structure-discriminator-slot str))
      `(values ',(var-structure-name str)
               ',(var-structure-resulting-cstruct-description str))
      `(case
         (cffi:foreign-slot-value ,native-var
                                  '(:struct ,(generated-cstruct-name
                                               (var-structure-name str)))
                                  ',(var-structure-discriminator-slot str))
         ,@(iter (for variant in (var-structure-variants str))
                 (for v-str = (var-structure-variant-structure variant))
                 (collect `(,(var-structure-variant-discriminating-values variant)
                             ,(generate-proxy-type-decision-procedure-1
                               v-str
                               native-var))))
         (t (values ',(var-structure-name str)
                    ',(var-structure-resulting-cstruct-description str))))))

(defun generate-proxy-type-decision-procedure (str)
  (let ((native (gensym "NATIVE-")))
    `(lambda (,native)
       (declare (ignorable ,native))
       ,(generate-proxy-type-decision-procedure-1 str native))))

(defun generate-native-type-decision-procedure (str)
  (let ((proxy (gensym "PROXY-")))
    `(lambda (,proxy)
       (declare (ignorable ,proxy))
       ,(generate-native-type-decision-procedure-1 str proxy))))

(defun compile-proxy-type-decision-procedure (str)
  (compile nil (generate-proxy-type-decision-procedure str)))

(defun compile-native-type-decision-procedure (str)
  (compile nil (generate-native-type-decision-procedure str)))

;;; ----------------------------------------------------------------------------

;; Type boxed-variant-cstruct-type

(defstruct (boxed-variant-info (:include boxed-info))
  root
  native-type-decision-procedure
  proxy-type-decision-procedure)

(defmethod make-load-form ((object boxed-variant-info) &optional env)
  (make-load-form-saving-slots object :environment env))

(cffi:define-foreign-type boxed-variant-cstruct-type (boxed-type) ())

(defmethod make-boxed-type ((info boxed-variant-info) &key returnp)
  (make-instance 'boxed-variant-cstruct-type
                 :info info
                 :returnp returnp))

(defmacro define-g-boxed-variant-cstruct (name gtype &body slots)
  (let* ((structure (parse-variant-structure-definition name slots)))
    `(progn
      ,@(generate-c-structures structure)
      ,(generate-variant-union structure)
      ,@(generate-structures structure)
      (eval-when (:compile-toplevel :load-toplevel :execute)
        ;; Register the Lisp symbol NAME for GTYPE
        (setf (symbol-for-gtype ,gtype) ',name)
        ;; Store the structure in a hash table
        (setf (get-boxed-info ',name)
              (make-boxed-variant-info
                       :name ',name
                       :gtype ,gtype
                       :root ,structure
                       :native-type-decision-procedure
                       ,(generate-native-type-decision-procedure structure)
                       :proxy-type-decision-procedure
                       ,(generate-proxy-type-decision-procedure structure)))))))

(defun decide-native-type (info proxy)
  (funcall (boxed-variant-info-native-type-decision-procedure info) proxy))

(defmethod boxed-copy-fn ((info boxed-variant-info) native)
  (if (boxed-info-gtype info)
      (boxed-copy (boxed-info-gtype info) native)
      (let ((copy (cffi:foreign-alloc
                      (generated-cunion-name (boxed-info-name info)))))
        (memcpy copy
                native
                (cffi:foreign-type-size
                  (list :struct
                        (generated-cunion-name (boxed-info-name info)))))
        copy)))

(defmethod boxed-free-fn ((info boxed-variant-info) native)
  (if (boxed-info-gtype info)
      (boxed-free (boxed-info-gtype info) native)
      (cffi:foreign-free native)))

(defmethod cffi:translate-to-foreign
    (proxy (foreign-type boxed-variant-cstruct-type))
  (if (null proxy)
      (cffi:null-pointer)
      (let* ((type (boxed-type-info foreign-type))
             (description (decide-native-type type proxy)))
        (cffi:with-foreign-object
          (native-struct (list :union
                               (generated-cunion-name
                                 (var-structure-name
                                   (boxed-variant-info-root type)))))
          (copy-slots-to-native proxy native-struct description)
          (values (boxed-copy-fn type native-struct) proxy)))))

(defun decide-proxy-type (info native-struct)
  (funcall (boxed-variant-info-proxy-type-decision-procedure info)
           native-struct))

(defmethod cffi:free-translated-object
    (native (foreign-type boxed-variant-cstruct-type) proxy)
  (when proxy
    (let ((type (boxed-type-info foreign-type)))
      (multiple-value-bind (actual-struct cstruct-description)
          (decide-proxy-type type native)
        (unless (eq (type-of proxy) actual-struct)
          (restart-case
              (cl:error "Expected type of boxed variant structure ~A and actual ~
                       type ~A do not match"
                     (type-of proxy) actual-struct)
            (skip-parsing-values () (return-from cffi:free-translated-object))))
        (copy-slots-to-proxy proxy native cstruct-description)
        (boxed-free-fn type native)))))

(defmethod cffi:translate-from-foreign
    (native (foreign-type boxed-variant-cstruct-type))
  (unless (cffi:null-pointer-p native)
    (let ((type (boxed-type-info foreign-type)))
      (multiple-value-bind (actual-struct cstruct-description)
          (decide-proxy-type type native)
        (let ((proxy (create-structure actual-struct)))
          (copy-slots-to-proxy proxy native cstruct-description)
          (when (boxed-type-returnp foreign-type)
            (boxed-free-fn type native))
          proxy)))))

(defmethod cleanup-translated-object-for-callback
    ((foreign-type boxed-variant-cstruct-type) proxy native)
  (when proxy
    (let ((type (boxed-type-info foreign-type)))
      (let ((cstruct-description (decide-native-type type proxy)))
        (copy-slots-to-native proxy native cstruct-description)))))

;;; ----------------------------------------------------------------------------

(defmacro define-boxed-opaque-accessor (boxed-name accessor-name
                                        &key type reader writer)
  (let ((var (gensym))
        (n-var (gensym)))
    `(progn
       ,@(when reader
           (list (etypecase reader
                   (symbol `(defun ,accessor-name (,var)
                              (funcall ,reader ,var)))
                   (string `(cffi:defcfun (,accessor-name ,reader) ,type
                              (,var (boxed ,boxed-name)))))))
       ,@(when writer
           (list (etypecase reader
                   (symbol `(defun (setf ,accessor-name) (,n-var ,var)
                              (funcall ,reader ,n-var ,var)))
                   (string `(defun (setf ,accessor-name) (,n-var ,var)
                              (cffi:foreign-funcall ,writer
                                                    (boxed ,boxed-name)
                                                    ,var
                                                    ,type
                                                    ,n-var
                                                    :void)))))))))

;;; ----------------------------------------------------------------------------

(defun boxed-related-symbols (name)
  (let ((info (get-boxed-info name)))
    (etypecase info
      (boxed-opaque-info
       (list name))
      (boxed-cstruct-info
       (append
         (list name)
         (iter (for slot in (cstruct-description-slots
                              (boxed-cstruct-info-cstruct-description info)))
               (for slot-name = (cstruct-slot-description-name slot))
               (collect (intern (format nil "~A-~A"
                                        (symbol-name name)
                                        (symbol-name slot-name)))))))
      (boxed-variant-info
       (append
         (list name)
         (iter (for var-struct in (all-structures (boxed-variant-info-root info)))
               (for s-name = (var-structure-name var-struct))
               (for cstruct-description = (var-structure-resulting-cstruct-description var-struct))
               (appending (append
                            (list s-name)
                            (list (intern (format nil "MAKE-~A" (symbol-name s-name)))
                                  (intern (format nil "COPY-~A" (symbol-name s-name))))
                            (iter (for slot in (cstruct-description-slots cstruct-description))
                                  (for slot-name = (cstruct-slot-description-name slot))
                                  (collect (intern (format nil "~A-~A"
                                                           (symbol-name s-name)
                                                           (symbol-name slot-name)))))))))))))

;;; ----------------------------------------------------------------------------

(defun copy-boxed-slots-to-foreign (struct
                                    native-ptr
                                    &optional (gtype (and struct
                                                          (type-of struct))))
  (when struct
    (copy-slots-to-native struct
                          native-ptr
                          (boxed-cstruct-info-cstruct-description
                                            (get-boxed-info gtype)))))

;; FIXME: We get a compiler warning. Check this more carefully
;;
;; in: DEFINE-COMPILER-MACRO COPY-BOXED-SLOTS-TO-FOREIGN
;;     (WARN "Unknown foreign GBoxed type ~S" GOBJECT::TYPE-R)
;; ==>
;;   "Unknown foreign GBoxed type ~S"
;;
;; note: deleting unreachable code

(define-compiler-macro copy-boxed-slots-to-foreign (&whole whole structure
                                                           native-ptr
                                                    &optional type)
  ;; Muffles the message about deleting unreachable code.
  #+sbcl
  (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (if (and type (constantp type))
      (let* ((type-r (eval type))
             (f-i (get-boxed-info type-r)))
        (unless (or f-i (typep f-i 'boxed-cstruct-info))
          (warn "Unknown foreign GBoxed type ~S" type-r)
          (return-from copy-boxed-slots-to-foreign whole))
        `(when ,structure
           (copy-slots-to-native
            ,structure
            ,native-ptr
            (load-time-value
              (boxed-cstruct-info-cstruct-description
                (get-boxed-info ',type-r))))))
      whole))

;;; ----------------------------------------------------------------------------

(defmacro with-foreign-boxed-array ((n-var array-var type values-seq)
                                    &body body)
  (let ((values-seq-1 (gensym "VALUES-SEQ-"))
        (cstruct (generated-cstruct-name type))
        (x (gensym "X-"))
        (i (gensym "I-")))
    `(let* ((,values-seq-1 ,values-seq)
            (,n-var (length ,values-seq-1)))
       (cffi:with-foreign-object (,array-var '(:struct ,cstruct) ,n-var)
         (let ((,i 0))
           (map nil
                (lambda (,x)
                  (copy-boxed-slots-to-foreign
                    ,x
                    (cffi:inc-pointer ,array-var
                                 (* ,i
                                    (cffi:foreign-type-size '(:struct ,cstruct))))
                       ',type)
                  (incf ,i))
                ,values-seq-1))
         ,@body))))

(defmacro with-g-boxed-array ((n-var array-var gtype values) &body body)
  (let ((values-1 (gensym "VALUES-"))
        (cstruct (generated-cstruct-name gtype))
        (x (gensym "X-"))
        (i (gensym "I-")))
    `(let* ((,values-1 ,values)
            (,n-var (length ,values-1)))
       (cffi:with-foreign-object (,array-var '(:struct ,cstruct) ,n-var)
         (let ((,i 0))
           (map nil
                (lambda (,x)
                  (copy-boxed-slots-to-foreign
                    ,x
                    (cffi:inc-pointer ,array-var
                                 (* ,i
                                    (cffi:foreign-type-size '(:struct ,cstruct))))
                       ',gtype)
                  (incf ,i))
                ,values-1))
         ,@body))))


;;; --- End of file glib.boxed-type.lisp ---------------------------------------
