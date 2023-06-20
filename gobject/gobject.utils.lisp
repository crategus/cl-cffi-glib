;;; ----------------------------------------------------------------------------
;;; gobject.utils.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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

(defvar *generation-exclusions* nil)

;; Get the definition of a GType

(defun get-g-type-definition (type &optional lisp-name-package)
  (maybe-call-type-init type)
  (cond ((type-is-a type (gtype +g-type-enum+))
         (get-g-enum-definition type lisp-name-package))
        ((type-is-a type (gtype +g-type-flags+))
         (get-g-flags-definition type lisp-name-package))
        ((type-is-a type (gtype +g-type-interface+))
         (get-g-interface-definition type lisp-name-package))
        ((type-is-a type (gtype +g-type-object+))
         (get-g-class-definition type lisp-name-package))
        (t
         (error "Do not know how to generate type definition for ~A type ~A"
                (gtype-name (type-fundamental type))
                (or (ignore-errors (gtype-name (gtype type)))
                    type)))))

;;; ----------------------------------------------------------------------------

(defun maybe-call-type-init (type)
  (when (and (stringp type)
             (null (gtype type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (cffi:foreign-symbol-pointer type-init-name)
        (cffi:foreign-funcall-pointer
            (cffi:foreign-symbol-pointer type-init-name)
                                         ()
                                         :int)))))

(defun probable-type-init-name (type-name)
  (with-output-to-string (stream)
    (iter (for c in-string type-name)
          (for prev-c previous c)
          (when (and (not (first-iteration-p))
                     (upper-case-p c)
                     (not (upper-case-p prev-c))
                     (not (char= prev-c #\_)))
            (write-char #\_ stream))
          (write-char (char-downcase c) stream))
    (write-string "_get_type" stream)))

;;; ----------------------------------------------------------------------------

;; "A structure describing a single enumeration item.
;;
;; See accessor functions:
;;    enum-item-name
;;    enum-item-value
;;    enum-item-nick

(defstruct enum-item
  name
  value
  nick)

;; Gets the list of enum items that belong to GEnum type type
;;
;; type :
;;     a string or an integer specifying GEnum type
;; return :
;;     a list of enum-item objects

(defun get-enum-items (gtype)
  (assert (type-is-a gtype +g-type-enum+))
  (let ((gclass (type-class-ref gtype)))
    (unwind-protect
      (loop
        with n = (cffi:foreign-slot-value gclass '(:struct enum-class) :n-values)
        with values = (cffi:foreign-slot-value gclass '(:struct enum-class) :values)
        for i from 0 below n
        for value = (cffi:mem-aptr values '(:struct enum-value) i)
        collect
          (make-enum-item
              :name (cffi:foreign-slot-value value '(:struct enum-value) :name)
              :value (cffi:foreign-slot-value value '(:struct enum-value) :value)
              :nick (cffi:foreign-slot-value value '(:struct enum-value) :nick)))
      (type-class-unref gclass))))

;; Get the definition of a GEnum type

(defun get-g-enum-definition (type &optional lisp-name-package)
  (when (and (stringp type)
             (null (gtype type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (cffi:foreign-symbol-pointer type-init-name)
        (cffi:foreign-funcall-pointer
            (cffi:foreign-symbol-pointer type-init-name)
                                         ()
                                         :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype type)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package* *package*))
         (type (gtype type))
         (g-name (gtype-name type))
         (name (g-name->name g-name))
         (items (get-enum-items type))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-enum ,g-name ,name
         (:export t
                  ,@(when (cffi:foreign-symbol-pointer probable-type-initializer)
                          (list :type-initializer
                                probable-type-initializer)))
       ,@(mapcar #'enum-value->definition items))))

(defun enum-value->definition (enum-value)
  (let ((value-name (intern (lispify-name (enum-item-nick enum-value))
                            (find-package :keyword)))
        (numeric-value (enum-item-value enum-value)))
    `(,value-name ,numeric-value)))

;;; ----------------------------------------------------------------------------

;; A structure describing a single flags item.

(defstruct flags-item
  name
  value
  nick)

;; Gets the list of flags items that belong to GFlags type type
;; type is a string or an integer specifying GFlags type.
;; Returns a list of flags-item objects

(defun get-flags-items (gtype)
  (assert (type-is-a gtype +g-type-flags+))
  (let ((gclass (type-class-ref gtype)))
    (unwind-protect
      (loop
        with n = (cffi:foreign-slot-value gclass '(:struct flags-class) :n-values)
        with values = (cffi:foreign-slot-value gclass '(:struct flags-class) :values)
        for i from 0 below n
        for value = (cffi:mem-aptr values '(:struct flags-value) i)
        collect
          (make-flags-item
              :name (cffi:foreign-slot-value value '(:struct flags-value) :name)
              :value (cffi:foreign-slot-value value '(:struct flags-value) :value)
              :nick (cffi:foreign-slot-value value '(:struct flags-value) :nick)))
      (type-class-unref gclass))))

;; Get the definition of a GFlags type

(defun get-g-flags-definition (type &optional lisp-name-package)
  (when (and (stringp type) (null (gtype type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (cffi:foreign-symbol-pointer type-init-name)
        (cffi:foreign-funcall-pointer
            (cffi:foreign-symbol-pointer type-init-name)
                                         ()
                                         :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype type)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package* *package*))
         (type (gtype type))
         (g-name (gtype-name type))
         (name (g-name->name g-name))
         (items (get-flags-items type))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-flags ,g-name ,name
         (:export t
                  ,@(when (cffi:foreign-symbol-pointer probable-type-initializer)
                          (list :type-initializer
                                probable-type-initializer)))
       ,@(mapcar #'flags-value->definition items))))

(defun flags-value->definition (flags-value)
  (let ((value-name (intern (lispify-name (flags-item-nick flags-value))
                            (find-package :keyword)))
        (numeric-value (flags-item-value flags-value)))
    `(,value-name ,numeric-value)))

;;; ----------------------------------------------------------------------------

;; Helper functions for getting the definitions

(defun property->property-definition (class-name property)
  (let ((name (g-name->name (%param-spec-name property)))
        (accessor-name (accessor-name class-name (%param-spec-name property)))
        (g-name (%param-spec-name property))
        (type (gtype-name (%param-spec-type property)))
        (readable (%param-spec-readable property))
        (writable (and (%param-spec-writable property)
                       (not (%param-spec-constructor-only property)))))
    `(,name ,accessor-name ,g-name ,type ,readable ,writable)))

(defun g-name->name (name)
  (or (get-lisp-name-exception name)
      (intern (string-upcase (lispify-name name)) *lisp-name-package*)))

;;; ----------------------------------------------------------------------------
;; Get the defintion of a GInterface type
;;; ----------------------------------------------------------------------------

;; A Helper method

(defclass print-readtime-condition ()
  ((condition :initarg :condition)))

(defmethod print-object ((o print-readtime-condition) stream)
  (format stream "#~A" (slot-value o 'condition)))

;;; ----------------------------------------------------------------------------

(defun get-g-interface-definition (interface &optional lisp-name-package)
  (when (and (stringp interface) (null (ignore-errors (gtype interface))))
    (let ((type-init-name (probable-type-init-name interface)))
      (when (cffi:foreign-symbol-pointer type-init-name)
        (cffi:foreign-funcall-pointer
            (cffi:foreign-symbol-pointer type-init-name)
                                         ()
                                        :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype interface)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package* *package*))
         (type (gtype interface))
         (g-name (gtype-name type))
         (name (g-name->name g-name))
         (properties (sort (copy-list (interface-properties type))
                           #'string< :key #'%param-spec-name))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-interface ,g-name ,name
         (:export t
                  ,@(when (cffi:foreign-symbol-pointer probable-type-initializer)
                          `(:type-initializer ,probable-type-initializer)))
       ,@(append (mapcar (lambda (property)
                           (property->property-definition name property))
                         properties)
                 (mapcan (lambda (property-definition)
                           (if (eq :cond (car property-definition))
                               (list (make-instance 'print-readtime-condition
                                                    :condition
                                                    (cadr property-definition))
                                     (cddr property-definition))
                               (list property-definition)))
                         (cdr (find g-name *additional-properties*
                                    :key 'car
                                    :test 'string=)))))))

;; Returns a list of properties of GObject interface g-type. Each property is
;; described by an object of type param-spec. type is an
;; integer or a string specifying the GType

(defun interface-properties (gtype)
  (assert (type-is-a gtype +g-type-interface+))
  (let ((iface (type-default-interface-ref gtype)))
    (unwind-protect
      (cffi:with-foreign-object (n-props :uint)
        (let ((pspecs (%object-interface-list-properties iface n-props)))
          (unwind-protect
            (loop for count from 0 below (cffi:mem-ref n-props :uint)
                  for pspec = (cffi:mem-aref pspecs :pointer count)
                  collect (parse-g-param-spec pspec))
            (glib:free pspecs))))
      (type-default-interface-unref iface))))

;;; ----------------------------------------------------------------------------

;; Get the defintion of a GClass type

(defun get-g-class-definition (gtype &optional lisp-name-package)
  (when (and (stringp gtype)
             (null (ignore-errors (gtype gtype))))
    (let ((type-init-name (probable-type-init-name gtype)))
      (when (cffi:foreign-symbol-pointer type-init-name)
        (cffi:foreign-funcall-pointer
            (cffi:foreign-symbol-pointer type-init-name)
                                         ()
                                         :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype gtype)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package*
                                  *package*))
         (gtype (gtype gtype))
         (g-name (gtype-name gtype))
         (name (g-name->name g-name))
         (superclass-g-type (type-parent gtype))
         (superclass-name (g-name->name (gtype-name superclass-g-type)))
         (interfaces (type-interfaces gtype))
         (properties (class-properties gtype))
         (type-init-name (probable-type-init-name g-name))
         (own-properties
          (sort (copy-list (remove gtype
                                   properties
                                   :key #'%param-spec-owner-type
                                   :test-not #'g-type=))
                #'string< :key #'%param-spec-name)))
    `(define-g-object-class ,g-name ,name
         (:superclass ,superclass-name
          :export t
          :interfaces
          (,@(sort (mapcar #'gtype-name interfaces) 'string<))
          ,@(when (and (cffi:foreign-symbol-pointer type-init-name)
                       (not (cffi:null-pointer-p
                                (cffi:foreign-symbol-pointer type-init-name))))
              `(:type-initializer ,type-init-name)))
         (,@(mapcar (lambda (property)
                      (property->property-definition name property))
                    own-properties)
          ,@(mapcan (lambda (property-definition)
                      (if (eq :cond (car property-definition))
                          (list (make-instance 'print-readtime-condition
                                               :condition
                                               (cadr property-definition))
                                (cddr property-definition))
                          (list property-definition)))
                    (cdr (find g-name *additional-properties*
                                      :key 'car :test 'string=)))))))

;; Returns a list of properties of GObject class g-type. Each property
;; is described by an object of type param-spec. type is an
;; integer or a string specifying the GType

(defun class-properties (gtype)
  (assert (type-is-a gtype +g-type-object+))
  (let ((class (type-class-ref gtype)))
    (unwind-protect
      (cffi:with-foreign-object (n-props :uint)
        (let ((pspecs (%object-class-list-properties class n-props)))
          (unwind-protect
            (loop for count from 0 below (cffi:mem-ref n-props :uint)
                  for pspec = (cffi:mem-aref pspecs :pointer count)
                  collect (parse-g-param-spec pspec))
            (glib:free pspecs))))
      (type-class-unref class))))

;;; ----------------------------------------------------------------------------

(defvar *referenced-types*)

(defun generate-types-hierarchy-to-file (file root-type
                                              &key include-referenced
                                              prefix package exceptions
                                              prologue interfaces enums flags
                                              objects exclusions
                                              additional-properties)
  (if (not (streamp file))
      (with-open-file (stream file :direction :output :if-exists :supersede)
        (generate-types-hierarchy-to-file stream root-type
                                          :prefix prefix
                                          :package package
                                          :exceptions exceptions
                                          :prologue prologue
                                          :include-referenced include-referenced
                                          :interfaces interfaces
                                          :enums enums
                                          :flags flags
                                          :objects objects
                                          :exclusions exclusions
                                          :additional-properties
                                          additional-properties))
      (let* ((*generation-exclusions* (mapcar #'gtype exclusions))
             (*lisp-name-package* (or package *package*))
             (*package* *lisp-name-package*)
             (*strip-prefix* (or prefix ""))
             (*lisp-name-exceptions* exceptions)
             (*print-case* :downcase)
             (*additional-properties* additional-properties)
             (*generated-types* (make-hash-table :test 'equalp))
             (referenced-types (and include-referenced
                                    (filter-types-by-prefix
                                     (get-referenced-types root-type)
                                     prefix))))
        (setf exclusions (mapcar #'gtype exclusions))
        (when prologue
          (write-string prologue file)
          (terpri file))
        (when include-referenced
          (loop
            for interface in interfaces
            do
            (loop
              for referenced-type in (get-shallow-referenced-types interface)
              do (pushnew referenced-type referenced-types :test 'g-type=)))
          (loop
            for object in objects
            do
            (loop
              for referenced-type in (get-shallow-referenced-types object)
              do (pushnew referenced-type referenced-types :test 'g-type=)))
          (loop
             for enum-type in (filter-types-by-fund-type
                               referenced-types "GEnum")
             for def = (get-g-enum-definition enum-type)
             unless (member enum-type exclusions :test 'g-type=)
             do (format file "~S~%~%" def))

          (loop
             for flags-type in (filter-types-by-fund-type
                                referenced-types "GFlags")
             for def = (get-g-flags-definition flags-type)
             unless (member flags-type exclusions :test 'g-type=)
             do (format file "~S~%~%" def)))
        (loop
           with auto-enums = (and include-referenced
                                  (filter-types-by-fund-type
                                   referenced-types "GEnum"))
           for enum in enums
           for def = (get-g-enum-definition enum)
           unless (find enum auto-enums :test 'g-type=)
           do (format file "~S~%~%" def))
        (loop
           with auto-flags = (and include-referenced
                                  (filter-types-by-fund-type
                                   referenced-types "GFlags"))
           for flags-type in flags
           for def = (get-g-flags-definition flags-type)
           unless (find flags-type auto-flags :test 'g-type=)
           do (format file "~S~%~%" def))
        (loop
           for interface in interfaces
           for def = (get-g-interface-definition interface)
           do (format file "~S~%~%" def))
        (loop
           for def in (get-g-class-definitions-for-root root-type)
           do (format file "~S~%~%" def))
        (iter (for object in objects)
              (unless (gethash (gtype-name (gtype object)) *generated-types*)
                (for def = (get-g-class-definition object))
                (format file "~S~%~%" def))))))

;;; ----------------------------------------------------------------------------

;; Helper functions for generate-types-hierarchy-to-file

(defun get-g-class-definitions-for-root (type)
  (setf type (gtype type))
  (get-g-class-definitions-for-root-1 type))

(defun get-g-class-definitions-for-root-1 (gtype)
  (unless (member (gtype gtype) *generation-exclusions* :test 'g-type=)
    (iter (when (first-iteration-p)
            (unless (and *generated-types*
                         (gethash (gtype-name (gtype gtype)) *generated-types*))
              (appending (list (get-g-class-definition gtype)))))
          (for child-type in (sort (copy-list (type-children gtype))
                                   #'string< :key #'gtype-name))
          (appending (get-g-class-definitions-for-root-1 child-type)))))

;;; ----------------------------------------------------------------------------

(defun get-referenced-types-1 (type)
  (setf type (gtype type))
  (loop
     for property-type in (sort (copy-list (get-shallow-referenced-types type))
                                #'string> :key #'gtype-name)
     do (pushnew property-type *referenced-types* :test 'g-type=))
  (loop
     for type in (sort (copy-list (type-children type))
                       #'string< :key #'gtype-name)
     do (get-referenced-types-1 type)))

(defun get-referenced-types (root-type)
  (let (*referenced-types*)
    (get-referenced-types-1 (gtype root-type))
    *referenced-types*))

;;; ----------------------------------------------------------------------------

(defun filter-types-by-prefix (types prefix)
  (remove-if-not
   (lambda (type)
     (starts-with (gtype-name (gtype type)) prefix))
   types))

(defun filter-types-by-fund-type (types fund-type)
  (setf fund-type (gtype fund-type))
  (remove-if-not
   (lambda (type)
     (equal (type-fundamental (gtype type)) fund-type))
   types))

;;; ----------------------------------------------------------------------------

(defun get-shallow-referenced-types (type)
  (setf type (gtype type))
  (remove-duplicates (sort (loop
                             for property in (class-or-interface-properties type)
                             when (g-type= type (%param-spec-owner-type property))
                             collect (%param-spec-type property))
                           #'string<
                           :key #'gtype-name)
                     :test 'equal))

(defun class-or-interface-properties (type)
  (setf type (gtype type))
  (cond
    ((g-type= (type-fundamental type) (gtype +g-type-object+))
     (class-properties type))
    ((g-type= (type-fundamental type) (gtype +g-type-interface+))
     (interface-properties type))))

;;; --- End of file gobject.utils.lisp -----------------------------------------
