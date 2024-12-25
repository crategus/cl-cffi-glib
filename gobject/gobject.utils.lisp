;;; ----------------------------------------------------------------------------
;;; gobject.utils.lisp
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

(defvar *generation-exclusions* nil)
(defvar *additional-properties* nil)

;;; ----------------------------------------------------------------------------

(defun probable-type-initializer (gname)
  (with-output-to-string (stream)
    (iter (with start = 0)
          (for end = (position-if (lambda (x) (upper-case-p x))
                                  gname
                                  :start (1+ start)))
          (while end)
          (finally (write-string (string-downcase (subseq gname start)) stream)
                   (write-string "_get_type" stream))
          (write-string (string-downcase (subseq gname start end)) stream)
          (write-char #\_ stream)
          (setf start end))))

(defun maybe-call-type-initializer (gname)
  (let* ((*warn-unknown-gtype* nil)
         (gtype (glib:gtype gname)))
    (if (and (stringp gname) (null gtype))
        (let* ((type-initializer (probable-type-initializer gname))
               (func (cffi:foreign-symbol-pointer type-initializer)))
          (when func
            (glib:gtype (cffi:foreign-funcall-pointer func () :size))))
        gtype)))

;;; ----------------------------------------------------------------------------

;; Generate Lisp symbols names for C names

(defvar *strip-prefix* "")
(defvar *lisp-name-package* nil)

(defun starts-with (name prefix)
  (and prefix
       (> (length name) (length prefix))
       (string-equal (subseq name 0 (length prefix)) prefix)))

(defun strip-start (name prefix)
  (if (starts-with name prefix)
      (subseq name (length prefix))
      name))

(defun lispify-name (name)
  (with-output-to-string (stream)
    (iter (for c in-vector (strip-start name *strip-prefix*))
          (for firstp initially t then nil)
          (when (and (not firstp) (upper-case-p c)) (write-char #\- stream))
          (write-char (char-upcase c) stream))))

(defun gname->name (name)
  (or (get-lisp-name-exception name)
      (glib:symbol-for-gtype name)
      (intern (string-upcase (lispify-name name)) *lisp-name-package*)))

(defun accessor-name (class-name property-name)
  (intern (format nil "~A-~A"
                      (symbol-name class-name)
                      (string-upcase property-name))
          *lisp-name-package*))

;;; ----------------------------------------------------------------------------

;; Get the definition of a GEnum type

;; A structure describing a single enumeration item
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
  (assert (type-is-enum gtype))
  (let ((gclass (type-class-ref gtype)))
    (unwind-protect
      (iter (with n = (cffi:foreign-slot-value gclass
                                               '(:struct enum-class)
                                               :n-values))
            (with values = (cffi:foreign-slot-value gclass
                                                    '(:struct enum-class)
                                                    :values))
            (for i from 0 below n)
            (for value = (cffi:mem-aptr values '(:struct enum-value) i))
            (collect
              (make-enum-item :name
                              (cffi:foreign-slot-value value
                                                       '(:struct enum-value)
                                                       :name)
                              :value
                              (cffi:foreign-slot-value value
                                                       '(:struct enum-value)
                                                       :value)
                              :nick
                              (cffi:foreign-slot-value value
                                                       '(:struct enum-value)
                                                       :nick))))
      (type-class-unref gclass))))

(defun enum-value->definition (enum-value)
  (let ((value-name (intern (lispify-name (enum-item-nick enum-value))
                            (find-package :keyword)))
        (numeric-value (enum-item-value enum-value)))
    `(,value-name ,numeric-value)))

(defun get-genum-definition (gtype &optional lisp-name-package)
  (let ((gtype (maybe-call-type-initializer gtype)))
    (when *generated-types*
      (setf (gethash (glib:gtype-name gtype) *generated-types*) t))
    (let* ((*lisp-name-package* (or lisp-name-package
                                    *lisp-name-package*
                                    (package-name *package*)))
           (*strip-prefix* (or lisp-name-package *lisp-name-package* ""))
           (gname (glib:gtype-name gtype))
           (name (gname->name gname))
           (items (get-enum-items gtype))
           (type-initializer (probable-type-initializer gname)))
      `(define-genum ,gname ,name
           (:export t
            ,@(when (cffi:foreign-symbol-pointer type-initializer)
                (list :type-initializer type-initializer)))
           ,@(mapcar #'enum-value->definition items)))))

;;; ----------------------------------------------------------------------------

;; A structure describing a single flags item
(defstruct flags-item
  name
  value
  nick)

;; Gets the list of flags items that belong to the GFlags type.
;; gtype is a string or an integer specifying the GFlags type.
;; Returns a list of flags-item objects
(defun get-flags-items (gtype)
  (assert (type-is-flags gtype))
  (let ((gclass (type-class-ref gtype)))
    (unwind-protect
      (iter (with n = (cffi:foreign-slot-value gclass
                                               '(:struct flags-class)
                                               :n-values))
            (with values = (cffi:foreign-slot-value gclass
                                                    '(:struct flags-class)
                                                    :values))
            (for i from 0 below n)
            (for value = (cffi:mem-aptr values '(:struct flags-value) i))
            (collect
              (make-flags-item :name
                               (cffi:foreign-slot-value value
                                                        '(:struct flags-value)
                                                        :name)
                               :value
                               (cffi:foreign-slot-value value
                                                        '(:struct flags-value)
                                                        :value)
                               :nick
                               (cffi:foreign-slot-value value
                                                        '(:struct flags-value)
                                                        :nick))))
      (type-class-unref gclass))))

(defun flags-value->definition (flags-value)
  (let ((value-name (intern (lispify-name (flags-item-nick flags-value))
                            (find-package :keyword)))
        (numeric-value (flags-item-value flags-value)))
    `(,value-name ,numeric-value)))

(defun get-gflags-definition (gtype &optional lisp-name-package)
  (let ((gtype (maybe-call-type-initializer gtype)))
    (when *generated-types*
      (setf (gethash (glib:gtype-name gtype) *generated-types*) t))
    (let* ((*lisp-name-package* (or lisp-name-package
                                    *lisp-name-package*
                                    (package-name *package*)))
           (gname (glib:gtype-name gtype))
           (name (gname->name gname))
           (items (get-flags-items gtype))
           (type-initializer (probable-type-initializer gname)))
      `(define-gflags ,gname ,name
           (:export t
            ,@(when (cffi:foreign-symbol-pointer type-initializer)
                (list :type-initializer type-initializer)))
         ,@(mapcar #'flags-value->definition items)))))

;;; ----------------------------------------------------------------------------

;; Helper functions for getting the definitions

(defun property->property-definition (class-name property)
  (let (; Do not lispify the name of a property
        ;(name (gname->name (%param-spec-name property)))
        (name (intern (string-upcase (%param-spec-name property))))
        (accessor-name (accessor-name class-name (%param-spec-name property)))
        (gname (%param-spec-name property))
        (gtype (glib:gtype-name (%param-spec-type property)))
        (readable (%param-spec-readable property))
        (writable (and (%param-spec-writable property)
                       (not (%param-spec-constructor-only property)))))
    `(,name ,accessor-name ,gname ,gtype ,readable ,writable)))

;;; ----------------------------------------------------------------------------
;; Get the defintion of a GInterface type
;;; ----------------------------------------------------------------------------

;; A Helper method

(defclass print-readtime-condition ()
  ((condition :initarg :condition)))

(defmethod print-object ((o print-readtime-condition) stream)
  (format stream "#~A" (slot-value o 'condition)))

;;; ----------------------------------------------------------------------------

(defun get-ginterface-definition (interface &optional lisp-name-package)
  (let ((gtype (maybe-call-type-initializer interface)))
    (when *generated-types*
      (setf (gethash (glib:gtype-name gtype) *generated-types*) t))
    (let* ((*lisp-name-package* (or lisp-name-package
                                    *lisp-name-package*
                                    (package-name *package*)))
           (gname (glib:gtype-name gtype))
           (name (gname->name gname))
           (properties (sort (copy-list (interface-properties gtype))
                             #'string< :key #'%param-spec-name))
           (type-initializer (probable-type-initializer gname)))
      `(define-ginterface ,gname ,name
           (:export t
            ,@(when (cffi:foreign-symbol-pointer type-initializer)
                `(:type-initializer ,type-initializer)))
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
                             (cdr (find gname *additional-properties*
                                        :key 'car
                                        :test 'string=))))))))

;; Returns the list of properties for an interface. Each property is described
;; by an instance of type G:PARM-SPEC.
;; TODO: This function duplicates the G:OBJECT-INTERFACE-LIST-PROPERTIES
;; function.
(defun interface-properties (gtype)
  (assert (type-is-interface gtype))
  (let ((iface (type-default-interface-ref gtype)))
    (unwind-protect
      (cffi:with-foreign-object (nprops :uint)
        (let ((pspecs (%object-interface-list-properties iface nprops)))
          (unwind-protect
            (iter (for n from 0 below (cffi:mem-ref nprops :uint))
                  (for pspec = (cffi:mem-aref pspecs :pointer n))
                  (collect (parse-param-spec pspec)))
            (glib:free pspecs))))
      (type-default-interface-unref iface))))

;;; ----------------------------------------------------------------------------

;; Get the defintion of a GClass type

(defun get-gobject-definition (gtype &optional lisp-name-package)
  (let ((gtype (maybe-call-type-initializer gtype)))
    (when *generated-types*
      (setf (gethash (glib:gtype-name gtype) *generated-types*) t))
    (let* ((*lisp-name-package* (or lisp-name-package
                                    *lisp-name-package*
                                    (package-name *package*)))
           (*strip-prefix* (or lisp-name-package *lisp-name-package* ""))
           (gname (glib:gtype-name gtype))
           (name (gname->name gname))
           (superclass-g-type (type-parent gtype))
           (superclass-name (gname->name (glib:gtype-name superclass-g-type)))
           (interfaces (type-interfaces gtype))
           (properties (class-properties gtype))
           (type-initializer (probable-type-initializer gname))
           (own-properties
            (sort (copy-list (remove gtype
                                     properties
                                     :key #'%param-spec-owner-type
                                     :test-not #'eq))
                  #'string< :key #'%param-spec-name)))
      `(define-gobject ,gname ,name
           (:superclass ,superclass-name
            :export t
            :interfaces
            (,@(sort (mapcar #'glib:gtype-name interfaces) 'string<))
            ,@(when (and (cffi:foreign-symbol-pointer type-initializer)
                         (not (cffi:null-pointer-p
                                  (cffi:foreign-symbol-pointer type-initializer))))
                `(:type-initializer ,type-initializer)))
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
                      (cdr (find gname *additional-properties*
                                       :key 'car :test 'string=))))))))

;; Returns a list of properties of GObject class g-type. Each property
;; is described by an object of type param-spec. type is an
;; integer or a string specifying the GType

(defun class-properties (gtype)
  (assert (type-is-object gtype))
  (let ((cclass (type-class-ref gtype)))
    (unwind-protect
      (cffi:with-foreign-object (nprops :uint)
        (let ((pspecs (%object-class-list-properties cclass nprops)))
          (unwind-protect
            (iter (for n from 0 below (cffi:mem-ref nprops :uint))
                  (for pspec = (cffi:mem-aref pspecs :pointer n))
                  (collect (parse-param-spec pspec)))
            (glib:free pspecs))))
      (type-class-unref cclass))))

;;; ----------------------------------------------------------------------------

;; Get the definition of a GType

(defun get-gtype-definition (gtype &optional lisp-name-package)
  (let ((gtype (maybe-call-type-initializer gtype)))
    (cond ((type-is-a gtype (glib:gtype "GEnum"))
           (get-genum-definition gtype lisp-name-package))
          ((type-is-a gtype (glib:gtype "GFlags"))
           (get-gflags-definition gtype lisp-name-package))
          ((type-is-a gtype (glib:gtype "GInterface"))
           (get-ginterface-definition gtype lisp-name-package))
          ((type-is-a gtype (glib:gtype "GObject"))
           (get-gobject-definition gtype lisp-name-package))
          (t
           (error "Do not know how to generate type definition for ~A type ~A"
                  (glib:gtype-name (type-fundamental gtype))
                  (or (ignore-errors (glib:gtype-name (glib:gtype gtype)))
                      gtype))))))

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
      (let* ((*generation-exclusions* (mapcar #'glib:gtype exclusions))
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
        (setf exclusions (mapcar #'glib:gtype exclusions))
        (when prologue
          (write-string prologue file)
          (terpri file))
        (when include-referenced
          (loop
            for interface in interfaces
            do
            (loop
              for referenced-type in (get-shallow-referenced-types interface)
              do (pushnew referenced-type referenced-types :test #'eq)))
          (loop
            for object in objects
            do
            (loop
              for referenced-type in (get-shallow-referenced-types object)
              do (pushnew referenced-type referenced-types :test #'eq)))
          (loop
             for enum-type in (filter-types-by-fund-type
                               referenced-types "GEnum")
             for def = (get-genum-definition enum-type)
             unless (member enum-type exclusions :test #'eq)
             do (format file "~S~%~%" def))

          (loop
             for flags-type in (filter-types-by-fund-type
                                referenced-types "GFlags")
             for def = (get-gflags-definition flags-type)
             unless (member flags-type exclusions :test #'eq)
             do (format file "~S~%~%" def)))
        (loop
           with auto-enums = (and include-referenced
                                  (filter-types-by-fund-type
                                   referenced-types "GEnum"))
           for enum in enums
           for def = (get-genum-definition enum)
           unless (find enum auto-enums :test #'eq)
           do (format file "~S~%~%" def))
        (loop
           with auto-flags = (and include-referenced
                                  (filter-types-by-fund-type
                                   referenced-types "GFlags"))
           for flags-type in flags
           for def = (get-gflags-definition flags-type)
           unless (find flags-type auto-flags :test #'eq)
           do (format file "~S~%~%" def))
        (loop
           for interface in interfaces
           for def = (get-ginterface-definition interface)
           do (format file "~S~%~%" def))
        (loop
           for def in (get-g-class-definitions-for-root root-type)
           do (format file "~S~%~%" def))
        (iter (for object in objects)
              (unless (gethash (glib:gtype-name (glib:gtype object))
                               *generated-types*)
                (for def = (get-gobject-definition object))
                (format file "~S~%~%" def))))))

;;; ----------------------------------------------------------------------------

;; Helper functions for generate-types-hierarchy-to-file

(defun get-g-class-definitions-for-root (gtype)
  (setf gtype (glib:gtype gtype))
  (get-g-class-definitions-for-root-1 gtype))

(defun get-g-class-definitions-for-root-1 (gtype)
  (unless (member (glib:gtype gtype) *generation-exclusions* :test #'eq)
    (iter (when (first-iteration-p)
            (unless (and *generated-types*
                         (gethash (glib:gtype-name (glib:gtype gtype))
                                  *generated-types*))
              (appending (list (get-gobject-definition gtype)))))
          (for child-type in (sort (copy-list (type-children gtype))
                                   #'string< :key #'glib:gtype-name))
          (appending (get-g-class-definitions-for-root-1 child-type)))))

;;; ----------------------------------------------------------------------------

(defun get-shallow-referenced-types (gtype)
  (setf gtype (glib:gtype gtype))
  (remove-duplicates (sort (loop
                             for property in (class-or-interface-properties gtype)
                             when (eq gtype
                                      (%param-spec-owner-type property))
                             collect (%param-spec-type property))
                           #'string<
                           :key #'glib:gtype-name)
                     :test 'equal))

(defun get-referenced-types-1 (gtype)
  (setf gtype (glib:gtype gtype))
  (loop
     for property-type in (sort (copy-list (get-shallow-referenced-types gtype))
                                #'string> :key #'glib:gtype-name)
     do (pushnew property-type *referenced-types* :test #'eq))
  (loop
     for gtype in (sort (copy-list (type-children gtype))
                       #'string< :key #'glib:gtype-name)
     do (get-referenced-types-1 gtype)))

(defun get-referenced-types (root-type)
  (let (*referenced-types*)
    (get-referenced-types-1 (glib:gtype root-type))
    *referenced-types*))

;;; ----------------------------------------------------------------------------

(defun filter-types-by-prefix (types prefix)
  (remove-if-not
   (lambda (gtype)
     (starts-with (glib:gtype-name (glib:gtype gtype)) prefix))
   types))

(defun filter-types-by-fund-type (types fund-type)
  (setf fund-type (glib:gtype fund-type))
  (remove-if-not
   (lambda (gtype)
     (eq (type-fundamental (glib:gtype gtype)) fund-type))
   types))

;;; ----------------------------------------------------------------------------

(defun class-or-interface-properties (gtype)
  (setf gtype (glib:gtype gtype))
  (cond ((type-is-object gtype)
         (class-properties gtype))
        ((type-is-interface gtype)
         (interface-properties gtype))))

;;; --- End of file gobject.utils.lisp -----------------------------------------
