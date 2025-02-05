(in-package :glib-test)

(def-suite gobject-gvalue :in gobject-suite)
(in-suite gobject-gvalue)

;;;     glib:with-value

(test with-value-macro.1
  (gobject:with-value (gvalue)
    (is (cffi:pointerp gvalue))
    (is (cffi:pointerp (g:value-init gvalue "gint")))
    (is (eq (g:gtype "gint") (g:value-type gvalue)))))

(test with-value-macro.2
  (gobject:with-value (gvalue "gint")
    (is (cffi:pointerp gvalue))
    (is (eq (g:gtype "gint") (g:value-type gvalue)))
    (is (= 0 (g:value-get gvalue)))))

(test with-value-macro.3
  (gobject:with-value (gvalue "gint" 199)
    (is (cffi:pointerp gvalue))
    (is (eq (g:gtype "gint") (g:value-type gvalue)))
    (is (= 199 (g:value-get gvalue)))))

;;;     with-values

(test with-values-macro
  (gobject:with-values (gvalue1
                        (gvalue2 "gint")
                        (gvalue3 "gint" 199))
    (is (cffi:pointerp gvalue1))
    (is (cffi:pointerp gvalue2))
    (is (eq (g:gtype "gint") (g:value-type gvalue2)))
    (is (= 0 (g:value-get gvalue2)))
    (is (cffi:pointerp gvalue3))
    (is (eq (g:gtype "gint") (g:value-type gvalue3)))
    (is (= 199 (g:value-get gvalue3)))))

;;;     GValue

(test g-value-class
  (is (= 24 (cffi:foreign-type-size '(:struct g:value))))
  (is (equal '(:data :gtype)
              (sort (cffi:foreign-slot-names '(:struct g:value))
                    #'string-lessp))))

;;;     G_VALUE_HOLDS

(test g-value-holds
  (g:with-value (value "gint")
    (is-false (g:value-holds value "gboolean"))
    (is-true  (g:value-holds value "gint"))))

;;;     G_VALUE_TYPE

(test g-value-type
  (gobject:with-value (value "gint")
    (is (eq (g:gtype "gint") (g:value-type value)))))

;;;     G_VALUE_TYPE_NAME

(test g-value-type-name
  (gobject:with-value (value "gint")
    (is (string= "gint" (g:value-type-name value)))))

;;;     G_TYPE_IS_VALUE

(test g-type-is-value
  (is-true (g:type-is-value "gint"))
  (is-true (g:type-is-value "GSimpleAction"))
  (is-false (g:type-is-value "GBoxed")))

;;;     G_TYPE_IS_VALUE_ABSTRACT                            not implemented
;;;     G_IS_VALUE                                          not implemented
;;;     G_TYPE_VALUE                                        not implemented
;;;     G_TYPE_VALUE_ARRAY                                  not implemented

;;;     g_value_init

(test g-value-init.1
  (cffi:with-foreign-object (value '(:struct g:value))
    (is-true (cffi:pointerp (g:value-init value)))
    (is-false (cffi:foreign-slot-value value '(:struct g:value) :gtype))
    (is-true  (cffi:foreign-slot-value value '(:struct g:value) :data))

    (is-true (cffi:pointerp (g:value-init value "gint")))
    (is (eq (g:gtype "gint")
            (cffi:foreign-slot-value value '(:struct g:value) :gtype)))
    (is-true  (cffi:foreign-slot-value value '(:struct g:value) :data))

    (is (=  0 (g:value-get value)))
    (is (= 10 (g:value-set value 10 "gint")))
    (is (= 10 (g:value-get value)))))

(test g-value-init.2
  (cffi:with-foreign-object (value '(:struct g:value))
    (is (cffi:pointer-eq value (g:value-init value "gint")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (cffi:pointer-eq value (g:value-init value "guint")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (cffi:pointer-eq value (g:value-init value "glong")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (cffi:pointer-eq value (g:value-init value "gulong")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (cffi:pointer-eq value (g:value-init value "gint64")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (cffi:pointer-eq value (g:value-init value "guint64")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (cffi:pointer-eq value (g:value-init value "gfloat")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (cffi:pointer-eq value (g:value-init value "gdouble")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (cffi:pointer-eq value (g:value-init value "gpointer")))
    (is (cffi:null-pointer-p (g:value-get value)))))

;;;     g_value_copy

(test g-value-copy
  (gobject:with-values ((value1 "gint") (value2 "gint"))
    (is (=   0 (g:value-get value1)))
    (is (= 100 (g:value-set value1 100 "gint")))
    (is (=  0  (g:value-get value2)))
    (is-false (g:value-copy value1 value2))
    (is (= 100 (g:value-get value2)))))

;;;     g_value_reset

(test g-value-reset
  (gobject:with-value (value "gint")
    (is (=  0 (g:value-get value)))
    (is (= 10 (g:value-set value 10 "gint"))
    (is (= 10 (g:value-get value)))
    (is-false (g:value-reset value))
    (is (= 0 (g:value-get value))))))

;;;     g_value_unset

(test g-value-unset
  (cffi:with-foreign-object (value '(:struct g:value))
    (is-true (cffi:pointerp (g:value-init value "gint")))
    (is (=  0 (g:value-get value)))
    (is (= 10 (g:value-set value 10 "gint")))
    (is (= 10 (g:value-get value)))
    (is-false (g:value-unset value))
    (is-false (cffi:foreign-slot-value value '(:struct g:value) :gtype))))

;;;     g-value-get
;;;     g-value-set

(test g-value-get
  (g:with-value (value "gint" 11)
    (is (= 11 (g:value-get value)))
    (is (= 11 (g:value-get value "gint")))
    (is (= 22 (setf (g:value-get value) 22)))
    (is (= 22 (g:value-get value)))
    (is (= 22 (g:value-get value "gint")))
    (is (= 33 (setf (g:value-get value "gint") 33)))
    (is (= 33 (g:value-get value)))
    (is (= 33 (g:value-get value "gint")))))

(test g-value-get/set.1
  (gobject:with-value (value "gint")
    (is (= 99 (g:value-set value 99 "gint")))
    (is (= 99 (g:value-get value)))
    (is (= 99 (g:value-set value 99 "guint")))
    (is (= 99 (g:value-get value)))
    (is (= 99.0d0 (g:value-set value 99.0d0 "gdouble")))
    (is (= 99.0d0 (g:value-get value)))))

;; Check handling of values of GValue type

(test g-value-get/set.2
  (gobject:with-values ((value1 "gint") (value2 "GValue"))
    ;; Init a GValue of "gint" type
    (is (= 123 (g:value-set value1 123 "gint")))
    (is (= 123 (g:value-get value1)))
    ;; Store value1 in a GValue of "GValue" type
    (is (eq (g:gtype "gint")
            (g:value-type (g:value-set value2 value1 "GValue"))))
    (is (eq (g:gtype "GValue") (g:value-type value2)))
    (is (eq (g:gtype "gint") (g:value-type (g:value-get value2))))
    ;; Get the value from value1 stored in value2
    (is (= 123 (g:value-get (g:value-get value2))))))

;;;     g_value_set_instance                                not implemented
;;;     g_value_fits_pointer                                not implemented
;;;     g_value_peek_pointer                                not implemented

;;;     g_value_type_compatible

(test g-value-type-compatible
  (is-false (g:value-type-compatible "gint" "gdouble"))
  (is-false (g:value-type-compatible "gint" "glong"))
  (is-false (g:value-type-compatible "gint" "gchararray")))

;;;     g_value_type_transformable                          not implemented
;;;     g_value_transform                                   not implemented
;;;     g_value_register_transform_func                     not implemented

;;;     g_strdup_value_contents

(test g-strdup-value-contents
  (g:with-values ((value1 "gboolean" nil)
                  (value2 "gint" 199)
                  (value3 "gdouble" 2.0)
                  (value4 "gchararray" "string"))
    (is (string= "FALSE" (g:strdup-value-contents value1)))
    (is (string= "199" (g:strdup-value-contents value2)))
    (is (string= "2.000000" (g:strdup-value-contents value3)))
    (is (string= "\"string\"" (g:strdup-value-contents value4)))))

;;; ----------------------------------------------------------------------------

;; Example for the documentation

(defun example-gvalue ()
  ;; Declare two variables of type g:value
  (gobject:with-values (value1 value2)
    ;; Initialization, setting and reading a value of type g:value
    (g:value-set value1 "string" "gchararray")
    (format t "value1 = ~a~%" (g:value-get value1))
    (format t "gtype  = ~a~%" (g:value-type value1))
    (format t "name   = ~a~%~%" (g:value-type-name value1))

    ;; The same for the second value
    (g:value-init value2 "gchararray")
    (setf (g:value-get value2) "a second string")
    (format t "value2 = ~a~%" (g:value-get value2))
    (format t "gtype  = ~a~%" (g:value-type value2))
    (format t "name   = ~a~%~%" (g:value-type-name value2))

    ;; Reuse value1 for an integer
    (g:value-unset value1)
    (g:value-set value1 42 "gint" )
    (format t "value1 = ~a~%" (g:value-get value1))
    (format t "gtype  = ~a~%" (g:value-type value1))
    (format t "name   = ~a~%~%" (g:value-type-name value1))

    ;; Some test functions
    (assert (g:value-holds value1 "gint"))
    (format t "value holds integer is ~a~%" (g:value-holds value1 "gint"))
    (format t "value is integer ~a~%~%" (g:type-is-value "gint"))))

;;; 2024-12-21
