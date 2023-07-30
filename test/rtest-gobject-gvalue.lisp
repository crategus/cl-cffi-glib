(in-package :glib-test)

(def-suite gobject-gvalue :in gobject-suite)
(in-suite gobject-gvalue)

;;;     GValue

(test g-value-class
  (is (= 24 (cffi:foreign-type-size '(:struct g:value))))
  (is (equal '(:data :gtype)
              (sort (cffi:foreign-slot-names '(:struct g:value))
                    #'string-lessp))))

;;;     G_VALUE_INIT                             * not implemented *

;;;     G_VALUE_HOLDS

(test g-value-holds
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "gint")
    (is-false (g:value-holds value "gboolean"))
    (is-true  (g:value-holds value "gint"))))

;;;     G_VALUE_TYPE

(test g-value-type
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "gint")
    (is (eq (g:gtype "gint") (g:value-type value)))))

;;;     G_VALUE_TYPE_NAME

(test g-value-type-name
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value "gint")
    (is (string= "gint" (g:value-type-name value)))))

;;;     G_TYPE_IS_VALUE

(test g-type-is-value
  (is-true (g:type-is-value "gint"))
  (is-true (g:type-is-value "GSimpleAction"))
  (is-false (g:type-is-value "GBoxed")))

;;;     G_TYPE_IS_VALUE_ABSTRACT

(test g-type-is-value-abstract
  (is-true (g:type-is-value "gint"))
  (is-true (g:type-is-value "GSimpleAction"))
  (is-false (g:type-is-value "GBoxed")))

;;;     G_IS_VALUE                               * not implemented *

;;;     G_TYPE_VALUE

(test g-type-value
  (is (eq (g:gtype "GValue") (g:type-value))))

;;;     G_TYPE_VALUE_ARRAY                       * not implemented *

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
    (is (eq value (g:value-init value "gint")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (eq value (g:value-init value "guint")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (eq value (g:value-init value "glong")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (eq value (g:value-init value "gulong")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (eq value (g:value-init value "gint64")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (eq value (g:value-init value "guint64")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (eq value (g:value-init value "gfloat")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (eq value (g:value-init value "gdouble")))
    (is (= 0 (g:value-get value)))
    (is-false (g:value-unset value))
    (is (eq value (g:value-init value "gpointer")))
    (is (cffi:null-pointer-p (g:value-get value)))))

;;;     g_value_copy

(test g-value-copy
  (cffi:with-foreign-objects ((value1 '(:struct g:value))
                              (value2 '(:struct g:value)))
    (is (eq value1 (g:value-init value1 "gint")))
    (is (eq value2 (g:value-init value2 "gint")))
    (is (=   0 (g:value-get value1)))
    (is (= 100 (g:value-set value1 100 "gint")))
    (is (=  0  (g:value-get value2)))
    (is-false (g:value-copy value1 value2))
    (is (= 100 (g:value-get value2)))))

;;;     g_value_reset

(test g-value-reset
  (cffi:with-foreign-object (value '(:struct g:value))
    (is-true (cffi:pointerp (g:value-init value "gint")))
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

(test g-value-get/set.1
  (cffi:with-foreign-object (value '(:struct g:value))
    (is (= 99 (g:value-set value 99 "gint")))
    (is (= 99 (g:value-get value)))
    (is (= 99 (g:value-set value 99 "guint")))
    (is (= 99 (g:value-get value)))
    (is (= 99.0d0 (g:value-set value 99.0d0 "gdouble")))
    (is (= 99.0d0 (g:value-get value)))))

;; Check the handling of values of GValue type
(test g-value-get/set.2
  (cffi:with-foreign-objects ((value1 '(:struct g:value))
                              (value2 '(:struct g:value)))
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

;;;     g_value_set_instance
;;;     g_value_fits_pointer                     * not implemented *
;;;     g_value_peek_pointer                     * not implemented *
;;;     g_value_type_compatible
;;;     g_value_type_transformable
;;;     g_value_transform
;;;     g_value_register_transform_func
;;;     g_strdup_value_contents

;;; ----------------------------------------------------------------------------

;; Example for the documentation

;; A transformation from an integer to a string
(cffi:defcallback int2string :void ((src (:pointer (:struct g:value)))
                                    (dest (:pointer (:struct g:value))))
  (if (= (g:value-int src) 42)
      (setf (g:value-string dest) "An important number")
      (setf (g:value-string dest) "What is that?")))

(defun example-g-value ()
  ;; Declare two variables of type g:value.
  (cffi:with-foreign-objects ((value1 '(:struct g:value))
                              (value2 '(:struct g:value)))

    ;; Initialization, setting and reading a value of type g:value
    (g:value-init value1 "gchararray")
    (setf (g:value-string value1) "string")
    (format t "value1 = ~A~%" (g:value-string value1))
    (format t "gtype  = ~A~%" (g:value-type value1))
    (format t "name   = ~A~%~%" (g:value-type-name value1))

    ;; The same in one step with the G:VALUE-SET function
    (g:value-set value2 "a second string" "gchararray")
    (format t "value2 = ~A~%" (gobject:parse-g-value value2))
    (format t "gtype  = ~A~%" (g:value-type value2))
    (format t "name   = ~A~%~%" (g:value-type-name value2))

    ;; Reuse value1 for an integer value.
    (g:value-unset value1)
    (g:value-init value1 "gint")
    (setf (g:value-int value1) 42)
    (format t "value1 = ~A~%" (g:value-get value1))
    (format t "gtype  = ~A~%" (g:value-type value1))
    (format t "name   = ~A~%~%" (g:value-type-name value1))

    ;; The types integer and string are transformable.
    (assert (g:value-type-transformable "gint" "gchararray"))

    ;; Transform value1 of type integer into value2 which is a string
    (g:value-transform value1 value2)
    (format t "value1 = ~A~%" (g:value-get value1))
    (format t "value2 = ~A~%~%" (g:value-get value2))

    ;; Some test functions.
    (assert (g:value-holds value1 "gint"))
    (format t "value-holds is ~A~%" (g:value-holds value1 "gint"))
    (format t "is-value is ~A~%~%" (g:type-is-value "gint"))

    ;; Reuse value2 again for a string.
    (g:value-unset value2)
    (g:value-init value2 "gchararray")
    (setf (g:value-string value2) "string")
    (format t "value2 = ~A~%" (g:value-get value2))

    ;; Register the transformation int2string
    (g:value-register-transform-func "gint"
                                     "gchararray"
                                     (cffi:callback int2string))
    ;; Try the transformation
    (g:value-transform value1 value2)
    (format t "value2 = ~A~%~%" (g:value-get value2))))

;;; --- 2023-7-10 --------------------------------------------------------------
