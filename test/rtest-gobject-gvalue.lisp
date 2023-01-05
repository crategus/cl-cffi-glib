(in-package :glib-test)

(def-suite gobject-gvalue :in gobject-suite)
(in-suite gobject-gvalue)

;;;     GValue

(test value-class
   #-windows
  (is (= 24 (cffi:foreign-type-size '(:struct g:value))))
   #+windows
  (is (= 24 (cffi:foreign-type-size '(:struct g:value))))
  (is (equal '(:data :type)
              (stable-sort (cffi:foreign-slot-names '(:struct g:value))
                           #'string-lessp))))

;;;     G_VALUE_INIT                             * not implemented *

;;;     G_VALUE_HOLDS

(test value-holds
  (with-foreign-object (value '(:struct g:value))
    (g:value-init value "gint")
    (is-false (g:value-holds value "gboolean"))
    (is-true  (g:value-holds value "gint"))))

;;;     G_VALUE_TYPE

(test value-type
  (with-foreign-object (value '(:struct g:value))
    (g:value-init value "gint")
    (is (eq (g:gtype "gint") (g:value-type value)))))

;;;     G_VALUE_TYPE_NAME

(test value-type-name
  (with-foreign-object (value '(:struct g:value))
    (g:value-init value "gint")
    (is (string= "gint" (g:value-type-name value)))))

;;;     G_TYPE_IS_VALUE

(test type-is-value
  (is-true (g:type-is-value "gint"))
  (is-true (g:type-is-value "GSimpleAction"))
  (is-false (g:type-is-value "GBoxed")))

;;;     G_TYPE_IS_VALUE_ABSTRACT

(test type-is-value-abstract
  (is-true (g:type-is-value "gint"))
  (is-true (g:type-is-value "GSimpleAction"))
  (is-false (g:type-is-value "GBoxed")))

;;;     G_IS_VALUE                               * not implemented *

;;;     G_TYPE_VALUE

(test type-value
  (is (eq (g:gtype "GValue") (g:type-value))))

;;;     G_TYPE_VALUE_ARRAY                       * not implemented *

;;;     g_value_init

(test value-init
  (with-foreign-object (value '(:struct g:value))
    (is-true (cffi:pointerp (g:value-init value)))
    (is-false (cffi:foreign-slot-value value '(:struct g:value) :type))
    (is-true  (cffi:foreign-slot-value value '(:struct g:value) :data))

    (is-true (cffi:pointerp (g:value-init value "gint")))
    (is (eq (g:gtype "gint")
        (cffi:foreign-slot-value value '(:struct g:value) :type)))
    (is-true  (cffi:foreign-slot-value value '(:struct g:value) :data))

    (is (= 0 (gobject::parse-g-value value)))
    (gobject::set-g-value value 10 "gint")
    (is (= 10 (gobject::parse-g-value value)))))

;;;     g_value_copy

(test value-copy
  (with-foreign-objects ((value1 '(:struct g:value))
                         (value2 '(:struct g:value)))
    (is-true (cffi:pointerp (g:value-init value1 "gint")))
    (is-true (cffi:pointerp (g:value-init value2 "gint")))
    (is (= 0 (gobject::parse-g-value value1)))
;    (is (= 0 (parse-g-value value2)))
;    (set-g-value value1 10 "gint")
;    (value-copy value1 value2)
;    (is (= 10 (parse-g-value value1)))
;    (is (= 10 (parse-g-value value2)))
    ))

;;;     g_value_reset

(test value-reset
  (with-foreign-object (value '(:struct g:value))
    (is-true (cffi:pointerp (g:value-init value "gint")))
    (is (= 0 (gobject::parse-g-value value)))
    (gobject::set-g-value value 10 "gint")
    (is (= 10 (gobject::parse-g-value value)))
    (g:value-reset value)
    (is (= 0 (gobject::parse-g-value value)))))

;;;     g_value_unset

(test value-unset
  (with-foreign-object (value '(:struct g:value))
    (is-true (cffi:pointerp (g:value-init value "gint")))
    (is (= 0 (gobject::parse-g-value value)))
    (gobject::set-g-value value 10 "gint")
    (is (= 10 (gobject::parse-g-value value)))
    (g:value-unset value)
    (is-false (cffi:foreign-slot-value value '(:struct g:value) :type))))

;;;     g_value_set_instance
;;;     g_value_fits_pointer                     * not implemented *
;;;     g_value_peek_pointer                     * not implemented *
;;;     g_value_type_compatible
;;;     g_value_type_transformable
;;;     g_value_transform
;;;     g_value_register_transform_func
;;;     g_strdup_value_contents

;;; --- 2023-1-1 ---------------------------------------------------------------
