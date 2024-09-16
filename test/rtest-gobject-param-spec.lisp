(in-package :glib-test)

(def-suite gobject-param-spec :in gobject-suite)
(in-suite gobject-param-spec)

;;; --- Types and Values -------------------------------------------------------

;;;     GParamSpec
;;;     GParamSpecClass

;;;     GParamFlags

(test g-param-flags
  (is (equal '((:READABLE)
               (:WRITABLE)
               (:READABLE :WRITABLE)
               (:CONSTRUCT)
               (:READABLE :CONSTRUCT)
               (:WRITABLE :CONSTRUCT)
               (:READABLE :WRITABLE :CONSTRUCT)
               (:CONSTRUCT-ONLY)
               (:READABLE :CONSTRUCT-ONLY)
               (:WRITABLE :CONSTRUCT-ONLY)
               (:READABLE :WRITABLE :CONSTRUCT-ONLY)
               (:CONSTRUCT :CONSTRUCT-ONLY)
               (:READABLE :CONSTRUCT :CONSTRUCT-ONLY)
               (:WRITABLE :CONSTRUCT :CONSTRUCT-ONLY)
               (:READABLE :WRITABLE :CONSTRUCT :CONSTRUCT-ONLY)
               (:LAX-VALIDATION))
             (mapcar (lambda (x)
                       (cffi:foreign-bitfield-symbols 'g:param-flags x))
                     '(1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16))))
  (is (equal '(1 2 4 8 16 32 64 128 2147483648)
             (mapcar (lambda (x)
                       (cffi:foreign-bitfield-value 'g:param-flags x))
                     '(:readable
                       :writable
                       :construct
                       :construct-only
                       :lax-validation
                       :static-name
                       :static-nick
                       :static-blurb
                       :deprecated)))))

;;;     G_PARAM_STATIC_STRINGS
;;;     G_PARAM_MASK
;;;     G_PARAM_USER_SHIFT

;;;     GParamSpecTypeInfo
;;;     GParamSpecPool

;;; --- Functions --------------------------------------------------------------

;;;     g_type_is_param

(test g-type-is-param
  (is-true (g:type-is-param
             (g:type-from-instance
               (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is-true (g:type-is-param
             (g:type-from-instance
               (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     G_PARAM_SPEC

;;;     g_is_param_spec

(test g-is-param-spec
  (is-true (g:is-param-spec
             (g:param-spec-boolean "Boolean" "Bool" "Doku" t '())))
  (is-true (g:is-param-spec
             (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '()))))

;;;     G_PARAM_SPEC_CLASS
;;;     G_IS_PARAM_SPEC_CLASS
;;;     G_PARAM_SPEC_GET_CLASS

;;;     g-param-spec-type

(test g-param-spec-type
  (is (eq (g:gtype "GParamBoolean")
          (g:param-spec-type
            (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is (eq (g:gtype "GParamChar")
          (g:param-spec-type
            (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     g_param_spec_type_name

(test g-param-spec-type-name
  (is (string= "GParamBoolean"
               (g:param-spec-type-name
                 (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is (string= "GParamChar"
               (g:param-spec-type-name
                 (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     g_param_spec_value_type

(test g-param-spec-value-type
  (is (eq (g:gtype "gboolean")
          (g:param-spec-value-type
            (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is (eq (g:gtype "gchar")
          (g:param-spec-value-type
            (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     g_param_spec_ref
;;;     g_param_spec_unref
;;;     g_param_spec_sink
;;;     g_param_spec_ref_sink

;;;     g_param_spec_default_value

(test g-param-spec-default-value
  (is-true (gobject:parse-g-value
             (g:param-spec-default-value
               (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is-false (gobject:parse-g-value
              (g:param-spec-default-value
                (g:param-spec-boolean "Boolean" "Bool" "Doku" nil '()))))
  (is (= 25 (gobject:parse-g-value
              (g:param-spec-default-value
                (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '()))))))

;;;     g_param_value_set_default

(test g-param-value-set-default
  (let ((param (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (cffi:with-foreign-object (value '(:struct g:value))
      (g:value-init value "gint")
      (is-false (g:param-value-set-default param value))
      (is (= 25 (gobject:parse-g-value value))))))

;;;     g_param_value_defaults

(test g-param-value-defaults
  (let ((param (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (cffi:with-foreign-object (value '(:struct g:value))
      (g:value-init value "gint")
      (is-false (g:param-value-defaults param value))
      (is-false (g:param-value-set-default param value))
      (is-true  (g:param-value-defaults param value)))))

;;;     g_param_value_validate

(test g-param-value-validate
  (let ((param (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (cffi:with-foreign-object (value '(:struct g:value))
      (g:value-init value "gint")
      (is-true (g:param-value-validate param value))
      (is (= 10 (gobject:parse-g-value value)))
      (gobject:set-g-value value 100 "gint")
      (is-true (g:param-value-validate param value))
      (is (= 50 (gobject:parse-g-value value)))
      (gobject:set-g-value value 25 "gint")
      (is-false (g:param-value-validate param value))
      (is (= 25 (gobject:parse-g-value value))))))

;;;     g_param_value_convert
;;;     g_param_values_cmp
;;;     g_param_spec_is_valid_name

;;;     g_param_spec_name

(test g-param-spec-name
  (let ((pspec (g:param-spec-internal "GParamBoolean"
                                      "Boolean"
                                      "Bool"
                                      "Doku"
                                      '(:readable :writable))))
    (is (string= "Boolean" (g:param-spec-name pspec)))))

;;;     g_param_spec_get_name_quark

;;;     g_param_spec_nick

(test g-param-spec-nick
  (let ((pspec (g:param-spec-internal "GParamBoolean"
                                      "Boolean"
                                      "Bool"
                                      "Doku"
                                      '(:readable :writable))))
    (is (string= "Bool" (g:param-spec-nick pspec)))))

;;;     g_param_spec_blurb

(test g-param-spec-blurb
  (let ((pspec (g:param-spec-internal "GParamBoolean"
                                      "Boolean"
                                      "Bool"
                                      "Doku"
                                      '(:readable :writable))))
    (is (string= "Doku" (g:param-spec-blurb pspec)))))

;;;     g_param_spec_get_qdata
;;;     g_param_spec_set_qdata
;;;     g_param_spec_set_qdata_full
;;;     g_param_spec_steal_qdata
;;;     g_param_spec_get_redirect_target

;;;     g_param_spec_internal

(test g-param-spec-internal
  (let ((pspec (g:param-spec-internal "GParamBoolean"
                                      "Boolean"
                                      "Bool"
                                      "Doku"
                                      '(:readable :writable))))
    (is-true (g:is-param-spec pspec))
    (is (eq (g:gtype "GParamBoolean") (g:param-spec-type pspec)))
    (is (string= "GParamBoolean" (g:param-spec-type-name pspec)))
    (is (eq (g:gtype "gboolean") (g:param-spec-value-type pspec)))))

;;;     g_param_type_register_static

;;;     g_param_spec_pool_new
;;;     g_param_spec_pool_insert
;;;     g_param_spec_pool_remove
;;;     g_param_spec_pool_lookup
;;;     g_param_spec_pool_list
;;;     g_param_spec_pool_list_owned

;;; 2024-9-13
