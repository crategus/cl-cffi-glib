(in-package :glib-test)

(def-suite gobject-param-spec :in gobject-suite)
(in-suite gobject-param-spec)

;;; --- Types and Values -------------------------------------------------------

;;;     GParamSpec
;;;     GParamSpecClass
;;;     GParamFlags

;;;     G_PARAM_STATIC_STRINGS
;;;     G_PARAM_MASK
;;;     G_PARAM_USER_SHIFT

;;;     GParamSpecTypeInfo
;;;     GParamSpecPool

;;; --- Functions --------------------------------------------------------------

;;;     g_type_is_param

(test type-is-param
  (is-true (g:type-is-param
             (g:type-from-instance
               (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is-true (g:type-is-param
             (g:type-from-instance
               (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     G_PARAM_SPEC

;;;     g_is_param_spec

(test is-param-spec
  (is-true (g:is-param-spec
             (g:param-spec-boolean "Boolean" "Bool" "Doku" t '())))
  (is-true (g:is-param-spec
             (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '()))))

;;;     G_PARAM_SPEC_CLASS
;;;     G_IS_PARAM_SPEC_CLASS
;;;     G_PARAM_SPEC_GET_CLASS

;;;     g-param-spec-type

(test param-spec-type
  (is (eq (g:gtype "GParamBoolean")
          (g:param-spec-type
            (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is (eq (g:gtype "GParamChar")
          (g:param-spec-type
            (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     g_param_spec_type_name

(test param-spec-type-name
  (is (string= "GParamBoolean"
               (g:param-spec-type-name
                 (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is (string= "GParamChar"
               (g:param-spec-type-name
                 (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     g_param_spec_value_type

(test param-spec-value-type
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

(test param-spec-default-value
  (is-true (parse-g-value
             (g:param-spec-default-value
               (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is-false (parse-g-value
              (g:param-spec-default-value
                (g:param-spec-boolean "Boolean" "Bool" "Doku" nil '()))))
  (is (= 25 (parse-g-value
              (g:param-spec-default-value
                (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '()))))))

;;;     g_param_value_set_default

(test param-value-set-default
  (let ((param (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (with-foreign-object (value '(:struct g:value))
      (g:value-init value +g-type-int+)
      (is-false (g:param-value-set-default param value))
      (is (= 25 (parse-g-value value))))))

;;;     g_param_value_defaults

(test param-value-defaults
  (let ((param (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (with-foreign-object (value '(:struct g:value))
      (g:value-init value +g-type-int+)
      (is-false (g:param-value-defaults param value))
      (is-false (g:param-value-set-default param value))
      (is-true  (g:param-value-defaults param value)))))

;;;     g_param_value_validate

(test param-value-validate
  (let ((param (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (with-foreign-object (value '(:struct g:value))
      (g:value-init value +g-type-int+)
      (is-true (g:param-value-validate param value))
      (is (= 10 (parse-g-value value)))
      (set-g-value value 100 +g-type-int+)
      (is-true (g:param-value-validate param value))
      (is (= 50 (parse-g-value value)))
      (set-g-value value 25 +g-type-int+)
      (is-false (g:param-value-validate param value))
      (is (= 25 (parse-g-value value))))))

;;;     g_param_value_convert
;;;     g_param_values_cmp
;;;     g_param_spec_is_valid_name

;;;     g_param_spec_name

(test param-spec-name
  (let ((pspec (g:param-spec-internal "GParamBoolean"
                                      "Boolean"
                                      "Bool"
                                      "Doku"
                                      '(:readable :writable))))
    (is (string= "Boolean" (g:param-spec-name pspec)))))

;;;     g_param_spec_get_name_quark

;;;     g_param_spec_nick

(test param-spec-nick
  (let ((pspec (g:param-spec-internal "GParamBoolean"
                                      "Boolean"
                                      "Bool"
                                      "Doku"
                                      '(:readable :writable))))
    (is (string= "Bool" (g:param-spec-nick pspec)))))

;;;     g_param_spec_blurb

(test param-spec-blurb
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

(test param-spec-internal
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

;;; 2022-11-5
