(in-package :glib-test)

(def-suite gobject-param-spec :in gobject-suite)
(in-suite gobject-param-spec)

;;; --- Types and Values -------------------------------------------------------

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

;;;     GParamSpec

;;; --- Functions --------------------------------------------------------------

;;;     g_type_is_param

(test g-type-is-param
  (is-true (g:type-is-param
             (g:type-from-instance
               (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is-true (g:type-is-param
             (g:type-from-instance
               (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     g_is_param_spec

(test g-is-param-spec
  (is-true (g:is-param-spec
             (g:param-spec-boolean "Boolean" "Bool" "Doku" t '())))
  (is-true (g:is-param-spec
             (g:param-spec-char "Char" "Char" "Doku" 10 50 25 '()))))

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
;;;     g_param_spec_ref_sink

(test g-param-spec-ref
  (let ((pspec (g:param-spec-boolean "Boolean" "Bool" "Doku" t nil)))
    (is (cffi:pointer-eq pspec (g:param-spec-ref-sink pspec)))
    (is (cffi:pointer-eq pspec (g:param-spec-ref pspec)))
    (is-false (g:param-spec-unref pspec))
    (is-false (g:param-spec-unref pspec))))

;;;     g_param_spec_default_value

(test g-param-spec-default-value
  (is-true (g:value-get
             (g:param-spec-default-value
               (g:param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is-false (g:value-get
              (g:param-spec-default-value
                (g:param-spec-boolean "Boolean" "Bool" "Doku" nil '()))))
  (is (= 25 (g:value-get
              (g:param-spec-default-value
                (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '()))))))

;;;     g_param_value_set_default

(test g-param-value-set-default
  (let ((param (g:param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (cffi:with-foreign-object (value '(:struct g:value))
      (g:value-init value "gint")
      (is-false (g:param-value-set-default param value))
      (is (= 25 (g:value-get value))))))

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
      (is (= 10 (g:value-get value)))
      (gobject:set-gvalue value 100 "gint")
      (is-true (g:param-value-validate param value))
      (is (= 50 (g:value-get value)))
      (gobject:set-gvalue value 25 "gint")
      (is-false (g:param-value-validate param value))
      (is (= 25 (g:value-get value))))))

;;;     g_param_spec_name

(test g-param-spec-name
  (let ((pspec (g:param-spec-internal "GParamBoolean"
                                      "Boolean"
                                      "Bool"
                                      "Doku"
                                      '(:readable :writable))))
    (is (string= "Boolean" (g:param-spec-name pspec)))))

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

;;; 2024-12-22
