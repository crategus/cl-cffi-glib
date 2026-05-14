(in-package :glib-test)

(def-suite gio-settings-schema :in gio-suite)
(in-suite gio-settings-schema)

;;; --- Types and Values -------------------------------------------------------

;;;     GSettingsSchemaSource

(test g-settings-schema-source-boxed
  ;; Check type
  (is (g:type-is-boxed "GSettingsSchemaSource"))
  ;; Check type initializer
  (is (eq (g:gtype "GSettingsSchemaSource")
          (g:gtype (cffi:foreign-funcall "g_settings_schema_source_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'g:settings-schema-source
          (glib:symbol-for-gtype "GSettingsSchemaSource"))))

;;;     GSettingsSchemaKey

(test g-settings-schema-key-boxed
  ;; Check type
  (is (g:type-is-boxed "GSettingsSchemaKey"))
  ;; Check type initializer
  (is (eq (g:gtype "GSettingsSchemaKey")
          (g:gtype (cffi:foreign-funcall "g_settings_schema_key_get_type" :size))))
  ;; Check registered name
  (is (eq 'g:settings-schema-key
          (glib:symbol-for-gtype "GSettingsSchemaKey"))))

;;;     GSettingsSchema

(test g-settings-schema-boxed
  ;; Check type
  (is (g:type-is-boxed "GSettingsSchema"))
  ;; Check type initializer
  (is (eq (g:gtype "GSettingsSchema")
          (g:gtype (cffi:foreign-funcall "g_settings_schema_get_type" :size))))
  ;; Check registered name
  (is (eq 'g:settings-schema
          (glib:symbol-for-gtype "GSettingsSchema"))))

;;;     g_settings_schema_source_default

(test g-settings-schema-source-default
  (is (typep (g:settings-schema-source-default) 'g:settings-schema-source)))

;;;     g_settings_schema_source_lookup

(test g-settings-schema-source-lookup
  (let ((source (g:settings-schema-source-default)))
    (is (typep source 'g:settings-schema-source))
    (is (typep (g:settings-schema-source-lookup source "com.crategus.rtest" t)
               'g:settings-schema))
    (is (typep (g:settings-schema-source-lookup source "com.crategus.rtest" nil)
               'g:settings-schema))
    (is-false (g:settings-schema-source-lookup source "com.crategus.test" t))
    (is-false (g:settings-schema-source-lookup source "com.crategus.test" nil))))

;;;     g_settings_schema_source_list-schemas

;; No longer works with GLIB 2.88. We get a memory default.
;; The array of pointer to strings no longer ends with NULL.

#+nil
(test g-settings-schema-source-list-schemas
  (let ((source (g:settings-schema-source-default)))
    (is (equal '("com.crategus.pinus" "com.crategus.pinus")
               (glib-sys:flatten
                   (multiple-value-list
                       (g:settings-schema-source-list-schemas source nil)))))))

;;;     g_settings_schema_key_get_default_value
;;;     g_settings_schema_key_get_description
;;;     g_settings_schema_key_get_name
;;;     g_settings_schema_key_get_range
;;;     g_settings_schema_key_get_summary
;;;     g_settings_schema_key_get_value_type
;;;     g_settings_schema_key_range_check

(test g-settings-schema-key-default-value
  (let* ((source (g:settings-schema-source-default))
         (schema (g:settings-schema-source-lookup source "com.crategus.rtest" t))
         (key (g:settings-schema-key schema "font")))
    (is (string= "Monospace 12"
                 (g:variant-get (g:settings-schema-key-default-value key))))
    (is (string= "The font to be used for content."
                 (g:settings-schema-key-description key)))
    (is (string= "font" (g:settings-schema-key-name key)))
    (is (string= "(sv)" (g:variant-type-string (g:settings-schema-key-range key))))
    (is (string= "Font" (g:settings-schema-key-summary key)))
    (is (string= "s" (g:variant-type-dup-string (g:settings-schema-key-value-type key))))
    (is-true (g:settings-schema-key-range-check key (g:variant-new-string "Monospace")))))

;;;     g_settings_schema_get_id
;;;     g_settings_schema_get_key
;;;     g_settings_schema_get_path
;;;     g_settings_schema_has_key
;;;     g_settings_schema_list_children
;;;     g_settings_schema_list_keys

(test g-settings-schema-get-id
  (let* ((source (g:settings-schema-source-default))
         (schema (g:settings-schema-source-lookup source "com.crategus.rtest" t)))
    (is (typep schema 'g:settings-schema))
    (is (string= "com.crategus.rtest" (g:settings-schema-get-id schema)))
    (is (typep (g:settings-schema-key schema "font") 'g:settings-schema-key))
    (is (string= "/com/crategus/rtest/" (g:settings-schema-path schema)))
    (is-true (g:settings-schema-has-key schema "font"))
    ;; Make an example with children
    (is-false (g:settings-schema-list-children schema))
    (is (equal '("transition" "font") (g:settings-schema-list-keys schema)))))

;;; 2026-05-14
