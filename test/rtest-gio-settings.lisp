(in-package :glib-test)

(def-suite gio-settings :in gio-suite)
(in-suite gio-settings)

;;; --- Types and Values -------------------------------------------------------

;;;     GSettingsBindFlags

(test g-settings-bind-flags
  ;; Check type
  (is (g:type-is-flags "GSettingsBindFlags"))
  ;; Check registered symbol
  (is (eq 'g:settings-bind-flags
          (glib:symbol-for-gtype "GSettingsBindFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GSettingsBindFlags")
          (g:gtype (cffi:foreign-funcall "g_settings_bind_flags_get_type" :size))))
  ;; Check names
  (is (equal '("G_SETTINGS_BIND_DEFAULT" "G_SETTINGS_BIND_GET"
               "G_SETTINGS_BIND_SET" "G_SETTINGS_BIND_NO_SENSITIVITY"
               "G_SETTINGS_BIND_GET_NO_CHANGES" "G_SETTINGS_BIND_INVERT_BOOLEAN")
             (glib-test:list-flags-item-names "GSettingsBindFlags")))
  ;; Check values
  (is (equal '(0 1 2 4 8 16)
             (glib-test:list-flags-item-values "GSettingsBindFlags")))
  ;; Check nick names
  (is (equal '("default" "get" "set" "no-sensitivity" "get-no-changes"
               "invert-boolean")
             (glib-test:list-flags-item-nicks "GSettingsBindFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GSettingsBindFlags" GIO:SETTINGS-BIND-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "g_settings_bind_flags_get_type")
                                     (:DEFAULT 0)
                                     (:GET 1)
                                     (:SET 2)
                                     (:NO-SENSITIVITY 4)
                                     (:GET-NO-CHANGES 8)
                                     (:INVERT-BOOLEAN 16))
             (gobject:get-gtype-definition "GSettingsBindFlags"))))

;;;     GSettings

(test g-settings-class
  ;; Check type
  (is (g:type-is-object "GSettings"))
  ;; Check registered symbol
  (is (eq 'gio:settings
          (glib:symbol-for-gtype "GSettings")))
  ;; Check type initializer
  (is (eq (g:gtype "GSettings")
          (g:gtype (cffi:foreign-funcall "g_settings_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GSettings")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GSettings")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GSettings")))
  ;; Check class properties
  (is (equal '("backend" "delay-apply" "has-unapplied" "path" "schema"
               "schema-id" "settings-schema")
             (glib-test:list-properties "GSettings")))
  ;; Check signals
  (is (equal '("change-event" "changed" "writable-change-event" "writable-changed")
             (glib-test:list-signals "GSettings")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GSettings" GIO:SETTINGS
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "g_settings_get_type")
                      ((BACKEND SETTINGS-BACKEND "backend" "GSettingsBackend"
                        T NIL)
                       (DELAY-APPLY SETTINGS-DELAY-APPLY "delay-apply"
                        "gboolean" T NIL)
                       (HAS-UNAPPLIED SETTINGS-HAS-UNAPPLIED "has-unapplied"
                        "gboolean" T NIL)
                       (PATH SETTINGS-PATH "path" "gchararray" T NIL)
                       (SCHEMA SETTINGS-SCHEMA "schema" "gchararray" T NIL)
                       (SCHEMA-ID SETTINGS-SCHEMA-ID "schema-id" "gchararray"
                        T NIL)
                       (SETTINGS-SCHEMA SETTINGS-SETTINGS-SCHEMA
                        "settings-schema" "GSettingsSchema" T NIL)))
             (gobject:get-gtype-definition "GSettings"))))

;;; --- Signals ----------------------------------------------------------------

;;;     change-event
;;;     changed
;;;     writable-change-event
;;;     writable-changed

;;; --- Properties -------------------------------------------------------------

;;;     backend
;;;     delay-apply
;;;     has-unapplied
;;;     path
;;;     schema
;;;     schema-id
;;;     settings-schema

(test g-settings-properties
  (let ((settings (g:settings-new "com.crategus.rtest")))
    (is (typep settings 'g:settings))
    (is (typep (g:settings-backend settings) 'g:object))
    (is-false (g:settings-delay-apply settings))
    (is-false (g:settings-has-unapplied settings))
    (is (string= "/com/crategus/rtest/" (g:settings-path settings)))
    (is (string= "com.crategus.rtest" (g:settings-schema settings)))
    (is (string= "com.crategus.rtest" (g:settings-schema-id settings)))
    (is (typep (g:settings-settings-schema settings) 'g:settings-schema))))

;;; Functions

;;;     g_settings_new

(test g-settings-new
  (is (typep (g:settings-new "com.crategus.rtest") 'g:settings)))

;;;     g_settings_new_full
;;;     g_settings_new_with_backend
;;;     g_settings_new_with_backend_and_path

;;;     g_settings_new_with_path

(test g-settings-new-with-path
  (is (typep (g:settings-new-with-path "com.crategus.rtest"
                                       "/com/crategus/rtest/") 'g:settings)))

;;;     g_settings_list_schemas

#+nil
(test g-settings-list-schemas
  (is (member "com.crategus.rtest"
              (g:settings-list-schemas) :test #'string=)))

;;;     g_settings_list_relocatable_schemas

#+nil
(test g-settings-list-relocatable-schemas
  (is (every #'stringp (g:settings-list-relocatable-schemas))))

;;;     g_settings_sync
;;;     g_settings_apply
;;;     g_settings_bind
;;;     g_settings_bind_with_mapping
;;;     g_settings_bind_with_mapping_closures
;;;     g_settings_bind_writable

;;;     g_settings_unbind

;;;     g_settings_create_action
;;;     g_settings_delay
;;;     g_settings_get
;;;     g_settings_get_boolean
;;;     g_settings_get_child
;;;     g_settings_get_default_value
;;;     g_settings_get_double
;;;     g_settings_get_enum
;;;     g_settings_get_flags
;;;     g_settings_get_has_unapplied
;;;     g_settings_get_int
;;;     g_settings_get_int64
;;;     g_settings_get_mapped
;;;     g_settings_get_range
;;;     g_settings_get_string
;;;     g_settings_get_strv
;;;     g_settings_get_uint
;;;     g_settings_get_uint64
;;;     g_settings_get_user_value
;;;     g_settings_get_value
;;;     g_settings_is_writable
;;;     g_settings_list_children
;;;     g_settings_list_keys
;;;     g_settings_range_check
;;;     g_settings_reset
;;;     g_settings_revert
;;;     g_settings_set
;;;     g_settings_set_boolean
;;;     g_settings_set_double
;;;     g_settings_set_enum
;;;     g_settings_set_flags
;;;     g_settings_set_int
;;;     g_settings_set_int64
;;;     g_settings_set_string
;;;     g_settings_set_strv
;;;     g_settings_set_uint
;;;     g_settings_set_uint64
;;;     g_settings_set_value

;;; 2026-03-24
