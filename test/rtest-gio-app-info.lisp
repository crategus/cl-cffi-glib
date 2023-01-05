(in-package :glib-test)

(def-suite gio-app-info :in gio-suite)
(in-suite gio-app-info)

;;; --- Types and Values -------------------------------------------------------

;;;     GAppInfoCreateFlags

;;;     GAppInfo

(test app-info-interface
  ;; Type check
  (is (g:type-is-interface "GAppInfo"))
  ;; Check the registered symbol
  (is (eq 'g:app-info
          (gobject:symbol-for-gtype "GAppInfo")))
  ;; Check the type initializer
  (is (eq (g:gtype "GAppInfo")
          (g:gtype (cffi:foreign-funcall "g_app_info_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GAppInfo")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GAppInfo" G-APP-INFO (:EXPORT T))
             (get-g-type-definition "GAppInfo"))))

;;;     GAppLaunchContext

(test app-launch-context-class
  ;; Type check
  (is (g:type-is-object "GAppLaunchContext"))
  ;; Check the registered symbol
  (is (eq 'gio:app-launch-context
          (gobject:symbol-for-gtype "GAppLaunchContext")))
  ;; Check the type initializer
  (is (eq (g:gtype "GAppLaunchContext")
          (g:gtype (cffi:foreign-funcall "g_app_launch_context_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GAppLaunchContext")))
  ;; Check the children
  (is (equal '()
             (list-children "GAppLaunchContext")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GAppLaunchContext")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GAppLaunchContext")))
  ;; Check the list of signals
  (is (equal '("launch-failed" "launch-started" "launched")
             (list-signals "GAppLaunchContext")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GAppLaunchContext"
                                     G-APP-LAUNCH-CONTEXT
                                     (:SUPERCLASS G-OBJECT
                                      :EXPORT T
                                      :INTERFACES NIL)
                                     NIL)
             (get-g-type-definition "GAppLaunchContext"))))

;;; Signals

;;;     launch-failed

(test app-launch-context-signal-launch-failed
  (let ((query (g:signal-query (g:signal-lookup "launch-failed"
                                                "GAppLaunchContext"))))
    (is (string= "launch-failed"
                 (g:signal-query-signal-name query)))
    (is (string= "GAppLaunchContext"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     launch-started

(test app-launch-context-signal-launch-started
  (let ((query (g:signal-query (g:signal-lookup "launch-started"
                                                "GAppLaunchContext"))))
    (is (string= "launch-started"
                 (g:signal-query-signal-name query)))
    (is (string= "GAppLaunchContext"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GAppInfo" "GVariant")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     launched

(test app-launch-context-signal-launched
  (let ((query (g:signal-query (g:signal-lookup "launched"
                                                "GAppLaunchContext"))))
    (is (string= "launched"
                 (g:signal-query-signal-name query)))
    (is (string= "GAppLaunchContext"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GAppInfo" "GVariant")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     g_app_info_create_from_commandline
;;;     g_app_info_dup
;;;     g_app_info_equal
;;;     g_app_info_get_id
;;;     g_app_info_get_name
;;;     g_app_info_get_display_name
;;;     g_app_info_get_description
;;;     g_app_info_get_executable
;;;     g_app_info_get_commandline
;;;     g_app_info_get_icon
;;;     g_app_info_launch
;;;     g_app_info_supports_files
;;;     g_app_info_supports_uris
;;;     g_app_info_launch_uris
;;;     g_app_info_launch_uris_async
;;;     g_app_info_launch_uris_finish
;;;     g_app_info_should_show
;;;     g_app_info_can_delete
;;;     g_app_info_delete
;;;     g_app_info_reset_type_associations
;;;     g_app_info_set_as_default_for_type
;;;     g_app_info_set_as_default_for_extension
;;;     g_app_info_set_as_last_used_for_type
;;;     g_app_info_add_supports_type
;;;     g_app_info_can_remove_supports_type
;;;     g_app_info_remove_supports_type
;;;     g_app_info_get_supported_types
;;;     g_app_info_get_all
;;;     g_app_info_get_all_for_type
;;;     g_app_info_get_default_for_type
;;;     g_app_info_get_default_for_uri_scheme
;;;     g_app_info_get_fallback_for_type
;;;     g_app_info_get_recommended_for_type
;;;     g_app_info_launch_default_for_uri
;;;     g_app_info_launch_default_for_uri_async
;;;     g_app_info_launch_default_for_uri_finish
;;;     g_app_launch_context_setenv
;;;     g_app_launch_context_unsetenv
;;;     g_app_launch_context_get_environment
;;;     g_app_launch_context_get_display
;;;     g_app_launch_context_get_startup_notify_id
;;;     g_app_launch_context_launch_failed
;;;     g_app_launch_context_new

;; 2022-12-27
