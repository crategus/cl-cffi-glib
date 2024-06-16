(in-package :glib-test)

(def-suite gio-app-info :in gio-suite)
(in-suite gio-app-info)

;;; --- Types and Values -------------------------------------------------------

;;;     GAppInfoCreateFlags

(test g-app-info-create-flags
  ;; Check the type
  (is (g:type-is-flags "GAppInfoCreateFlags"))
  ;; Check the registered symbol
  (is (eq 'g:app-info-create-flags
          (glib:symbol-for-gtype "GAppInfoCreateFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GAppInfoCreateFlags")
          (g:gtype (cffi:foreign-funcall "g_app_info_create_flags_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("G_APP_INFO_CREATE_NONE" "G_APP_INFO_CREATE_NEEDS_TERMINAL"
               "G_APP_INFO_CREATE_SUPPORTS_URIS"
               "G_APP_INFO_CREATE_SUPPORTS_STARTUP_NOTIFICATION")
             (list-flags-item-name "GAppInfoCreateFlags")))
  ;; Check the values
  (is (equal '(0 1 2 4)
             (list-flags-item-value "GAppInfoCreateFlags")))
  ;; Check the nick names
  (is (equal '("none" "needs-terminal" "supports-uris"
               "supports-startup-notification")
             (list-flags-item-nick "GAppInfoCreateFlags")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GAppInfoCreateFlags"
                              G-APP-INFO-CREATE-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "g_app_info_create_flags_get_type")
                              (:NONE 0)
                              (:NEEDS-TERMINAL 1)
                              (:SUPPORTS-URIS 2)
                              (:SUPPORTS-STARTUP-NOTIFICATION 4))
             (gobject:get-g-type-definition "GAppInfoCreateFlags"))))

;;;     GAppInfo

(test g-app-info-interface
  ;; Type check
  (is (g:type-is-interface "GAppInfo"))
  ;; Check the registered symbol
  (is (eq 'g:app-info
          (glib:symbol-for-gtype "GAppInfo")))
  ;; Check the type initializer
  (is (eq (g:gtype "GAppInfo")
          (g:gtype (cffi:foreign-funcall "g_app_info_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GAppInfo")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GAppInfo")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GAppInfo" G-APP-INFO
                                          (:EXPORT T
                                           :TYPE-INITIALIZER
                                           "g_app_info_get_type"))
             (gobject:get-g-type-definition "GAppInfo"))))

;;;     GAppLaunchContext

(test g-app-launch-context-class
  ;; Type check
  (is (g:type-is-object "GAppLaunchContext"))
  ;; Check the registered symbol
  (is (eq 'gio:app-launch-context
          (glib:symbol-for-gtype "GAppLaunchContext")))
  ;; Check the type initializer
  (is (eq (g:gtype "GAppLaunchContext")
          (g:gtype (cffi:foreign-funcall "g_app_launch_context_get_type"
                                         :size))))
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
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GAppLaunchContext"
                                     G-APP-LAUNCH-CONTEXT
                                     (:SUPERCLASS G-OBJECT
                                      :EXPORT T
                                      :INTERFACES NIL
                                      :TYPE-INITIALIZER
                                      "g_app_launch_context_get_type")
                                     NIL)
             (gobject:get-g-type-definition "GAppLaunchContext"))))

;;; --- Signals ----------------------------------------------------------------

;;;     launch-failed

(test g-app-launch-context-signal-launch-failed
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

(test g-app-launch-context-signal-launch-started
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

(test g-app-launch-context-signal-launched
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

(test g-app-info-create-from-commandline
  (is (g:type-is-a (g:type-from-instance
                       (g:app-info-create-from-commandline "" "gedit" :none))
                   "GAppInfo")))

;;;     g_app_info_dup
;;;     g_app_info_equal

;;;     g_app_info_get_id

(test g-app-info-id
  (is (every #'stringp
             (mapcar #'g:app-info-id (g:app-info-all)))))

;;;     g_app_info_get_name

(test g-app-info-name
  (is (every #'stringp
             (mapcar #'g:app-info-name (g:app-info-all)))))

;;;     g_app_info_get_display_name

(test g-app-info-display-name
  (is (every #'stringp
             (mapcar #'g:app-info-display-name (g:app-info-all)))))

;;;     g_app_info_get_description

(test g-app-info-description
  (is (every (lambda (x)
               (or (null x) (stringp x)))
             (mapcar #'g:app-info-description (g:app-info-all)))))

;;;     g_app_info_get_executable

#-windows
(test g-app-info-executable
  (is (every #'stringp
             (remove nil
                     (mapcar #'g:app-info-executable (g:app-info-all))))))

;;;     g_app_info_get_commandline

#-windows
(test g-app-info-commandline
  (is (every #'stringp
             (remove nil
                     (mapcar #'g:app-info-commandline (g:app-info-all))))))

;;;     g_app_info_get_icon

(test g-app-info-icon
  (is (every (lambda (x)
               (or (null x)
                   (g:type-is-a (g:type-from-instance x) "GIcon")))
             (mapcar #'g:app-info-icon (g:app-info-all)))))

;;;     g_app_info_launch

;;;     g_app_info_supports_files

(test g-app-info-supports-files
  (let ((info (g:app-info-default-for-type "text/plain" nil)))
    (is-false (g:app-info-supports-files info))))

;;;     g_app_info_supports_uris

#-windows
(test g-app-info-supports-uris
  (let ((info (g:app-info-default-for-type "text/plain" nil)))
    (is-true (g:app-info-supports-uris info))))

;;;     g_app_info_launch_uris
;;;     g_app_info_launch_uris_async
;;;     g_app_info_launch_uris_finish

;;;     g_app_info_should_show

#-windows
(test g-app-info-should-show
  (let ((info (g:app-info-default-for-type "text/plain" nil)))
    (is-true (g:app-info-should-show info))))

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

#-windows
(test g-app-info-supported-types
  (let ((info (g:app-info-default-for-type "text/plain" nil)))
    (is (member "text/plain"
                (g:app-info-supported-types info) :test #'string=))))

;;;     g_app_info_get_all

(test g-app-info-all
  (is (every (lambda (x)
               (g:type-is-a (g:type-from-instance x) "GAppInfo"))
             (g:app-info-all))))

;;;     g_app_info_get_all_for_type

(test g-app-info-all-for-type
  (is (every (lambda (x)
               (g:type-is-a (g:type-from-instance x) "GAppInfo"))
             (g:app-info-all-for-type "text/plain"))))

;;;     g_app_info_get_default_for_type

#-windows
(test g-app-info-default-for-type
  (is (g:type-is-a (g:type-from-instance
                       (g:app-info-default-for-type "text/plain" nil))
                   "GAppInfo")))

;;;     g_app_info_get_default_for_uri_scheme

(test g-app-info-default-for-uri-scheme
  (is (g:type-is-a (g:type-from-instance
                       (g:app-info-default-for-uri-scheme "http"))
                   "GAppInfo")))

;;;     g_app_info_get_fallback_for_type

(test g-app-info-fallback-for-type
  ;; no fallback for "text/plain" !?
  (is-false (g:app-info-fallback-for-type "text/plain")))

;;;     g_app_info_get_recommended_for_type

(test g-app-info-recommended-for-type
  (is (every (lambda (x)
               (g:type-is-a (g:type-from-instance x) "GAppInfo"))
             (g:app-info-recommended-for-type "text/plain"))))

;;;     g_app_info_launch_default_for_uri
;;;     g_app_info_launch_default_for_uri_async
;;;     g_app_info_launch_default_for_uri_finish

;;;     g_app_launch_context_new

(test g-app-launch-context-new
  (is (typep (g:app-launch-context-new) 'g:app-launch-context)))

;;;     g_app_launch_context_setenv
;;;     g_app_launch_context_unsetenv
;;;     g_app_launch_context_get_environment
;;;     g_app_launch_context_get_display
;;;     g_app_launch_context_get_startup_notify_id
;;;     g_app_launch_context_launch_failed

;;; 2024-6-12
