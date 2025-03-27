(in-package :glib-test)

(def-suite gio-app-info :in gio-suite)
(in-suite gio-app-info)

;;; --- Types and Values -------------------------------------------------------

;;;     GAppInfoCreateFlags

(test g-app-info-create-flags
  ;; Check type
  (is (g:type-is-flags "GAppInfoCreateFlags"))
  ;; Check registered symbol
  (is (eq 'g:app-info-create-flags
          (glib:symbol-for-gtype "GAppInfoCreateFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GAppInfoCreateFlags")
          (g:gtype (cffi:foreign-funcall "g_app_info_create_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("G_APP_INFO_CREATE_NONE" "G_APP_INFO_CREATE_NEEDS_TERMINAL"
               "G_APP_INFO_CREATE_SUPPORTS_URIS"
               "G_APP_INFO_CREATE_SUPPORTS_STARTUP_NOTIFICATION")
             (glib-test:list-flags-item-names "GAppInfoCreateFlags")))
  ;; Check values
  (is (equal '(0 1 2 4)
             (glib-test:list-flags-item-values "GAppInfoCreateFlags")))
  ;; Check nick names
  (is (equal '("none" "needs-terminal" "supports-uris"
               "supports-startup-notification")
             (glib-test:list-flags-item-nicks "GAppInfoCreateFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GAppInfoCreateFlags"
                                     GIO:APP-INFO-CREATE-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "g_app_info_create_flags_get_type")
                                     (:NONE 0)
                                     (:NEEDS-TERMINAL 1)
                                     (:SUPPORTS-URIS 2)
                                     (:SUPPORTS-STARTUP-NOTIFICATION 4))
             (gobject:get-gtype-definition "GAppInfoCreateFlags"))))

;;;     GAppInfo

(test g-app-info-interface
  ;; Check type
  (is (g:type-is-interface "GAppInfo"))
  ;; Check registered symbol
  (is (eq 'g:app-info
          (glib:symbol-for-gtype "GAppInfo")))
  ;; Check type initializer
  (is (eq (g:gtype "GAppInfo")
          (g:gtype (cffi:foreign-funcall "g_app_info_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GAppInfo")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GAppInfo")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GAppInfo")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GAppInfo" GIO:APP-INFO
                       (:EXPORT T
                        :TYPE-INITIALIZER "g_app_info_get_type"))
             (gobject:get-gtype-definition "GAppInfo"))))

;;;     GAppLaunchContext

(test g-app-launch-context-class
  ;; Check type
  (is (g:type-is-object "GAppLaunchContext"))
  ;; Check registered symbol
  (is (eq 'gio:app-launch-context
          (glib:symbol-for-gtype "GAppLaunchContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GAppLaunchContext")
          (g:gtype (cffi:foreign-funcall "g_app_launch_context_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GAppLaunchContext")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GAppLaunchContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GAppLaunchContext")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GAppLaunchContext")))
  ;; Check signals
  (is (equal '("launch-failed" "launch-started" "launched")
             (glib-test:list-signals "GAppLaunchContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GAppLaunchContext" GIO:APP-LAUNCH-CONTEXT
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "g_app_launch_context_get_type")
                       NIL)
             (gobject:get-gtype-definition "GAppLaunchContext"))))

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
  (glib-test:with-check-memory (info)
    (is (g:type-is-a
            (g:type-from-instance
                (setf info
                      (g:app-info-create-from-commandline
                              ""
                              "org.gnome.TextEditor.desktop"
                              :none)))
            "GAppInfo"))))

;;;     g_app_info_dup
;;;     g_app_info_equal

(test g-app-info-dup/equal
  (glib-test:with-check-memory (info1 info2)
    (is (g:type-is-a
            (g:type-from-instance
                (setf info1
                      (g:app-info-create-from-commandline
                              ""
                              "Text Editor"
                              :none)))
            "GAppInfo"))
    (is (g:type-is-a
            (g:type-from-instance (setf info2 (g:app-info-dup info1)))
             "GAppInfo"))
    (is (string= "Text Editor" (g:app-info-name info1)))
    (is (string= "Text Editor" (g:app-info-name info2)))
    (is-true (g:app-info-equal info1 info1))
    (is-true (g:app-info-equal info2 info2))
    ;; TODO: info1 and info2 are not equal. Why?
    (is-false (g:app-info-equal info1 info2))))

;;;     g_app_info_get_id

(test g-app-info-id
  (glib-test:with-check-memory ()
    (is (every #'stringp
               (mapcar #'g:app-info-id (g:app-info-all))))))

;;;     g_app_info_get_name

(test g-app-info-name
  (glib-test:with-check-memory ()
    (is (every #'stringp
               (mapcar #'g:app-info-name (g:app-info-all))))))

;;;     g_app_info_get_display_name

(test g-app-info-display-name
  (glib-test:with-check-memory ()
    (is (every #'stringp
               (mapcar #'g:app-info-display-name (g:app-info-all))))))

;;;     g_app_info_get_description

(test g-app-info-description
  (glib-test:with-check-memory ()
    (is (every (lambda (x)
                 (or (null x) (stringp x)))
               (mapcar #'g:app-info-description (g:app-info-all))))))

;;;     g_app_info_get_executable

#-windows
(test g-app-info-executable
  (glib-test:with-check-memory ()
    (is (every #'stringp
               (remove nil
                       (mapcar #'g:app-info-executable (g:app-info-all)))))))

;;;     g_app_info_get_commandline

#-windows
(test g-app-info-commandline
  (glib-test:with-check-memory ()
    (is (every #'stringp
               (remove nil
                       (mapcar #'g:app-info-commandline (g:app-info-all)))))))

;;;     g_app_info_get_icon

(test g-app-info-icon
  (glib-test:with-check-memory ()
    (g:type-is-a (g:type-from-instance (first (g:app-info-all))) "GIcon")))

;;;     g_app_info_launch

;;;     g_app_info_supports_files

#-windows
(test g-app-info-supports-files
  (glib-test:with-check-memory (info)
    (setf info (g:app-info-default-for-type "text/plain" nil))
    (is-false (g:app-info-supports-files info))))

;;;     g_app_info_supports_uris

#-windows
(test g-app-info-supports-uris
  (glib-test:with-check-memory (info)
    (setf info (g:app-info-default-for-type "text/plain" nil))
    (is-true (g:app-info-supports-uris info))))

;;;     g_app_info_launch_uris
;;;     g_app_info_launch_uris_async
;;;     g_app_info_launch_uris_finish

;;;     g_app_info_should_show

#-windows
(test g-app-info-should-show
  (glib-test:with-check-memory (info)
    (setf info (g:app-info-default-for-type "text/plain" nil))
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
  (glib-test:with-check-memory (info)
    (setf info (g:app-info-default-for-type "text/plain" nil))
    (is (member "text/plain"
                (g:app-info-supported-types info) :test #'string=))))

;;;     g_app_info_get_all

(test g-app-info-all
  (glib-test:with-check-memory ()
    (is (every (lambda (x)
                 (g:type-is-a (g:type-from-instance x) "GAppInfo"))
               (g:app-info-all)))))

;;;     g_app_info_get_all_for_type

(test g-app-info-all-for-type
  (glib-test:with-check-memory ()
    (is (every (lambda (x)
                 (g:type-is-a (g:type-from-instance x) "GAppInfo"))
               (g:app-info-all-for-type "text/plain")))))

;;;     g_app_info_get_default_for_type

#-windows
(test g-app-info-default-for-type
  (glib-test:with-check-memory ()
    (is (g:type-is-a (g:type-from-instance
                         (g:app-info-default-for-type "text/plain" nil))
                     "GAppInfo"))))

;;;     g_app_info_get_default_for_type_async               Since 2.74
;;;     g_app_info_get_default_for_type_finish              Since 2.74

;;;     g_app_info_get_default_for_uri_scheme

(test g-app-info-default-for-uri-scheme
  (glib-test:with-check-memory ()
    (is (g:type-is-a (g:type-from-instance
                         (g:app-info-default-for-uri-scheme "http"))
                     "GAppInfo"))))

;;;     g_app_info_get_default_for_uri_scheme_async         Since 2.74
;;;     g_app_info_get_default_for_uri_scheme_finish        Since 2.74

;;;     g_app_info_get_fallback_for_type

(test g-app-info-fallback-for-type
  (glib-test:with-check-memory ()
    ;; no fallback for "text/plain" !?
    (is-false (g:app-info-fallback-for-type "text/plain"))))

;;;     g_app_info_get_recommended_for_type

(test g-app-info-recommended-for-type
  (glib-test:with-check-memory ()
    (is (every (lambda (x)
                 (g:type-is-a (g:type-from-instance x) "GAppInfo"))
               (g:app-info-recommended-for-type "text/plain")))))

;;;     g_app_info_launch_default_for_uri
;;;     g_app_info_launch_default_for_uri_async
;;;     g_app_info_launch_default_for_uri_finish

;;;     g_app_launch_context_new

(test g-app-launch-context-new
  (glib-test:with-check-memory ()
    (is (typep (g:app-launch-context-new) 'g:app-launch-context))))

;;;     g_app_launch_context_setenv
;;;     g_app_launch_context_unsetenv
;;;     g_app_launch_context_get_environment
;;;     g_app_launch_context_get_display
;;;     g_app_launch_context_get_startup_notify_id
;;;     g_app_launch_context_launch_failed

;;; 2024-12-22
