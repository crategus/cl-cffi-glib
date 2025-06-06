(in-package :glib-test)

(def-suite gio-application :in gio-suite)
(in-suite gio-application)

(defvar *verbose-g-application* nil)

;;; --- Types and Values -------------------------------------------------------

;;;     GApplicationFlags

(test g-application-flags
  ;; Check type
  (is (g:type-is-flags "GApplicationFlags"))
  ;; Check registered symbol
  (is (eq 'g:application-flags
          (glib:symbol-for-gtype "GApplicationFlags")))
  ;; Check names
  (is (equal '("G_APPLICATION_FLAGS_NONE" "G_APPLICATION_DEFAULT_FLAGS"
               "G_APPLICATION_IS_SERVICE" "G_APPLICATION_IS_LAUNCHER"
               "G_APPLICATION_HANDLES_OPEN" "G_APPLICATION_HANDLES_COMMAND_LINE"
               "G_APPLICATION_SEND_ENVIRONMENT" "G_APPLICATION_NON_UNIQUE"
               "G_APPLICATION_CAN_OVERRIDE_APP_ID"
               "G_APPLICATION_ALLOW_REPLACEMENT" "G_APPLICATION_REPLACE")
             (glib-test:list-flags-item-names "GApplicationFlags")))
  ;; Check values
  (is (equal '(0 0 1 2 4 8 16 32 64 128 256)
             (glib-test:list-flags-item-values "GApplicationFlags")))
  ;; Check nick names
  (is (equal '("flags-none" "default-flags" "is-service" "is-launcher"
               "handles-open" "handles-command-line" "send-environment"
               "non-unique" "can-override-app-id" "allow-replacement" "replace")
             (glib-test:list-flags-item-nicks "GApplicationFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GApplicationFlags" GIO:APPLICATION-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "g_application_flags_get_type")
                                     (:FLAGS-NONE 0)
                                     (:DEFAULT-FLAGS 0)
                                     (:IS-SERVICE 1)
                                     (:IS-LAUNCHER 2)
                                     (:HANDLES-OPEN 4)
                                     (:HANDLES-COMMAND-LINE 8)
                                     (:SEND-ENVIRONMENT 16)
                                     (:NON-UNIQUE 32)
                                     (:CAN-OVERRIDE-APP-ID 64)
                                     (:ALLOW-REPLACEMENT 128)
                                     (:REPLACE 256))
             (gobject:get-gtype-definition "GApplicationFlags"))))

;;;     GApplication

(test g-application-class
  ;; Check type
  (is (g:type-is-object "GApplication"))
  ;; Check registered symbol
  (is (eq 'g:application
          (glib:symbol-for-gtype "GApplication")))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GApplication")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GApplication")))
  ;; Check interfaces
  (is (equal '("GActionGroup" "GActionMap")
             (glib-test:list-interfaces "GApplication")))
  ;; Check class properties
  (is (equal '("action-group" "application-id" "flags" "inactivity-timeout"
               "is-busy" "is-registered" "is-remote" "resource-base-path"
               "version")
             (glib-test:list-properties "GApplication")))
  (is (equal '("activate" "command-line" "handle-local-options" "name-lost"
               "open" "shutdown" "startup")
             (glib-test:list-signals "GApplication")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GApplication" GIO:APPLICATION
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GActionGroup" "GActionMap")
                       :TYPE-INITIALIZER "g_application_get_type")
                      ((ACTION-GROUP APPLICATION-ACTION-GROUP
                        "action-group" "GActionGroup" NIL T)
                       (APPLICATION-ID APPLICATION-APPLICATION-ID
                        "application-id" "gchararray" T T)
                       (FLAGS APPLICATION-FLAGS
                        "flags" "GApplicationFlags" T T)
                       (INACTIVITY-TIMEOUT APPLICATION-INACTIVITY-TIMEOUT
                        "inactivity-timeout" "guint" T T)
                       (IS-BUSY APPLICATION-IS-BUSY "is-busy" "gboolean" T NIL)
                       (IS-REGISTERED APPLICATION-IS-REGISTERED
                        "is-registered" "gboolean" T NIL)
                       (IS-REMOTE APPLICATION-IS-REMOTE
                        "is-remote" "gboolean" T NIL)
                       (RESOURCE-BASE-PATH APPLICATION-RESOURCE-BASE-PATH
                        "resource-base-path" "gchararray" T T)
                       (VERSION APPLICATION-VERSION
                        "version" "gchararray" T T)))
             (gobject:get-gtype-definition "GApplication"))))

;;; --- Signals ----------------------------------------------------------------

;;     activate
;;     command-line
;;     handle-local-options
;;     name-lost
;;     open
;;     shutdown
;;     startup

;; TODO: This example does not work on Windows. The "open" signal handler is
;; not exectuted, but the "activate" signal handler.

(defun example-application-open (&optional (argv nil))
  (let ((in-startup nil) (in-activate nil) (in-open nil) (in-shutdown nil))
    (let ((app (make-instance 'g:application
                              :application-id "com.crategus.application-open"
                              :inactivity-timeout 2000
                              :flags :handles-open)))
      ;; Signal handler "startup"
      (g:signal-connect app "startup"
                        (lambda (application)
                          (declare (ignore application))
                          (setf in-startup t)
                          (when *verbose-g-application*
                            (format t "~&The application is in startup.~%"))))
      ;; Signal handler "activate"
      (g:signal-connect app "activate"
                        (lambda (application)
                          (declare (ignore application))
                          (g:application-hold app)
                          (setf in-activate t)
                          (when *verbose-g-application*
                            (format t "The application is in activate.~%"))
                          ;; Note: when doing a longer-lasting action that
                          ;; returns to the main loop, you should use
                          ;; g-application-hold and g-application-release to
                          ;; keep the application alive until the action is
                          ;; completed.
                          (g:application-release app)))
      ;; Signal handler "open"
      (g:signal-connect app "open"
                        (lambda (application files n-files hint)
                          (declare (ignore application files n-files hint))
                          (setf in-open t)
                          (when *verbose-g-application*
                            (format t "The application is in open.~%"))
                          ;; TODO: The argument "files" is a C pointer to an
                          ;; array of GFiles. The conversion to a list of
                          ;; strings with the call
                          ;; (cffi:convert-from-foreign files 'g-strv)
                          ;; does not work. Search a better implementation to
                          ;; get a list of GFiles.
                        ))
      ;; Signal handler "shutdown"
      (g:signal-connect app "shutdown"
                        (lambda (application)
                          (declare (ignore application))
                          (setf in-shutdown t)
                          (when *verbose-g-application*
                            (format t "The application is in shutdown.~%"))))
      ;; Start the application
      (g:application-run app argv))
      ;; Return the results
      (list in-startup in-activate in-open in-shutdown)))

#-windows
(test g-application-signals
  (glib-test:with-check-memory (:strong 1)
    (is (equal '(t t nil t)
               (example-application-open)))
    (is (equal '(t nil t t)
               (example-application-open '("demo" "file1" "file2"))))))

;;; --- Properties and Accessors -----------------------------------------------

;;;     g_application_action_group

(test g-application-action-group-property
  (glib-test:with-check-memory (app group)
    (setf app (make-instance 'g:application))
    (setf group (make-instance 'g:simple-action-group))
    ;; action-group is not readable
    (signals (error) (g:application-action-group app))
    (is-true (setf (g:application-action-group app) group))
    ;; Remove references
    (is-false (setf (g:application-action-group app) nil))))

;;;     g_application_application_id

(test g-application-application-id-property
  (glib-test:with-check-memory (app)
    (setf app (make-instance 'g:application))
    (is-false (g:application-application-id app))
    (is-true (setf (g:application-application-id app) "com.crategus.app"))
    (is (string= "com.crategus.app" (g:application-application-id app)))))

;;;     g_application_flags

(test g-application-flags-property
  (glib-test:with-check-memory (app)
    (setf app (make-instance 'g:application))
    (is (equal '() (g:application-flags app)))
    ;; a single flag
    (is-true (setf (g:application-flags app) :handles-open))
    (is (equal '(:handles-open) (g:application-flags app)))
    ;; the flag :default-flags does not set a non-nil value !?
    (is-true (setf (g:application-flags app) :default-flags))
    (is (equal '() (g:application-flags app)))
    ;; a list of flags
    (is-true (setf (g:application-flags app) '(:is-service :handles-open)))
    (is (equal '(:is-service :handles-open) (g:application-flags app)))))

;;;     g_applicaton_inactivity_timeout

(test g-application-inactivity-timeout-property
  (glib-test:with-check-memory (app)
    (setf app (make-instance 'g:application))
    (is (= 0 (g:application-inactivity-timeout app)))
    (is (= 10000 (setf (g:application-inactivity-timeout app) 10000)))
    (is (= 10000 (g:application-inactivity-timeout app)))))

;;;     g_application_is_busy

(test g-application-is-busy-property
  (glib-test:with-check-memory (app)
    (setf app (make-instance 'g:application))
    ;; Default value is nil
    (is-false (g:application-is-busy app))
    ;; is-busy is not writeable
    (signals (error) (setf (g:application-is-busy app) t))))

;;;     g_application_is_registered

(test g-application-is-registered-property
  (glib-test:with-check-memory (app)
    (setf app (make-instance 'g:application))
    (is-false (g:application-is-registered app))
    ;; is-registered is not writeable
    (signals (error) (setf (g:application-is-registered app) t))))

;;;     g_application_is_remote

(test g-application-is-remote-property
  (glib-test:with-check-memory (app)
    (setf app (make-instance 'g:application))
    ;; is-remote is not readable before registration
;   (is-false (g:application-is-remote app))
    ;; is-remote is not writeable
    (signals (error) (setf (g:application-is-remote app) t))))

;;;     g_application_resource_base_path

(test g-application-resource-base-path-property
  (glib-test:with-check-memory (app)
    (setf app (make-instance 'g:application))
    (is-false (g:application-resource-base-path app))
    (is (string= "/test" (setf (g:application-resource-base-path app) "/test")))
    (is (string= "/test" (g:application-resource-base-path app)))))

;;; --- Functions --------------------------------------------------------------

;;;     g_application_id_is_valid

(test g-application-id-is-valid
  (is-true (g:application-id-is-valid "com.crategus.application"))
  (is-false (g:application-id-is-valid "application")))

;;;     g_application_new
;;;     g_application_get_dbus_connection
;;;     g_application_get_dbus_object_path
;;;     g_application_set_action_group                     deprecated
;;;     g_application_register
;;;     g_application_hold
;;;     g_application_release
;;;     g_application_quit
;;;     g_application_activate

;;;     g_application_open

(test g-application-open
  (let ((app (make-instance 'g:application
                            :application-id "com.crategus.application-open"
                            :flags :handles-open))
        (files (list "file1" "file2" "file3"))
        (hint "hint"))
    ;; Signal handler "startup"
    (g:signal-connect app "startup"
        (lambda (application)
          (when *verbose-g-application*
            (format t "~&The application is in startup.~%"))
          (g:application-open application files hint)))
    ;; Signal handler "activate"
    (g:signal-connect app "activate"
        (lambda (application)
          (declare (ignore application))
          (g:application-hold app)
          (when *verbose-g-application*
            (format t "The application is in activate.~%"))
            (g:application-release app)))
    ;; Signal handler "open"
    (g:signal-connect app "open"
        (lambda (application files n-files hint)
          (declare (ignore application hint))
          (when *verbose-g-application*
            (format t "in OPEN signal handler~%"))
          (dotimes (i n-files)
            (let ((file (cffi:mem-aref files '(g:object g:file) i)))
              (when *verbose-g-application*
                (format t "~a~%" (g:file-path file)))))))
    ;; Start the application
    (g:application-run app nil)))

;;;     g_application_send_notification
;;;     g_application_withdraw_notification
;;;     g_application_run
;;;     g_application_add_main_option_entries
;;;     g_application_add_main_option
;;;     g_application_add_option_group
;;;     g_application_set_option_context_parameter_string
;;;     g_application_set_option_context_summary
;;;     g_application_set_option_context_description
;;;     g_application_set_default
;;;     g_application_get_default
;;;     g_application_mark_busy
;;;     g_application_unmark_busy
;;;     g_application_bind_busy_property
;;;     g_application_unbind_busy_property

;;; 2024-12-19
