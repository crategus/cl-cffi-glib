(in-package :glib-test)

(def-suite gio-application-command-line :in gio-suite)
(in-suite gio-application-command-line)

(defvar *verbose-g-application-command-line* nil)

;;; --- Types and Values -------------------------------------------------------

;;;     GApplicationCommandLine

(test g-application-command-line-class
  ;; Type check
  (is (g:type-is-object "GApplicationCommandLine"))
  ;; Check the registered symbol
  (is (eq 'g:application-command-line
          (glib:symbol-for-gtype "GApplicationCommandLine")))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GApplicationCommandLine")))
  ;; Check the children
  (is (equal '()
             (list-children "GApplicationCommandLine")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GApplicationCommandLine")))
  ;; Check the class properties
  (is (equal '("arguments" "is-remote" "options" "platform-data")
             (list-properties "GApplicationCommandLine")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GApplicationCommandLine"
                                      G-APPLICATION-COMMAND-LINE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL)
                       ((ARGUMENTS G-APPLICATION-COMMAND-LINE-ARGUMENTS
                         "arguments" "GVariant" NIL NIL)
                        (IS-REMOTE G-APPLICATION-COMMAND-LINE-IS-REMOTE
                         "is-remote" "gboolean" T NIL)
                        (OPTIONS G-APPLICATION-COMMAND-LINE-OPTIONS "options"
                         "GVariant" NIL NIL)
                        (PLATFORM-DATA G-APPLICATION-COMMAND-LINE-PLATFORM-DATA
                         "platform-data" "GVariant" NIL NIL)))
             (gobject:get-g-type-definition "GApplicationCommandLine"))))

;;; --- Properties and Accessors -----------------------------------------------

;;;     arguments
;;;     is-remote
;;;     options
;;;     platform-data

(test g-application-command-line-properties
  (let ((cmdline (make-instance 'g:application-command-line)))
    ;; Property arguments is not readable
    (signals (error) (g:application-command-line-arguments cmdline))
    ;; Default value is nil
    (is-false (g:application-command-line-is-remote cmdline))
    ;; Property options is not readable
    (signals (error) (g:application-command-line-options cmdline))
    ;; Property platform-data is not readable
    (signals (error) (g:application-command-line-platform-data cmdline))))

;;; --- Functions --------------------------------------------------------------

;;;     g_application_command_line_get_arguments

(test g-application-command-line-get-arguments
  (let ((app (make-instance 'g:application
                            :flags :handles-command-line
                            :inactivity-timeout 1000)))
    ;; Signal handler "command-line"
    (g:signal-connect app "command-line"
        (lambda (application cmdline)
          (g:application-hold application)
          (when *verbose-g-application-command-line*
            (format t "~%The application is in command-line.~%")
            (format t "~A~%" (g:application-command-line-get-arguments cmdline)))
          (is (equal '("file1" "file2" "file3")
                     (g:application-command-line-get-arguments cmdline)))
          (g:application-release application)
          0))
    (g:application-run app '("file1" "file2" "file3"))))

;;;     g_application_command_line_get_cwd
;;;     g_application_command_line_get_environ
;;;     g_application_command_line_get_options_dict
;;;     g_application_command_line_get_stdin
;;;     g_application_command_line_create_file_for_arg
;;;     g_application_command_line_getenv
;;;     g_application_command_line_get_is_remote
;;;     g_application_command_line_get_platform_data
;;;     g_application_command_line_set_exit_status
;;;     g_application_command_line_get_exit_status
;;;     g_application_command_line_print
;;;     g_application_command_line_printerr

;;; --- 2023-7-9 ---------------------------------------------------------------
