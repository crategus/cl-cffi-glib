(in-package :glib-test)

(def-suite glib-utils :in glib-suite)
(in-suite glib-utils)

;;; --- Types and Values -------------------------------------------------------

;;;     GUserDirectory

;;;     G_OS_INFO_KEY_NAME
;;;     G_OS_INFO_KEY_PRETTY_NAME
;;;     G_OS_INFO_KEY_VERSION
;;;     G_OS_INFO_KEY_VERSION_CODENAME
;;;     G_OS_INFO_KEY_VERSION_ID
;;;     G_OS_INFO_KEY_ID
;;;     G_OS_INFO_KEY_HOME_URL
;;;     G_OS_INFO_KEY_DOCUMENTATION_URL
;;;     G_OS_INFO_KEY_SUPPORT_URL
;;;     G_OS_INFO_KEY_BUG_REPORT_URL
;;;     G_OS_INFO_KEY_PRIVACY_POLICY_URL

;;;     GFormatSizeFlags
;;;     GDebugKey

;;; --- Functions --------------------------------------------------------------

;;;     g_application_name

(defvar *first-run-application* t)

#+nil ; Do not set the application name in the testsuite
(test application-name
  (when *first-run-application*
    #+(and sbcl (not windows))
    (is (string= "sbcl" (g:application-name)))
    #+(and sbcl windows)
    (is (string= "sbcl" (g:application-name)))
    #+(and ccl (not windows))
    (is (string= "Program" (g:application-name)))
    (is (string= "Application" (setf (g:application-name) "Application"))))
  (is (string= "Application" (g:application-name)))
  (setf *first-run-application* nil))

;;;     g_prgname

;; The PRGNAME is set in the rtest-glib.lisp file.

(test prgname
  (is (string= "glib-test" (g:prgname))))

#|

;;;     g_environ

(test environ
  (is (every #'stringp (g:environ))))

;;;     g_environ_getenv
;;;     g_environ_setenv
;;;     g_environ_unsetenv

;;;     g_getenv
;;;     g_setenv

#-windows
(test getenv
  (is (string= "/home/dieter" (g:getenv "HOME"))))

#+windows
(test getenv
  (is (string= "C:\\Program Files\\Steel Bank Common Lisp\\2.0.0\\"
               (g:getenv "SBCL_HOME"))))

;;;     g_unsetenv

;;;     g_listenv

(test listenv
  (is (every #'stringp (g:listenv)))
  (is (every #'stringp (mapcar #'g:getenv (g:listenv)))))

;;;     g_user-name

(test user-name
  (is (stringp (g:user-name))))

;;;     g_real_name

(test real-name
  (is (stringp (g:real-name))))

;;;     g_user_cache_dir

(test user-cache-dir
  (is (stringp (g:user-cache-dir))))

;;;     g_user_data_dir

(test user-data-dir
  (is (stringp (g:user-data-dir))))

;;;     g_user_config_dir

(test user-config-dir
  (is (stringp (g:user-config-dir))))

;;;     g_user_runtime_dir

(test user-runtime-dir
  (is (stringp (g:user-runtime-dir))))

;;;     g_user_special_dir

(test get-user-special-dir
 (is (stringp (g:user-special-dir :desktop)))
 (is (stringp (g:user-special-dir :documents)))
 (is (stringp (g:user-special-dir :download)))
 (is (stringp (g:user-special-dir :music)))
 (is (stringp (g:user-special-dir :pictures)))
 (is (stringp (g:user-special-dir :public-share)))
 (is (stringp (g:user-special-dir :templates)))
 (is (stringp (g:user-special-dir :videos))))

;;;     g_system_data_dirs

(test system-data-dirs
  (is (every #'stringp (g:system-data-dirs))))

;;;     g_system-config_dirs

(test system-config-dirs
  (is (every #'stringp (g:system-config-dirs))))

;;;     g_reload_user_special_dirs_cache
;;;     g_get_os_info

;;;     g_host_name

(test host-name
  (is (stringp (g:host-name))))

;;;     g_home_dir

(test home-dir
  (is (stringp (g:home-dir))))

;;;     g_tmp_dir

(test tmp-dir
  (is (stringp (g:tmp-dir))))

;;;     g_get_current_dir

(test current-dir
  (is (stringp (g:current-dir))))

;;;     g_basename
;;;     g_dirname
;;;     g_canonicalize_filename

;;;     g_path_is_absolute

(test path-is-absolute
  (is-false (g:path-is-absolute "../dieter"))
  (is-true  (g:path-is-absolute "/home"))
  (is-true  (g:path-is-absolute "/home/dieter"))
  (is-true  (g:path-is-absolute "/home/dieter/Lisp")))

;;;     g_path_skip_root
;;;     g_path_get_basename
;;;     g_path_get_dirname

;;;     g_build_filename

#-windows
(test build-filename
  (is (string= "home/dieter/Lisp"
               (g:build-filename "home" "dieter" "Lisp"))))

#+windows
(test build-filename
  (is (string= "home\\dieter\\Lisp"
               (g:build-filename "home" "dieter" "Lisp"))))

;;;     g_build_filenamev
;;;     g_build_filename_valist

;;;     g_build_path

(test build-path
  (is (string= "home/dieter/Lisp"
               (g:build-path "/" "home" "dieter" "Lisp"))))

;;;     g_build_pathv
;;;     g_format_size
;;;     g_format_size_full
;;;     g_format_size_for_display
;;;     g_find_program_in_path
;;;     g_bit_nth_lsf
;;;     g_bit_nth_msf
;;;     g_bit_storage
;;;     g_spaced_primes_closest
;;;     g_atexit
;;;     g_abort
;;;     g_parse_debug_string
;;;
;;;     (*GVoidFunc) ()
;;;     (*GFreeFunc) ()
;;;
;;;     g_qsort_with_data
;;;     g_nullify_pointer

|#

;;; 2022-11-23
