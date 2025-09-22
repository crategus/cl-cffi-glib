(in-package :glib-test)

(def-suite gio-file :in gio-suite)
(in-suite gio-file)

;;; --- Types and Values -------------------------------------------------------

;;;     GFileQueryInfoFlags

(test g-file-query-info-flags
  ;; Check type
  (is (g:type-is-flags "GFileQueryInfoFlags"))
  ;; Check registered symbol
  (is (eq 'g:file-query-info-flags
          (glib:symbol-for-gtype "GFileQueryInfoFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GFileQueryInfoFlags")
          (g:gtype (cffi:foreign-funcall "g_file_query_info_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("G_FILE_QUERY_INFO_NONE" "G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS")
             (glib-test:list-flags-item-names "GFileQueryInfoFlags")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-flags-item-values "GFileQueryInfoFlags")))
  ;; Check nick names
  (is (equal '("none" "nofollow-symlinks")
             (glib-test:list-flags-item-nicks "GFileQueryInfoFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GFileQueryInfoFlags"
                                     GIO:FILE-QUERY-INFO-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "g_file_query_info_flags_get_type")
                                     (:NONE 0)
                                     (:NOFOLLOW-SYMLINKS 1))
             (gobject:get-gtype-definition "GFileQueryInfoFlags"))))

;;;     GFile

(test g-file-interface
  ;; Check type
  (is (g:type-is-interface "GFile"))
  ;; Check registered symbol
  (is (eq 'g:file
          (glib:symbol-for-gtype "GFile")))
  ;; Check type initializer
  (is (eq (g:gtype "GFile")
          (g:gtype (cffi:foreign-funcall "g_file_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GFile")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GFile")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GFile")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GFile" GIO:FILE
                      (:EXPORT T
                       :TYPE-INITIALIZER "g_file_get_type"))
             (gobject:get-gtype-definition "GFile"))))

;;; --- Functions --------------------------------------------------------------

;;;     g:file-as-namestring

#-windows
(test g-file-as-namestring
  (glib-test:with-check-memory (file)
    (let ((path (glib-sys:sys-path "test/rtest-gio-file.lisp")))
      (is (cffi:pointerp (setf file
                               (cffi:convert-to-foreign path
                                                        'g:file-as-namestring))))
      (is (string= "rtest-gio-file.lisp" (g:file-basename file)))
      (is (string= (namestring path) (g:file-path file)))
      (is (string= (namestring path) (g:file-get-parse-name file)))
      (is (string= (namestring path)
                   (cffi:convert-from-foreign file 'g:file-as-namestring)))
      (is (cffi:null-pointer-p (cffi:convert-to-foreign nil 'g:file-as-namestring)))
      (is-false (cffi:convert-from-foreign nil 'g:file-as-namestring))
      (is-false (cffi:convert-from-foreign (cffi:null-pointer)
                                           'g:file-as-namestring)))))

;;;     g_file_new_for_path

#-windows
(test g-file-new-for-path
  (glib-test:with-check-memory (file)
    (let ((path (glib-sys:sys-path "test/rtest-gio-file.lisp")))
      (is (typep (setf file (g:file-new-for-path path)) 'g:object))
      (is (string= "rtest-gio-file.lisp" (g:file-basename file)))
      (is (string= (namestring path) (g:file-path file))))))

;;;     g_file_new_for_uri

(test file-new-for-uri
  (glib-test:with-check-memory (file)
    (let ((path "http://crategus.com/"))
      (is (typep (setf file (g:file-new-for-uri path)) 'g:object))
      (is-false (g:file-path file))
      (is (string= path (g:file-uri file))))))

;;;     g_file_new_for_commandline_arg

(test g-file-new-for-commandline-arg
  (glib-test:with-check-memory ()
    (is (typep (g:file-new-for-commandline-arg "commandline") 'g:object))))

;;;     g_file_new_for_commandline_arg_and_cwd

(test g-file-new-for-commandline-arg-and-cwd
  (glib-test:with-check-memory ()
    (is (typep (g:file-new-for-commandline-arg-and-cwd "commandline" "directory")
               'g:object))))

;;;     g_file_query_info

#-windows
(test g-file-query-info
  (glib-test:with-check-memory (file info)
    (let ((path (glib-sys:sys-path "test/rtest-gio-file.lisp")))
      (is (typep (setf file (g:file-new-for-path path)) 'g:object))
      (is (typep (setf info (g:file-query-info file "*" :none)) 'g:file-info)))))

;;;     g_file_parse_name

#-windows
(test g-file-parse-name.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/dieter/path.lisp"))
    (is (string= "/home/dieter/path.lisp" (g:file-path file)))
    (is (string= "file:///home/dieter/path.lisp" (g:file-uri file)))))

#+windows
(test g-file-parse-name.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/dieter/path.lisp"))
    (is (string= "\\home\\dieter\\path.lisp" (g:file-path file)))
    (is (string= "file:///home/dieter/path.lisp" (g:file-uri file)))))

#-windows
(test g-file-parse-name.2
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "http://crategus.com"))
    (is-false (g:file-path file))
    (is (string= "http://crategus.com" (g:file-uri file)))))

#+windows
(test g-file-parse-name.2
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "http://crategus.com"))
    (is-false (g:file-path file))
    (is (string= "http://crategus.com/" (g:file-uri file)))))

;;;     g_file_get_basename

(test g-file-basename.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/dieter/path.lisp"))
    (is (string= "path.lisp" (g:file-basename file)))))

#-windows
(test g-file-basename.2
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "http://crategus.com"))
    (is (string= "/" (g:file-basename file)))))

#+windows
(test g-file-basename.2
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "http://crategus.com"))
    (is (string= "" (g:file-basename file)))))

;;;     g_file_get_path

#-windows
(test g-file-path.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/dieter/path.lisp"))
    (is (string= "/home/dieter/path.lisp" (g:file-path file)))))

#+windows
(test g-file-path.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/dieter/path.lisp"))
    (is (string= "\\home\\dieter\\path.lisp" (g:file-path file)))))

(test g-file-path.2
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "http://crategus.com"))
    (is-false (g:file-path file))))

;;;     g_file_get_uri

(test g-file-uri.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/dieter/path.lisp"))
    (is (string= "file:///home/dieter/path.lisp" (g:file-uri file)))))

#-windows
(test g-file-uri.2
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "http://crategus.com"))
    (is (string= "http://crategus.com" (g:file-uri file)))))

#+windows
(test g-file-uri.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is (string= "http://crategus.com/" (g:file-uri file)))))

;;;     g_file_get_parse_name

#-windows
(test g-file-get-parse.name.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/dieter/path.lisp"))
    (is (string= "/home/dieter/path.lisp" (g:file-get-parse-name file)))))

#+windows
(test g-file-get-parse.name.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/dieter/path.lisp"))
    (is (string= "\\home\\dieter\\path.lisp" (g:file-get-parse-name file)))))

#-windows
(test g-file-get-parse-name.2
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "http://crategus.com"))
    (is (string= "http://crategus.com" (g:file-get-parse-name file)))))

#+windows
(test g-file-get-parse-name.2
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "http://crategus.com"))
    (is (string= "http://crategus.com/" (g:file-get-parse-name file)))))

;;; 2024-12-28
