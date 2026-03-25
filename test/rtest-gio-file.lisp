(in-package :glib-test)

(def-suite gio-file :in gio-suite)
(in-suite gio-file)

;;; --- Types and Values -------------------------------------------------------

;;;     GFileType

(test g-file-type
  ;; Check type
  (is (g:type-is-enum "GFileType"))
  ;; Check type initializer
  (is (eq (g:gtype "GFileType")
          (g:gtype (cffi:foreign-funcall "g_file_type_get_type" :size))))
  ;; Check registered symbol
  (is (eq 'gio:file-type
          (glib:symbol-for-gtype "GFileType")))
  ;; Check names
  (is (equal '("G_FILE_TYPE_UNKNOWN" "G_FILE_TYPE_REGULAR"
               "G_FILE_TYPE_DIRECTORY" "G_FILE_TYPE_SYMBOLIC_LINK"
               "G_FILE_TYPE_SPECIAL" "G_FILE_TYPE_SHORTCUT"
               "G_FILE_TYPE_MOUNTABLE")
             (glib-test:list-enum-item-names "GFileType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6)
             (glib-test:list-enum-item-values "GFileType")))
  ;; Check nick names
  (is (equal '("unknown" "regular" "directory" "symbolic-link" "special"
               "shortcut" "mountable")
             (glib-test:list-enum-item-nicks "GFileType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GFileType" GIO:FILE-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "g_file_type_get_type")
                                    (:UNKNOWN 0)
                                    (:REGULAR 1)
                                    (:DIRECTORY 2)
                                    (:SYMBOLIC-LINK 3)
                                    (:SPECIAL 4)
                                    (:SHORTCUT 5)
                                    (:MOUNTABLE 6))
             (gobject:get-gtype-definition "GFileType"))))

;;;     GFileQueryInfoFlags

(test g-file-query-info-flags
  ;; Check type
  (is (g:type-is-flags "GFileQueryInfoFlags"))
  ;; Check registered symbol
  (is (eq 'g:file-query-info-flags
          (glib:symbol-for-gtype "GFileQueryInfoFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GFileQueryInfoFlags")
          (g:gtype (cffi:foreign-funcall "g_file_query_info_flags_get_type" :size))))
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
    (let ((path (glib-sys:sys-path "test/resource/rtest-gio-file.txt")))
      (is (cffi:pointerp (setf file
                               (cffi:convert-to-foreign path 'g:file-as-namestring))))
      (is (string= "rtest-gio-file.txt" (g:file-basename file)))
      (is (string= (namestring path) (g:file-path file)))
      (is (string= (namestring path) (g:file-get-parse-name file)))
      (is (string= (namestring path)
                   (cffi:convert-from-foreign file 'g:file-as-namestring)))
      (is (cffi:null-pointer-p (cffi:convert-to-foreign nil 'g:file-as-namestring)))
      (is-false (cffi:convert-from-foreign nil 'g:file-as-namestring))
      (is-false (cffi:convert-from-foreign (cffi:null-pointer) 'g:file-as-namestring)))))

;;;     g_file_new_for_path

#-windows
(test g-file-new-for-path
  (glib-test:with-check-memory (file)
    (let ((path (glib-sys:sys-path "test/resource/rtest-gio-file.txt")))
      (is (typep (setf file (g:file-new-for-path path)) 'g:object))
      (is (string= "rtest-gio-file.txt" (g:file-basename file)))
      (is (string= (namestring path) (g:file-path file))))))

;;;     g_file_new_for_uri

(test file-new-for-uri
  (glib-test:with-check-memory (file)
    (let ((path "http://crategus.com/"))
      (is (typep (setf file (g:file-new-for-uri path)) 'g:object))
      (is-false (g:file-path file))
      (is (string= path (g:file-uri file))))))

;;;     g_file_new_for_commandline_arg
;;;     g_file_new_for_commandline_arg_and_cwd

;;;     g_file_parse_name

#-windows
(test g-file-parse-name.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/crategus/path.lisp"))
    (is (string= "/home/crategus/path.lisp" (g:file-path file)))
    (is (string= "file:///home/crategus/path.lisp" (g:file-uri file)))))

#+windows
(test g-file-parse-name.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/crategus/path.lisp"))
    (is (string= "\\home\\crategus\\path.lisp" (g:file-path file)))
    (is (string= "file:///home/crategus/path.lisp" (g:file-uri file)))))

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
    (setf file (g:file-parse-name "/home/crategus/path.lisp"))
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
    (setf file (g:file-parse-name "/home/crategus/path.lisp"))
    (is (string= "/home/crategus/path.lisp" (g:file-path file)))))

#+windows
(test g-file-path.1
  (glib-test:with-check-memory (file)
    (setf file (g:file-parse-name "/home/crategus/path.lisp"))
    (is (string= "\\home\\crategus\\path.lisp" (g:file-path file)))))

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

;;;     g_file_query_info

#-windows
(test g-file-query-info.1
  (glib-test:with-check-memory (file info)
    (let ((path (glib-sys:sys-path "test/resource/rtest-gio-file.txt")))
      (is (typep (setf file (g:file-new-for-path path)) 'g:object))
      (is (typep (setf info (g:file-query-info file "*" :none)) 'g:file-info))
      ;; Get standard attributes
      (is (equal '("standard::type"
                   "standard::is-hidden"
                   "standard::is-backup"
                   "standard::is-symlink"
                   "standard::name"
                   "standard::display-name"
                   "standard::edit-name"
                   "standard::copy-name"
                   "standard::icon"
                   "standard::content-type"
                   "standard::fast-content-type"
                   "standard::size"
                   "standard::allocated-size"
                   "standard::symbolic-icon")
                 (g:file-info-list-attributes info "standard"))))))

#+windows
(test g-file-query-info.1
  (glib-test:with-check-memory (file info)
    (let ((path (glib-sys:sys-path "test/resource/rtest-gio-file.txt")))
      (is (typep (setf file (g:file-new-for-path path)) 'g:object))
      (is (typep (setf info (g:file-query-info file "*" :none)) 'g:file-info))
      ;; Get standard attributes
      (is (equal '("standard::type"
                   "standard::is-hidden"
                   "standard::is-backup"
                   "standard::is-symlink"
                   "standard::name"
                   "standard::display-name"
                   "standard::edit-name"
                   "standard::copy-name"
                   "standard::icon"
                   "standard::content-type"
                   "standard::fast-content-type"
                   "standard::size"
                   "standard::allocated-size"
                   "standard::symbolic-icon")
                 (g:file-info-list-attributes info "standard"))))))

#-windows
(test g-file-query-info.2
  (glib-test:with-check-memory (file info)
    (let ((path (glib-sys:sys-path "test/resource/rtest-gio-file.txt")))
      (is (typep (setf file (g:file-new-for-path path)) 'g:object))
      (is (typep (setf info (g:file-query-info file "standard::*" :none)) 'g:file-info))
      ;; Get standard attributes
      (is (equal '("standard::type"
                   "standard::is-hidden"
                   "standard::is-backup"
                   "standard::is-symlink"
                   "standard::name"
                   "standard::display-name"
                   "standard::edit-name"
                   "standard::copy-name"
                   "standard::icon"
                   "standard::content-type"
                   "standard::fast-content-type"
                   "standard::size"
                   "standard::allocated-size"
                   "standard::symbolic-icon")
                 (g:file-info-list-attributes info))))))

#+windows
(test g-file-query-info.2
  (glib-test:with-check-memory (file info)
    (let ((path (glib-sys:sys-path "test/resource/rtest-gio-file.txt")))
      (is (typep (setf file (g:file-new-for-path path)) 'g:object))
      (is (typep (setf info (g:file-query-info file "standard::*" :none)) 'g:file-info))
      ;; Get standard attributes
      (is (equal '("standard::type"
                   "standard::is-hidden"
                   "standard::is-backup"
                   "standard::is-symlink"
                   "standard::name"
                   "standard::display-name"
                   "standard::edit-name"
                   "standard::copy-name"
                   "standard::icon"
                   "standard::content-type"
                   "standard::fast-content-type"
                   "standard::size"
                   "standard::allocated-size"
                   "standard::symbolic-icon")
                 (g:file-info-list-attributes info))))))

(test g-file-query-info.3
  (is-false (g:file-query-info (g:file-parse-name "") "*" :none)))

;;;     g_file_set_attributes_from_info

(test g-file-set-attributes-from-info
  (glib-test:with-check-memory (file info)
    (let ((path (glib-sys:sys-path "test/resource/rtest-gio-file.txt"))
          info1)
      (is (typep (setf file (g:file-new-for-path path)) 'g:object))
      (is (typep (setf info (g:file-query-info file "*" :none)) 'g:file-info))
      (is (typep (setf info1 (g:file-info-new)) 'g:file-info))
      (is-true (g:file-set-attributes-from-info file info1 :none)))))

;;; 2026-03-22
