(in-package :glib-test)

(def-suite gio-file :in gio-suite)
(in-suite gio-file)

;;; --- Types and Values -------------------------------------------------------

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

(test g-file-as-namestring
  (let ((path (glib-sys:sys-path "test/rtest-gio-file.lisp"))
        (file nil))

    (is (cffi:pointerp (setf file
                             (cffi:convert-to-foreign path
                                                      'g:file-as-namestring))))

    (is (string= "rtest-gio-file.lisp" (g:file-basename file)))
    (is (string= (namestring path) (g:file-path file)))
    (is (string= (namestring path) (g:file-get-parse-name file)))

    (is (string= (namestring path)
                 (cffi:convert-from-foreign file 'g:file-as-namestring)))
    (is-false (cffi:convert-from-foreign nil 'g:file-as-namestring))
    (is-false (cffi:convert-from-foreign (cffi:null-pointer)
                                         'g:file-as-namestring))))

;;;     g_file_new_for_path

(test g-file-new-for-path
  (let* ((path (glib-sys:sys-path "test/rtest-gio-file.lisp"))
         (file (g:file-new-for-path path)))
    (is (typep file 'g:object))
    (is (string= "rtest-gio-file.lisp" (g:file-basename file)))
    (is (string= (namestring path) (g:file-path file)))))

;;;     g_file_new_for_uri

(test file-new-for-uri
  (let* ((path "http://crategus.com")
         (file (g:file-new-for-uri path)))
    (is (typep file 'g:object))
    (is-false (g:file-path file))
    (is (string= path (g:file-uri file)))))

;;;     g_file_new_for_commandline_arg

(test g-file-new-for-commandline-arg
  (is (typep (g:file-new-for-commandline-arg "commandline") 'g:object)))

;;;     g_file_new_for_commandline_arg_and_cwd

(test g-file-new-for-commandline-arg-and-cwd
  (is (typep (g:file-new-for-commandline-arg-and-cwd "commandline"
                                                     "directory") 'g:object)))

;;;     g_file_parse_name

#-windows
(test g-file-parse-name.1
  (let ((file (g:file-parse-name "/home/dieter/path.lisp")))
    (is (string= "/home/dieter/path.lisp" (g:file-path file)))
    (is (string= "file:///home/dieter/path.lisp" (g:file-uri file)))))

#+windows
(test g-file-parse-name.1
  (let ((file (g:file-parse-name "/home/dieter/path.lisp")))
    (is (string= "\\home\\dieter\\path.lisp" (g:file-path file)))
    (is (string= "file:///home/dieter/path.lisp" (g:file-uri file)))))

#-windows
(test g-file-parse-name.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is-false (g:file-path file))
    (is (string= "http://crategus.com" (g:file-uri file)))))

#+windows
(test g-file-parse-name.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is-false (g:file-path file))
    (is (string= "http://crategus.com/" (g:file-uri file)))))

;;;     g_file_get_basename

(test g-file-basename.1
  (let ((file (g:file-parse-name "/home/dieter/path.lisp")))
    (is (string= "path.lisp" (g:file-basename file)))))

#-windows
(test g-file-basename.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is (string= "/" (g:file-basename file)))))

#+windows
(test g-file-basename.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is (string= "" (g:file-basename file)))))

;;;     g_file_get_path

#-windows
(test g-file-path.1
  (let ((file (g:file-parse-name "/home/dieter/path.lisp")))
    (is (string= "/home/dieter/path.lisp" (g:file-path file)))))

#+windows
(test g-file-path.1
  (let ((file (g:file-parse-name "/home/dieter/path.lisp")))
    (is (string= "\\home\\dieter\\path.lisp" (g:file-path file)))))

(test g-file-path.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is-false (g:file-path file))))

;;;     g_file_get_uri

(test g-file-uri.1
  (let ((file (g:file-parse-name "/home/dieter/path.lisp")))
    (is (string= "file:///home/dieter/path.lisp" (g:file-uri file)))))

#-windows
(test g-file-uri.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is (string= "http://crategus.com" (g:file-uri file)))))

#+windows
(test g-file-uri.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is (string= "http://crategus.com/" (g:file-uri file)))))

;;;     g_file_get_parse_name

#-windows
(test g-file-get-parse.name.1
  (let ((file (g:file-parse-name "/home/dieter/path.lisp")))
    (is (string= "/home/dieter/path.lisp" (g:file-get-parse-name file)))))

#+windows
(test g-file-get-parse.name.1
  (let ((file (g:file-parse-name "/home/dieter/path.lisp")))
    (is (string= "\\home\\dieter\\path.lisp" (g:file-get-parse-name file)))))

#-windows
(test g-file-get-parse-name.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is (string= "http://crategus.com" (g:file-get-parse-name file)))))

#+windows
(test g-file-get-parse-name.2
  (let ((file (g:file-parse-name "http://crategus.com")))
    (is (string= "http://crategus.com/" (g:file-get-parse-name file)))))

;;; 2024-10-12
