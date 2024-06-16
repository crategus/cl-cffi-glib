(in-package :glib-test)

(def-suite glib-key-file :in glib-suite)
(in-suite glib-key-file)

(defvar *key-values*
"# this is just an example
 # there can be comments before the first group

 [First Group]

  Name=Key File Example this value shows escaping

 # localized strings are stored in multiple key-value pairs
 Welcome=Hello
 Welcome[de]=Hallo
 Welcome[fr_FR]=Bonjour
 Welcome[it]=Ciao
 Welcome[be@@latin]=Hello

 [Another Group]

 Boolean=true
 Integer=100

 Numbers=2;20;-200;0
 Booleans=true;false;true;true")

;;; --- Types and Values -------------------------------------------------------

;;;     G_KEY_FILE_ERROR
;;;     GKeyFileError

;;;     GKeyFile

;;;     GKeyFileFlags

(test g-key-file-flags
  (is-false (cffi:foreign-bitfield-symbols 'g:key-file-flags #b00))
  (is (equal '(:keep-comments)
             (cffi:foreign-bitfield-symbols 'g:key-file-flags #b01)))
  (is (equal '(:keep-translations)
             (cffi:foreign-bitfield-symbols 'g:key-file-flags #b10)))
  (is (equal '(:keep-comments :keep-translations)
             (cffi:foreign-bitfield-symbols 'g:key-file-flags #b11))))

;;; --- Functions --------------------------------------------------------------

;;;     g_key_file_new
;;;     g_key_file_free

(test g-key-file-new
  (let ((keyfile nil))
    (is (cffi:pointerp (setf keyfile (g:key-file-new))))
    (is (string= "" (g:key-file-to-data keyfile)))
    (is-false (g:key-file-free keyfile))))

;;;     g_key_file_ref
;;;     g_key_file_unref

(test g-key-file-ref/unref
  (let ((keyfile (g:key-file-new)))
    (is (string= "" (g:key-file-to-data keyfile)))
    (is (cffi:pointer-eq keyfile (g:key-file-ref keyfile)))
    (is (string= "" (g:key-file-to-data keyfile)))
    (is-false (g:key-file-unref keyfile))
    (is (string= "" (g:key-file-to-data keyfile)))
    (is-false (g:key-file-unref keyfile))))

;;;     g_key_file_set_list_separator

(test g-key-file-set-list-separator
  (glib:with-g-key-file (keyfile)
    (is-false (g:key-file-set-list-separator keyfile #\$))
    (is (equal '("string1" "string2" "string3")
               (setf (g:key-file-string-list keyfile "Group" "strings")
                     '("string1" "string2" "string3"))))
    (is (string= "string1$string2$string3$"
                 (g:key-file-string keyfile "Group" "strings")))))

;;;     g_key_file_load_from_file

(test g-key-file-load-from-file
  (glib:with-g-key-file (keyfile)
    (let ((path (glib-sys:sys-path "test/resource/rtest-glib-key-file.ini")))
      (is-true (g:key-file-load-from-file keyfile path :none))
      (is (= 161 (length (g:key-file-to-data keyfile)))))))

;;;     g_key_file_load_from_data

(test g-key-file-load-from-data
  (glib:with-g-key-file (keyfile)
    (is-false (g:key-file-load-from-data keyfile "test" :none))
    (is-true (g:key-file-load-from-data keyfile *key-values* :none))
    (is (= 186 (length (g:key-file-to-data keyfile))))))

;;;     g_key_file_load_from_bytes

(test g-key-file-load-from-bytes
  (glib:with-g-key-file (keyfile)
    (multiple-value-bind (data len)
        (cffi:foreign-string-alloc *key-values*)
      (let ((bytes (g:bytes-new data len)))
        (is-true (g:key-file-load-from-bytes keyfile bytes :none))
        (is (= 186 (length (g:key-file-to-data keyfile))))))))

;;;     g_key_file_load_from_data_dirs
;;;     g_key_file_load_from_dirs

;;;     g_key_file_to_data

(test g-key-file-to-data.1
  (glib:with-g-key-file (keyfile)
    (is (string= "" (g:key-file-to-data keyfile)))
    (is (= 0 (second (multiple-value-list (g:key-file-to-data keyfile)))))))

(test g-key-file-to-data.2
  (glib:with-g-key-file (keyfile)
    (let ((path (glib-sys:sys-path "test/resource/rtest-glib-key-file.ini")))
      (is-true (g:key-file-load-from-file keyfile path :none))
      (is (= 161 (second (multiple-value-list (g:key-file-to-data keyfile))))))))

;;;     g_key_file_save_to_file

(test g-key-file-to-file
  (let ((path (glib-sys:sys-path "test/out/rtest-glib-key-file.tmp")))
    (glib:with-g-key-file (keyfile)
      (is-true (g:key-file-load-from-data keyfile *key-values* :none))
      (is-true (g:key-file-save-to-file keyfile path)))
    (glib:with-g-key-file (keyfile)
      (is-true (g:key-file-load-from-file keyfile path :none))
      (is (= 186 (second (multiple-value-list (g:key-file-to-data keyfile))))))))

;;;     g_key_file_get_start_group

(test g-key-file-start-group
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :none))
    (is (string= "First Group" (g:key-file-start-group keyfile)))))

;;;     g_key_file_get_groups

(test g-key-file-groups
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :none))
    (is (equal '("First Group" "Another Group")
               (g:key-file-groups keyfile)))))

;;;     g_key_file_get_keys

(test g-key-file-keys
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :none))
    (is (equal '("Boolean" "Integer" "Numbers" "Booleans")
               (g:key-file-keys keyfile "Another Group")))))

;;;     g_key_file_has_group

(test g-key-file-has-group
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :none))
    (is-false (g:key-file-has-group keyfile "unknown"))
    (is-true (g:key-file-has-group keyfile "First Group"))
    (is-true (g:key-file-has-group keyfile "Another Group"))))

;;;     g_key_file_has_key

(test g-key-file-has-key
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :none))
    (is-false (g:key-file-has-key keyfile "Another Group" "unknown"))
    (is-true (g:key-file-has-key keyfile "Another Group" "Numbers"))
    (is-true (g:key-file-has-key keyfile "Another Group" "Booleans"))))

;;;     g_key_file_get_value
;;;     g_key_file_set_value

(test g-key-file-value
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :none))
    (is (string= "true" (g:key-file-value keyfile "Another Group" "Boolean")))
    (is (string= "false"
                 (setf (g:key-file-value keyfile "Another Group" "Boolean")
                       "false")))
    (is (string= "false" (g:key-file-value keyfile "Another Group" "Boolean")))
    (is (string= "100" (g:key-file-value keyfile "Another Group" "Integer")))
    ;; Returns nil, if the group or key cannot be found
    (is-false (g:key-file-value keyfile "Another Group" "unknown"))
    (is-false (g:key-file-value keyfile "unknown" "unknown"))))

;;;     g_key_file_get_string
;;;     g_key_file_set_string

(test g-key-file-string.1
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :none))
    (is (string= "Hello" (g:key-file-string keyfile "First Group" "Welcome")))
    (is (string= "Hello Dieter"
                 (setf (g:key-file-string keyfile "First Group" "Welcome")
                       "Hello Dieter")))
    (is (string= "Hello Dieter"
                 (g:key-file-string keyfile "First Group" "Welcome")))))

(test g-key-file-string.2
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :none))
    (is (string= "Hello" (g:key-file-string keyfile "First Group" "Welcome")))
    (is-false (g:key-file-string keyfile "First Group" "unknown"))
    (is-false (g:key-file-string keyfile "unknown" "unkonwn"))))

;;;     g_key_file_get_locale_string
;;;     g_key_file_get_locale_for_key
;;;     g_key_file_get_boolean
;;;     g_key_file_get_integer
;;;     g_key_file_get_int64
;;;     g_key_file_get_uint64
;;;     g_key_file_get_double

;;;     g_key_file_get_string_list
;;;     g_key_file_set_string_list

(test g-key-file-string-list.1
  (glib:with-g-key-file (keyfile)
    (let ((path (glib-sys:sys-path "test/resource/rtest-glib-key-file.ini")))
      (is-true (g:key-file-load-from-file keyfile path :none))
      (is (equal '("2" "20" "-200" "0")
                 (g:key-file-string-list keyfile "Another Group" "Numbers"))))))

(test g-key-file-string-list.2
  (glib:with-g-key-file (keyfile)
    (is (equal '("string1" "string2" "string3")
               (setf (g:key-file-string-list keyfile "New Group" "strings")
                     '("string1" "string2" "string3"))))
    (is (equal '("string1" "string2" "string3")
               (g:key-file-string-list keyfile "New Group" "strings")))))

;;;     g_key_file_get_locale_string_list
;;;     g_key_file_get_boolean_list
;;;     g_key_file_get_integer_list
;;;     g_key_file_get_double_list

;;;     g_key_file_get_comment
;;;     g_key_file_set_comment

(test g-key-file-comment.1
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :keep-comments))
    (is (stringp (g:key-file-comment keyfile "First Group" "Welcome")))
    (is (stringp (g:key-file-comment keyfile "First Group" nil)))
    (is (stringp (g:key-file-comment keyfile nil nil)))))

(test g-key-file-comment.2
  (glib:with-g-key-file (keyfile)
    (is-true (g:key-file-load-from-data keyfile *key-values* :keep-comments))
    (is (string= "Comment"
                 (setf (g:key-file-comment keyfile "Another Group" "Integer")
                       "Comment")))
    (is (string= "Comment"
                 (g:key-file-comment keyfile "Another Group" "Integer")))
    (is (string= "Another Comment"
                 (setf (g:key-file-comment keyfile "Another Group" nil)
                       "Another Comment")))
    #+glib-2-78
    (is (string= "Another Comment"
                 (g:key-file-comment keyfile "Another Group" nil)))))

;;;     g_key_file_set_locale_string
;;;     g_key_file_set_boolean
;;;     g_key_file_set_integer
;;;     g_key_file_set_int64
;;;     g_key_file_set_uint64
;;;     g_key_file_set_double
;;;     g_key_file_set_locale_string_list
;;;     g_key_file_set_boolean_list
;;;     g_key_file_set_integer_list
;;;     g_key_file_set_double_list

;;;     g_key_file_remove_group
;;;     g_key_file_remove_key
;;;     g_key_file_remove_comment

;;; Examples from the GKeyFile documentation

(test g-key-file-example.1
  (glib:with-g-key-file (keyfile)
    ;; Load a key file
    (unless (g:key-file-load-from-file keyfile
                                       (glib-sys:sys-path "test/resource/rtest-glib-key-file.ini")
                                       :none)
      (error "Error loading the key file: RTEST-GLIB-KEY-FILE.INI"))
    ;; Read a string from the key file
    (let ((value (g:key-file-string keyfile "First Group" "Welcome")))
      (unless value
        (setf value "default-value"))
      (is (string= "Hello" value)))))

(test g-key-file-example.2
  (glib:with-g-key-file (keyfile)
    ;; Load existing key file
    (g:key-file-load-from-file keyfile
                               (glib-sys:sys-path "test/resource/rtest-glib-key-file.ini")
                               :none)
    ;; Add a string to the First Group
    (setf (g:key-file-string keyfile "First Group" "SomeKey") "New Value")
    ;; Save to a file
    (unless (g:key-file-save-to-file keyfile
                                     (glib-sys:sys-path "test/out/rtest-glib-key-file.tmp"))
      (error "Error saving key file."))
    ;; Or save to data for use elsewhere
    (let ((data (g:key-file-to-data keyfile)))
      (unless data
        (error "Error saving key file."))
      (is (stringp data)))))

;;; 2024-6-14
