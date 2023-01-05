(in-package :glib-test)

(def-suite glib-quark :in glib-suite)
(in-suite glib-quark)

(test quark-convert-to-foreign
  (is (= 0 (cffi:convert-to-foreign nil 'g:quark-as-string)))
  (is (= 0 (cffi:convert-to-foreign (cffi:null-pointer) 'g:quark-as-string)))
  (is (integerp (cffi:convert-to-foreign "gboolean" 'g:quark-as-string))))

(test quark-convert-from-foreign
  (let ((id (cffi:convert-to-foreign "string1" 'g:quark-as-string)))
    (is (string= "string1" (cffi:convert-from-foreign id 'g:quark-as-string))))
  (is-false (cffi:convert-from-foreign 0 'g:quark-as-string))
  (is (stringp (cffi:convert-from-foreign 9 'g:quark-as-string))))

;;; --- 2023-1-1 ---------------------------------------------------------------
