(in-package :glib-test)

(def-suite glib-bytes :in glib-suite)
(in-suite glib-bytes)

;;; --- Types and Values -------------------------------------------------------

;;;     GBytes

(test g-bytes-structure
  ;; Check type
  (is (g:type-is-a (g:gtype "GBytes") (g:gtype "GBoxed")))
  ;; Check type initializer
  (is (eq (g:gtype "GBytes")
          (g:gtype (cffi:foreign-funcall "g_bytes_get_type" :size))))
  ;; Check boxed info
  (is (eq 'glib::boxed-opaque-info
          (type-of (glib:get-boxed-info "GBytes"))))
  (is (eq 'glib::bytes (glib::boxed-info-name (glib:get-boxed-info "GBytes"))))
  (is (string= "GBytes"
               (glib::boxed-info-gtype (glib:get-boxed-info "GBytes"))))
  (is-false (glib::boxed-opaque-info-alloc (glib:get-boxed-info "GBytes")))
  (is-false (glib::boxed-opaque-info-free (glib:get-boxed-info "GBytes"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_bytes_new

(test g-bytes-new.1
  (is (typep (g:bytes-new (cffi:null-pointer) 0) 'g:bytes)))

(test g-bytes-new.2
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc "a test string")
    (is (typep (g:bytes-new data len) 'g:bytes))
    (cffi:foreign-string-free data)))

;;;     g_bytes_new_take
;;;     g_bytes_new_static
;;;     g_bytes_new_with_free_func
;;;     g_bytes_new_from_bytes

;;;     g_bytes_get_data
;;;     g_bytes_get_size

(test g-bytes-data.1
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc "a test string")
    (let ((bytes (g:bytes-new data len)))
      (is (typep bytes 'g:bytes))
      (is (cffi:pointerp (g:bytes-data bytes)))
      (is (= 14 (g:bytes-size bytes)))
      (cffi:foreign-string-free data))))

(test g-bytes-data.2
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc "a o u")
    (let ((bytes (g:bytes-new data len)))
      (is (typep bytes 'g:bytes))
      (is (cffi:pointerp (g:bytes-data bytes)))
      (is (= 6 (g:bytes-size bytes)))
      (cffi:foreign-string-free data))))

(test g-bytes-data.3
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc "ä ö ü")
    (let ((bytes (g:bytes-new data len)))
      (is (typep bytes 'g:bytes))
      (is (cffi:pointerp (g:bytes-data bytes)))
      (is (= 9 (g:bytes-size bytes)))
      (cffi:foreign-string-free data))))

;;;     g_bytes_hash
;;;     g_bytes_equal
;;;     g_bytes_compare
;;;     g_bytes_ref
;;;     g_bytes_unref
;;;     g_bytes_unref_to_data
;;;     g_bytes_unref_to_array

;;; 2024-9-17
