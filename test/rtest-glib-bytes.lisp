(in-package :glib-test)

(def-suite glib-bytes :in glib-suite)
(in-suite glib-bytes)

;;; --- Types and Values -------------------------------------------------------

;;;     GBytes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:foreign-funcall "g_bytes_get_type" :size))

(test bytes-structure
  ;; Type check
  (is (g:type-is-a (g:gtype "GBytes") (g:gtype "GBoxed")))
  ;; Check the type initializer
  (is (eq (g:gtype "GBytes")
          (g:gtype (cffi:foreign-funcall "g_bytes_get_type" :size))))
  ;; Get the boxed info
  (is (eq 'gobject::boxed-opaque-info
          (type-of (get-boxed-info "GBytes"))))
  (is (eq 'glib::bytes (gobject::boxed-info-name (get-boxed-info "GBytes"))))
  (is (string= "GBytes" (gobject::boxed-info-type (get-boxed-info "GBytes"))))
  (is-false (gobject::boxed-opaque-info-alloc (get-boxed-info "GBytes")))
  (is-false (gobject::boxed-opaque-info-free (get-boxed-info "GBytes"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_bytes_new

(test bytes-new.1
  (is (typep (g:bytes-new (cffi:null-pointer) 0) 'g:bytes)))

(test bytes-new.2
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

(test bytes-data.1
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc "a test string")
    (let ((bytes (g:bytes-new data len)))
      (is (typep bytes 'g:bytes))
      (is (cffi:pointerp (g:bytes-data bytes)))
      (is (= 14 (g:bytes-size bytes)))
      (cffi:foreign-string-free data))))

(test bytes-data.2
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc "a o u")
    (let ((bytes (g:bytes-new data len)))
      (is (typep bytes 'g:bytes))
      (is (cffi:pointerp (g:bytes-data bytes)))
      (is (= 6 (g:bytes-size bytes)))
      (cffi:foreign-string-free data))))

(test bytes-data.3
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

;;; --- 2023-1-1 ---------------------------------------------------------------
