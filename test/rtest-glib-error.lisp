(in-package :glib-test)

(def-suite glib-error :in glib-suite)
(in-suite glib-error)

(test g-error-boxed
  ;; Check type
  (is (g:type-is-boxed "GError"))
  ;; Check type initializer
  (is (eq (g:gtype "GError")
          (g:gtype (cffi:foreign-funcall "g_error_get_type" :size))))
  ;; Check registered name
  (is (eq 'glib:error
          (glib:symbol-for-gtype "GError"))))

;;;     with-g-error

(test with-g-error
  ;; Successfully loaded.
  (is-true (glib:with-g-error (err)
             (glib::%key-file-load-from-file
                 (g:key-file-new)
                 (namestring (glib-sys:sys-path "test/resource/rtest-glib-key-file.ini"))
                 :none
                 err)))
  ;; Signals an error
  (signals (error)
           (glib:with-g-error (err)
             (glib::%key-file-load-from-file
                 (g:key-file-new)
                 (namestring (glib-sys:sys-path ""))
                 :none
                 err))))

;;;     with-ignore-g-error

(test with-ignore-g-error
  ;; Successfully loaded
  (is-true (glib:with-ignore-g-error (err)
             (glib::%key-file-load-from-file
                 (g:key-file-new)
                 (namestring (glib-sys:sys-path "test/resource/rtest-glib-key-file.ini"))
                 :none
                 err)))
  ;; Error is ignored, the return value is NIL
  (is-false (glib:with-ignore-g-error (err)
             (glib::%key-file-load-from-file
                 (g:key-file-new)
                 (namestring (glib-sys:sys-path ""))
                 :none
                 err))))

;;;     with-catching-to-g-error

;; TODO: The WITH-CATCHING-TO-G-ERROR is for usage in callback functions which
;; have an error argument. We have no example at this time.

;;; 2024-6-22
