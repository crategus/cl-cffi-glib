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

;;;     with-error

(test with-error
  ;; Successfully loaded
  (is-true (glib:with-error (err)
             (glib::%key-file-load-from-file
                 (g:key-file-new)
                 (namestring (glib-sys:sys-path "test/resource/rtest-glib-key-file.ini"))
                 :none
                 err)))
  ;; Signals an error
  (signals (error)
           (glib:with-error (err)
             (glib::%key-file-load-from-file
                 (g:key-file-new)
                 (namestring (glib-sys:sys-path ""))
                 :none
                 err))))

;;;     with-ignore-error

(test with-ignore-error
  ;; Successfully loaded
  (is-true (glib:with-ignore-error (err)
             (glib::%key-file-load-from-file
                 (g:key-file-new)
                 (namestring (glib-sys:sys-path "test/resource/rtest-glib-key-file.ini"))
                 :none
                 err)))
  ;; Error is ignored, the return value is NIL
  (is-false (glib:with-ignore-error (err)
             (glib::%key-file-load-from-file
                 (g:key-file-new)
                 (namestring (glib-sys:sys-path ""))
                 :none
                 err))))

;;;     with-catching-to-error

;; TODO: The WITH-CATCHING-TO-ERROR is for usage in callback functions which
;; have an error argument. We have no example at this time.

;;; 2025-05-18
