(in-package :glib-test)

(def-suite gio-async-result :in gio-suite)
(in-suite gio-async-result)

;;; --- Types and Values -------------------------------------------------------

;;;     GAsyncResult

(test g-async-result-interface
  ;; Check type
  (is (g:type-is-interface "GAsyncResult"))
  ;; Check registered symbol
  (is (eq 'g:async-result
          (glib:symbol-for-gtype "GAsyncResult")))
  ;; Check type initializer
  (is (eq (g:gtype "GAsyncResult")
          (g:gtype (cffi:foreign-funcall "g_async_result_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GAsyncResult")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GAsyncResult")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GAsyncResult")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GAsyncResult" GIO:ASYNC-RESULT
                       (:EXPORT T
                        :TYPE-INITIALIZER "g_async_result_get_type"))
             (gobject:get-gtype-definition "GAsyncResult"))))

;;; --- Functions --------------------------------------------------------------

;;;     GAsyncReadyCallback

;;;     g_async_result_get_user_data
;;;     g_async_result_get_source_object
;;;     g_async_result_is_tagged
;;;     g_async_result_legacy_propagate_error

;;; 2024-10-23
