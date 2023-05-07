(in-package :glib-test)

(def-suite gio-cancellable :in gio-suite)
(in-suite gio-cancellable)

;;; Types and Values

;;;     GCancellable

(test g-cancellable-class
  ;; Type check
  (is (g:type-is-object "GCancellable"))
  ;; Check the registered symbol
  (is (eq 'g:cancellable
          (gobject:symbol-for-gtype "GCancellable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GCancellable")
          (g:gtype (cffi:foreign-funcall "g_cancellable_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GCancellable")))
  ;; Check the children
  (is (equal '()
             (list-children "GCancellable")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GCancellable")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GCancellable")))
  ;; Check the list of signals
  (is (equal '("cancelled")
             (list-signals "GCancellable")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GCancellable" G-CANCELLABLE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL) NIL)
             (get-g-type-definition "GCancellable"))))

;;; Signals

;;;     cancelled

(test g-cancellable-cancelled-signal
  (let ((query (g:signal-query (g:signal-lookup "cancelled" "GCancellable"))))
    (is (string= "cancelled" (g:signal-query-signal-name query)))
    (is (string= "GCancellable" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; Functions

;;;     GCancellableSourceFunc

;;;     g_cancellable_new

(test g-cancellable-new
  (is (typep (g:cancellable-new) 'g:cancellable))
)

;;;     g_cancellable_is_cancelled
;;;     g_cancellable_set_error_if_cancelled
;;;     g_cancellable_get_fd
;;;     g_cancellable_make_pollfd
;;;     g_cancellable_release_fd
;;;     g_cancellable_source_new
;;;     g_cancellable_get_current
;;;     g_cancellable_pop_current
;;;     g_cancellable_push_current
;;;     g_cancellable_reset
;;;     g_cancellable_connect
;;;     g_cancellable_disconnect
;;;     g_cancellable_cancel

;;; --- 2023-5-7 ---------------------------------------------------------------
