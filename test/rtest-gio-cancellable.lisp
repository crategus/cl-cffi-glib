(in-package :glib-test)

(def-suite gio-cancellable :in gio-suite)
(in-suite gio-cancellable)

;;; Types and Values

;;;     GCancellable

(test g-cancellable-class
  ;; Check type
  (is (g:type-is-object "GCancellable"))
  ;; Check registered symbol
  (is (eq 'g:cancellable
          (glib:symbol-for-gtype "GCancellable")))
  ;; Check type initializer
  (is (eq (g:gtype "GCancellable")
          (g:gtype (cffi:foreign-funcall "g_cancellable_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GCancellable")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GCancellable")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GCancellable")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GCancellable")))
  ;; Check signals
  (is (equal '("cancelled")
             (glib-test:list-signals "GCancellable")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GCancellable" GIO:CANCELLABLE
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "g_cancellable_get_type")
                       NIL)
             (gobject:get-gtype-definition "GCancellable"))))

;;; --- Signals ----------------------------------------------------------------

;;;     cancelled

(test g-cancellable-cancelled-signal
  (let* ((name "cancelled")
         (gtype (g:gtype "GCancellable"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     GCancellableSourceFunc

;;;     g_cancellable_new

(test g-cancellable-new
  (let (cancellable)
    (is (typep (setf cancellable (g:cancellable-new)) 'g:cancellable))
    (is (= 1 (g:object-ref-count cancellable)))))

;;;     g_cancellable_is_cancelled

(test g-cancellable-is-cancelled
  (is-false (g:cancellable-is-cancelled (g:cancellable-new)))
  (is-false (g:cancellable-is-cancelled nil)))

;;;     g_cancellable_set_error_if_cancelled
;;;     g_cancellable_get_fd
;;;     g_cancellable_make_pollfd
;;;     g_cancellable_release_fd

;;;     g_cancellable_source_new

;;;     g_cancellable_get_current
;;;     g_cancellable_pop_current
;;;     g_cancellable_push_current

;;;     g_cancellable_connect
;;;     g_cancellable_disconnect

;;;     g_cancellable_cancel
;;;     g_cancellable_reset

(test g-cancellabel-cancel
  (let ((cancellable (g:cancellable-new)))
    (is-false (g:cancellable-is-cancelled cancellable))
    (is-false (g:cancellable-cancel cancellable))
    (is-true (g:cancellable-is-cancelled cancellable))
    (is-false (g:cancellable-reset cancellable))
    (is-false (g:cancellable-is-cancelled cancellable))))

;;; 2024-10-23
