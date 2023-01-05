(in-package :glib-test)

(def-suite gio-permission :in gio-suite)
(in-suite gio-permission)

;;; --- Types and Values -------------------------------------------------------

;;;     GPermission

(test permission-class
  ;; Type check
  (is (g:type-is-object "GPermission"))
  ;; Check the registered symbol
  (is (eq 'g:permission
          (gobject:symbol-for-gtype "GPermission")))
  ;; Check the type initializer
  (is (eq (g:gtype "GPermission")
          (g:gtype (cffi:foreign-funcall "g_permission_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GPermission")))
  ;; Check the children
  (is (equal '()
             (list-children "GPermission")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GPermission")))
  ;; Check the class properties
  (is (equal '("allowed" "can-acquire" "can-release")
             (list-properties "GPermission")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GPermission")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GPermission" G-PERMISSION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL)
                       ((ALLOWED G-PERMISSION-ALLOWED "allowed" "gboolean" T
                         NIL)
                        (CAN-ACQUIRE G-PERMISSION-CAN-ACQUIRE "can-acquire"
                         "gboolean" T NIL)
                        (CAN-RELEASE G-PERMISSION-CAN-RELEASE "can-release"
                         "gboolean" T NIL)))
             (get-g-type-definition "GPermission"))))

;;; --- Properties -------------------------------------------------------------

;;;     allowed
;;;     can-acquire
;;;     can-release

;;; --- Functions --------------------------------------------------------------

;;;     g_permission_get_allowed
;;;     g_permission_get_can_acquire
;;;     g_permission_get_can_release
;;;     g_permission_acquire
;;;     g_permission_acquire_async
;;;     g_permission_acquire_finish
;;;     g_permission_release
;;;     g_permission_release_async
;;;     g_permission_release_finish
;;;     g_permission_impl_update

;;; 2022-10-30
