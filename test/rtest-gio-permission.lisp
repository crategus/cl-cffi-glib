(in-package :glib-test)

(def-suite gio-permission :in gio-suite)
(in-suite gio-permission)

;;; --- Types and Values -------------------------------------------------------

;;;     GPermission

(test g-permission-class
  ;; Check type
  (is (g:type-is-object "GPermission"))
  ;; Check registered symbol
  (is (eq 'g:permission
          (glib:symbol-for-gtype "GPermission")))
  ;; Check type initializer
  (is (eq (g:gtype "GPermission")
          (g:gtype (cffi:foreign-funcall "g_permission_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GPermission")))
  ;; Check children
  (is (equal '("GSimplePermission")
             (glib-test:list-children "GPermission")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GPermission")))
  ;; Check class properties
  (is (equal '("allowed" "can-acquire" "can-release")
             (glib-test:list-properties "GPermission")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GPermission")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GPermission" GIO:PERMISSION
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "g_permission_get_type")
                      ((ALLOWED PERMISSION-ALLOWED
                        "allowed" "gboolean" T NIL)
                       (CAN-ACQUIRE PERMISSION-CAN-ACQUIRE
                        "can-acquire" "gboolean" T NIL)
                       (CAN-RELEASE PERMISSION-CAN-RELEASE
                        "can-release" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GPermission"))))

;;; --- Properties -------------------------------------------------------------

;;;     allowed
;;;     can-acquire
;;;     can-release

(test g-permission-properties.1
  (glib-test:with-check-memory (permission)
    (setf permission (g:simple-permission-new t))
    (is-true (g:permission-allowed permission))
    (is-false (g:permission-can-acquire permission))
    (is-false (g:permission-can-release permission))))

(test g-permission-properties.2
  (glib-test:with-check-memory (permission)
    (setf permission (g:simple-permission-new nil))
    (is-false (g:permission-allowed permission))
    (is-false (g:permission-can-acquire permission))
    (is-false (g:permission-can-release permission))))

;;; --- Functions --------------------------------------------------------------

;;;     g_permission_acquire
;;;     g_permission_acquire_async
;;;     g_permission_acquire_finish
;;;     g_permission_release
;;;     g_permission_release_async
;;;     g_permission_release_finish
;;;     g_permission_impl_update

;;; 2025-05-26
