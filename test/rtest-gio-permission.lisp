(in-package :glib-test)

(def-suite gio-permission :in gio-suite)
(in-suite gio-permission)

;;; --- Types and Values -------------------------------------------------------

;;;     GPermission

(test g-permission-class
  ;; Type check
  (is (g:type-is-object "GPermission"))
  ;; Check the registered symbol
  (is (eq 'g:permission
          (glib:symbol-for-gtype "GPermission")))
  ;; Check the type initializer
  (is (eq (g:gtype "GPermission")
          (g:gtype (cffi:foreign-funcall "g_permission_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GPermission")))
  ;; Check the children
  (is (equal '("GSimplePermission")
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
             (gobject:get-g-type-definition "GPermission"))))

;;;     GSimplePermission

(test g-simple-permission-class
  ;; Type check
  (is (g:type-is-object "GSimplePermission"))
  ;; Check the registered symbol
  (is (eq 'g:simple-permission
          (glib:symbol-for-gtype "GSimplePermission")))
  ;; Check the type initializer
  (is (eq (g:gtype "GSimplePermission")
          (g:gtype (cffi:foreign-funcall "g_simple_permission_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GPermission")
          (g:type-parent "GSimplePermission")))
  ;; Check the children
  (is (equal '()
             (list-children "GSimplePermission")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GSimplePermission")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GSimplePermission")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GSimplePermission")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GSimplePermission" G-SIMPLE-PERMISSION
                       (:SUPERCLASS G-PERMISSION :EXPORT T :INTERFACES NIL) NIL)
             (gobject:get-g-type-definition "GSimplePermission"))))

;;; --- Properties -------------------------------------------------------------

;;;     allowed
;;;     can-acquire
;;;     can-release

(test g-permission-properties.1
  (let ((permission (g:simple-permission-new t)))
    (is-true (g:permission-allowed permission))
    (is-false (g:permission-can-acquire permission))
    (is-false (g:permission-can-release permission))))

(test g-permission-properties.2
  (let ((permission (g:simple-permission-new nil)))
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

;;; --- 2023-5-29 --------------------------------------------------------------
