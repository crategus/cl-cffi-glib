(in-package :glib-test)

(def-suite gio-simple-permission :in gio-suite)
(in-suite gio-simple-permission)

;;;     GSimplePermission

(test g-simple-permission-class
  ;; Check type
  (is (g:type-is-object "GSimplePermission"))
  ;; Check registered symbol
  (is (eq 'g:simple-permission
          (glib:symbol-for-gtype "GSimplePermission")))
  ;; Check type initializer
  (is (eq (g:gtype "GSimplePermission")
          (g:gtype (cffi:foreign-funcall "g_simple_permission_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GPermission")
          (g:type-parent "GSimplePermission")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GSimplePermission")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GSimplePermission")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GSimplePermission")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GSimplePermission")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GSimplePermission" GIO:SIMPLE-PERMISSION
                      (:SUPERCLASS GIO:PERMISSION
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "g_simple_permission_get_type")
                      NIL)
             (gobject:get-gtype-definition "GSimplePermission"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_simple_permission_new

(test g-simple-permission-new
  (glib-test:with-check-memory (permission)
    (is (typep (setf permission
                     (g:simple-permission-new t)) 'g:simple-permission))
    (is-true (g:permission-allowed permission))
    (is (typep (setf permission
                     (g:simple-permission-new nil)) 'g:simple-permission))
    (is-false (g:permission-allowed permission))))

;;; 2025-05-26
