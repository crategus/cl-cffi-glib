(in-package :glib-test)

(def-suite gio-loadable-icon :in gio-suite)
(in-suite gio-loadable-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GLoadableIcon

(test g-loadable-icon-interface
  ;; Check type
  (is (g:type-is-interface "GLoadableIcon"))
  ;; Check registered symbol
  (is (eq 'g:loadable-icon
          (glib:symbol-for-gtype "GLoadableIcon")))
  ;; Check type initializer
  (is (eq (g:gtype "GLoadableIcon")
          (g:gtype (cffi:foreign-funcall "g_loadable_icon_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GIcon" "GObject")
             (glib-test:list-interface-prerequisites "GLoadableIcon")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GLoadableIcon")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GLoadableIcon")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GLoadableIcon" GIO:LOADABLE-ICON
                                         (:EXPORT T
                                          :TYPE-INITIALIZER
                                          "g_loadable_icon_get_type"))
             (gobject:get-gtype-definition "GLoadableIcon"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_loadable_icon_load
;;;     g_loadable_icon_load_async
;;;     g_loadable_icon_load_finish

;;; 2024-9-17
