(in-package :glib-test)

(def-suite gio-loadable-icon :in gio-suite)
(in-suite gio-loadable-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GLoadableIcon

(test g-loadable-icon-interface
  ;; Type check
  (is (g:type-is-interface "GLoadableIcon"))
  ;; Check the registered symbol
  (is (eq 'g:loadable-icon
          (glib:symbol-for-gtype "GLoadableIcon")))
  ;; Check the type initializer
  (is (eq (g:gtype "GLoadableIcon")
          (g:gtype (cffi:foreign-funcall "g_loadable_icon_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GLoadableIcon")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GLoadableIcon")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GLoadableIcon" G-LOADABLE-ICON
                                          (:EXPORT T
                                           :TYPE-INITIALIZER
                                           "g_loadable_icon_get_type"))
             (gobject:get-g-type-definition "GLoadableIcon"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_loadable_icon_load
;;;     g_loadable_icon_load_async
;;;     g_loadable_icon_load_finish

;;; 2024-6-12
