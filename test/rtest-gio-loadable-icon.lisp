(in-package :glib-test)

(def-suite gio-loadable-icon :in gio-suite)
(in-suite gio-loadable-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GLoadableIcon

(test loadable-icon-interface
  ;; Type check
  (is (g:type-is-interface "GLoadableIcon"))
  ;; Check the registered symbol
  (is (eq 'g:loadable-icon
          (gobject:symbol-for-gtype "GLoadableIcon")))
  ;; Check the type initializer
  (is (eq (g:gtype "GLoadableIcon")
          (g:gtype (cffi:foreign-funcall "g_loadable_icon_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'g:param-spec-name
                     (g:object-interface-list-properties "GLoadableIcon"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GLoadableIcon" G-LOADABLE-ICON
                                  (:EXPORT T))
             (get-g-type-definition "GLoadableIcon"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_loadable_icon_load
;;;     g_loadable_icon_load_async
;;;     g_loadable_icon_load_finish

;;; 2022-10-27
