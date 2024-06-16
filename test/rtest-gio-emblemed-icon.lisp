(in-package :glib-test)

(def-suite gio-emblemed-icon :in gio-suite)
(in-suite gio-emblemed-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GEmblemedIcon

(test g-emblemed-icon-class
  ;; Type check
  (is (g:type-is-object "GEmblemedIcon"))
  ;; Check the registered symbol
  (is (eq 'gio:emblemed-icon
          (glib:symbol-for-gtype "GEmblemedIcon")))
  ;; Check the type initializer
  (is (eq (g:gtype "GEmblemedIcon")
          (g:gtype (cffi:foreign-funcall "g_emblemed_icon_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GEmblemedIcon")))
  ;; Check the children
  #+gtk4
  (is (equal '()
             (list-children "GEmblemedIcon")))
  #+gtk3
  (is (equal '("GtkNumerableIcon")
             (list-children "GEmblemedIcon")))
  ;; Check the interfaces
  (is (equal '("GIcon")
             (list-interfaces "GEmblemedIcon")))
  ;; Check the class properties
  (is (equal '("gicon")
             (list-properties "GEmblemedIcon")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GEmblemedIcon")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GEmblemedIcon" G-EMBLEMED-ICON
                       (:SUPERCLASS G-OBJECT
                        :EXPORT T
                        :INTERFACES ("GIcon")
                        :TYPE-INITIALIZER "g_emblemed_icon_get_type")
                       ((GICON G-EMBLEMED-ICON-GICON "gicon" "GIcon" T NIL)))
             (gobject:get-g-type-definition "GEmblemedIcon"))))

;;; --- Properties -------------------------------------------------------------

;;;     gicon

;;; --- Functions --------------------------------------------------------------

;;;     g_emblemed_icon_new
;;;     g_emblemed_icon_get_icon
;;;     g_emblemed_icon_get_emblems
;;;     g_emblemed_icon_add_emblem
;;;     g_emblemed_icon_clear_emblems

;;; 2024-6-12
