(in-package :glib-test)

(def-suite gio-emblemed-icon :in gio-suite)
(in-suite gio-emblemed-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GEmblemedIcon

(test g-emblemed-icon-class
  ;; Check type
  (is (g:type-is-object "GEmblemedIcon"))
  ;; Check the registered symbol
  (is (eq 'gio:emblemed-icon
          (glib:symbol-for-gtype "GEmblemedIcon")))
  ;; Check type initializer
  (is (eq (g:gtype "GEmblemedIcon")
          (g:gtype (cffi:foreign-funcall "g_emblemed_icon_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GEmblemedIcon")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GEmblemedIcon")))
  ;; Check interfaces
  (is (equal '("GIcon")
             (glib-test:list-interfaces "GEmblemedIcon")))
  ;; Check class properties
  (is (equal '("gicon")
             (glib-test:list-properties "GEmblemedIcon")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GEmblemedIcon")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GEmblemedIcon" GIO:EMBLEMED-ICON
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GIcon")
                        :TYPE-INITIALIZER "g_emblemed_icon_get_type")
                       ((GICON EMBLEMED-ICON-GICON "gicon" "GIcon" T NIL)))
             (gobject:get-gtype-definition "GEmblemedIcon"))))

;;; --- Properties -------------------------------------------------------------

;;;     gicon

;;; --- Functions --------------------------------------------------------------

;;;     g_emblemed_icon_new
;;;     g_emblemed_icon_get_icon
;;;     g_emblemed_icon_get_emblems
;;;     g_emblemed_icon_add_emblem
;;;     g_emblemed_icon_clear_emblems

;;; 2024-9-18
