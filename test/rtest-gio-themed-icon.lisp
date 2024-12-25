(in-package :glib-test)

(def-suite gio-themed-icon :in gio-suite)
(in-suite gio-themed-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GThemedIcon

(test g-themed-icon-class
  ;; Check type
  (is (g:type-is-object "GThemedIcon"))
  ;; Check registered symbol
  (is (eq 'g:themed-icon
          (glib:symbol-for-gtype "GThemedIcon")))
  ;; Check type initializer
  (is (eq (g:gtype "GThemedIcon")
          (g:gtype (cffi:foreign-funcall "g_themed_icon_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GThemedIcon")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GThemedIcon")))
  ;; Check interfaces
  (is (equal '("GIcon")
             (glib-test:list-interfaces "GThemedIcon")))
  ;; Check class properties
  (is (equal '("name" "names" "use-default-fallbacks")
             (glib-test:list-properties "GThemedIcon")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GThemedIcon")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GThemedIcon" GIO:THEMED-ICON
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GIcon")
                        :TYPE-INITIALIZER "g_themed_icon_get_type")
                       ((NAME THEMED-ICON-NAME "name" "gchararray" NIL NIL)
                        (NAMES THEMED-ICON-NAMES "names" "GStrv" T NIL)
                        (USE-DEFAULT-FALLBACKS THEMED-ICON-USE-DEFAULT-FALLBACKS
                         "use-default-fallbacks" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GThemedIcon"))))

;;; --- Functions --------------------------------------------------------------

;;;   g_themed_icon_new
;;;   g_themed_icon_append_name
;;;   g_themed_icon_prepend_name
;;;   g_themed_icon_get_names

(test g-themed-icon-new
  (glib-test:with-check-memory (icon)
    (setf icon (g:themed-icon-new "gnome-dev-cdrom"))
    (is (equal '("gnome-dev-cdrom")
               (g:themed-icon-names icon)))
    (is-false (g:themed-icon-append-name icon "gnome-dev"))
    (is (equal '("gnome-dev-cdrom" "gnome-dev")
               (g:themed-icon-names icon)))
    (is-false (g:themed-icon-prepend-name icon "gnome-dev-cdrom-audio"))
    (is (equal '("gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev")
               (g:themed-icon-names icon)))))

;;;   g_themed_icon_new_from_names
;;;   g_themed_icon_get_names

(test g-themed-icon-new-from-names
  (glib-test:with-check-memory (icon)
    (setf icon (g:themed-icon-new-from-names "gnome-dev-cdrom-audio"
                                             "gnome-dev-cdrom"
                                             "gnome-dev"
                                             "gnome"))
    (is (equal '("gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev"
                 "gnome")
               (g:themed-icon-names icon)))))

;;;   g_themed_icon_new_with_default_fallbacks

(test g-themed-icon-new-with-default-fallbacks
  (glib-test:with-check-memory (icon1 icon2)
    (let ((names (list "gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev"
                       "gnome")))
      (setf icon1 (g:themed-icon-new-from-names names))
      (setf icon2 (g:themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio"))
      (is (equal '("gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev" "gnome")
                 (g:themed-icon-names icon1)))
      (is (equal '("gnome-dev-cdrom-audio")
                 (g:themed-icon-names icon2)))
      (is (= 2604122446 (g:icon-hash icon1)))
      (is (= 2604122446 (g:icon-hash icon2)))
      (is (= (g:icon-hash icon1) (g:icon-hash icon2)))
      (is-true (g:icon-equal icon1 icon2)))))

;;; 2024-9-19
