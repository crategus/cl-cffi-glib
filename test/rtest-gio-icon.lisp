(in-package :glib-test)

(def-suite gio-icon :in gio-suite)
(in-suite gio-icon)

;;;     GIcon

(test g-icon-interface
  ;; Type check
  (is (g:type-is-interface "GIcon"))
  ;; Check the registered symbol
  (is (eq 'g:icon
          (glib:symbol-for-gtype "GIcon")))
  ;; Check the type initializer
  (is (eq (g:gtype "GIcon")
          (g:gtype (cffi:foreign-funcall "g_icon_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GIcon")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GIcon")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GIcon" G-ICON
                                          (:EXPORT T
                                           :TYPE-INITIALIZER "g_icon_get_type"))
             (gobject:get-g-type-definition "GIcon"))))

;;;   g_icon_hash

(test g-icon-hash
  (let* ((names (list "gnome-dev-cdrom-audio"))
         (icon1 (g:themed-icon-new-from-names names))
         (icon2 (g:themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
    (is (= 1275220063 (g:icon-hash icon1)))
    (is (= 2604122446 (g:icon-hash icon2)))))

;;;   g_icon_equal

(test g-icon-equal
  (let* ((names (list "gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev" "gnome"))
         (icon1 (g:themed-icon-new-from-names names))
         (icon2 (g:themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
    (is-true (g:icon-equal icon1 icon2))))

;;;   g_icon_to_string

(test g-icon-to-string
  (let* ((names (list "gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev" "gnome"))
         (icon1 (g:themed-icon-new "gnome-dev-cdrom-audio"))
         (icon2 (g:themed-icon-new-from-names names))
         (icon3 (g:themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
    (is (string= "gnome-dev-cdrom-audio" (g:icon-to-string icon1)))
    (is (string= ". GThemedIcon gnome-dev-cdrom-audio gnome-dev-cdrom gnome-dev gnome gnome-dev-cdrom-audio-symbolic gnome-dev-cdrom-symbolic gnome-dev-symbolic gnome-symbolic"
                 (g:icon-to-string icon2)))
    (is (string= ". GThemedIcon gnome-dev-cdrom-audio gnome-dev-cdrom gnome-dev gnome gnome-dev-cdrom-audio-symbolic gnome-dev-cdrom-symbolic gnome-dev-symbolic gnome-symbolic"
                 (g:icon-to-string icon3)))))

;;;   g_icon_new_for_string

(test g-icon-new-for-string
  (let ((icon1 (g:icon-new-for-string "gnome-dev-cdrom-audio"))
        (icon2 (g:icon-new-for-string ". GThemedIcon gnome-dev-cdrom-audio
                                       gnome-dev-cdrom gnome-dev gnome"))
        (icon3 (g:themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
    (is (string= "gnome-dev-cdrom-audio" (g:icon-to-string icon1)))
    (is (string= ". GThemedIcon gnome-dev-cdrom-audio%0A  gnome-dev-cdrom gnome-dev gnome gnome-dev-cdrom-audio%0A-symbolic -symbolic gnome-dev-cdrom-symbolic gnome-dev-symbolic gnome-symbolic"
                 (g:icon-to-string icon2)))
    (is (string= ". GThemedIcon gnome-dev-cdrom-audio gnome-dev-cdrom gnome-dev gnome gnome-dev-cdrom-audio-symbolic gnome-dev-cdrom-symbolic gnome-dev-symbolic gnome-symbolic"
                 (g:icon-to-string icon3)))))

;;;     g_icon_serialize

(test g-icon-serialize
  (let ((icon (g:icon-new-for-string "gnome-dev-cdrom-audio")))
    (is (cffi:pointerp (g:icon-serialize icon)))
    (is (string= "('themed', <['gnome-dev-cdrom-audio', 'gnome-dev-cdrom-audio-symbolic']>)"
                 (g:variant-print (g:icon-serialize icon))))))

;;;     g_icon_deserialize

(test g-icon-deserialize
  (let* ((icon (g:icon-new-for-string "gnome-dev-cdrom-audio"))
         (value (g:icon-serialize icon)))
    (is (cffi:pointerp value))
    (is (typep (g:icon-deserialize value) 'g:icon))))

;;; --- 2023-7-9 ---------------------------------------------------------------
