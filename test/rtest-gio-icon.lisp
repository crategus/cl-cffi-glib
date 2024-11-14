(in-package :glib-test)

(def-suite gio-icon :in gio-suite)
(in-suite gio-icon)

;;;     GIcon

(test g-icon-interface
  ;; Check type
  (is (g:type-is-interface "GIcon"))
  ;; Check registered symbol
  (is (eq 'g:icon
          (glib:symbol-for-gtype "GIcon")))
  ;; Check type initializer
  (is (eq (g:gtype "GIcon")
          (g:gtype (cffi:foreign-funcall "g_icon_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GIcon")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GIcon")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GIcon")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GIcon" GIO:ICON
                                         (:EXPORT T
                                          :TYPE-INITIALIZER "g_icon_get_type"))
             (gobject:get-gtype-definition "GIcon"))))

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
                 (g:icon-to-string icon3)))

    (is (= 1 (g:object-ref-count icon1)))
    (is (= 1 (g:object-ref-count icon2)))
    (is (= 1 (g:object-ref-count icon3)))))

;;;     g_icon_serialize

(test g-icon-serialize
  (let ((icon (g:icon-new-for-string "gnome-dev-cdrom-audio")))
    (is (cffi:pointerp (g:icon-serialize icon)))
    (is (string= "('themed', <['gnome-dev-cdrom-audio', 'gnome-dev-cdrom-audio-symbolic']>)"
                 (g:variant-print (g:icon-serialize icon))))))

;;;     g_icon_deserialize

(test g-icon-deserialize
  (let* ((icon1 (g:icon-new-for-string "gnome-dev-cdrom-audio"))
         (value (g:icon-serialize icon1))
         (icon2 nil))
    (is (cffi:pointerp value))
    (is (typep (setf icon2 (g:icon-deserialize value)) 'g:icon))

    (is (= 1 (g:object-ref-count icon1)))
    (is (= 1 (g:object-ref-count icon2)))))

;;; 2024-10-23
