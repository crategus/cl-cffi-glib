(in-package :glib-test)

(def-suite gio-content-type :in gio-suite)
(in-suite gio-content-type)

;; TODO: Check the memory management of the funtions, there may be a problem.

;;;     g_content_type_equals

(test g-content-type-equals
  (is-true (g:content-type-equals "text/plain" "text/plain"))
  (is-false (g:content-type-equals "text/css" "text/plain"))
  (is-false (g:content-type-equals "text/plain" "text/html")))

;;;     g_content_type_is_a

#-windows
(test g-content-type-is-a
  (is-true (g:content-type-is-a "text/plain" "text/plain"))
  (is-true (g:content-type-is-a "text/css" "text/plain"))
  (is-true (g:content-type-is-a "text/html" "text/plain"))
  (is-false (g:content-type-is-a "text/plain" "text")))

;;;     g_content_type_is_mime_type

#-windows
(test g-content-type-is-mime-type
  (is-true (g:content-type-is-mime-type "text/plain" "text/plain"))
  (is-true (g:content-type-is-mime-type "text/css" "text/plain")))

;;;     g_content_type_is_unknown

#-windows
(test g-content-type-is-unkown
  (is-true (g:content-type-is-unknown "application/octet-stream")))

;;;     g_content_type_get_description

(test g-content-type-description
  (is (every #'stringp
             (mapcar #'g:content-type-description
                     (g:content-types-registered)))))

;;;     g_content_type_get_mime_type

(test g-content-type-mime-type
  (is (every #'stringp
             (mapcar #'g:content-type-mime-type
                     (g:content-types-registered)))))

;;;     g_content_type_get_mime_dirs

(test g-context-type-mime-dirs.1
  (is (every #'stringp
             (g:content-type-mime-dirs))))

#+crategus
(test g-content-type-mime-dirs.2
  (is (equal '("/home/dieter/.local/share/mime"
               "/usr/share/ubuntu/mime"
               "/usr/share/gnome/mime"
               "/home/dieter/.local/share/flatpak/exports/share/mime"
               "/var/lib/flatpak/exports/share/mime"
               "/usr/local/share/mime"
               "/usr/share/mime"
               "/var/lib/snapd/desktop/mime") (g:content-type-mime-dirs))))

;;;     g_content_type_get_icon

(test g-content-type-icon
  (glib-test:with-check-memory (icon)
    (is (typep (setf icon
                     (g:content-type-icon "image/cgm")) 'g:themed-icon))))

;;;     g_content_type_get_symbolic_icon

(test g-content-type-symbolic-icon
  (glib-test:with-check-memory (icon)
    (is (typep (setf icon
                     (g:content-type-symbolic-icon "image/cgm"))
               'g:themed-icon))))

;;;     g_content_type_get_generic_icon_name

#-windows ; On Windows we have no generic icon name
(test g-content-type-generic-icon-name
  (is (every #'stringp
             (mapcar #'g:content-type-generic-icon-name
                     (g:content-types-registered)))))

;;;     g_content_type_can_be_executable

#-windows
(test g-content-type-can-be-executable
  (is-true (g:content-type-can-be-executable "text/html"))
  (is-true (g:content-type-can-be-executable "application/xml")))

;;;   g_content_type_from_mime_type

(test g-content-type-from-mime-type
  (is (every #'stringp
             (mapcar #'g:content-type-from-mime-type
                     (g:content-types-registered)))))

;;;   g_content_type_guess                                  not implemented
;;;   g_content_type_guess_for_tree                         not implemented

;;;   g_content_types_get_registered

(test g-content-types-registered
  (is (every #'stringp (g:content-types-registered))))

;;; 2025-5-1
