(in-package :glib-test)

(def-suite gio-file-icon :in gio-suite)
(in-suite gio-file-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GFileIcon

(test g-file-icon-class
  ;; Check type
  (is (g:type-is-object "GFileIcon"))
  ;; Check registered symbol
  (is (eq 'g:file-icon
          (glib:symbol-for-gtype "GFileIcon")))
  ;; Check type initializer
  (is (eq (g:gtype "GFileIcon")
          (g:gtype (cffi:foreign-funcall "g_file_icon_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GFileIcon")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GFileIcon")))
  ;; Check interfaces
  (is (equal '("GIcon" "GLoadableIcon")
             (glib-test:list-interfaces "GFileIcon")))
  ;; Check class properties
  (is (equal '("file")
             (glib-test:list-properties "GFileIcon")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GFileIcon")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GFileIcon" GIO:FILE-ICON
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GIcon" "GLoadableIcon")
                        :TYPE-INITIALIZER "g_file_icon_get_type")
                       ((FILE FILE-ICON-FILE "file" "GFile" T NIL)))
             (gobject:get-gtype-definition "GFileIcon"))))

;;; --- Properties -------------------------------------------------------------

;;;     file

;;; --- Functions --------------------------------------------------------------

;;;     g_file_icon_new

(test g-file-icon-new
  (let* ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png"))
         (file (g:file-new-for-path path))
         (icon (g:file-icon-new file)))
    (is (typep icon 'g:file-icon))
    (is (string= "gtk-logo-24.png"
                 (g:file-basename (g:file-icon-file icon))))))

;;; 2024-9-17
