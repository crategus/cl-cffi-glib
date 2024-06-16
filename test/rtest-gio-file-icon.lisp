(in-package :glib-test)

(def-suite gio-file-icon :in gio-suite)
(in-suite gio-file-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GFileIcon

(test g-file-icon-class
  ;; Type check
  (is (g:type-is-object "GFileIcon"))
  ;; Check the registered symbol
  (is (eq 'g:file-icon
          (glib:symbol-for-gtype "GFileIcon")))
  ;; Check the type initializer
  (is (eq (g:gtype "GFileIcon")
          (g:gtype (cffi:foreign-funcall "g_file_icon_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GFileIcon")))
  ;; Check the children
  (is (equal '()
             (list-children "GFileIcon")))
  ;; Check the interfaces
  (is (equal '("GIcon" "GLoadableIcon")
             (list-interfaces "GFileIcon")))
  ;; Check the class properties
  (is (equal '("file")
             (list-properties "GFileIcon")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GFileIcon")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GFileIcon" G-FILE-ICON
                       (:SUPERCLASS G-OBJECT
                        :EXPORT T
                        :INTERFACES ("GIcon" "GLoadableIcon")
                        :TYPE-INITIALIZER "g_file_icon_get_type")
                       ((FILE G-FILE-ICON-FILE "file" "GFile" T NIL)))
             (gobject:get-g-type-definition "GFileIcon"))))

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

;;; 2024-6-14
