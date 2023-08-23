;;;; gio-example.asd

(asdf:defsystem :gio-example
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :depends-on (:cl-cffi-glib)
  :components ((:file "gio-example")
               (:file "application-action")
               (:file "application-cmdline")
               (:file "application-open")
  ))

;;; --- 2023-8-23 --------------------------------------------------------------
