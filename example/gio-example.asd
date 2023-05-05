;;;; gio-example.asd

(asdf:defsystem :gio-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-glib)
  :components ((:file "gio-example")
               (:file "application-action")
               (:file "application-cmdline")
               (:file "application-open")
  ))

;;; --- 2023-5-1 ---------------------------------------------------------------
