;;; ----------------------------------------------------------------------------
;;; cl-cffi-glib.asd
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defsystem :cl-cffi-glib
  :name "cl-cffi-glib"
  :version "0.6.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :components
  ((:module glib
    :serial t
    :components
    ((:file "glib.package")
     ;; Lisp utilities
     (:file "glib.cl-utils")
     ;; Lisp support for GType, and GBoxed
     (:file "glib.stable-pointer")
     (:file "glib.gtype")
     (:file "glib.boxed-type")
     ;; GLIB library
     (:file "glib.version")
     (:file "glib.misc")
     (:file "glib.error")
     (:file "glib.main-loop")
     (:file "glib.bytes")
     (:file "glib.option")
     (:file "glib.key-file")
     (:file "glib.variant-type")
     (:file "glib.variant")))
   (:module gobject
    :serial t
    :components
    ((:file "gobject.package")
     (:file "gobject.init")
     (:file "gobject.object-function")
     (:file "gobject.generating")

     (:file "gobject.type-info")
     (:file "gobject.gvalue")
     (:file "gobject.enumeration")
     (:file "gobject.boxed-lisp")
     (:file "gobject.boxed")
     (:file "gobject.param-spec")
     (:file "gobject.param")
     (:file "gobject.gobject-class")
     (:file "gobject.base")
;     (:file "gobject.closures")
     (:file "gobject.signals")
     (:file "gobject.binding")

     (:file "gobject.utils")
     (:file "gobject.foreign-gobject-subclassing")
     ))
   (:module gio
    :serial t
    :components
    ((:file "gio.package")
     ;; Application information and launch contexts
     (:file "gio.content-type")        ; Platform-specific content typing
     (:file "gio.app-info")            ; Application information, launch context
     ;; Icons
     (:file "gio.icon")                ; Interface for icons
     (:file "gio.loadable-icon")       ; Interface for loadable icons
     (:file "gio.file-icon")           ; Icons pointing to an image file
     (:file "gio.themed-icon")         ; Icon theming support
     (:file "gio.emblemed-icon")       ; Icon with emblems
     (:file "gio.emblem")              ; An object for emblems
     ;; Settings
     (:file "gio.settings")
     ;; Resources
     (:file "gio.resource")            ; Resource framework
     ;; Permissions
     (:file "gio.permission")          ; Permission to perform a certain action
     (:file "gio.simple-permission")   ; Trivial implementation of GPermission
     ;; Data models
     (:file "gio.list-model")          ; Interface for dynamic list of objects
     (:file "gio.list-store")          ; Implementation of GListModel
     ;; Application support
     (:file "gio.action")              ; An action interface
     (:file "gio.action-group")        ; A group of actions
     (:file "gio.action-map")          ; Interface for action containers
     (:file "gio.simple-action")       ; Simple GAction implementation
     (:file "gio.property-action")     ; A GAction reflecting a GObject property
     (:file "gio.simple-action-group") ; Simple GActionGroup implementation
     (:file "gio.application")         ; Core application class
     (:file "gio.application-command-line") ; Command-line invocation
     (:file "gio.menu-model")          ; Representing the contents of a menu
     (:file "gio.menu")                ; Simple implementation of GMenuModel
     (:file "gio.notification")        ; User Notifications, pop up messages
     ;; File Operations
     (:file "gio.file")
     (:file "gio.file-info")
     ;; Asynchronous I/O
     (:file "gio.cancellable")
     (:file "gio.async-result")
     (:file "gio.task")
    ))
    (:file "glib-user.package")
    )
  :in-order-to ((asdf:test-op (test-op "cl-cffi-glib/test")))
  :defsystem-depends-on (:cl-cffi-glib-init)
  :depends-on (:iterate
               :bordeaux-threads
               :closer-mop
               :trivial-garbage))

;; Definine a test operation for the library

(defsystem :cl-cffi-glib/test
  :name "cl-cffi-glib/test"
  :version "0.6.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :components
  ((:module test
    :serial t
    :components
    (;; glib-suite
     (:file "rtest-glib")
     (:file "rtest-glib-stable-pointer")
     (:file "rtest-glib-gtype")
     (:file "rtest-glib-boxed-type")
     (:file "rtest-glib-version")
     (:file "rtest-glib-misc")
     (:file "rtest-glib-error")
     (:file "rtest-glib-main-loop")
     (:file "rtest-glib-bytes")
     (:file "rtest-glib-option")
     (:file "rtest-glib-key-file")
     (:file "rtest-glib-variant-type")
     (:file "rtest-glib-variant")

     ;; gobject-suite
     (:file "rtest-gobject-type-info")
     (:file "rtest-gobject-gvalue")
     (:file "rtest-gobject-enumeration")
     (:file "rtest-gobject-boxed-lisp")
     (:file "rtest-gobject-param-spec")
     (:file "rtest-gobject-param")
     (:file "rtest-gobject-gobject-class")
     (:file "rtest-gobject-base")
     (:file "rtest-gobject-signals")
     (:file "rtest-gobject-binding")
     (:file "rtest-gobject-utils")
     (:file "rtest-gobject-generating")
     (:file "rtest-gobject-subclassing")

     ;; gio-suite

     ;; Application information and launch contexts
     (:file "rtest-gio-content-type")
     (:file "rtest-gio-app-info")
     ;; Icons
     (:file "rtest-gio-icon")
     (:file "rtest-gio-loadable-icon")
     (:file "rtest-gio-file-icon")
     (:file "rtest-gio-themed-icon")
     (:file "rtest-gio-emblemed-icon")
     (:file "rtest-gio-emblem")
     ;; Resources
     (:file "rtest-gio-resource")
     ;; Permissions
     (:file "rtest-gio-permission")
;    (:file "gio.simple-permission")
     ;; Data models
     (:file "rtest-gio-list-model")
     (:file "rtest-gio-list-store")
     ;; Application support
     (:file "rtest-gio-action")
     (:file "rtest-gio-action-group")
     (:file "rtest-gio-action-map")
     (:file "rtest-gio-simple-action")
     (:file "rtest-gio-property-action")
     (:file "rtest-gio-simple-action-group")
     (:file "rtest-gio-application")
     (:file "rtest-gio-application-command-line")
     (:file "rtest-gio-menu-model")
     (:file "rtest-gio-menu")
     (:file "rtest-gio-notification")
     ;; File Operations
     (:file "rtest-gio-file")
     (:file "rtest-gio-file-info")
     ;; Asynchronous I/O
     (:file "rtest-gio-cancellable")
     (:file "rtest-gio-async-result")
     (:file "rtest-gio-task")

     (:file "rtest-glib-check-memory")
     (:file "rtest-glib-finish")
     )))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :glib-test :glib-test)))
  :depends-on (:cl-cffi-glib :local-time :fiveam))

;;; --- End of file cl-cffi-glib.asd -------------------------------------------
