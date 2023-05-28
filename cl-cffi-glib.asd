;;; ----------------------------------------------------------------------------
;;; cl-cffi-glib.asd
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
  :version "0.3.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :components
  ((:module glib
    :serial t
    :components
    ((:file "glib.package")
     ;; Lisp support for callbacks, GType, and GBoxed
     (:file "glib.stable-pointer")
     (:file "glib.gtype")
     (:file "glib.boxed-type")
     ;; GLIB library
     (:file "glib.version")
     (:file "glib.quark")
     (:file "glib.misc")
     (:file "glib.error")
     (:file "glib.main-loop")
     (:file "glib.utils")
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
     (:file "gobject.glib-defcallback")
     (:file "gobject.generating")

     (:file "gobject.type-info")
     (:file "gobject.gvalue")
     (:file "gobject.enumeration")
     (:file "gobject.boxed-lisp")
     (:file "gobject.boxed")
     ;; TODO: Glib types which need GBoxed, can we improve this
;     (:file "../glib/glib.bytes")
;     (:file "../glib/glib.variant-type")
;     (:file "../glib/glib.variant")
     (:file "gobject.param-spec")      ; GParamSpec
     (:file "gobject.param")           ; Parameters and Values
     (:file "gobject.gobject-class")
     (:file "gobject.base")            ; The Base Object Type
     (:file "gobject.closures")        ; Closures
     (:file "gobject.signals")         ; Signals
     (:file "gobject.binding")         ; Bind two object properties

     (:file "gobject.utils")
     (:file "gobject.foreign-gobject-subclassing")
     ))
   (:module gio
    :serial t
    :components
    ((:file "gio.package")
     (:file "gio.init")
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
     (:file "gio.file")                ; File and Directory Handling
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
  :components
  ((:module test
    :serial t
    :components
    (;; glib-suite
     (:file "rtest-glib")
     (:file "rtest-glib-stable-pointer")
     (:file "rtest-glib-gtype")
     (:file "rtest-glib-version")
     (:file "rtest-glib-quark")
     (:file "rtest-glib-misc")
     (:file "rtest-glib-error")
     (:file "rtest-glib-main-loop")
     (:file "rtest-glib-utils")
     (:file "rtest-glib-option")
     (:file "rtest-glib-key-file")
     (:file "rtest-glib-variant-type")
     (:file "rtest-glib-variant")
     (:file "rtest-glib-bytes")
     ;; gobject-suite
     (:file "rtest-gobject-generating")
     (:file "rtest-gobject-type-info")
     (:file "rtest-gobject-gvalue")
     (:file "rtest-gobject-enumeration")
     (:file "rtest-gobject-boxed-lisp")
     (:file "rtest-gobject-param-spec")
     (:file "rtest-gobject-param")
;      (:file "rtest-gobject-class")
     (:file "rtest-gobject-base")
     (:file "rtest-gobject-signals")
     (:file "rtest-gobject-binding")
     (:file "rtest-gobject-utils")
     ;; gio-suite
     (:file "rtest-gio-content-type")
     (:file "rtest-gio-app-info")
     (:file "rtest-gio-icon")
     (:file "rtest-gio-loadable-icon")
     (:file "rtest-gio-file-icon")
     (:file "rtest-gio-themed-icon")
     (:file "rtest-gio-emblemed-icon")
     (:file "rtest-gio-emblem")
     (:file "rtest-gio-resource")
     (:file "rtest-gio-permission")
     (:file "rtest-gio-list-model")
     (:file "rtest-gio-list-store")
     (:file "rtest-gio-action")
     (:file "rtest-gio-action-group")
     (:file "rtest-gio-action-map")
     (:file "rtest-gio-simple-action")
     (:file "rtest-gio-property-action")
     (:file "rtest-gio-simple-action-group")
     (:file "rtest-gio-application")
     (:file "rtest-gio-menu-model")
     (:file "rtest-gio-menu")
     (:file "rtest-gio-notification")
     (:file "rtest-gio-file")
     (:file "rtest-gio-cancellable"))))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :glib-test :glib-test)))
  :depends-on (:cl-cffi-glib :fiveam))

;;; --- End of file cl-cffi-glib.asd -------------------------------------------
