;;; ----------------------------------------------------------------------------
;;; cl-cffi-glib.asd
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(defsystem :cl-cffi-glib
  :name "cl-cffi-glib"
  :version "2.56"                      ; Minimum required C library version
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components
  ((:module glib
    :serial t
    :components
    ((:file "glib.package")
     (:file "glib.stable-pointer")     ; Stable Pointers for callbacks
     (:file "glib.version")            ; Glib Version information
     (:file "glib.quark")              ; Association between string and id
     (:file "glib.misc")               ; Various Glib Types and Functions
     (:file "glib.error")              ; Error Reporting
     (:file "glib.convert")            ; Convert strings
     (:file "glib.main-loop")          ; The Main Event Loop
     (:file "glib.utils")              ; Miscellaneous Utility Functions
     (:file "glib.option")             ; Parses command line options
     (:file "glib.key-file")           ; parses .ini-like config files
     ))
   (:module gobject
    :serial t
    :components
    ((:file "gobject.package")
     (:file "gobject.init")
     (:file "gobject.object-function")
     (:file "gobject.glib-defcallback")
     (:file "gobject.generating")

     (:file "gobject.type-info")       ; Type Information
     (:file "gobject.gvalue")          ; Generic Values
     (:file "gobject.enumeration")     ; Enumeration and flag types
     (:file "gobject.boxed-lisp")
     (:file "gobject.boxed")           ; Boxed Types
     ;; TODO: Glib types which need GBoxed, can we improve this
     (:file "../glib/glib.variant-type") ; GVariant type system
     (:file "../glib/glib.variant")    ; Strongly typed value datatype
     (:file "../glib/glib.bytes")      ; Array of bytes
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
    ))
    (:file "glib-user.package")
    )
  :in-order-to ((asdf:test-op (test-op "cl-cffi-glib/test")))
  :depends-on (:cl-cffi-glib/init
               :iterate
               :bordeaux-threads
               :closer-mop
               :trivial-garbage))

(defsystem :cl-cffi-glib/init
  :name "cl-cffi-glib/init"
  :version "0.9.0"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "cl-cffi-glib-init"))         ; Libraray Initialization
  :depends-on (:cffi
               :trivial-features))

;; Definine a test operation for the library

(defsystem :cl-cffi-glib/test
  :name "cl-cffi-glib/test"
  :components
  ((:module test
    :serial t
    :components
    (;; glib-suite
     (:file "rtest-glib")
     (:file "rtest-utilities")                   ; Utilities for the testsutie
     (:file "rtest-glib-stable-pointer")
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
     (:file "rtest-gobject")
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
     (:file "rtest-gio")
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
     (:file "rtest-gio-file"))))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :glib-test :glib-test)))
  :depends-on (:cl-cffi-glib :fiveam))

;;; --- End of file cl-cffi-glib.asd -------------------------------------------
