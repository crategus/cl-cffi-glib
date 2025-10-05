;;; ----------------------------------------------------------------------------
;;; glib-user.package.lisp
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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

(defpackage :glib-user
  (:nicknames :g)
  (:use :glib :gobject :gio :common-lisp)
  (:shadow #:error)
  (:export ;; Symbols from glib.version.lisp
           #:+major-version+
           #:+minor-version+
           #:+micro-version+
           #:check-version
           #:cl-cffi-glib-build-info

           ;; Symbbols from glib.quark.lisp
           #:quark-as-string

           ;; Symbols from glib.misc.lisp
           #:malloc
           #:free
           #:strv-t
           #:list-t
           #:slist-t
           #:date-time
           #:unichar

           ;; Symbols from glib.error.lisp
           #:error                               ; FIXME: g:error not accessible
           #:with-error
           #:with-ignore-error
           #:with-catching-to-error

           ;; Symbols from glib.main-loop.lisp
           #:+priority-high+
           #:+priority-default+
           #:+priority-high-idle+
           #:+priority-default-idle+
           #:+priority-low+
           #:+source-continue+
           #:+source-remove+
           #:main-loop
           #:main-loop-new
           #:main-loop-ref
           #:main-loop-unref
           #:main-loop-run
           #:main-loop-quit
           #:main-loop-is-running
           #:main-loop-context
           #:main-context
           #:main-context-new
           #:main-context-ref
           #:main-context-unref
           #:main-context-default
           #:main-context-iteration
           #:main-context-pending
           #:main-context-find-source-by-id
           #:main-context-wakeup
           #:main-context-acquire
           #:main-context-release
           #:main-context-is-owner
           #:main-context-dispatch
           #:main-depth
           #:main-current-source
           #:source
           #:timeout-source-new
           #:timeout-source-new-seconds
           #:timeout-add
           #:timeout-add-seconds
           #:idle-source-new
           #:idle-add
           #:source-ref
           #:source-unref
           #:source-attach
           #:source-destroy
           #:source-is-destroyed
           #:source-priority
           #:source-can-recurse
           #:source-id
           #:source-name
           #:source-set-name-by-id
           #:source-context
           #:source-set-callback
           #:source-func
           #:source-add-child-source
           #:source-remove-child-source
           #:source-time
           #:source-ready-time
           #:source-remove

           ;; Symbols from glib.utils.lisp
           #:application-name
           #:prgname
           #:environ
           #:getenv
           #:setenv
           #:listenv
           #:user-name
           #:real-name
           #:user-cache-dir
           #:user-data-dir
           #:user-config-dir
           #:user-runtime-dir
           #:user-directory
           #:user-special-dir
           #:system-data-dirs
           #:system-config-dirs
           #:host-name
           #:home-dir
           #:tmp-dir
           #:current-dir
           #:path-is-absolute
           #:build-filename
           #:build-path

           ;; Symbols from glib.option.lisp
           #:with-option-context
           #:with-option-group
           #:option-arg
           #:option-flags
           #:option-context
           #:option-group
           #:option-context-new
           #:option-context-summary
           #:option-context-description
           #:translate-func
           #:option-context-set-translate-func
           #:option-context-set-translation-domain
           #:option-context-free
           #:option-context-parse
           #:option-context-parse-strv
           #:option-context-help-enabled
           #:option-context-ignore-unknown-options
           #:option-context-help
           #:option-context-strict-posix
           #:option-context-add-main-entries
           #:option-context-add-group
           #:option-context-main-group
           #:option-group-new
           #:option-group-ref
           #:option-group-unref
           #:option-group-add-entries
           #:option-group-set-translate-func
           #:option-group-set-translation-domain

           ;; Symbols from glib.key-file.lisp
           #:with-key-file
           #:with-key-file-from-file
           #:with-key-file-from-data
           #:key-file
           #:key-file-flags
           #:key-file-new
           #:key-file-free
           #:key-file-ref
           #:key-file-unref
           #:key-file-set-list-separator
           #:key-file-load-from-file
           #:key-file-load-from-data
           #:key-file-load-from-bytes
           #:key-file-load-from-data-dirs
           #:key-file-load-from-dirs
           #:key-file-to-data
           #:key-file-save-to-file
           #:key-file-start-group
           #:key-file-groups
           #:key-file-keys
           #:key-file-has-group
           #:key-file-has-key
           #:key-file-value
           #:key-file-string
           #:key-file-locale-string
           #:key-file-locale-for-key
           #:key-file-boolean
           #:key-file-integer
           #:key-file-int64
           #:key-file-uint64
           #:key-file-double
           #:key-file-string-list
           #:key-file-locale-string-list
           #:key-file-boolean-list
           #:key-file-integer-list
           #:key-file-double-list
           #:key-file-comment
           #:key-file-remove-group
           #:key-file-remove-key
           #:key-file-remove-comment

           ;; Symbols glib.variant-type.lisp
           #:variant-type
           #:variant-type-copy
           #:variant-type-new
           #:variant-type-string-is-valid
           #:variant-type-dup-string
           #:variant-type-is-definite
           #:variant-type-is-container
           #:variant-type-is-basic
           #:variant-type-is-maybe
           #:variant-type-is-array
           #:variant-type-is-tuple
           #:variant-type-is-dict-entry
           #:variant-type-is-variant
           #:variant-type-hash
           #:variant-type-equal
           #:variant-type-is-subtype-of
           #:variant-type-new-maybe
           #:variant-type-new-array
           #:variant-type-new-tuple
           #:variant-type-new-dict-entry
           #:variant-type-element
           #:variant-type-n-items
           #:variant-type-first
           #:variant-type-next
           #:variant-type-key
           #:variant-type-value

           ;; Symbols from glib.variant.lisp
           #:variant
           #:with-variant
           #:variant-class
           #:variant-dict
           #:variant-unref
           #:variant-ref
           #:variant-get
           #:variant-set
           #:variant-ref-sink
           #:variant-is-floating
           #:variant-take-ref
           #:variant-type
           #:variant-type-string
           #:variant-is-of-type
           #:variant-is-container
           #:variant-compare
           #:variant-classify
           #:variant-new-boolean
           #:variant-new-byte
           #:variant-new-int16
           #:variant-new-uint16
           #:variant-new-int32
           #:variant-new-uint32
           #:variant-new-int64
           #:variant-new-uint64
           #:variant-new-handle
           #:variant-new-double
           #:variant-new-string
           #:variant-new-object-path
           #:variant-is-object-path
           #:variant-new-signature
           #:variant-is-signature
           #:variant-new-variant
           #:variant-new-tuple
           #:variant-boolean
           #:variant-byte
           #:variant-int16
           #:variant-uint16
           #:variant-int32
           #:variant-uint32
           #:variant-int64
           #:variant-uint64
           #:variant-handle
           #:variant-double
           #:variant-string
           #:variant-variant
           #:variant-new-from-bytes
           #:variant-equal
           #:variant-print
           #:variant-dict-unref
           #:variant-dict-ref
           #:variant-dict-new
           #:variant-dict-init
           #:variant-dict-clear
           #:variant-dict-contains
           #:variant-dict-lookup
           #:variant-dict-lookup-value
           #:variant-dict-insert
           #:variant-dict-insert-value
           #:variant-dict-remove
           #:variant-dict-end
           #:variant-parse
           #:variant-iter
           #:variant-iter-new
           #:variant-iter-copy
           #:variant-iter-free
           #:variant-iter-next-value

           ;; Symbols from glib.bytes.lisp
           #:bytes
           #:bytes-new
           #:bytes-new-take
           #:bytes-new-static
           #:bytes-new-with-free-func
           #:bytes-new-from-bytes
           #:bytes-data
           #:bytes-size
           #:bytes-hash
           #:bytes-equal
           #:bytes-compare
           #:bytes-ref
           #:bytes-unref
           #:bytes-unref-to-data
           #:bytes-unref-to-array

           ;; Symbols from gobject.type-info.lisp
           #:type-t
           #:gtype                               ; TODO: Change the name?
           #:gtype-name
           #:gtype-id
           #:symbol-for-gtype
           #:type-interface
           #:type-class
           #:type-instance
           #:type-fundamental
           #:type-is-abstract
           #:type-is-derived
           #:type-is-fundamental
           #:type-is-value-type
           #:type-is-classed
           #:type-is-interface
           #:type-from-instance
           #:type-from-class
           #:type-from-interface
           #:type-instance-class
           #:type-check-instance-type
           #:type-check-class-type
           #:type-name
           #:type-parent
           #:type-depth
           #:type-next-base
           #:type-is-a
           #:type-class-ref
           #:type-class-peek
           #:type-class-unref
           #:type-interface-peek
           #:type-default-interface-ref
           #:type-default-interface-peek
           #:type-default-interface-unref
           #:type-children
           #:type-interfaces
           #:type-interface-prerequisites
           #:type-qdata
           #:type-ensure

           ;; Symbols from gobject.gvalue.lisp
           #:with-value
           #:with-values
           #:value
           #:value-holds
           #:value-type
           #:value-type-name
           #:type-is-value
           #:value-init
           #:value-copy
           #:value-reset
           #:value-unset
           #:value-get
           #:value-set
           #:value-type-compatible
           #:value-type-transformable
           #:value-transform
           #:value-register-transform-func
           #:strdup-value-contents

           ;; Symbols from gobject.enumeration.lisp
           #:enum-class
           #:enum-value
           #:type-is-enum
           #:flags-class
           #:flags-value
           #:type-is-flags

           ;; Symbols from gobject.boxed.lisp
           #:boxed
           #:type-is-boxed
           #:boxed-copy
           #:boxed-free
           #:boxed-type-register-static
           #:pointer-type-register-static
           #:type-hash-table
           #:type-date
           #:type-gstring
           #:type-strv
           #:type-regex
           #:type-match-info
           #:type-array
           #:type-byte-array
           #:type-ptr-array
           #:type-bytes
           #:type-variant-type
           #:type-error
           #:type-date-time
           #:type-time-zone
           #:type-io-channel
           #:type-io-condition
           #:type-variant-builder
           #:type-key-file
           #:type-main-context
           #:type-main-loop
           #:type-markup-parse-context
           #:type-source
           #:type-polled
           #:type-thread

           ;; Symbols from gobject.param-spec.lisp
           #:param-spec
           #:param-flags
           #:type-is-param
           #:is-param-spec
           #:param-spec-type
           #:param-spec-type-name
           #:param-spec-value-type
           #:param-spec-ref
           #:param-spec-unref
           #:param-spec-sink
           #:param-spec-ref-sink
           #:param-spec-default-value
           #:param-value-set-default
           #:param-value-defaults
           #:param-value-validate
           #:param-spec-name
           #:param-spec-nick
           #:param-spec-blurb
           #:param-spec-internal

           ;; Symbols from gobject.param.lisp
           #:param-spec-boolean
           #:value-boolean
           #:param-spec-char
           #:value-char
           #:value-schar
           #:param-spec-uchar
           #:value-uchar
           #:param-spec-int
           #:value-int
           #:param-spec-uint
           #:value-uint
           #:param-spec-long
           #:value-long
           #:param-spec-ulong
           #:value-ulong
           #:param-spec-int64
           #:value-int64
           #:param-spec-uint64
           #:value-uint64
           #:param-spec-float
           #:value-float
           #:param-spec-double
           #:value-double
           #:param-spec-enum
           #:value-enum
           #:param-spec-flags
           #:value-flags
           #:param-spec-string
           #:value-string
           #:value-set-static-string
           #:value-take-string
           #:value-set-string-take-ownership
           #:value-dup-string
           #:param-spec-param
           #:value-param
           #:value-take-param
           #:value-set-param-take-ownership
           #:value-dup-param
           #:param-spec-boxed
           #:value-boxed
           #:value-set-static-boxed
           #:value-take-boxed
           #:value-set-boxed-take-ownership
           #:value-dup-boxed
           #:param-spec-pointer
           #:value-pointer
           #:param-spec-object
           #:value-object
           #:value-take-object
           #:value-set-object-take-ownership
           #:value-dup-object
           #:param-spec-unichar
           #:param-spec-value-array
           #:param-spec-override
           #:param-spec-gtype
           #:value-gtype
           #:param-spec-variant
           #:value-variant
           #:value-dup-variant
           #:value-take-variant

           ;; Symbols from gobject.base.lisp
           #:object-class
           #:object
           #:object-has-reference
           #:object-pointer
           #:initially-unowned
           #:callback
           #:type-is-object
           #:is-object
           #:object-class-find-property
           #:object-class-list-properties
           #:object-interface-find-property
           #:object-interface-list-properties
           #:object-new
           #:object-ref
           #:object-ref-count
           #:object-unref
           #:object-notify
           #:object-freeze-notify
           #:object-thaw-notify
           #:object-data
           #:destroy-notify
           #:object-set-data-full
           #:object-object-steal-data
           #:object-property
           #:object-class-init
           #:object-instance-init
           #:object-install-vtable

           ;; Symbols from gobject.closures.lisp
           #:closure
           #:type-closure

           #:closure-ref
           #:closure-unref
           #:closure-invalidate

           ;; Symbols from gobject.signals.lisp
           #:signal-flags
           #:connect-flags
           #:signal-query
           #:signal-query-signal-id
           #:signal-query-signal-name
           #:signal-query-owner-type
           #:signal-query-signal-flags
           #:signal-query-return-type
           #:signal-query-param-types
           #:signal-query-signal-detail
           #:signal-lookup
           #:signal-name
           #:signal-list-ids
           #:signal-emit
           #:signal-connect
           #:signal-handler-block
           #:signal-handler-unblock
           #:signal-handler-disconnect
           #:signal-handler-find
           #:signal-handler-is-connected
           #:signal-has-handler-pending
           #:signal-stop-emission

           ;; Symbols from gobject.binding.lisp
           #:binding
           #:binding-flags
           #:binding-source
           #:binding-source-property
           #:binding-target
           #:binding-target-property
           #:binding-dup-source
           #:binding-dup-target
           #:binding-unbind
           #:object-bind-property
           #:binding-transform-func
           #:object-bind-property-full

           ;; Symbols from gio.content-type.lisp
           #:content-type-equals
           #:content-type-is-a
           #:content-type-is-mime-type
           #:content-type-is-unknown
           #:content-type-description
           #:content-type-mime-type
           #:content-type-mime-dirs
           #:content-type-icon
           #:content-type-symbolic-icon
           #:content-type-generic-icon-name
           #:content-type-can-be-executable
           #:content-type-from-mime-type
           #:content-type-guess
           #:content-type-guess-for-tree
           #:content-types-registered

           ;; Symbols from gio.app.info.lisp
           #:app-info-create-flags
           #:app-info
           #:app-info-create-from-commandline
           #:app-info-dup
           #:app-info-equal
           #:app-info-id
           #:app-info-name
           #:app-info-display-name
           #:app-info-description
           #:app-info-executable
           #:app-info-commandline
           #:app-info-icon
           #:app-info-supports-files
           #:app-info-supports-uris
           #:app-info-should-show
           #:app-info-can-delete
           #:app-info-delete
           #:app-info-reset-type-associations
           #:app-info-set-as-default-for-type
           #:app-info-set-as-default-for-extension
           #:app-info-set-as-last-used-for-type
           #:app-info-add-supports-type
           #:app-info-can-remove-supports-type
           #:app-info-remove-supports-type
           #:app-info-supported-types
           #:app-info-all
           #:app-info-all-for-type
           #:app-info-fallback-for-type
           #:app-info-recommended-for-type
           #:app-info-default-for-type
           #:app-info-default-for-type-async
           #:app-info-default-for-type-finish
           #:app-info-default-for-uri-scheme
           #:app-info-default-for-uri-scheme-async
           #:app-info-default-for-uri-scheme-finish
           #:app-info-launch
           #:app-info-launch-uris
           #:app-info-launch-uris-async
           #:app-info-launch-uris-finish
           #:app-info-launch-default-for-uri
           #:app-info-launch-default-for-uri-async
           #:app-info-launch-default-for-uri-finish
           #:app-launch-context
           #:app-launch-context-new
           #:app-launch-context-setenv
           #:app-launch-context-unsetenv
           #:app-launch-context-environment
           #:app-launch-context-display
           #:app-launch-context-startup-notify-id
           #:app-launch-context-launch-failed

           ;; Symbols from gio.icon.lisp
           #:icon
           #:icon-hash
           #:icon-equal
           #:icon-to-string
           #:icon-new-for-string
           #:icon-serialize
           #:icon-deserialize

           ;; Symbols from gio.loadable-icon.lisp
           #:loadable-icon
           #:loadable-icon-load
           #:loadable-icon-load-async
           #:loadable-icon-load-finish

           ;; Symbols from gio.file-icon.lisp
           #:file-icon
           #:file-icon-file
           #:file-icon-new

           ;; Symbols from gio.themed-icon.lisp
           #:themed-icon
           #:themed-icon-name
           #:themed-icon-names
           #:themed-icon-use-default-fallbacks
           #:themed-icon-new
           #:themed-icon-new-from-names
           #:themed-icon-new-with-default-fallbacks
           #:themed-icon-prepend-name
           #:themed-icon-append-name

           ;; Symbols from gio.emblemed-icon.lisp
           #:emblemed-icon
           #:emblemed-icon-gicon
           #:emblemed-icon-new
           #:emblemed-icon-icon
           #:emblemed-icon-emblems
           #:emblemed-icon-add-emblem
           #:emblemed-icon-clear-emblems

           ;; Symbols from gio.emblem.lisp
           #:emblem
           #:emblem-origin
           #:emblem-new
           #:emblem-new-with-origin
           #:emblem-icon

           ;; Symbols from gio.settings.lisp
           #:settings
           #:settings-new
           #:settings-new-with-path

           ;; Symbols from gio.resource.lisp
           #:resource
           #:resource-lookup-flags
           #:with-resource
           #:with-resources
           #:resource-new-from-data
           #:resource-load
           #:resource-info
           #:resource-lookup-data
           #:resource-has-children
           #:resource-enumerate-children
           #:resources-register
           #:resources-unregister
           #:resources-info
           #:resources-lookup-data
           #:resources-has-children
           #:resources-enumerate-children

           ;; Symbols from gio.permission.lisp
           #:permission
           #:permission-allowed
           #:permission-can-acquire
           #:permission-can-release
           #:permission-acquire
           #:permission-acquire-async
           #:permission-acquire-finish
           #:permission-release
           #:permission-release-async
           #:permission-release-finish
           #:permission-impl-update

           ;; Symbols from gio.simple-permission.lisp
           #:simple-permission
           #:simple-permission-new

           ;; Symbols from gio.list-model.lisp
           #:list-model
           #:list-model-vtable
           #:list-model-get-item-type-impl
           #:list-model-get-n-items-impl
           #:list-model-get-item-impl
           #:list-model-item-type
           #:list-model-n-items
           #:list-model-item
           #:list-model-object
           #:list-model-items-changed

           ;; Symbols from gio.list-store.lisp
           #:list-store
           #:list-store-item-type
           #:list-store-n-items
           #:compare-data-func
           #:equal-func-full
           #:list-store-new
           #:list-store-insert
           #:list-store-insert-sorted
           #:list-store-append
           #:list-store-remove
           #:list-store-remove-all
           #:list-store-splice
           #:list-store-sort
           #:list-store-find
           #:list-store-find-with-equal-func

           ;; Symbols from gio.action.lisp
           #:action
           #:action-enabled
           #:action-name
           #:action-parameter-type
           #:action-state
           #:action-state-type
           #:action-name-is-valid
           #:action-state-hint
           #:action-change-state
           #:action-activate
           #:action-parse-detailed-name
           #:action-print-detailed-name

           ;; Symbols from gio.action-group.lisp
           #:action-group
           #:action-group-list-actions
           #:action-group-query-action
           #:action-group-has-action
           #:action-group-action-enabled
           #:action-group-action-parameter-type
           #:action-group-action-state-type
           #:action-group-action-state-hint
           #:action-group-action-state
           #:action-group-change-action-state
           #:action-group-activate-action
           #:action-group-action-added
           #:action-group-action-removed
           #:action-group-action-enabled-changed
           #:action-group-action-state-changed

           ;; Symbols from gio.action-map.lisp
           #:action-map
           #:action-map-lookup-action
           #:action-map-add-action-entries
           #:action-map-add-action
           #:action-map-remove-action

           ;; Symbols from gio.simple-action.lisp
           #:simple-action
           #:simple-action-enabled
           #:simple-action-name
           #:simple-action-parameter-type
           #:simple-action-state
           #:simple-action-state-type
           #:simple-action-new
           #:simple-action-new-stateful
           #:simple-action-set-state-hint

           ;; Symbols from gio.property-action.lisp
           #:property-action
           #:property-action-enabled
           #:property-action-invert-boolean
           #:property-action-name
           #:property-action-object
           #:property-action-parameter-type
           #:property-action-property-name
           #:property-action-state
           #:property-action-state-type
           #:property-action-new

           ;; Symbols from gio.simple-action-group.lisp
           #:simple-action-group
           #:simple-action-group-new
           #:simple-action-group-lookup
           #:simple-action-group-insert
           #:simple-action-group-remove
           #:simple-action-group-add-entries

           ;; Symbols from gio.application.lisp
           #:application
           #:application-flags
           #:application-action-group
           #:application-application-id
           #:application-flags
           #:application-inactivity-timeout
           #:application-is-busy
           #:application-is-registered
           #:application-is-remote
           #:application-resource-base-path
           #:application-version
           #:application-id-is-valid
           #:application-new
           #:application-dbus-connection
           #:application-dbus-object-path
           #:application-register
           #:application-hold
           #:application-release
           #:application-quit
           #:application-activate
           #:application-open
           #:application-send-notification
           #:application-withdraw-notification
           #:application-run
           #:application-add-main-option-entries
           #:application-add-main-option
           #:application-add-option-group
           #:application-set-option-context-parameter-string
           #:application-set-option-context-summary
           #:application-set-option-context-description
           #:application-default
           #:application-mark-busy
           #:application-unmark-busy
           #:application-bind-busy-property
           #:application-unbind-busy-property

           ;; Symbols from gio.application-command-line.lisp
           #:application-command-line
           #:application-command-line-is-remote
           #:application-command-line-arguments
           #:application-command-line-cwd
           #:application-command-line-environ
           #:application-command-line-options-dict
           #:application-command-line-stdin
           #:application-command-line-create-file-for-arg
           #:application-command-line-getenv
           #:application-command-line-platform-data
           #:application-command-line-exit-status

           ;; Symbols from gio.menu-model.lisp
           #:menu-model
           #:menu-model-is-mutable
           #:menu-model-n-items
           #:menu-model-get-item-attribute-value
           #:menu-model-get-item-attribute
           #:menu-model-get-item-link
           #:menu-model-iterate-item-attributes
           #:menu-model-iterate-item-links
           #:menu-model-items-changed
           #:menu-attribute-iter
           #:menu-attribute-iter-get-next
           #:menu-attribute-iter-get-name
           #:menu-attribute-iter-get-value
           #:menu-attribute-iter-next
           #:menu-link-iter
           #:menu-link-iter-get-name
           #:menu-link-iter-get-next
           #:menu-link-iter-get-value
           #:menu-link-iter-next

           ;; Symbols from gio.menu.lisp
           #:menu
           #:menu-new
           #:menu-freeze
           #:menu-insert
           #:menu-prepend
           #:menu-append
           #:menu-insert-item
           #:menu-prepend-item
           #:menu-append-item
           #:menu-insert-section
           #:menu-prepend-section
           #:menu-append-section
           #:menu-insert-submenu
           #:menu-prepend-submenu
           #:menu-append-submenu
           #:menu-remove
           #:menu-remove-all
           #:menu-item
           #:menu-item-new
           #:menu-item-new-section
           #:menu-item-new-submenu
           #:menu-item-new-from-model
           #:menu-item-set-label
           #:menu-item-set-icon
           #:menu-item-set-action-and-target-value
           #:menu-item-set-action-and-target
           #:menu-item-set-detailed-action
           #:menu-item-set-section
           #:menu-item-set-submenu
           #:menu-item-attribute-value
           #:menu-item-attribute
           #:menu-item-link

           ;; Symbols from gio.notification.lisp
           #:notification-priority
           #:notification
           #:notification-new
           #:notification-set-title
           #:notification-set-body
           #:notification-set-icon
           #:notification-set-priority
           #:notification-set-urgent
           #:notification-set-default-action
           #:notification-set-default-action-and-target
           #:notification-set-default-action-and-target-value
           #:notification-add-button
           #:notification-add-button-with-target
           #:notification-add-button-with-target-value

           ;; Symbols from gio.file.lisp
           #:file
           #:file-query-info-flags
           #:file-as-namestring
           #:file-new-for-path
           #:file-new-for-uri
           #:file-new-for-commandline-arg
           #:file-new-for-commandline-arg-and-cwd
           #:file-parse-name
           #:file-basename
           #:file-path
           #:file-uri
           #:file-get-parse-name
           #:file-query-info

           ;; Symbols from gio.file-info.lisp
           #:file-info
           #:file-info-new
           #:file-info-clear-status
           #:file-info-copy-into
           #:file-info-dup
           #:file-info-access-date-time
           #:file-info-attribute-as-string
           #:file-info-attribute-boolean
           #:file-info-file-info-attribute-byte-string
           #:file-info-attribute-data
           #:file-info-attribute-file-path
           #:file-info-attribute-int32
           #:file-info-attribute-int64
           #:file-infoattribute-object
           #:file-info-attribute-status
           #:file-info-attribute-string
           #:file-info-attribute-stringv
           #:file-info-attribute-type
           #:file-info-attribute-uint32
           #:file-info-attribute-uint64
           #:file-info-set-attribute
           #:file-infoset-attribute-mask
           #:file-info-content-type
           #:file-info-creation-date-time
           #:file-info-deletion-date
           #:file-info-display-name
           #:file-info-edit-name
           #:file-info-etag
           #:file-info-file-type
           #:file-info-icon
           #:file-info-is-backup
           #:file-info-is-hidden
           #:file-info-is-symlink
           #:file-info-modification-date-time
           #:file-info-modification-time
           #:file-info-name
           #:file-info-size
           #:file-info-sort-order
           #:file-info-symbolic-icon
           #:file-info-symlink-target
           #:file-info-has-attribute
           #:file-info-has-namespace
           #:file-info-list-attributes
           #:file-info-remove-attribute
           #:file-info-unset-attribute-mask

           ;; Symbols from gio.cancellable.lisp
           #:cancellable
           #:cancellable-source-func
           #:cancellable-new
           #:cancellable-is-cancelled
           #:cancellable-set-error-if-cancelled
           #:cancellable-fd
           #:cancellable-make-pollfd
           #:cancellable-release-fd
           #:cancellable-source-new
           #:cancellable-current
           #:cancellable-pop-current
           #:cancellable-push-current
           #:cancellable-reset
           #:cancellable-connect
           #:cancellable-disconnect
           #:cancellable-cancel

           ;; Symbols from gio.async-result.lisp
           #:async-result
           #:async-ready-callback
           #:async-result-user-data
           #:async-result-source-object
           #:async-result-is-tagged
           #:async-result-legacy-propagate-error

           ;; Symbols from gio.task.lisp
           #:task
           #:task-completed
           #:task-new
           #:task-task-data
           #:task-priority
           #:task-check-cancellable
           #:task-return-on-cancel
           #:task-source-tag
           #:task-name
           #:task-report-error
           #:task-report-new-error
           #:task-cancellable
           #:task-context
           #:task-source-object
           #:task-return-boolean
           #:task-return-int
           #:task-return-pointer
           #:task-return-value
           #:task-return-error
           #:task-return-new-error
           #:task-return-error-if-cancelled
           #:task-propagate-boolean
           #:task-propagate-int
           #:task-propagate-pointer
           #:task-propagate-value
           #:task-had-error
           #:task-run-in-thread
           #:task-run-in-thread-sync
           #:task-threadfunc
           #:task-attach-source
           #:task-is-valid
           ))

;;; --- End of file glib-user.package.lisp -------------------------------------
