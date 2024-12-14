;;; ----------------------------------------------------------------------------
;;; gio.package.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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

(defpackage :gio
  (:use :iterate :common-lisp)
  (:import-from #:cffi)
  (:import-from #:glib)
  (:import-from #:gobject))

(in-package :gio)

;;; ----------------------------------------------------------------------------

#+sbcl
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :gio) t)
 "This is the API documentation of a Lisp binding to GIO.
  GIO is striving to provide a modern, easy-to-use VFS API that sits at the
  right level in the library stack, as well as other generally useful APIs for
  desktop applications (such as networking and D-Bus support). The goal is to
  overcome the shortcomings of GnomeVFS and provide an API that is so good that
  developers prefer it over raw POSIX calls. Among other things that means
  using GObject. It also means not cloning the POSIX API, but providing
  higher-level, document-centric interfaces.
  @begin[File Operations]{section}
    @begin[GFile]{subsection}
      @about-class{file}
      @about-type{file-as-namestring}
      @about-function{file-new-for-path}
      @about-function{file-new-for-uri}
      @about-function{file-new-for-commandline-arg}
      @about-function{file-new-for-commandline-arg-and-cwd}
      @about-function{file-parse-name}
      @about-function{file-basename}
      @about-function{file-path}
      @about-function{file-uri}
      @about-function{file-get-parse-name}
    @end{subsection}
    @begin[GFileInfo]{subsection}
      @about-class{file-info}
      @about-function{file-info-new}
      @about-function{file-info-clear-status}
      @about-function{file-info-copy-into}
      @about-function{file-info-dup}
      @about-function{file-info-access-date-time}
      @about-function{file-info-attribute-as-string}
      @about-function{file-info-attribute-boolean}
      @about-function{file-info-attribute-byte-string}
      @about-function{file-info-attribute-data}
      @about-function{file-info-attribute-file-path}
      @about-function{file-info-attribute-int32}
      @about-function{file-info-attribute-int64}
      @about-function{file-info-attribute-object}
      @about-function{file-info-attribute-status}
      @about-function{file-info-attribute-string}
      @about-function{file-info-attribute-stringv}
      @about-function{file-info-attribute-type}
      @about-function{file-info-attribute-uint32}
      @about-function{file-info-attribute-uint64}
      @about-function{file-info-set-attribute}
      @about-function{file-info-set-attribute-mask}
      @about-function{file-info-content-type}
      @about-function{file-info-creation-date-time}
      @about-function{file-info-deletion-date}
      @about-function{file-info-display-name}
      @about-function{file-info-edit-name}
      @about-function{file-info-etag}
      @about-function{file-info-file-type}
      @about-function{file-info-icon}
      @about-function{file-info-is-backup}
      @about-function{file-info-is-hidden}
      @about-function{file-info-is-symlink}
      @about-function{file-info-modification-date-time}
      @about-function{file-info-modification-time}
      @about-function{file-info-name}
      @about-function{file-info-size}
      @about-function{file-info-sort-order}
      @about-function{file-info-symbolic-icon}
      @about-function{file-info-symlink-target}
      @about-function{file-info-has-attribute}
      @about-function{file-info-has-namespace}
      @about-function{file-info-list-attributes}
      @about-function{file-info-remove-attribute}
      @about-function{file-info-unset-attribute-mask}
    @end{subsection}
  @end{section}
  @begin[Asynchronous I/O]{section}
    @begin[GCancellable]{subsection}
      @about-class{cancellable}
      @about-symbol{cancellable-source-func}
      @about-function{cancellable-new}
      @about-function{cancellable-is-cancelled}
      @about-function{cancellable-set-error-if-cancelled}
      @about-function{cancellable-fd}
      @about-function{cancellable-make-pollfd}
      @about-function{cancellable-release-fd}
      @about-function{cancellable-source-new}
      @about-function{cancellable-current}
      @about-function{cancellable-pop-current}
      @about-function{cancellable-push-current}
      @about-function{cancellable-reset}
      @about-function{cancellable-connect}
      @about-function{cancellable-disconnect}
      @about-function{cancellable-cancel\}
    @end{subsection}
    @begin[GAsyncResult]{subsection}
      @about-class{async-result}
      @about-symbol{async-ready-callback}
      @about-function{async-result-user-data}
      @about-function{async-result-source-object}
      @about-function{async-result-is-tagged }
      @about-function{async-result-legacy-propagate-error}
    @end{subsection}
    @begin[GTask]{subsection}
      @about-class{task}
      @about-generic{task-completed}
      @about-function{task-new}
      @about-function{task-task-data}
      @about-function{task-priority}
      @about-function{task-check-cancellable}
      @about-function{task-return-on-cancel}
      @about-function{task-source-tag}
      @about-function{task-name}
      @about-function{task-set-static-name}
      @about-function{task-report-error}
      @about-function{task-report-new-error}
      @about-function{task-cancellable}
      @about-function{task-context}
      @about-function{task-source-object}
      @about-function{task-return-boolean}
      @about-function{task-return-int}
      @about-function{task-return-pointer}
      @about-function{task-return-value}
      @about-function{task-return-error}
      @about-function{task-return-new-error}
      @about-function{task-return-error-if-cancelled}
      @about-function{task-propagate-boolean}
      @about-function{task-propagate-int}
      @about-function{task-propagate-pointer}
      @about-function{task-propagate-value}
      @about-function{task-had-error}
      @about-function{task-run-in-thread}
      @about-function{task-run-in-thread-sync}
      @about-symbol{task-threadfunc}
      @about-function{task-attach-source}
      @about-function{task-task-is-valid}
    @end{subsection}
  @end{section}
  @begin[File types and applications]{section}
    @begin[Introduction to GContentType]{subsection}
      A content type is a platform specific string that defines the type of a
      file. On UNIX it is a MIME type like @code{\"text/plain\"} or
      @code{\"image/png\"}. On Win32 it is an extension string like
      @code{\".doc\"}, @code{\".txt\"} or a perceived string like
      @code{\"audio\"}. Such strings can be looked up in the registry at
      @code{HKEY_CLASSES_ROOT}. On macOS it is a Uniform Type Identifier such
      as @code{com.apple.application}.
    @end{subsection}
    @begin[Functions for GContentType]{subsection}
      @about-function{content-type-equals}
      @about-function{content-type-is-a}
      @about-function{content-type-is-mime-type}
      @about-function{content-type-is-unknown}
      @about-function{content-type-description}
      @about-function{content-type-mime-type}
      @about-function{content-type-mime-dirs}
      @about-function{content-type-icon}
      @about-function{content-type-symbolic-icon}
      @about-function{content-type-generic-icon-name}
      @about-function{content-type-can-be-executable}
      @about-function{content-type-from-mime-type}
      @about-function{content-type-guess}
      @about-function{content-type-guess-for-tree}
      @about-function{content-types-registered}
    @end{subsection}
    @begin[GAppInfo]{subsection}
      @about-symbol{app-info-create-flags}
      @about-class{app-info}
      @about-symbol{app-info-iface}
      @about-function{app-info-create-from-commandline}
      @about-function{app-info-dup}
      @about-function{app-info-equal}
      @about-function{app-info-id}
      @about-function{app-info-name}
      @about-function{app-info-display-name}
      @about-function{app-info-description}
      @about-function{app-info-executable}
      @about-function{app-info-commandline}
      @about-function{app-info-icon}
      @about-function{app-info-launch}
      @about-function{app-info-supports-files}
      @about-function{app-info-supports-uris}
      @about-function{app-info-launch-uris}
      @about-function{app-info-launch-uris-async}
      @about-function{app-info-launch-uris-finish}
      @about-function{app-info-should-show}
      @about-function{app-info-can-delete}
      @about-function{app-info-delete}
      @about-function{app-info-reset-type-associations}
      @about-function{app-info-set-as-default-for-type}
      @about-function{app-info-set-as-default-for-extension}
      @about-function{app-info-set-as-last-used-for-type}
      @about-function{app-info-add-supports-type}
      @about-function{app-info-can-remove-supports-type}
      @about-function{app-info-remove-supports-type}
      @about-function{app-info-supported-types}
      @about-function{app-info-all}
      @about-function{app-info-all-for-type}
      @about-function{app-info-default-for-type}
      @about-function{app-info-default-for-type-async}
      @about-function{app-info-default-for-type-finish}
      @about-function{app-info-default-for-uri-scheme}
      @about-function{app-info-default-for-uri-scheme-async}
      @about-function{app-info-default-for-uri-scheme-finish}
      @about-function{app-info-fallback-for-type}
      @about-function{app-info-recommended-for-type}
      @about-function{app-info-launch-default-for-uri}
      @about-function{app-info-launch-default-for-uri-async}
      @about-function{app-info-launch-default-for-uri-finish}
      @about-class{app-launch-context}
      @about-function{app-launch-context-new}
      @about-function{app-launch-context-setenv}
      @about-function{app-launch-context-unsetenv}
      @about-function{app-launch-context-environment}
      @about-function{app-launch-context-display}
      @about-function{app-launch-context-startup-notify-id}
      @about-function{app-launch-context-launch-failed}
    @end{subsection}
  @end{section}
  @begin[Icons]{section}
    @begin[GIcon]{subsection}
      @about-class{icon}
      @about-function{icon-hash}
      @about-function{icon-equal}
      @about-function{icon-to-string}
      @about-function{icon-new-for-string}
      @about-function{icon-serialize}
      @about-function{icon-deserialize}
    @end{subsection}
    @begin[GFileIcon]{subsection}
      @about-class{file-icon}
      @about-generic{file-icon-file}
      @about-function{file-icon-new}
    @end{subsection}
    @begin[GLoadableIcon]{subsection}
      @about-class{loadable-icon}
      @about-function{loadable-icon-load}
      @about-function{loadable-icon-load-async}
      @about-function{loadable-icon-load-finish}
    @end{subsection}
    @begin[GThemedIcon]{subsection}
      @about-class{themed-icon}
      @about-generic{themed-icon-name}
      @about-generic{themed-icon-names}
      @about-generic{themed-icon-use-default-fallbacks}
      @about-function{themed-icon-new}
      @about-function{themed-icon-new-from-names}
      @about-function{themed-icon-new-with-default-fallbacks}
      @about-function{themed-icon-prepend-name}
      @about-function{themed-icon-append-name}
    @end{subsection}
    @begin[GEmblem]{subsection}
      @about-symbol{emblem-origin}
      @about-class{emblem}
      @about-generic{emblem-icon}
      @about-generic{emblem-origin}
      @about-function{emblem-new}
      @about-function{emblem-new-with-origin}
    @end{subsection}
    @begin[GEmblemedIcon]{subsection}
      @about-class{emblemed-icon}
      @about-generic{emblemed-icon-gicon}
      @about-function{emblemed-icon-new}
      @about-function{emblemed-icon-icon}
      @about-function{emblemed-icon-emblems}
      @about-function{emblemed-icon-add-emblem}
      @about-function{emblemed-icon-clear-emblems}
    @end{subsection}
  @end{section}
  @begin[Resources]{section}
    @about-symbol{resource-flags}
    @about-symbol{resource-lookup-flags}
    @about-class{resource}
    @about-macro{with-resource}
    @about-macro{with-resources}
    @about-function{resource-load}
    @about-function{resource-new-from-data}
    @about-function{resource-ref}
    @about-function{resource-unref}
    @about-function{resource-lookup-data}
    @about-function{resource-open-stream}
    @about-function{resource-enumerate-children}
    @about-function{resource-info}
    @about-function{resources-register}
    @about-function{resources-unregister}
    @about-function{resources-lookup-data}
    @about-function{resources-open-stream}
    @about-function{resources-enumerate-children}
    @about-function{resources-info}
  @end{section}
  @begin[Permissions]{section}
    @begin[GPermission]{subsection}
      @about-class{permission}
      @about-generic{permission-allowed}
      @about-generic{permission-can-acquire}
      @about-generic{permission-can-release}
      @about-function{permission-acquire}
      @about-function{permission-acquire-async}
      @about-function{permission-acquire-finish}
      @about-function{permission-release}
      @about-function{permission-release-async}
      @about-function{permission-release-finish}
      @about-function{permission-impl-update}
    @end{subsection}
    @begin[GSimplePermission]{subsection}
      @about-class{simple-permission}
      @about-function{simple-permission-new}
    @end{subsection}
  @end{section}
  @begin[Data models]{section}
    @begin[GListModel]{subsection}
      @about-class{list-model}
      @about-function{list-model-item-type}
      @about-function{list-model-n-items}
      @about-function{list-model-item}
      @about-function{list-model-object}
      @about-function{list-model-items-changed}
    @end{subsection}
    @begin[GListStore]{subsection}
      @about-class{list-store}
      @about-function{list-store-item-type}
      @about-generic{list-store-n-items}
      @about-function{list-store-new}
      @about-function{list-store-insert}
      @about-symbol{compare-data-func}
      @about-function{list-store-insert-sorted}
      @about-function{list-store-append}
      @about-function{list-store-remove}
      @about-function{list-store-remove-all}
      @about-function{list-store-splice}
      @about-function{list-store-sort}
      @about-function{list-store-find}
      @about-symbol{equal-func-full}
      @about-function{list-store-find-with-equal-func}
    @end{subsection}
  @end{section}
  @begin[Application support]{section}
    @begin[GApplication]{subsection}
      @about-symbol{application-flags}
      @about-class{application}
      @about-generic{application-action-group}
      @about-generic{application-application-id}
      @about-generic{application-flags}
      @about-generic{application-inactivity-timeout}
      @about-generic{application-is-busy}
      @about-generic{application-is-registered}
      @about-generic{application-is-remote}
      @about-generic{application-resource-base-path}
      @about-generic{application-version}
      @about-function{application-id-is-valid}
      @about-function{application-new}
      @about-function{application-get-dbus-connection}
      @about-function{application-get-dbus-object-path}
      @about-function{application-set-action-group}
      @about-function{application-register}
      @about-function{application-hold}
      @about-function{application-release}
      @about-function{application-quit}
      @about-function{application-activate}
      @about-function{application-open}
      @about-function{application-run}
      @about-function{application-send-notification}
      @about-function{application-withdraw-notification}
      @about-function{application-add-main-option-entries}
      @about-function{application-add-main-option}
      @about-function{application-add-option-group}
      @about-function{application-set-option-context-parameter-string}
      @about-function{application-set-option-context-summary}
      @about-function{application-set-option-context-description}
      @about-function{application-default}
      @about-function{application-mark-busy}
      @about-function{application-unmark-busy}
      @about-function{application-bind-busy-property}
      @about-function{application-unbind-busy-property}
    @end{subsection}
    @begin[GApplicationCommandLine]{subsection}
      @about-class{application-command-line}
      @about-generic{application-command-line-arguments}
      @about-generic{application-command-line-is-remote}
      @about-generic{application-command-line-options}
      @about-generic{application-command-line-platform-data}
      @about-function{application-command-line-get-arguments}
      @about-function{application-command-line-cwd}
      @about-function{application-command-line-environ}
      @about-function{application-command-line-options-dict}
      @about-function{application-command-line-stdin}
      @about-function{application-command-line-create-file-for-arg}
      @about-function{application-command-line-getenv}
      @about-function{application-command-line-get-platform-data}
      @about-function{application-command-line-exit-status}
      @about-function{application-command-line-print}
      @about-function{application-command-line-printerr}
    @end{subsection}
    @begin[GActionGroup]{subsection}
      @about-class{action-group}
      @about-function{action-group-list-actions}
      @about-function{action-group-query-action}
      @about-function{action-group-has-action}
      @about-function{action-group-action-enabled}
      @about-function{action-group-action-parameter-type}
      @about-function{action-group-action-state-type}
      @about-function{action-group-action-state-hint}
      @about-function{action-group-action-state}
      @about-function{action-group-change-action-state}
      @about-function{action-group-activate-action}
      @about-function{action-group-action-added}
      @about-function{action-group-action-removed}
      @about-function{action-group-action-enabled-changed}
      @about-function{action-group-action-state-changed}
    @end{subsection}
    @begin[GActionMap]{subsection}
      @about-class{action-map}
      @about-function{action-map-lookup-action}
      @about-function{action-map-add-action-entries}
      @about-function{action-map-add-action}
      @about-function{action-map-remove-action}
    @end{subsection}
    @begin[GSimpleActionGroup]{subsection}
      @about-class{simple-action-group}
      @about-function{simple-action-group-new}
      @about-function{simple-action-group-lookup}
      @about-function{simple-action-group-insert}
      @about-function{simple-action-group-remove}
      @about-function{simple-action-group-add-entries}
    @end{subsection}
    @begin[GAction]{subsection}
      @about-class{action}
      @about-generic{action-enabled}
      @about-generic{action-name}
      @about-generic{action-parameter-type}
      @about-generic{action-state}
      @about-generic{action-state-type}
      @about-function{action-name-is-valid}
      @about-function{action-state-hint}
      @about-function{action-change-state}
      @about-function{action-activate}
      @about-function{action-parse-detailed-name}
      @about-function{action-print-detailed-name}
    @end{subsection}
    @begin[GSimpleAction]{subsection}
      @about-class{simple-action}
      @about-generic{simple-action-enabled}
      @about-generic{simple-action-name}
      @about-generic{simple-action-parameter-type}
      @about-generic{simple-action-state}
      @about-generic{simple-action-state-type}
      @about-function{simple-action-new}
      @about-function{simple-action-new-stateful}
      @about-function{simple-action-set-state-hint}
    @end{subsection}
    @begin[GPropertyAction]{subsection}
      @about-class{property-action}
      @about-generic{property-action-enabled}
      @about-generic{property-action-invert-boolean}
      @about-generic{property-action-name}
      @about-generic{property-action-object}
      @about-generic{property-action-parameter-type}
      @about-generic{property-action-property-name}
      @about-generic{property-action-state}
      @about-generic{property-action-state-type}
      @about-function{property-action-new}
    @end{subsection}
    @begin[GMenuModel]{subsection}
      @about-class{menu-model}
      @about-function{menu-model-is-mutable}
      @about-function{menu-model-n-items}
      @about-function{menu-model-get-item-attribute-value}
      @about-function{menu-model-get-item-attribute}
      @about-function{menu-model-get-item-link}
      @about-function{menu-model-iterate-item-attributes}
      @about-function{menu-model-iterate-item-links}
      @about-function{menu-model-items-changed}
      @about-symbol{menu-attribute-iter}
      @about-function{menu-attribute-iter-get-next}
      @about-function{menu-attribute-iter-get-name}
      @about-function{menu-attribute-iter-get-value}
      @about-function{menu-attribute-iter-next}
      @about-function{menu-link-iter}
      @about-function{menu-link-iter-get-name}
      @about-function{menu-link-iter-get-next}
      @about-function{menu-link-iter-get-value}
      @about-function{menu-link-iter-next}
    @end{subsection}
    @begin[GMenu]{subsection}
      @about-class{menu}
      @about-function{menu-new}
      @about-function{menu-freeze}
      @about-function{menu-insert}
      @about-function{menu-prepend}
      @about-function{menu-append}
      @about-function{menu-insert-item}
      @about-function{menu-prepend-item}
      @about-function{menu-append-item}
      @about-function{menu-insert-section}
      @about-function{menu-prepend-section}
      @about-function{menu-append-section}
      @about-function{menu-insert-submenu}
      @about-function{menu-prepend-submenu}
      @about-function{menu-append-submenu}
      @about-function{menu-remove}
      @about-function{menu-remove-all}
      @about-class{menu-item}
      @about-function{menu-item-new}
      @about-function{menu-item-new-section}
      @about-function{menu-item-new-submenu}
      @about-function{menu-item-new-from-model}
      @about-function{menu-item-set-label}
      @about-function{menu-item-set-icon}
      @about-function{menu-item-set-action-and-target-value}
      @about-function{menu-item-set-action-and-target}
      @about-function{menu-item-set-detailed-action}
      @about-function{menu-item-set-section}
      @about-function{menu-item-set-submenu}
      @about-function{menu-item-attribute-value}
      @about-function{menu-item-attribute}
      @about-function{menu-item-link}
    @end{subsection}
    @begin[GNotification]{subsection}
      @about-symbol{notification-priority}
      @about-class{notification}
      @about-function{notification-new}
      @about-function{notification-set-title}
      @about-function{notification-set-body}
      @about-function{notification-set-icon}
      @about-function{notification-set-priority}
      @about-function{notification-set-urgent}
      @about-function{notification-set-default-action}
      @about-function{notification-set-default-action-and-target}
      @about-function{notification-set-default-action-and-target-value}
      @about-function{notification-add-button}
      @about-function{notification-add-button-with-target}
      @about-function{notification-add-button-with-target-value}
    @end{subsection}
  @end{section}")

;;; --- End of file gio.package.lisp -------------------------------------------
