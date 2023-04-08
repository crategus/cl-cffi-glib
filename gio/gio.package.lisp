;;; ----------------------------------------------------------------------------
;;; gio.package.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.74 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
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
  (:use :common-lisp)
  (:import-from #:cffi    #:with-foreign-object
                          #:with-foreign-objects
                          #:with-foreign-slots
                          #:define-foreign-type
                          #:define-parse-method
                          #:defcfun)
  (:import-from #:glib    #:with-g-error
                          #:with-ignore-g-error)
  (:import-from #:gobject #:define-g-interface
                          #:define-g-object-class
                          #:define-g-enum
                          #:define-g-flags))

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
      File and Directory Handling.
      @about-symbol{file-query-info-flags}
      @about-symbol{file-create-flags}
      @about-symbol{file-copy-flags}
      @about-symbol{file-monitor-flags}
      @about-symbol{file-measure-flags}
      @about-symbol{filesystem-preview-type}
      @about-class{file}
      @about-function{GFileProgressCallback}
      @about-function{GFileReadMoreCallback}
      @about-function{GFileMeasureProgressCallback}
      @about-function{file-new-for-path}
      @about-function{file-new-for-uri}
      @about-function{file-new-for-commandline-arg}
      @about-function{file-new-for-commandline-arg-and-cwd}
      @about-function{file-new-tmp}
      @about-function{file-parse-name}
      @about-function{file-new-build-filename}
      @about-function{file-dup}
      @about-function{file-hash}
      @about-function{file-equal}
      @about-function{file-basename}
      @about-function{file-path}
      @about-function{file-peek-path}
      @about-function{file-uri}
      @about-function{file-get-parse-name}
      @about-function{file-get-parent}
      @about-function{file-has-parent}
      @about-function{file-get-child}
      @about-function{file-get-child-for-display-name}
      @about-function{file-has-prefix}
      @about-function{file-get-relative-path}
      @about-function{file-resolve-relative-path}
      @about-function{file-is-native}
      @about-function{file-has-uri-scheme}
      @about-function{file-get-uri-scheme}
      @about-function{file-read}
      @about-function{file-read-async}
      @about-function{file-read-finish}
      @about-function{file-append-to}
      @about-function{file-create}
      @about-function{file-replace}
      @about-function{file-append-to-async}
      @about-function{file-append-to-finish}
      @about-function{file-create-async}
      @about-function{file-create-finish}
      @about-function{file-replace-async}
      @about-function{file-replace-finish}
      @about-function{file-query-info}
      @about-function{file-query-info-async}
      @about-function{file-query-info-finish}
      @about-function{file-query-exists}
      @about-function{file-query-file-type}
      @about-function{file-query-filesystem-info}
      @about-function{file-query-filesystem-info-async}
      @about-function{file-query-filesystem-info-finish}
      @about-function{file-query-default-handler}
      @about-function{file-query-default-handler-async}
      @about-function{file-query-default-handler-finish}
      @about-function{file-measure-disk-usage}
      @about-function{file-measure-disk-usage-async}
      @about-function{file-measure-disk-usage-finish}
      @about-function{file-find-enclosing-mount}
      @about-function{file-find-enclosing-mount-async}
      @about-function{file-find-enclosing-mount-finish}
      @about-function{file-enumerate-children}
      @about-function{file-enumerate-children-async}
      @about-function{file-enumerate-children-finish}
      @about-function{file-set-display-name}
      @about-function{file-set-display-name-async}
      @about-function{file-set-display-name-finish}
      @about-function{file-delete}
      @about-function{file-delete-async}
      @about-function{file-delete-finish}
      @about-function{file-trash}
      @about-function{file-trash-async}
      @about-function{file-trash-finish}
      @about-function{file-copy}
      @about-function{file-copy-async}
      @about-function{file-copy-finish}
      @about-function{file-move}
      @about-function{file-make-directory}
      @about-function{file-make-directory-async}
      @about-function{file-make-directory-finish}
      @about-function{file-make-directory-with-parents}
      @about-function{file-make-symbolic-link}
      @about-function{file-query-settable-attributes}
      @about-function{file-query-writable-namespaces}
      @about-function{file-set-attribute}
      @about-function{file-set-attributes-from-info}
      @about-function{file-set-attributes-async}
      @about-function{file-set-attributes-finish}
      @about-function{file-set-attribute-string}
      @about-function{file-set-attribute-byte-string}
      @about-function{file-set-attribute-uint32}
      @about-function{file-set-attribute-int32}
      @about-function{file-set-attribute-uint64}
      @about-function{file-set-attribute-int64}
      @about-function{file-mount-mountable}
      @about-function{file-mount-mountable-finish}
      @about-function{file-unmount-mountable}
      @about-function{file-unmount-mountable-finish}
      @about-function{file-unmount-mountable-with-operation}
      @about-function{file-unmount-mountable-with-operation-finish}
      @about-function{file-eject-mountable}
      @about-function{file-eject-mountable-finish}
      @about-function{file-eject-mountable-with-operation}
      @about-function{file-eject-mountable-with-operation-finish}
      @about-function{file-start-mountable}
      @about-function{file-start-mountable-finish}
      @about-function{file-stop-mountable}
      @about-function{file-stop-mountable-finish}
      @about-function{file-poll-mountable}
      @about-function{file-poll-mountable-finish}
      @about-function{file-mount-enclosing-volume}
      @about-function{file-mount-enclosing-volume-finish}
      @about-function{file-monitor-directory}
      @about-function{file-monitor-file}
      @about-function{file-monitor}
      @about-function{file-load-bytes}
      @about-function{file-load-bytes-async}
      @about-function{file-load-bytes-finish}
      @about-function{file-load-contents}
      @about-function{file-load-contents-async}
      @about-function{file-load-contents-finish}
      @about-function{file-load-partial-contents-async}
      @about-function{file-load-partial-contents-finish}
      @about-function{file-replace-contents}
      @about-function{file-replace-contents-async}
      @about-function{file-replace-contents-bytes-async}
      @about-function{file-replace-contents-finish}
      @about-function{file-copy-attributes}
      @about-function{file-create-readwrite}
      @about-function{file-create-readwrite-async}
      @about-function{file-create-readwrite-finish}
      @about-function{file-open-readwrite}
      @about-function{file-open-readwrite-async}
      @about-function{file-open-readwrite-finish}
      @about-function{file-replace-readwrite}
      @about-function{file-replace-readwrite-async}
      @about-function{file-replace-readwrite-finish}
      @about-function{file-supports-thread-contexts}
    @end{subsection}
  @end{section}
  @begin[File types and applications]{section}
    @begin[GContentType]{subsection}
      A content type is a platform specific string that defines the type of a
      file. On UNIX it is a mime type like \"text/plain\" or \"image/png\". On
      Win32 it is an extension string like \".doc\", \".txt\" or a perceived
      string like \"audio\". Such strings can be looked up in the registry at
      @code{HKEY_CLASSES_ROOT}. On macOS it is a Uniform Type Identifier such
      as @code{com.apple.application}.
      @about-function{content-type-equals}
      @about-function{content-type-is-a}
      @about-function{content-type-is-mime-type}
      @about-function{content-type-is-unknown}
      @about-function{content-type-description}
      @about-function{content-type-mime-type}
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
      Application information and launch contexts.
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
      Interface for icons.
      @about-class{icon}
      @about-function{icon-hash}
      @about-function{icon-equal}
      @about-function{icon-to-string}
      @about-function{icon-new-for-string}
      @about-function{icon-serialize}
      @about-function{icon-deserialize}
    @end{subsection}
    @begin[GFileIcon]{subsection}
      Icons pointing to an image file.
      @about-class{file-icon}
      @about-generic{file-icon-file}
      @about-function{file-icon-new}
    @end{subsection}
    @begin[GLoadableIcon]{subsection}
      Loadable Icons.
      @about-class{loadable-icon}
      @about-function{loadable-icon-load}
      @about-function{loadable-icon-load-async}
      @about-function{loadable-icon-load-finish}
    @end{subsection}
    @begin[GThemedIcon]{subsection}
      Icon theming support.
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
    @begin[GEmblemedIcon]{subsection}
      An implementation of GIcon for icons with emblems.
      @about-class{emblemed-icon}
      @about-generic{emblemed-icon-gicon}
      @about-function{emblemed-icon-new}
      @about-function{emblemed-icon-icon}
      @about-function{emblemed-icon-emblems}
      @about-function{emblemed-icon-add-emblem}
      @about-function{emblemed-icon-clear-emblems}
    @end{subsection}
    @begin[GEmblem]{subsection}
      An object for emblems.
      @about-symbol{emblem-origin}
      @about-class{emblem}
      @about-generic{emblem-icon}
      @about-generic{emblem-origin}
      @about-function{emblem-new}
      @about-function{emblem-new-with-origin}
    @end{subsection}
  @end{section}
  @begin[Resources]{section}
    Resource framework.
    @about-symbol{resource-flags}
    @about-symbol{resource-lookup-flags}
    @about-class{resource}
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
      @about-generic{list-store-item-type}
      @about-function{list-store-new}
      @about-function{list-store-insert}
      @about-function{list-store-insert-sorted}
      @about-function{list-store-append}
      @about-function{list-store-remove}
      @about-function{list-store-remove-all}
      @about-function{list-store-splice}
      @about-function{list-store-sort}
      @about-function{list-store-find}
      @about-function{list-store-find-with-equal-func}
    @end{subsection}
  @end{section}
  @begin[Application support]{section}
    @begin[GApplication]{subsection}
      Core application class.
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
      A command line invocation of an application.
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
      A group of actions.
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
      Interface for action containers.
      @about-class{action-map}
      @about-function{action-map-lookup-action}
      @about-function{action-map-add-action-entries}
      @about-function{action-map-add-action}
      @about-function{action-map-remove-action}
    @end{subsection}
    @begin[GSimpleActionGroup]{subsection}
      A simple @class{g:action-group} implementation.
      @about-class{simple-action-group}
      @about-function{simple-action-group-new}
      @about-function{simple-action-group-lookup}
      @about-function{simple-action-group-insert}
      @about-function{simple-action-group-remove}
      @about-function{simple-action-group-add-entries}
    @end{subsection}
    @begin[GAction]{subsection}
      An action interface.
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
      An implementation of the @class{g:action} interface.
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
      A @class{g:action} object reflecting a @class{g:object} property.
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
      An abstract class representing the contents of a menu.
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
      An implementation of the abstract @class{g:menu-model} class.
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
      User Notifications, pop up messages.
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
