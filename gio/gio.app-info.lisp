;;; ----------------------------------------------------------------------------
;;; gio.app-info.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;;
;;; GAppInfo
;;;
;;;     Application information and launch contexts
;;;
;;; Types and Values
;;;
;;;     GAppInfoCreateFlags
;;;     GAppInfo
;;;
;;;     GAppLaunchContext
;;;
;;; Functions
;;;
;;;     g_app_info_create_from_commandline
;;;     g_app_info_dup
;;;     g_app_info_equal
;;;     g_app_info_get_id
;;;     g_app_info_get_name
;;;     g_app_info_get_display_name
;;;     g_app_info_get_description
;;;     g_app_info_get_executable
;;;     g_app_info_get_commandline
;;;     g_app_info_get_icon
;;;     g_app_info_supports_files
;;;     g_app_info_supports_uris
;;;     g_app_info_should_show
;;;     g_app_info_can_delete
;;;     g_app_info_delete
;;;     g_app_info_reset_type_associations
;;;     g_app_info_set_as_default_for_type
;;;     g_app_info_set_as_default_for_extension
;;;     g_app_info_set_as_last_used_for_type
;;;     g_app_info_add_supports_type
;;;     g_app_info_can_remove_supports_type
;;;     g_app_info_remove_supports_type
;;;     g_app_info_get_supported_types
;;;     g_app_info_get_all
;;;     g_app_info_get_all_for_type
;;;     g_app_info_get_fallback_for_type
;;;     g_app_info_get_recommended_for_type
;;;     g_app_info_get_default_for_type
;;;     g_app_info_get_default_for_type_async               Since 2.74
;;;     g_app_info_get_default_for_type_finish              Since 2.74
;;;     g_app_info_get_default_for_uri_scheme
;;;     g_app_info_get_default_for_uri_scheme_async         Since 2.74
;;;     g_app_info_get_default_for_uri_scheme_finish        Since 2.74
;;;     g_app_info_launch
;;;     g_app_info_launch_uris
;;;     g_app_info_launch_uris_async
;;;     g_app_info_launch_uris_finish
;;;     g_app_info_launch_default_for_uri
;;;     g_app_info_launch_default_for_uri_async
;;;     g_app_info_launch_default_for_uri_finish
;;;
;;;     g_app_launch_context_new
;;;     g_app_launch_context_setenv
;;;     g_app_launch_context_unsetenv
;;;     g_app_launch_context_get_environment
;;;     g_app_launch_context_get_display
;;;     g_app_launch_context_get_startup_notify_id
;;;     g_app_launch_context_launch_failed
;;;
;;; Signals
;;;
;;;     launch-failed
;;;     launch-started                                      Since 2.72
;;;     launched
;;;
;;; Object Hierarchy
;;;
;;;     GFlags
;;;     ╰── GAppInfoCreateFlags
;;;
;;;     GInterface
;;;     ╰── GAppInfo
;;;
;;;     GObject
;;;     ╰── GAppLaunchContext
;;;
;;; Prerequisites
;;;
;;;     GAppInfo requires GObject.
;;;
;;; Known Implementations
;;;
;;;     GAppInfo is implemented by GDesktopAppInfo.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GAppInfoCreateFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GAppInfoCreateFlags" app-info-create-flags
  (:export t
   :type-initializer "g_app_info_create_flags_get_type")
  (:none 0)
  (:needs-terminal #.(ash 1 0))
  (:supports-uris #.(ash 1 1))
  (:supports-startup-notification #.(ash 1 2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'app-info-create-flags)
      "GFlags"
      (liber:symbol-documentation 'app-info-create-flags)
 "@version{2025-05-01}
  @begin{declaration}
(gobject:define-gflags \"GAppInfoCreateFlags\" app-info-create-flags
  (:export t
   :type-initializer \"g_app_info_create_flags_get_type\")
  (:none 0)
  (:needs-terminal #.(ash 1 0))
  (:supports-uris #.(ash 1 1))
  (:supports-startup-notification #.(ash 1 2)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No flags set.}
      @entry[:needs-terminal]{Application opens in a terminal window.}
      @entry[:supports-uris]{Application supports URI arguments.}
      @entry[:supports-startup-notification]{Application supports startup
        notification.}
    @end{table}
  @end{values}
  @begin{short}
    Flags used when creating a @class{g:app-info} instance.
  @end{short}
  @see-class{g:app-info}")

;;; ----------------------------------------------------------------------------
;;; GAppInfo
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GAppInfo" app-info
  (:export t
   :type-initializer "g_app_info_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'app-info)
      "Interface"
      (documentation 'app-info 'type)
 "@version{2025-05-01}
  @begin{short}
    Information about an installed application and methods to launch it with
    file arguments.
  @end{short}
  The @class{g:app-info} interface and the @class{g:app-launch-context} object
  are used for describing and launching applications installed on the system.
  @see-class{g:app-info}
  @see-class{g:app-launch-context}")

;;; ----------------------------------------------------------------------------
;;; GAppLaunchContext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GAppLaunchContext" app-launch-context
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_app_launch_context_get_type")
  nil)

#+liber-documentation
(setf (documentation 'app-launch-context 'type)
 "@version{2025-05-01}
  @begin{short}
    Integrating the launch with the launching application.
  @end{short}
  This is used to handle for instance startup notification and launching the
  new application on the same screen as the launching window.
  @begin[Signal Details]{dictionary}
    @subheading{The \"launch-failed\" signal}
      @begin{pre}
lambda (context startup-notify-id)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[context]{The @class{g:app-launch-context} object emitting the
          signal.}
        @entry[startup-notify-id]{The string with the startup notification ID
          for the failed launch.}
      @end{table}
      The signal is emitted when a @class{g:app-info} launch fails. The startup
      notification ID is provided, so that the launcher can cancel the startup
      notification.
   @subheading{The \"launch-started\" signal}
     @begin{pre}
lambda (context appinfo platform-data)    :run-first
     @end{pre}
     @begin[code]{table}
       @entry[context]{The @class{g:app-launch-context} object emitting the
         signal.}
       @entry[appinfo]{The @class{g:app-info} instance that is about to be
         launched.}
       @entry[platform-data]{The @symbol{g:variant} parameter with additional
         platform specific data for this launch. The argument can be
         @code{NULL}.}
     @end{table}
     The signal is emitted when a @class{g:app-info} instance is about to be
     launched. If non-@code{null} the @arg{platform-data} is a
     @symbol{g:variant} dictionary mapping strings to variants, that is
     @code{a{sv@}}, which contains additional, platform specific data about
     this launch. On UNIX, at least the @code{startup-notification-id} keys
     will be present.

     The value of the @code{startup-notification-id} key (type @code{s}) is a
     startup notification ID corresponding to the format from the startup
     notification specification. It allows tracking the progress of the launchee
     through startup.

     It is guaranteed that this signal is followed by either a
     @code{\"launched\"} or @code{\"launch-failed\"} signal.

     Because a launch operation may involve spawning multiple instances of the
     target application, you should expect this signal to be emitted multiple
     times, one for each spawned instance.

     The default handler is called after the handlers added via the
     @fun{g:signal-connect} function.

     Since 2.72
    @subheading{The \"launched\" signal}
      @begin{pre}
lambda (context appinfo platform-data)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[context]{The @class{g:app-launch-context} object emitting the
          signal.}
        @entry[appinfo]{The @class{g:app-info} object that was just launched.}
        @entry[platform-data]{The @symbol{g:variant} parameter with additional
          platform specific data for this launch.}
      @end{table}
      The signal is emitted when a @class{g:app-info} object is successfully
      launched. The argument @arg{platform-data} is a @symbol{g:variant}
      dictionary mapping strings to variants, that is @code{a{sv@}}, which
      contains additional, platform-specific data about this launch. On UNIX,
      at least the @code{\"pid\"} and @code{\"startup-notification-id\"} keys
      will be present.
  @end{dictionary}
  @see-construtor{g:app-launch-context-new}
  @see-class{g:app-info}
  @see-class{gdk:app-launch-context}")

;;; ----------------------------------------------------------------------------
;;; g_app_info_create_from_commandline
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_create_from_commandline"
               %app-info-create-from-commandline)
    (gobject:object app-info :return)
  (cmdline :string)
  (application :string)
  (flags app-info-create-flags)
  (err :pointer))

(defun app-info-create-from-commandline (cmdline application flags)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[cmdline]{a string for the commandline to use}
  @argument[application]{a string for the application name, or @code{nil} to
    use @arg{cmdline}}
  @argument[flags]{a @symbol{g:app-info-create-flags} value for the flags that
    can specify details of the created @class{g:app-info} instance}
  @return{The new @class{g:app-info} instance.}
  @begin{short}
    Creates a new @class{g:app-info} instance from the given information.
  @end{short}
  Note that for the @arg{cmdline} argument, the quoting rules of the Exec key
  of the freedesktop.org Desktop Entry Specification are applied. For example,
  if the @arg{cmdline} argument contains percent-encoded URIs, the percent
  character must be doubled in order to prevent it from being swallowed by Exec
  key unquoting. See the specification for exact quoting rules.
  @see-class{g:app-info}
  @see-symbol{g:app-info-create-flags}"
  (glib:with-error (err)
    (%app-info-create-from-commandline cmdline application flags err)))

(export 'app-info-create-from-commandline)

;;; ----------------------------------------------------------------------------
;;; g_app_info_dup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_dup" app-info-dup) (gobject:object :return)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{The @class{g:app-info} instance with the duplicate of @arg{appinfo}.}
  @begin{short}
    Creates a duplicate of an application info.
  @end{short}
  @see-class{g:app-info}"
  (appinfo gobject:object))

(export 'app-info-dup)

;;; ----------------------------------------------------------------------------
;;; g_app_info_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_equal" app-info-equal) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo1]{a first @class{g:app-info} instance}
  @argument[appinfo2]{a second @class{g:app-info} instance}
  @begin{return}
    @em{True} if @arg{appinfo1} is equal to @arg{appinfo2}, @em{false}
    otherwise.
  @end{return}
  @begin{short}
    Checks if two application infos are equal.
  @end{short}
  @see-class{g:app-info}"
  (appinfo1 gobject:object)
  (appinfo2 gobject:object))

(export 'app-info-equal)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_id" app-info-id) :string
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{The string containing the ID of the application.}
  @begin{short}
    Gets the ID of an application.
  @end{short}
  An ID is a string that identifies the application. The exact format of the ID
  is platform dependent. For instance, on Unix this is the desktop file ID from
  the xdg menu specification.

  Note that the returned ID may be @code{nil}, depending on how @arg{appinfo}
  has been constructed.
  @see-class{g:app-info}"
  (appinfo gobject:object))

(export 'app-info-id)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_name" app-info-name) :string
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{The string with the the name of the application for @arg{appinfo}.}
  @begin{short}
    Gets the installed name of the application.
  @end{short}
  @see-class{g:app-info}
  @see-function{g:app-info-display-name}"
  (appinfo gobject:object))

(export 'app-info-name)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_display_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_display_name" app-info-display-name) :string
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @begin{return}
    The string with the the display name of the application for @arg{appinfo},
    or the name of the application if no display name is available.
  @end{return}
  @begin{short}
    Gets the display name of the application.
  @end{short}
  The display name is often more descriptive to the user than the
  application name itself.
  @see-class{g:app-info}
  @see-function{g:app-info-name}"
  (appinfo gobject:object))

(export 'app-info-display-name)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_description
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_description" app-info-description) :string
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{The string containing a description of the application @arg{appinfo},
    or @code{nil} if none.}
  @begin{short}
    Gets a human readable description of an installed application.
  @end{short}
  @see-class{g:app-info}"
  (appinfo gobject:object))

(export 'app-info-description)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_executable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_executable" app-info-executable) :string
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{The string containing the application binaries name of @arg{appinfo}.}
  @begin{short}
    Gets the name of the executable for the installed application.
  @end{short}
  @see-class{g:app-info}"
  (appinfo gobject:object))

(export 'app-info-executable)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_commandline
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_commandline" app-info-commandline) :string
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{The string containing the commandline of @arg{appinfo}, or @code{nil}
    if this information is not available.}
  @begin{short}
    Gets the commandline with which the application will be started.
  @end{short}
  @see-class{g:app-info}"
  (appinfo gobject:object))

(export 'app-info-commandline)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_icon" app-info-icon) (gobject:object icon)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @begin{return}
    The default @class{g:icon} object for @arg{appinfo} or @code{nil} if there
    is no default icon.
  @end{return}
  @begin{short}
    Gets the icon for the application.
  @end{short}
  @see-class{g:app-info}
  @see-class{g:icon}"
  (appinfo gobject:object))

(export 'app-info-icon)

;;; ----------------------------------------------------------------------------
;;; g_app_info_supports_files
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_supports_files" app-info-supports-files) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{appinfo} supports files.}
  @begin{short}
    Checks if the application accepts files as arguments.
  @end{short}
  @see-class{g:app-info}"
  (appinfo gobject:object))

(export 'app-info-supports-files)

;;; ----------------------------------------------------------------------------
;;; g_app_info_supports_uris
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_supports_uris" app-info-supports-uris) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{appinfo} supports URIs.}
  @begin{short}
    Checks if the application supports reading files and directories from URIs.
  @end{short}
  @see-class{g:app-info}"
  (appinfo gobject:object))

(export 'app-info-supports-uris)

;;; ----------------------------------------------------------------------------
;;; g_app_info_should_show
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_should_show" app-info-should-show) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{appinfo} should be shown, @em{false} otherwise.}
  @begin{short}
    Checks if the application info should be shown in menus that list available
    applications.
  @end{short}
  @see-class{g:app-info}"
  (appinfo gobject:object))

(export 'app-info-should-show)

;;; ----------------------------------------------------------------------------
;;; g_app_info_can_delete
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_can_delete" app-info-can-delete) :boolean
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{appinfo} can be deleted.}
  @begin{short}
    Obtains the information whether the application info can be deleted.
  @end{short}
  See the @fun{g:app-info-delete} function.
  @see-class{g:app-info}
  @see-function{g:app-info-delete}"
  (appinfo gobject:object))

(export 'app-info-can-delete)

;;; ----------------------------------------------------------------------------
;;; g_app_info_delete
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_delete" app-info-delete) :boolean
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{appinfo} has been deleted.}
  @begin{short}
    Tries to delete an application info.
  @end{short}

  On some platforms, there may be a difference between user-defined application
  infos which can be deleted, and system-wide ones which cannot. See the
  @fun{g:app-info-can-delete} function.
  @see-class{g:app-info}
  @see-function{g:app-info-can-delete}"
  (appinfo gobject:object))

(export 'app-info-delete)

;;; ----------------------------------------------------------------------------
;;; g_app_info_reset_type_associations
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_reset_type_associations"
                app-info-reset-type-associations) :void
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[ctype]{a string for the content type}
  @begin{short}
    Removes all changes to the type associations done by the
    @fun{g:app-info-set-as-default-for-type},
    @fun{g:app-info-set-as-default-for-extension},
    @fun{g:app-info-add-supports-type} or @fun{g:app-info-remove-supports-type}
    function.
  @end{short}
  @see-class{g:app-info}
  @see-function{g:app-info-set-as-default-for-type}
  @see-function{g:app-info-set-as-default-for-extension}
  @see-function{g:app-info-add-supports-type}
  @see-function{g:app-info-remove-supports-type}"
  (ctype :string))

(export 'app-info-reset-type-associations)

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_default_for_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_set_as_default_for_type"
                %app-info-set-as-default-for-type) :boolean
  (appinfo gobject:object)
  (ctype :string)
  (err :pointer))

(defun app-info-set-as-default-for-type (appinfo ctype)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[ctype]{a string for the content type}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Sets the application as the default handler for a given content type.
  @end{short}
  @see-class{g:app-info}"
  (glib:with-error (err)
    (%app-info-set-as-default-for-type appinfo ctype err)))

(export 'app-info-set-as-default-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_default_for_extension
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_set_as_default_for_extension"
                %app-info-set-as-default-for-extension) :boolean
  (appinfo gobject:object)
  (extension :string)
  (err :pointer))

(defun app-info-set-as-default-for-extension (appinfo extension)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[extension]{a string containing the file extension, without the dot}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Sets the application as the default handler for the given file extension.
  @end{short}
  @see-class{g:app-info}"
  (glib:with-error (err)
    (%app-info-set-as-default-for-extension appinfo extension err)))

(export 'app-info-set-as-default-for-extension)

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_last_used_for_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_set_as_last_used_for_type"
                %app-info-set-as-last-used-for-type) :boolean
  (appinfo gobject:object)
  (ctype :string)
  (err :pointer))

(defun app-info-set-as-last-used-for-type (appinfo ctype)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[ctype]{a string for the content type}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Sets the application as the last used application for a given content type.
  @end{short}
  This will make the application appear as first in the list returned by the
  @fun{g:app-info-recommended-for-type} function, regardless of the default
  application for that content type.
  @see-class{g:app-info}
  @see-function{g:app-info-recommended-for-type}"
  (glib:with-error (err)
    (%app-info-set-as-last-used-for-type appinfo ctype err)))

(export 'app-info-set-as-last-used-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_add_supports_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_add_supports_type" %app-info-add-supports-type)
    :boolean
  (appinfo gobject:object)
  (ctype :string)
  (err :pointer))

(defun app-info-add-supports-type (appinfo ctype)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[ctype]{a string for the content type}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Adds a content type to the application information to indicate the
    application is capable of opening files with the given content type.
  @end{short}
  @see-class{g:app-info}"
  (glib:with-error (err)
    (%app-info-add-supports-type appinfo ctype err)))

(export 'app-info-add-supports-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_can_remove_supports_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_can_remove_supports_type"
                app-info-can-remove-supports-type) :boolean
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @begin{return}
    @em{True} if it is possible to remove supported content types from a
    given @arg{appinfo}, @em{false} if not.
  @end{return}
  @begin{short}
    Checks if a supported content type can be removed from an application.
  @end{short}
  @see-class{g:app-info}"
  (appinfo gobject:object))

(export 'app-info-can-remove-supports-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_remove_supports_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_remove_supports_type" %app-info-remove-supports-type)
    :boolean
  (appinfo gobject:object)
  (ctype :string)
  (err :pointer))

(defun app-info-remove-supports-type (appinfo ctype)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[ctype]{a string for the content type}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Removes a supported content type from an application information, if
    possible.
  @end{short}
  @see-class{g:app-info}"
  (glib:with-error (err)
    (%app-info-remove-supports-type appinfo ctype err)))

(export 'app-info-remove-supports-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_supported_types
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_supported_types" app-info-supported-types)
    (glib:strv-t :free-from-foreign nil)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @return{The list of strings with the content types.}
  @begin{short}
    Retrieves the list of content types that @arg{appinfo} claims to support.
  @end{short}
  If this information is not provided by the environment, this function will
  return @code{nil}. This function does not take in consideration associations
  added with the @fun{g:app-info-add-supports-type} function, but only those
  exported directly by the application.
  @see-class{g:app-info}
  @see-function{g:app-info-add-supports-type}"
  (appinfo gobject:object))

(export 'app-info-supported-types)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_all" app-info-all)
    (glib:list-t (gobject:object :return))
 #+liber-documentation
 "@version{2025-05-01}
  @return{The list of @class{g:app-info} instances.}
  @begin{short}
    Gets a list of all application infos for the applications currently
    registered on this system.
  @end{short}
  For desktop files, this includes applications that have @code{NoDisplay=true}
  set or are excluded from display by means of @code{OnlyShowIn} or
  @code{NotShowIn}. See the @fun{g:app-info-should-show} function. The returned
  list does not include applications which have the @code{Hidden} key set.
  @see-class{g:app-info}
  @see-function{g:app-info-should-show}")

(export 'app-info-all)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_all_for_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_all_for_type" app-info-all-for-type)
    (glib:list-t (gobject:object :return))
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a string for the content type}
  @begin{return}
    The list of @class{g:app-info} instances for the given @arg{ctype} or
    @code{nil} on error.
  @end{return}
  @begin{short}
    Gets a list of all application infos for a given content type, including
    the recommended and fallback application infos.
  @end{short}
  See the @fun{g:app-info-recommended-for-type} and
  @fun{g:app-info-fallback-for-type} functions.
  @see-class{g:app-info}
  @see-function{g:app-info-recommended-for-type}
  @see-function{g:app-info-fallback-for-type}"
  (ctype :string))

(export 'app-info-all-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_fallback_for_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_fallback_for_type" app-info-fallback-for-type)
    (glib:list-t (gobject:object :return))
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a string for the content type}
  @begin{return}
    The list of @class{g:app-info} instances for the given @arg{ctype} or
    @code{nil} on error.
  @end{return}
  @begin{short}
    Gets a list of fallback application infos for a given content type, that is,
    those applications which claim to support the given content type by MIME
    type subclassing and not directly.
  @end{short}
  @see-class{g:app-info}"
  (ctype :string))

(export 'app-info-fallback-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_recommended_for_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_recommended_for_type"
                app-info-recommended-for-type)
    (glib:list-t (gobject:object :return))
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a string for the content type}
  @begin{return}
    The list of @class{g:app-info} instances for the given @arg{ctype} or
    @code{nil} on error.
  @end{return}
  @begin{short}
    Gets a list of recommended application infos for a given content type, that
    is, those applications which claim to support the given content type
    exactly, and not by MIME type subclassing.
  @end{short}
  Note that the first application of the list is the last used one, that is,
  the last one for which the @fun{g:app-info-set-as-last-used-for-type} function
  has been called.
  @see-class{g:app-info}
  @see-function{g:app-info-set-as-last-used-for-type}"
  (ctype :string))

(export 'app-info-recommended-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_default_for_type" app-info-default-for-type)
    (gobject:object app-info :return)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a string for the content type}
  @argument[must-support-uris]{if @em{true}, the application info is expected
    to support URIs}
  @begin{return}
    The @class{g:app-info} instance for the given @arg{ctype} or @code{nil}
    on error.
  @end{return}
  @begin{short}
    Gets the default application info for a given content type.
  @end{short}
  @see-class{g:app-info}"
  (ctype :string)
  (must-support-uris :boolean))

(export 'app-info-default-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_type_async                   Since 2.74
;;; ----------------------------------------------------------------------------

#+glib-2-74
(cffi:defcfun ("g_app_info_get_default_for_type_async"
               %app-info-default-for-type-async) :void
  (ctype :string)
  (must-support-uris :boolean)
  (cancellable (gobject:object cancellable))
  (func :pointer)
  (data :pointer))

#+glib-2-74
(defun app-info-default-for-type-async (ctype
                                        must-support-uris
                                        cancellable
                                        func)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[ctype]{a string for the content type}
  @argument[must-support-uris]{if @em{true}, the application info is expected
    to support URIs}
  @argument[cancellable]{a @class{g:cancellable} object, can be @code{nil}}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to
    call when the request is done}
  @begin{return}
    The @class{g:app-info} instance for the given @arg{ctype} or @code{nil}
    on error.
  @end{return}
  @begin{short}
    Asynchronously gets the default @class{g:app-info} instance for a given
    content type.
  @end{short}

  This function completes asynchronously. Use the
  @fun{g:app-info-default-for-type-finish} function inside the
  @symbol{g:async-ready-callback} callback function to obtain the result of the
  operation.

  Since 2.74
  @see-class{g:app-info}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{g:app-info-default-for-type}
  @see-function{g:app-info-default-for-type-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%app-info-default-for-type-async ctype
                                      must-support-uris
                                      (or cancellable (cffi:null-pointer))
                                      (cffi:callback async-ready-callback)
                                      ptr)))

#+glib-2-74
(export 'app-info-default-for-type-async)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_type_finish                  Since 2.74
;;; ----------------------------------------------------------------------------

#+glib-2-74
(cffi:defcfun ("g_app_info_get_default_for_type_finish"
               %app-info-default-for-type-finish)
    (gobject:object app-info :return)
  (result (gobject:object async-result))
  (err :pointer))

#+glib-2-74
(defun app-info-default-for-type-finish (result)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[result]{a @class{g:async-result} object}
  @return{The @class{g:app-info} instance for the given content type.}
  @begin{short}
    Finishes a default @class{g:app-info} instance lookup started by the
    @fun{g:app-info-default-for-type-async} function.
  @end{short}

  Since 2.74
  @see-class{g:app-info}
  @see-class{g:async-result}
  @see-function{g:app-info-default-for-type-async}"
  (glib:with-ignore-error (err)
    (%app-info-default-for-type-finish result err)))

#+glib-2-74
(export 'app-info-default-for-type-finish)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_uri_scheme
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_default_for_uri_scheme"
                app-info-default-for-uri-scheme)
    (gobject:object app-info :return)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[uri-scheme]{a string containing a URI scheme}
  @begin{return}
    The @class{g:app-info} instance for the given @arg{uri-scheme} or
    @code{nil} on error.
  @end{return}
  @begin{short}
    Gets the default application for handling URIs with the given URI scheme.
  @end{short}
  A URI scheme is the initial part of the URI, up to but not including the
  @file{':'}, for example, @file{\"http\"}, @file{\"ftp\"} or @file{\"sip\"}.
  @see-class{g:app-info}"
  (uri-scheme :string))

(export 'app-info-default-for-uri-scheme)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_uri_scheme_async            Since 2.74
;;; ----------------------------------------------------------------------------

#+glib-2-74
(cffi:defcfun ("g_app_info_get_default_for_uri_scheme_async"
               %app-info-default-for-uri-scheme-async) :void
  (uri-scheme :string)
  (cancellable (gobject:object cancellable))
  (func :pointer)
  (data :pointer))

#+glib-2-74
(defun app-info-default-for-uri-scheme-async (uri-scheme cancellable func)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[uri-scheme]{a string containing a URI scheme}
  @argument[cancellable]{a @class{g:cancellable} object, can be @code{nil}}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to
    call when the request is done}
  @begin{short}
    Asynchronously gets the default application for handling URIs with the given
    URI scheme.
  @end{short}
  A URI scheme is the initial part of the URI, up to but not including the
  @file{:}, for example, @file{http}, @file{ftp} or @file{sip}.

  This function completes asynchronously. Use the
  @fun{g:app-info-default-for-uri-scheme-finish} function inside the
  @symbol{g:async-ready-callback} callback function to obtain the result of the
  operation.

  Since 2.74
  @see-class{g:app-info}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{g:app-info-default-for-type}
  @see-function{g:app-info-default-for-type-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%app-info-default-for-uri-scheme-async uri-scheme
                                            (or cancellable (cffi:null-pointer))
                                            (cffi:callback async-ready-callback)
                                            ptr)))

#+glib-2-74
(export 'app-info-default-for-uri-scheme-async)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_uri_scheme_finish           Since 2.74
;;; ----------------------------------------------------------------------------

#+glib-2-74
(cffi:defcfun ("g_app_info_get_default_for_uri_scheme_finish"
               %app-info-default-for-uri-scheme-finish)
    (gobject:object app-info)
  (result (gobject:object async-result))
  (err :pointer))

#+glib-2-74
(defun app-info-default-for-uri-scheme-finish (result)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[result]{a @class{g:async-result} object}
  @return{The @class{g:app-info} instance for the given Uri scheme.}
  @begin{short}
    Finishes a default @class{g:app-info} instance lookup started by the
    @fun{g:app-info-default-for-uri-scheme-async} function.
  @end{short}

  Since 2.74
  @see-class{g:app-info}
  @see-class{g:async-result}
  @see-function{g:app-info-default-for-uri-scheme-async}"
  (glib:with-ignore-error (err)
    (%app-info-default-for-uri-scheme-finish result err)))

#+glib-2-74
(export 'app-info-default-for-uri-scheme-finish)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch
;;; ----------------------------------------------------------------------------

;; TODO: Consider to change the implementation and pass a list of Lisp path
;; or namestring arguments.

(cffi:defcfun ("g_app_info_launch" %app-info-launch) :boolean
  (appinfo gobject:object)
  (files (glib:list-t gobject:object))
  (context (gobject:object app-launch-context))
  (err :pointer))

(defun app-info-launch (appinfo files context)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[files]{a list of @class{g:file} objects}
  @argument[context]{a @class{g:app-launch-context} instance or @code{nil}}
  @return{@em{True} on successful launch, @em{false} otherwise.}
  @begin{short}
    Launches the application.
  @end{short}
  Passes @arg{files} to the launched application as arguments, using the
  optional @arg{context} argument to get information about the details of the
  application launcher, like what screen it is on. To launch the application
  without arguments pass @code{nil} for the files list.

  Note that even if the launch is successful the application launched can fail
  to start if it runs into problems during startup. There is no way to detect
  this.

  Some URIs can be changed when passed through a @class{g:file} object, for
  instance unsupported URIs with strange formats like mailto:, so if you have a
  textual URI you want to pass in as argument, consider using the
  @fun{g:app-info-launch-uris} function instead.

  The launched application inherits the environment of the launching process,
  but it can be modified with the @fun{g:app-launch-context-setenv} function
  and the @fun{g:app-launch-context-unsetenv} function.

  On UNIX, this function sets the @code{GIO_LAUNCHED_DESKTOP_FILE} environment
  variable with the path of the launched desktop file and
  @code{GIO_LAUNCHED_DESKTOP_FILE_PID} to the process ID of the launched
  process. This can be used to ignore @code{GIO_LAUNCHED_DESKTOP_FILE}, should
  it be inherited by further processes. The @code{DISPLAY} and
  @code{DESKTOP_STARTUP_ID} environment variables are also set, based on
  information provided in @arg{context}.
  @see-class{g:app-info}
  @see-class{g:app-launch-context}
  @see-class{g:file}
  @see-function{g:app-info-launch-uris}
  @see-function{g:app-launch-context-setenv}
  @see-function{g:app-launch-context-unsetenv}"
  (glib:with-error (err)
    (%app-info-launch appinfo files context err)))

(export 'app-info-launch)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_uris" %app-info-launch-uris) :boolean
  (appinfo gobject:object)
  (uris (glib:list-t :string))
  (context (gobject:object app-launch-context))
  (err :pointer))

(defun app-info-launch-uris (appinfo uris context)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[uris]{a list of strings containing the URIs to launch}
  @argument[context]{a @class{g:app-launch-context} instance or @code{nil}}
  @return{@em{True} on successful launch, @em{false} otherwise.}
  @begin{short}
    Launches the application.
  @end{short}
  This passes the URIs to the launched application as arguments, using the
  optional @arg{context} to get information about the details of the launcher,
  like what screen it is on.

  To launch the application without arguments pass a @code{nil} URIs list.

  Note that even if the launch is successful the application launched can fail
  to start if it runs into problems during startup. There is no way to detect
  this.
  @see-class{g:app-info}
  @see-class{g:app-launch-context}"
  (glib:with-error (err)
    (%app-info-launch-uris appinfo uris context err)))

(export 'app-info-launch-uris)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_uris_async"
               %app-info-launch-uris-async) :void
  (appinfo gobject:object)
  (uris (glib:list-t :string))
  (context (gobject:object app-launch-context))
  (cancellable (gobject:object cancellable))
  (func :pointer)
  (data :pointer))

(defun app-info-launch-uris-async (appinfo uris context cancellable func)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[uris]{a list of strings containing URIs to launch}
  @argument[context]{a @class{g:app-launch-context} instance or @code{nil}}
  @argument[canellable]{a @class{g:cancellable} instance}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to
    call when the request is done}
  @begin{short}
    Asynchronous version of the @fun{g:app-info-launch-uris} function.
  @end{short}
  The callback is invoked immediately after the application launch, but it
  waits for activation in case of D-Bus–activated applications and also provides
  extended error information for sandboxed applications, see notes for
  the @fun{g:app-info-launch-default-for-uri-async} function.

  This method completes asynchronously. Use the
  @fun{g:app-info-launch-uris-finish} function inside the
  @symbol{g:async-ready-callback} callback function to obtain the result of the
  operation.
  @see-class{g:app-info}
  @see-class{g:app-launch-context}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{g:app-info-launch-uris}
  @see-function{g:app-info-launch-uris-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%app-info-launch-uris-async appinfo
                                 uris
                                 context
                                 (or cancellable (cffi:null-pointer))
                                 (cffi:callback async-ready-callback)
                                 ptr)))

(export 'app-info-launch-uris-async)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_uris_finish"
               %app-info-launch-uris-finish) :boolean
  (result (gobject:object async-result))
  (err :pointer))

(defun app-info-launch-uris-finish (result)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[result]{a @class{g:async-result} object}
  @return{@em{True} on sucessful launch, @em{false} otherwise.}
  @begin{short}
    Finishes a @fun{g:app-info-launch-uris-async} operation.
  @end{short}
  @see-class{g:app-info}
  @see-class{g:async-result}
  @see-function{g:app-info-launch-uris-async}"
  (glib:with-error (err)
    (%app-info-launch-uris-finish result err)))

(export 'app-info-launch-uris-finish)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_default_for_uri"
                %app-info-launch-default-for-uri) :boolean
  (uri :string)
  (context (gobject:object app-launch-context))
  (err :pointer))

(defun app-info-launch-default-for-uri (uri &optional context)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[uri]{a string for the URI to show}
  @argument[context]{an optional @class{g:app-launch-context} object,
    the argument can be @code{nil}, that is the default}
  @return{@em{True} on sucess, @em{false} on error.}
  @begin{short}
    Utility function that launches the default application registered to handle
    the specified URI.
  @end{short}
  Synchronous I/O is done on the URI to detect the type of the file if required.
  @see-class{g:app-info}
  @see-class{g:app-launch-context}"
  (glib:with-ignore-error (err)
    (let ((context (or context (cffi:null-pointer))))
      (%app-info-launch-default-for-uri uri context err))))

(export 'app-info-launch-default-for-uri)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_default_for_uri_async"
               %app-info-launch-default-for-uri-async) :void
  (uri :string)
  (context (gobject:object app-launch-context))
  (cancellable (gobject:object cancellable))
  (func :pointer)
  (data :pointer))

(defun app-info-launch-default-for-uri-async (uri context cancellable func)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[uri]{a string for the URI to show}
  @argument[context]{an optional @class{g:app-launch-context} object, or
    @code{nil}}
  @argument[cancellable]{a @class{g:cancellable} object}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to
    call when the request is done}
  @begin{short}
    Asynchronous version of the @fun{g:app-info-launch-default-for-uri}
    function.
  @end{short}
  This version is useful if you are interested in receiving error information
  in the case where the application is sandboxed and the portal may present
  an application chooser dialog to the user.

  This is also useful if you want to be sure that the D-Bus–activated
  applications are really started before termination and if you are interested
  in receiving error information from their activation.
  @see-class{g:app-launch-context}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{g:app-info-launch-default-for-uri}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%app-info-launch-default-for-uri-async
                       uri
                       (or context (cffi:null-pointer))
                       (or cancellable (cffi:null-pointer))
                       (cffi:callback async-ready-callback)
                       ptr)))

(export 'app-info-launch-default-for-uri-async)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_default_for_uri_finish"
               %app-info-launch-default-for-uri-finish) :boolean
  (result (gobject:object async-result))
  (err :pointer))

(defun app-info-launch-default-for-uri-finish (result)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[result]{a @class{g:async-result} object}
  @return{@em{True} if the launch was successful, @em{false} otherwise}
  @begin{short}
    Finishes an asynchronous @fun{g:app-info-launch-default-for-uri-async}
    operation.
  @end{short}
  @see-class{g:app-launch-context}
  @see-class{g:async-result}
  @see-function{g:app-info-launch-default-for-uri-async}"
  (glib:with-ignore-error (err)
    (%app-info-launch-default-for-uri-finish result err)))

(export 'app-info-launch-default-for-uri-finish)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_new
;;; ----------------------------------------------------------------------------

(declaim (inline app-launch-context-new))

(defun app-launch-context-new ()
 #+liber-documentation
 "@version{2025-05-01}
  @return{The @class{g:app-launch-context} instance.}
  @begin{short}
    Creates a new application launch context.
  @end{short}
  This is not normally used, instead you instantiate a subclass of this, such
  as a @class{gdk:app-launch-context} object.
  @see-class{g:app-launch-context}
  @see-class{gdk:app-launch-context}"
  (make-instance 'app-launch-context))

(export 'app-launch-context-new)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_setenv
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_setenv" app-launch-context-setenv) :void
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[variable]{a string for the enviroment variable to set}
  @argument[value]{a string for the value to set the variabel to}
  @begin{short}
    Arranges for @arg{variable} to be set to @arg{value} in the child's
    environment when @arg{context} is used to launch an application.
  @end{short}
  @see-class{g:app-launch-context}"
  (context (gobject:object app-launch-context))
  (variable :string)
  (value :string))

(export 'app-launch-context-setenv)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_unsetenv
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_unsetenv" app-launch-context-unsetenv)
    :void
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[variable]{a string for the enviroment variable to remove}
  @begin{short}
    Arranges for @arg{variable} to be unset in the child's environment when
    @arg{context} is used to launch an application.
  @end{short}
  @see-class{g:app-launch-context}"
  (context (gobject:object app-launch-context))
  (variable :string))

(export 'app-launch-context-unsetenv)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_environment
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_get_environment"
                app-launch-context-environment) glib:strv-t
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[context]{a @class{g:app-launch-context} instance}
  @return{The list of strings with the child's enviroment.}
  @begin{short}
    Gets the complete environment variable list to be passed to the child
    process when @arg{context} is used to launch an application.
  @end{short}
  This is a list of strings, where each string has the form @code{KEY=VALUE}.
  @see-class{g:app-launch-context}"
  (context (gobject:object app-launch-context)))

(export 'app-launch-context-environment)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_get_display" app-launch-context-display)
    :string
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[files]{a list of @class{g:file} objects}
  @return{The display string for the display.}
  @begin{short}
    Gets the display string for the context.
  @end{short}
  This is used to ensure new applications are started on the same display as
  the launching application, by setting the @code{DISPLAY} environment variable.
  @see-class{g:app-launch-context}
  @see-class{g:app-info}"
  (context (gobject:object app-launch-context))
  (appinfo gobject:object)
  (files (glib:list-t gobject:object)))

(export 'app-launch-context-display)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_startup_notify_id
;;; ----------------------------------------------------------------------------

;; TODO: Replace FILES with a list of Lisp namestrings.

(cffi:defcfun ("g_app_launch_context_get_startup_notify_id"
                app-launch-context-startup-notify-id) :string
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[appinfo]{a @class{g:app-info} instance}
  @argument[files]{a list of @class{g:file} objects}
  @begin{return}
    The string with a startup notification ID for the application, or
    @code{nil} if not supported.
  @end{return}
  @begin{short}
    Initiates startup notification for the application and returns the
    @code{DESKTOP_STARTUP_ID} for the launched operation, if supported.
  @end{short}
  Startup notification IDs are defined in the FreeDesktop.Org Startup
  Notifications standard.
  @see-class{g:app-launch-context}
  @see-class{g:app-info}
  @see-class{g:file}"
  (context (gobject:object app-launch-context))
  (appinfo gobject:object)
  (files (glib:list-t gobject:object)))

(export 'app-launch-context-startup-notify-id)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_launch_failed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_launch_failed"
                app-launch-context-launch-failed) :void
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[startup-notify-id]{a string for the startup notification ID}
  @begin{short}
    Called when an application has failed to launch, so that it can cancel the
    application startup notification started in the
    @fun{g:app-launch-context-startup-notify-id} function.
  @end{short}
  @see-class{g:app-launch-context}
  @see-function{g:app-launch-context-startup-notify-id}"
  (context (gobject:object app-launch-context))
  (startup-notify-id :string))

(export 'app-launch-context-launch-failed)

;;; --- End of file gio.app-info.lisp ------------------------------------------
