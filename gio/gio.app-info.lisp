;;; ----------------------------------------------------------------------------
;;; gio.app-info.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;     g_app_info_launch
;;;     g_app_info_supports_files
;;;     g_app_info_supports_uris
;;;     g_app_info_launch_uris
;;;     g_app_info_launch_uris_async
;;;     g_app_info_launch_uris_finish
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
;;;     g_app_info_get_default_for_type
;;;
;;;     g_app_info_get_default_for_type_async              Since 2.74
;;;     g_app_info_get_default_for_type_finish             Since 2.74
;;;
;;;     g_app_info_get_default_for_uri_scheme
;;;
;;;     g_app_info_get_default_for_uri_scheme_async        Since 2.74
;;;     g_app_info_get_default_for_uri_scheme_finish       Since 2.74
;;;
;;;     g_app_info_get_fallback_for_type
;;;     g_app_info_get_recommended_for_type
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
;;;     launch-started                                     Since 2.72
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
;;; enum GAppInfoCreateFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GAppInfoCreateFlags" app-info-create-flags
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
 "@version{2023-7-11}
  @begin{short}
    Flags used when creating a @class{g:app-info} instance.
  @end{short}
  @begin{pre}
(gobject:define-g-flags \"GAppInfoCreateFlags\" app-info-create-flags
  (:export t
   :type-initializer \"g_app_info_create_flags_get_type\")
  (:none 0)
  (:needs-terminal #.(ash 1 0))
  (:supports-uris #.(ash 1 1))
  (:supports-startup-notification #.(ash 1 2)))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No flags set.}
    @entry[:needs-terminal]{Application opens in a terminal window.}
    @entry[:supports-uris]{Application supports URI arguments.}
    @entry[:supports-startup-notification]{Application supports startup
      notification.}
  @end{table}
  @see-class{g:app-info}")

;;; ----------------------------------------------------------------------------
;;; GAppInfo
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GAppInfo" app-info
  (:export t
   :type-initializer "g_app_info_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'app-info)
      "Interface"
      (documentation 'app-info 'type)
 "@version{2022-12-27}
  @begin{short}
    Information about an installed application and methods to launch it with
    file arguments.
  @end{short}
  @see-class{g:app-launch-context}")

;;; ----------------------------------------------------------------------------
;;; GAppLaunchContext
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GAppLaunchContext" app-launch-context
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_app_launch_context_get_type")
  nil)

#+liber-documentation
(setf (documentation 'app-launch-context 'type)
 "@version{2023-9-18}
  @begin{short}
    Integrating the launch with the launching application.
  @end{short}
  This is used to handle for instance startup notification and launching the
  new application on the same screen as the launching window.
  @begin[Signal Details]{dictionary}
    @subheading{The \"launch-failed\" signal}
      The signal is emitted when a @class{g:app-info} launch fails. The startup
      notification ID is provided, so that the launcher can cancel the startup
      notification.
      @begin{pre}
lambda (context startup-notify-id)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[context]{The @class{g:app-launch-context} object emitting the
          signal.}
        @entry[startup-notify-id]{A string with the startup notification ID
          for the failed launch.}
      @end{table}
   @subheading{The \"launch-started\" signal}
     The signal is emitted when a @class{g:app-info} instance is about to be
     launched. If non-@code{null} the @arg{platform-data} is a @type{g:variant}
     dictionary mapping strings to variants, i.e. @code{a{sv@}}, which contains
     additional, platform specific data about this launch. On UNIX, at least
     the startup-notification-id keys will be present.

     The value of the startup-notification-id key (type @code{s}) is a startup
     notification ID corresponding to the format from the startup-notification
     specification. It allows tracking the progress of the launchee through
     startup.

     It is guaranteed that this signal is followed by either a \"launched\" or
     \"launch-failed\" signal.

     Because a launch operation may involve spawning multiple instances of the
     target application, you should expect this signal to be emitted multiple
     times, one for each spawned instance.

     The default handler is called after the handlers added via the
     @fun{g:signal-connect} function.

     Since: 2.72
      @begin{pre}
lambda (context info platform-data)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[context]{The @class{g:app-launch-context} object emitting the
          signal.}
        @entry[info]{A @class{g:app-info} instance that is about to be
          launched.}
        @entry[platform-data]{A @type{g:variant} value with additional
          platform specific data for this launch. The argument can be
          @code{NULL}.}
      @end{table}
    @subheading{The \"launched\" signal}
      The signal is emitted when a @class{g:app-info} object is successfully
      launched. The argument @arg{platform-data} is an @type{g:variant}
      dictionary mapping strings to variants, i.e. @code{a{sv@}}, which contains
      additional, platform-specific data about this launch. On UNIX, at least
      the \"pid\" and \"startup-notification-id\" keys will be present.
      @begin{pre}
lambda (context info platform-data)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[context]{The @class{g:app-launch-context} object emitting the
          signal.}
        @entry[info]{The @class{g:app-info} object that was just launched.}
        @entry[platform-data]{A @type{g:variant} instance with additional
          platform specific data for this launch.}
      @end{table}
  @end{dictionary}
  @see-construtor{g:app-launch-context-new}
  @see-class{g:app-info}
  @see-class{gdk:app-launch-context}")

;;; ----------------------------------------------------------------------------
;;; g_app_info_create_from_commandline ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_create_from_commandline"
                %app-info-create-from-commandline) (gobject:object app-info)
  (commandline :string)
  (application :string)
  (flags app-info-create-flags)
  (err :pointer))

(defun app-info-create-from-commandline (commandline application flags)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[commandline]{a string with the the commandline to use}
  @argument[application]{a string the application name, or @code{nil} to use
    @arg{commandline}}
  @argument[flags]{a @symbol{g:app-info-create-flags} value with the flags that
    can specify details of the created @class{g:app-info} instance}
  @return{The new @class{g:app-info} instance.}
  @begin{short}
    Creates a new @class{g:app-info} instance from the given information.
  @end{short}
  Note that for the @arg{commandline} argument, the quoting rules of the Exec
  key of the freedesktop.org Desktop Entry Specification are applied. For
  example, if the @arg{commandline} argument contains percent-encoded URIs, the
  percent-character must be doubled in order to prevent it from being swallowed
  by Exec key unquoting. See the specification for exact quoting rules.
  @see-class{g:app-info}
  @see-symbol{g:app-info-create-flags}"
  (glib:with-g-error (err)
    (%app-info-create-from-commandline commandline application flags err)))

(export 'app-info-create-from-commandline)

;;; ----------------------------------------------------------------------------
;;; g_app_info_dup ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_dup" app-info-dup) (gobject:object app-info)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{A duplicate of @arg{info}.}
  @begin{short}
    Creates a duplicate of an application info.
  @end{short}
  @see-class{g:app-info}"
  (info (gobject:object app-info)))

(export 'app-info-dup)

;;; ----------------------------------------------------------------------------
;;; g_app_info_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_equal" app-info-equal) :boolean
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info1]{a first @class{g:app-info} instance}
  @argument[info2]{a second @class{g:app-info} instance}
  @return{@em{True} if @arg{info1} is equal to @arg{info2}, @em{false}
    otherwise.}
  @begin{short}
    Checks if two application infos are equal.
  @end{short}
  @see-class{g:app-info}"
  (info1 (gobject:object app-info))
  (info2 (gobject:object app-info)))

(export 'app-info-equal)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_id ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_id" app-info-id) :string
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{A string containing the ID of the application.}
  @begin{short}
    Gets the ID of an application.
  @end{short}
  An ID is a string that identifies the application. The exact format of the ID
  is platform dependent. For instance, on Unix this is the desktop file ID from
  the xdg menu specification.

  Note that the returned ID may be @code{nil}, depending on how @arg{info}
  has been constructed.
  @see-class{g:app-info}"
  (info gobject:object))

(export 'app-info-id)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_name" app-info-name) :string
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{A string with the the name of the application for @arg{info}.}
  @begin{short}
    Gets the installed name of the application.
  @end{short}
  @see-class{g:app-info}
  @see-function{g:app-info-display-name}"
  (info gobject:object))

(export 'app-info-name)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_display_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_display_name" app-info-display-name) :string
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{A string with the the display name of the application for @arg{info},
    or the name if no display name is available.}
  @begin{short}
    Gets the display name of the application.
  @end{short}
  The display name is often more descriptive to the user than the name itself.
  @see-class{g:app-info}
  @see-function{g:app-info-name}"
  (info gobject:object))

(export 'app-info-display-name)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_description ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_description" app-info-description) :string
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{A string containing a description of the application @arg{info},
    or @code{nil} if none.}
  @begin{short}
    Gets a human readable description of an installed application.
  @end{short}
  @see-class{g:app-info}"
  (info gobject:object))

(export 'app-info-description)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_executable ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_executable" app-info-executable) :string
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{A string containing the application binaries name of @arg{info}.}
  @begin{short}
    Gets the name of the executable for the installed application.
  @end{short}
  @see-class{g:app-info}"
  (info gobject:object))

(export 'app-info-executable)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_commandline ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_commandline" app-info-commandline) :string
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{A string containing the commandline of @arg{info}, or @code{nil}
    if this information is not available.}
  @begin{short}
    Gets the commandline with which the application will be started.
  @end{short}
  @see-class{g:app-info}"
  (info gobject:object))

(export 'app-info-commandline)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_icon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_icon" app-info-icon) (gobject:object icon)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{The default @class{g:icon} object for @arg{info} or @code{nil} if
    there is no default icon.}
  @begin{short}
    Gets the icon for the application.
  @end{short}
  @see-class{g:app-info}
  @see-class{g:icon}"
  (info gobject:object))

(export 'app-info-icon)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch" %app-info-launch) :boolean
  (info gobject:object)
  (files (glib:list-t gobject:object))
  (context (gobject:object app-launch-context))
  (err :pointer))

(defun app-info-launch (info files context)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @argument[files]{a list of @class{g:files} objects}
  @argument[context]{a @class{g:app-launch-context} instance or @code{nil}}
  @return{@em{True} on successful launch, @em{false} otherwise.}
  @begin{short}
    Launches the application.
  @end{short}
  Passes @arg{files} to the launched application as arguments, using the
  optional @arg{context} argument to get information about the details of the
  launcher, like what screen it is on.

  To launch the application without arguments pass a @code{nil} files list.

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
  @see-function{g:app-info-launch-uris}
  @see-function{g:app-launch-context-setenv}
  @see-function{g:app-launch-context-unsetenv}"
  (glib:with-g-error (err)
    (%app-info-launch info files context err)))

(export 'app-info-launch)

;;; ----------------------------------------------------------------------------
;;; g_app_info_supports_files ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_supports_files" app-info-supports-files) :boolean
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{info} supports files.}
  @begin{short}
    Checks if the application accepts files as arguments.
  @end{short}
  @see-class{g:app-info}"
  (info gobject:object))

(export 'app-info-supports-files)

;;; ----------------------------------------------------------------------------
;;; g_app_info_supports_uris ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_supports_uris" app-info-supports-uris) :boolean
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{info} supports URIs.}
  @begin{short}
    Checks if the application supports reading files and directories from URIs.
  @end{short}
  @see-class{g:app-info}"
  (info gobject:object))

(export 'app-info-supports-uris)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_uris" %app-info-launch-uris) :boolean
  (info gobject:object)
  (uris (glib:list-t :string))
  (context (gobject:object app-launch-context))
  (err :pointer))

(defun app-info-launch-uris (info uris context)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @argument[uris]{a list of strings with the containing URIs to launch}
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
  (glib:with-g-error (err)
    (%app-info-launch-uris info uris context err)))

(export 'app-info-launch-uris)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris_async ()
;;;
;;; void
;;; g_app_info_launch_uris_async (GAppInfo *appinfo,
;;;                               GList *uris,
;;;                               GAppLaunchContext *context,
;;;                               GCancellable *cancellable,
;;;                               GAsyncReadyCallback callback,
;;;                               gpointer user_data);
;;;
;;; Async version of g_app_info_launch_uris().
;;;
;;; The callback is invoked immediately after the application launch, but it
;;; waits for activation in case of D-Bus–activated applications and also
;;; provides extended error information for sandboxed applications, see notes
;;; for g_app_info_launch_default_for_uri_async().
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; uris :
;;;     a GList containing URIs to launch.
;;;
;;; context :
;;;     a GAppLaunchContext or NULL.
;;;
;;; cancellable :
;;;     a GCancellable.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is done.
;;;
;;; user_data :
;;;     data to pass to callback .
;;;
;;; Since 2.60
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris_finish ()
;;;
;;; gboolean
;;; g_app_info_launch_uris_finish (GAppInfo *appinfo,
;;;                                GAsyncResult *result,
;;;                                GError **error);
;;;
;;; Finishes a g_app_info_launch_uris_async() operation.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on successful launch, FALSE otherwise.
;;;
;;; Since 2.60
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_should_show ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_should_show" app-info-should-show) :boolean
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{info} should be shown, @em{false} otherwise.}
  @begin{short}
    Checks if the application info should be shown in menus that list available
    applications.
  @end{short}
  @see-class{g:app-info}"
  (info gobject:object))

(export 'app-info-should-show)

;;; ----------------------------------------------------------------------------
;;; g_app_info_can_delete ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_can_delete" app-info-can-delete) :boolean
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{info} can be deleted.}
  @begin{short}
    Obtains the information whether the application info can be deleted.
  @end{short}
  See the @fun{g:app-info-delete} function.
  @see-class{g:app-info}
  @see-function{g:app-info-delete}"
  (info gobject:object))

(export 'app-info-can-delete)

;;; ----------------------------------------------------------------------------
;;; g_app_info_delete ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_delete" app-info-delete) :boolean
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{@em{True} if @arg{info} has been deleted.}
  @begin{short}
    Tries to delete an application info.
  @end{short}

  On some platforms, there may be a difference between user-defined application
  infos which can be deleted, and system-wide ones which cannot. See the
  @fun{g:app-info-can-delete} function.
  @see-class{g:app-info}
  @see-function{g:app-info-can-delete}"
  (info gobject:object))

(export 'app-info-delete)

;;; ----------------------------------------------------------------------------
;;; g_app_info_reset_type_associations ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_reset_type_associations"
                app-info-reset-type-associations) :void
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[content-type]{a string with the content type}
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
  (content-type :string))

(export 'app-info-reset-type-associations)

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_default_for_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_set_as_default_for_type"
                %app-info-set-as-default-for-type) :boolean
  (info gobject:object)
  (content-type :string)
  (err :pointer))

(defun app-info-set-as-default-for-type (info content-type)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @argument[content-type]{a string with the content type}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Sets the application as the default handler for a given type.
  @end{short}
  @see-class{g:app-info}"
  (glib:with-g-error (err)
    (%app-info-set-as-default-for-type info content-type err)))

(export 'app-info-set-as-default-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_default_for_extension ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_set_as_default_for_extension"
                %app-info-set-as-default-for-extension) :boolean
  (info gobject:object)
  (extension :string)
  (err :pointer))

(defun app-info-set-as-default-for-extension (info extension)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @argument[extension]{a string containing the file extension, without the dot}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Sets the application as the default handler for the given file extension.
  @end{short}
  @see-class{g:app-info}"
  (glib:with-g-error (err)
    (%app-info-set-as-default-for-extension info extension err)))

(export 'app-info-set-as-default-for-extension)

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_last_used_for_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_set_as_last_used_for_type"
                %app-info-set-as-last-used-for-type) :boolean
  (info gobject:object)
  (content-type :string)
  (err :pointer))

(defun app-info-set-as-last-used-for-type (info content-type)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @argument[content-type]{a string with the content type}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Sets the application as the last used application for a given type.
  @end{short}
  This will make the application appear as first in the list returned by the
  @fun{g:app-info-get-recommended-for-type} function, regardless of the default
  application for that content type.
  @see-class{g:app-info}
  @see-function{g:app-info-get-recommended-for-type}"
  (glib:with-g-error (err)
    (%app-info-set-as-last-used-for-type info content-type err)))

(export 'app-info-set-as-last-used-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_add_supports_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_add_supports_type" %app-info-add-supports-type)
    :boolean
  (info gobject:object)
  (content-type :string)
  (err :pointer))

(defun app-info-add-supports-type (info content-type)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @argument[content-type]{a string with the content type}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Adds a content type to the application information to indicate the
    application is capable of opening files with the given content type.
  @end{short}
  @see-class{g:app-info}"
  (glib:with-g-error (err)
    (%app-info-add-supports-type info content-type err)))

(export 'app-info-add-supports-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_can_remove_supports_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_can_remove_supports_type"
                app-info-can-remove-supports-type) :boolean
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{@em{True} if it is possible to remove supported content types from a
    given @arg{info}, @em{false} if not.}
  @begin{short}
    Checks if a supported content type can be removed from an application.
  @end{short}
  @see-class{g:app-info}"
  (info gobject:object))

(export 'app-info-can-remove-supports-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_remove_supports_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_remove_supports_type" %app-info-remove-supports-type)
    :boolean
  (info gobject:object)
  (content-type :string)
  (err :pointer))

(defun app-info-remove-supports-type (info content-type)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @argument[content-type]{a string with the content type}
  @return{@em{True} on success, @em{false} on error.}
  @begin{short}
    Removes a supported type from an application, if possible.
  @end{short}
  @see-class{g:app-info}"
  (glib:with-g-error (err)
    (%app-info-remove-supports-type info content-type err)))

(export 'app-info-remove-supports-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_supported_types ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_supported_types" app-info-supported-types)
    glib:strv-t
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[info]{a @class{g:app-info} instance}
  @return{A list of strings with the content types.}
  @begin{short}
    Retrieves the list of content types that @arg{info} claims to support.
  @end{short}
  If this information is not provided by the environment, this function will
  return @code{nil}. This function does not take in consideration associations
  added with the @fun{g:app-info-add-supports-type} function, but only those
  exported directly by the application.
  @see-class{g:app-info}
  @see-function{g:app-info-add-supports-type}"
  (info gobject:object))

(export 'app-info-supported-types)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_all ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_all" app-info-all) (glib:list-t gobject:object)
 #+liber-documentation
 "@version{#2023-7-11}
  @return{A list of references to @class{g:app-info} instances.}
  @begin{short}
    Gets a list of all of the applications currently registered on this system.
  @end{short}

  For desktop files, this includes applications that have @code{NoDisplay=true}
  set or are excluded from display by means of @code{OnlyShowIn} or
  @code{NotShowIn}. See the @fun{g:app-info-should-show} function. The returned
  list does not include applications which have the @code{Hidden} key set.
  @see-class{g:app-info}
  @see-function{g:app-info-should-show}")

(export 'app-info-all)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_all_for_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_all_for_type" app-info-all-for-type)
    (glib:list-t gobject:object)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[content-type]{a string with the content type}
  @return{A list of @class{g:app-info} instances for given @arg{content-type}
    or @code{nil} on error.}
  @begin{short}
    Gets a list of all application infos for a given content type, including
    the recommended and fallback application infos.
  @end{short}
  See the @fun{g:app-info-recommended-for-type} and
  @fun{g:app-info-fallback-for-type} functions.
  @see-class{g:app-info}
  @see-function{g:app-info-recommended-for-type}
  @see-function{g:app-info-fallback-for-type}"
  (content-type :string))

(export 'app-info-all-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_default_for_type" app-info-default-for-type)
    (gobject:object app-info)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[content-type]{a string with the content type}
  @argument[must-support-uris]{if @em{true}, the application info is expected
    to support URIs}
  @return{The @class{g:app-info} instance for given @arg{content-type} or
    @code{nil} on error.}
  @begin{short}
    Gets the default application info for a given content type.
  @end{short}
  @see-class{g:app-info}"
  (content-type :string)
  (must-support-uris :boolean))

(export 'app-info-default-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_type_async                  Since 2.74
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_type_finish                 Since 2.74
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_uri_scheme ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_default_for_uri_scheme"
                app-info-default-for-uri-scheme) (gobject:object app-info)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[uri-scheme]{a string containing a URI scheme}
  @return{The @class{g:app-info} instance for given @arg{uri-scheme} or
    @code{nil} on error.}
  @begin{short}
    Gets the default application for handling URIs with the given URI scheme.
  @end{short}
  A URI scheme is the initial part of the URI, up to but not including the ':',
  e.g. \"http\", \"ftp\" or \"sip\".
  @see-class{g:app-info}"
  (uri-scheme :string))

(export 'app-info-default-for-uri-scheme)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_uri_scheme_async            Since 2.74
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_uri_scheme_finish           Since 2.74
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_fallback_for_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_fallback_for_type" app-info-fallback-for-type)
    (glib:list-t gobject:object)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[content-type]{a string with the content type}
  @return{A list of @class{g:app-info} instances for given @arg{content-type}
    or @code{nil} on error.}
  @begin{short}
    Gets a list of fallback application infos for a given content type, i.e.
    those applications which claim to support the given content type by MIME
    type subclassing and not directly.
  @end{short}
  @see-class{g:app-info}"
  (content-type :string))

(export 'app-info-fallback-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_recommended_for_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_recommended_for_type"
                app-info-recommended-for-type) (glib:list-t gobject:object)
 #+liber-documentation
 "@version{#2023-7-11}
  @argument[content-type]{a string with the content type}
  @return{A list of @class{g:app-info} instances for given @arg{content-type}
    or @code{nil} on error.}
  @begin{short}
    Gets a list of recommended application infos for a given content type, i.e.
    those applications which claim to support the given content type exactly,
    and not by MIME type subclassing.
  @end{short}
  Note that the first application of the list is the last used one, i.e. the
  last one for which the @fun{g:app-info-set-as-last-used-for-type} function
  has been called.
  @see-class{g:app-info}
  @see-function{g:app-info-set-as-last-used-for-type}"
  (content-type :string))

(export 'app-info-recommended-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_default_for_uri"
                %app-info-launch-default-for-uri) :boolean
  (uri :string)
  (context (gobject:object app-launch-context))
  (err :pointer))

(defun app-info-launch-default-for-uri (uri &optional context)
 #+liber-documentation
 "@version{2023-9-18}
  @argument[uri]{a string with the URI to show}
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
  (glib:with-ignore-g-error (err)
    (let ((context (if context context (cffi:null-pointer))))
      (%app-info-launch-default-for-uri uri context err))))

(export 'app-info-launch-default-for-uri)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri_async ()
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
 "@version{2023-9-18}
  @argument[uri]{a string with the URI to show}
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
                       (if context context (cffi:null-pointer))
                       (if cancellable cancellable (cffi:null-pointer))
                       (cffi:callback async-ready-callback)
                       ptr)))

(export 'app-info-launch-default-for-uri-async)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri_finish ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_default_for_uri_finish"
               %app-info-launch-default-for-uri-finish) :boolean
  (result (gobject:object async-result))
  (err :pointer))

(defun app-info-launch-default-for-uri-finish (result)
 #+liber-documentation
 "@version{2023-9-18}
  @argument[result]{a @class{g:async-result} object}
  @return{@em{True} if the launch was successful, @em{false} otherwise}
  @begin{short}
    Finishes an asynchronous launch-default-for-uri operation.
  @end{short}
  @see-class{g:app-launch-context}
  @see-class{g:async-result}
  @see-function{g:app-info-launch-default-for-uri-async}"
  (glib:with-ignore-g-error (err)
    (%app-info-launch-default-for-uri-finish result err)))

(export 'app-info-launch-default-for-uri-finish)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline app-launch-context-new))

(defun app-launch-context-new ()
 #+liber-documentation
 "@version{#2023-7-14}
  @return{A @class{g:app-launch-context} instance}
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
;;; g_app_launch_context_setenv ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_setenv" app-launch-context-setenv) :void
 #+liber-documentation
 "@version{#2023-7-14}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[variable]{a string with the enviroment variable to set}
  @argument[value]{a string with the value for to set the variabel to}
  @begin{short}
    Arranges for variable to be set to value in the child's environment when
    @arg{context} is used to launch an application.
  @end{short}
  @see-class{g:app-launch-context}"
  (context (gobject:object app-launch-context))
  (variable :string)
  (value :string))

(export 'app-launch-context-setenv)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_unsetenv ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_unsetenv" app-launch-context-unsetenv)
    :void
 #+liber-documentation
 "@version{#2023-7-14}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[variable]{a string with the enviroment variable to remove}
  @begin{short}
    Arranges for @arg{variable} to be unset in the child's environment when
    @arg{context} is used to launch an application.
  @end{short}
  @see-class{g:app-launch-context}"
  (context (gobject:object app-launch-context))
  (variable :string))

(export 'app-launch-context-unsetenv)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_environment ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_get_environment"
                app-launch-context-environment) glib:strv-t
 #+liber-documentation
 "@version{#2023-7-14}
  @argument[context]{a @class{g:app-launch-context} instance}
  @return{A list of strings with the child's enviroment.}
  @begin{short}
    Gets the complete environment variable list to be passed to the child
    process when @arg{context} is used to launch an application.
  @end{short}
  This is a list of strings, where each string has the form @code{KEY=VALUE}.
  @see-class{g:app-launch-context}"
  (context (gobject:object app-launch-context)))

(export 'app-launch-context-environment)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_display ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_get_display" app-launch-context-display)
    :string
 #+liber-documentation
 "@version{#2023-7-14}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[info]{a @class{g:app-info} instance}
  @argument[files]{a list of @class{g:file} objects}
  @return{A display string for the display.}
  @begin{short}
    Gets the display string for the context.
  @end{short}
  This is used to ensure new applications are started on the same display as
  the launching application, by setting the @code{DISPLAY} environment variable.
  @see-class{g:app-launch-context}
  @see-class{g:app-info}"
  (context (gobject:object app-launch-context))
  (info gobject:object)
  (files (glib:list-t gobject:object)))

(export 'app-launch-context-display)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_startup_notify_id ()
;;; ----------------------------------------------------------------------------

;; TODO: Replace FILES with a list of Lisp namestrings.

(cffi:defcfun ("g_app_launch_context_get_startup_notify_id"
                app-launch-context-startup-notify-id) :string
 #+liber-documentation
 "@version{#2023-7-14}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[info]{a @class{g:app-info} instance}
  @argument[files]{a list of @class{g:file} objects}
  @return{A string with a startup notification ID for the application, or
    @code{nil} if not supported.}
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
  (info gobject:object)
  (files (glib:list-t gobject:object)))

(export 'app-launch-context-startup-notify-id)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_launch_failed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_launch_failed"
                app-launch-context-launch-failed) :void
 #+liber-documentation
 "@version{#2023-7-14}
  @argument[context]{a @class{g:app-launch-context} instance}
  @argument[startup-notify-id]{a string with the startup notification ID}
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
