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
;;;
;;; Description
;;;
;;; GAppInfo and GAppLaunchContext are used for describing and launching
;;; applications installed on the system.
;;;
;;; As of GLib 2.20, URIs will always be converted to POSIX paths (using
;;; g_file_get_path()) when using g_app_info_launch() even if the application
;;; requested an URI and not a POSIX path. For example for an desktop-file based
;;; application with Exec key totem %U and a single URI, sftp://foo/file.avi,
;;; then /home/user/.gvfs/sftp on foo/file.avi will be passed. This will only
;;; work if a set of suitable GIO extensions (such as gvfs 2.26 compiled with
;;; FUSE support), is available and operational; if this is not the case, the
;;; URI will be passed unmodified to the application. Some URIs, such as
;;; mailto:, of course cannot be mapped to a POSIX path (in gvfs there's no
;;; FUSE mount for it); such URIs will be passed unmodified to the application.
;;;
;;; Specifically for gvfs 2.26 and later, the POSIX URI will be mapped back to
;;; the GIO URI in the GFile constructors (since gvfs implements the GVfs
;;; extension point). As such, if the application needs to examine the URI, it
;;; needs to use g_file_get_uri() or similar on GFile. In other words, an
;;; application cannot assume that the URI passed to e.g.
;;; g_file_new_for_commandline_arg() is equal to the result of g_file_get_uri().
;;; The following snippet illustrates this:
;;;
;;;   GFile *f;
;;;   char *uri;
;;;
;;;   file = g_file_new_for_commandline_arg (uri_from_commandline);
;;;
;;;   uri = g_file_get_uri (file);
;;;   strcmp (uri, uri_from_commandline) == 0; // FALSE
;;;   g_free (uri);
;;;
;;;   if (g_file_has_uri_scheme (file, "cdda"))
;;;     {
;;;       // do something special with uri
;;;     }
;;;   g_object_unref (file);
;;;
;;; This code will work when both cdda://sr0/Track 1.wav and
;;; /home/user/.gvfs/cdda on sr0/Track 1.wav is passed to the application. It
;;; should be noted that it's generally not safe for applications to rely on
;;; the format of a particular URIs. Different launcher applications (e.g. file
;;; managers) may have different ideas of what a given URI means.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; enum GAppInfoCreateFlags
;;;
;;; typedef enum {
;;;   /*< nick=none >*/
;;;   G_APP_INFO_CREATE_NONE                           = 0,
;;;   /*< nick=needs-terminal >*/
;;;   G_APP_INFO_CREATE_NEEDS_TERMINAL                 = (1 << 0),
;;;   /*< nick=supports-uris >*/
;;;   G_APP_INFO_CREATE_SUPPORTS_URIS                  = (1 << 1),
;;;   /*< nick=supports-startup-notification >*/
;;;   G_APP_INFO_CREATE_SUPPORTS_STARTUP_NOTIFICATION  = (1 << 2)
;;; } GAppInfoCreateFlags;
;;;
;;; Flags used when creating a GAppInfo.
;;;
;;; G_APP_INFO_CREATE_NONE
;;;     No flags.
;;;
;;; G_APP_INFO_CREATE_NEEDS_TERMINAL
;;;     Application opens in a terminal window.
;;;
;;; G_APP_INFO_CREATE_SUPPORTS_URIS
;;;     Application supports URI arguments.
;;;
;;; G_APP_INFO_CREATE_SUPPORTS_STARTUP_NOTIFICATION
;;;     Application supports startup notification. Since 2.26
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GAppInfoCreateFlags" app-info-create-flags
  (:export t
   :type-initializer "g_app_info_create_flags_get_type")
  (:none 0)
  (:needs-terminal #.(ash 1 0))
  (:supports-uris #.(ash 1 1))
  (:supports-startup-notification #.(ash 1 2)))

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
 "@version{2022-12-27}
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
        @entry[context]{The @sym{g:app-launch-context} object emitting the
          signal.}
        @entry[startup-notify-id]{A string with the startup notification ID
          for the failed launch.}
      @end{table}
   @subheading{The \"launch-started\" signal}
     The signal is emitted when a @class{g:app-info} instance is about to be
     launched. If non-@code{null} the @arg{platform-data} is a @type{g:variant}
     dictionary mapping strings to variants, i.e. @code{a{sv@}}, which contains
     additional, platform specific data about this launch. On UNIX, at least the
     startup-notification-id keys will be present.

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
        @entry[context]{The @sym{g:app-launch-context} object emitting the
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
        @entry[context]{The @sym{g:app-launch-context} object emitting the
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
;;;
;;; GAppInfo * g_app_info_create_from_commandline (const char *commandline,
;;;                                                const char *application_name,
;;;                                                GAppInfoCreateFlags flags,
;;;                                                GError **error);
;;;
;;; Creates a new GAppInfo from the given information.
;;;
;;; Note that for commandline, the quoting rules of the Exec key of the
;;; freedesktop.org Desktop Entry Specification are applied. For example, if
;;; the commandline contains percent-encoded URIs, the percent-character must
;;; be doubled in order to prevent it from being swallowed by Exec key
;;; unquoting. See the specification for exact quoting rules.
;;;
;;; commandline :
;;;     the commandline to use
;;;
;;; application_name :
;;;     the application name, or NULL to use commandline
;;;
;;; flags :
;;;     flags that can specify details of the created GAppInfo
;;;
;;; error :
;;;     a GError location to store the error occurring, NULL to ignore.
;;;
;;; Returns :
;;;     new GAppInfo for given command
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_create_from_commandline"
                %app-info-create-from-commandline) (gobject:object app-info)
  (commandline :string)
  (application :string)
  (flags app-info-create-flags)
  (err :pointer))

(defun app-info-create-from-commandline (commandline application flags)
  (glib:with-g-error (err)
    (%app-info-create-from-commandline commandline application flags err)))

(export 'app-info-create-from-commandline)

;;; ----------------------------------------------------------------------------
;;; g_app_info_dup ()
;;;
;;; GAppInfo * g_app_info_dup (GAppInfo *appinfo);
;;;
;;; Creates a duplicate of a GAppInfo.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     a duplicate of appinfo
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_dup" app-info-dup) (gobject:object app-info)
  (info (gobject:object app-info)))

(export 'app-info-dup)

;;; ----------------------------------------------------------------------------
;;; g_app_info_equal ()
;;;
;;; gboolean g_app_info_equal (GAppInfo *appinfo1, GAppInfo *appinfo2);
;;;
;;; Checks if two GAppInfos are equal.
;;;
;;; appinfo1 :
;;;     the first GAppInfo.
;;;
;;; appinfo2 :
;;;     the second GAppInfo.
;;;
;;; Returns :
;;;     TRUE if appinfo1 is equal to appinfo2. FALSE otherwise.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_equal" app-info-equal) :boolean
  (info1 (gobject:object app-info))
  (info2 (gobject:object app-info)))

(export 'app-info-equal)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_id ()
;;;
;;; const char * g_app_info_get_id (GAppInfo *appinfo);
;;;
;;; Gets the ID of an application. An id is a string that identifies the
;;; application. The exact format of the id is platform dependent. For instance,
;;; on Unix this is the desktop file id from the xdg menu specification.
;;;
;;; Note that the returned ID may be NULL, depending on how the appinfo has
;;; been constructed.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     a string containing the application's ID.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_id" app-info-id) :string
  (appinfo gobject:object))

(export 'app-info-id)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_name ()
;;;
;;; const char * g_app_info_get_name (GAppInfo *appinfo);
;;;
;;; Gets the installed name of the application.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     the name of the application for appinfo.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_name" app-info-name) :string
  (appinfo gobject:object))

(export 'app-info-name)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_display_name ()
;;;
;;; const char * g_app_info_get_display_name (GAppInfo *appinfo);
;;;
;;; Gets the display name of the application. The display name is often more
;;; descriptive to the user than the name itself.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     the display name of the application for appinfo, or the name if no
;;;     display name is available.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_display_name" app-info-display-name) :string
  (appinfo gobject:object))

(export 'app-info-display-name)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_description ()
;;;
;;; const char * g_app_info_get_description (GAppInfo *appinfo);
;;;
;;; Gets a human readable description of an installed application.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     a string containing a description of the application appinfo, or NULL
;;;     if none.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_description" app-info-description) :string
  (appinfo gobject:object))

(export 'app-info-description)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_executable ()
;;;
;;; const char * g_app_info_get_executable (GAppInfo *appinfo);
;;;
;;; Gets the executable's name for the installed application.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; Returns :
;;;     a string containing the appinfo's application binaries name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_executable" app-info-executable) :string
  (appinfo gobject:object))

(export 'app-info-executable)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_commandline ()
;;;
;;; const char * g_app_info_get_commandline (GAppInfo *appinfo);
;;;
;;; Gets the commandline with which the application will be started.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; Returns :
;;;     a string containing the appinfo's commandline, or NULL if this
;;;     information is not available
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_commandline" app-info-commandline) :string
  (appinfo gobject:object))

(export 'app-info-commandline)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_icon ()
;;;
;;; GIcon * g_app_info_get_icon (GAppInfo *appinfo);
;;;
;;; Gets the icon for the application.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     the default GIcon for appinfo or NULL if there is no default icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_icon" app-info-icon) (gobject:object icon)
  (appinfo gobject:object))

(export 'app-info-icon)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch ()
;;;
;;; gboolean g_app_info_launch (GAppInfo *appinfo,
;;;                             GList *files,
;;;                             GAppLaunchContext *launch_context,
;;;                             GError **error);
;;;
;;; Launches the application. Passes files to the launched application as
;;; arguments, using the optional launch_context to get information about the
;;; details of the launcher (like what screen it is on). On error, error will
;;; be set accordingly.
;;;
;;; To launch the application without arguments pass a NULL files list.
;;;
;;; Note that even if the launch is successful the application launched can
;;; fail to start if it runs into problems during startup. There is no way to
;;; detect this.
;;;
;;; Some URIs can be changed when passed through a GFile (for instance
;;; unsupported URIs with strange formats like mailto:), so if you have a
;;; textual URI you want to pass in as argument, consider using
;;; g_app_info_launch_uris() instead.
;;;
;;; The launched application inherits the environment of the launching process,
;;; but it can be modified with g_app_launch_context_setenv() and
;;; g_app_launch_context_unsetenv().
;;;
;;; On UNIX, this function sets the GIO_LAUNCHED_DESKTOP_FILE environment
;;; variable with the path of the launched desktop file and
;;; GIO_LAUNCHED_DESKTOP_FILE_PID to the process id of the launched process.
;;; This can be used to ignore GIO_LAUNCHED_DESKTOP_FILE, should it be inherited
;;; by further processes. The DISPLAY and DESKTOP_STARTUP_ID environment
;;; variables are also set, based on information provided in launch_context.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; files :
;;;     a GList of GFile objects
;;;
;;; launch_context :
;;;     a GAppLaunchContext or NULL
;;;
;;; error :
;;;     a GError
;;;
;;; Returns :
;;;     TRUE on successful launch, FALSE otherwise.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch" %app-info-launch) :boolean
  (info gobject:object)
  (files (glib:list-t gobject:object))
  (context (gobject:object app-launch-context))
  (err :pointer))

(defun app-info-launch (info files context)
  (glib:with-g-error (err)
    (%app-info-launch info files context err)))

(export 'app-info-launch)

;;; ----------------------------------------------------------------------------
;;; g_app_info_supports_files ()
;;;
;;; gboolean g_app_info_supports_files (GAppInfo *appinfo);
;;;
;;; Checks if the application accepts files as arguments.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     TRUE if the appinfo supports files.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_supports_files" app-info-supports-files) :boolean
  (info gobject:object))

(export 'app-info-supports-files)

;;; ----------------------------------------------------------------------------
;;; g_app_info_supports_uris ()
;;;
;;; gboolean g_app_info_supports_uris (GAppInfo *appinfo);
;;;
;;; Checks if the application supports reading files and directories from URIs.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     TRUE if the appinfo supports URIs.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_supports_uris" app-info-supports-uris) :boolean
  (info gobject:object))

(export 'app-info-supports-uris)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_uris ()
;;;
;;; gboolean g_app_info_launch_uris (GAppInfo *appinfo,
;;;                                  GList *uris,
;;;                                  GAppLaunchContext *launch_context,
;;;                                  GError **error);
;;;
;;; Launches the application. This passes the uris to the launched application
;;; as arguments, using the optional launch_context to get information about
;;; the details of the launcher (like what screen it is on). On error, error
;;; will be set accordingly.
;;;
;;; To launch the application without arguments pass a NULL uris list.
;;;
;;; Note that even if the launch is successful the application launched can
;;; fail to start if it runs into problems during startup. There is no way to
;;; detect this.
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; uris :
;;;     a GList containing URIs to launch
;;;
;;; launch_context :
;;;     a GAppLaunchContext or NULL
;;;
;;; error :
;;;     a GError
;;;
;;; Returns :
;;;     TRUE on successful launch, FALSE otherwise.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_launch_uris" %app-info-launch-uris) :boolean
  (info gobject:object)
  (uris (glib:list-t :string))
  (context (gobject:object app-launch-context))
  (err :pointer))

(defun app-info-launch-uris (info uris context)
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
;;;
;;; gboolean g_app_info_should_show (GAppInfo *appinfo);
;;;
;;; Checks if the application info should be shown in menus that list available
;;; applications.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     TRUE if the appinfo should be shown, FALSE otherwise.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_should_show" app-info-should-show) :boolean
  (info gobject:object))

(export 'app-info-should-show)

;;; ----------------------------------------------------------------------------
;;; g_app_info_can_delete ()
;;;
;;; gboolean g_app_info_can_delete (GAppInfo *appinfo);
;;;
;;; Obtains the information whether the GAppInfo can be deleted. See
;;; g_app_info_delete().
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; Returns :
;;;     TRUE if appinfo can be deleted
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_can_delete" app-info-can-delete) :boolean
  (info gobject:object))

(export 'app-info-can-delete)

;;; ----------------------------------------------------------------------------
;;; g_app_info_delete ()
;;;
;;; gboolean g_app_info_delete (GAppInfo *appinfo);
;;;
;;; Tries to delete a GAppInfo.
;;;
;;; On some platforms, there may be a difference between user-defined GAppInfos
;;; which can be deleted, and system-wide ones which cannot. See
;;; g_app_info_can_delete().
;;;
;;; Virtual: do_delete
;;;
;;; appinfo :
;;;     a GAppInfo
;;;
;;; Returns :
;;;     TRUE if appinfo has been deleted
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_delete" app-info-delete) :boolean
  (info gobject:object))

(export 'app-info-delete)

;;; ----------------------------------------------------------------------------
;;; g_app_info_reset_type_associations ()
;;;
;;; void g_app_info_reset_type_associations (const char *content_type);
;;;
;;; Removes all changes to the type associations done by
;;; g_app_info_set_as_default_for_type(),
;;; g_app_info_set_as_default_for_extension(), g_app_info_add_supports_type()
;;; or g_app_info_remove_supports_type().
;;;
;;; content_type :
;;;     a content type
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_reset_type_associations"
                app-info-reset-type-associations) :void
  (content-type :string))

(export 'app-info-reset-type-associations)

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_default_for_type ()
;;;
;;; gboolean g_app_info_set_as_default_for_type (GAppInfo *appinfo,
;;;                                              const char *content_type,
;;;                                              GError **error);
;;;
;;; Sets the application as the default handler for a given type.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; content_type :
;;;     the content type.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_set_as_default_for_type"
                %app-info-set-as-default-for-type) :boolean
  (info gobject:object)
  (content-type :string)
  (err :pointer))

(defun app-info-set-as-default-for-type (info content-type)
  (glib:with-g-error (err)
    (%app-info-set-as-default-for-type info content-type err)))

(export 'app-info-set-as-default-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_default_for_extension ()
;;;
;;; gboolean g_app_info_set_as_default_for_extension (GAppInfo *appinfo,
;;;                                                   const char *extension,
;;;                                                   GError **error);
;;;
;;; Sets the application as the default handler for the given file extension.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; extension :
;;;     a string containing the file extension (without the dot).
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_set_as_default_for_extension"
                %app-info-set-as-default-for-extension) :boolean
  (info gobject:object)
  (extension :string)
  (err :pointer))

(defun app-info-set-as-default-for-extension (info extension)
  (glib:with-g-error (err)
    (%app-info-set-as-default-for-extension info extension err)))

(export 'app-info-set-as-default-for-extension)

;;; ----------------------------------------------------------------------------
;;; g_app_info_set_as_last_used_for_type ()
;;;
;;; gboolean g_app_info_set_as_last_used_for_type (GAppInfo *appinfo,
;;;                                                const char *content_type,
;;;                                                GError **error);
;;;
;;; Sets the application as the last used application for a given type. This
;;; will make the application appear as first in the list returned by
;;; g_app_info_get_recommended_for_type(), regardless of the default
;;; application for that content type.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; content_type :
;;;     the content type.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_set_as_last_used_for_type"
                %app-info-set-as-last-used-for-type) :boolean
  (info gobject:object)
  (content-type :string)
  (err :pointer))

(defun app-info-set-as-last-used-for-type (info content-type)
  (glib:with-g-error (err)
    (%app-info-set-as-last-used-for-type info content-type err)))

(export 'app-info-set-last-used-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_add_supports_type ()
;;;
;;; gboolean g_app_info_add_supports_type (GAppInfo *appinfo,
;;;                                        const char *content_type,
;;;                                        GError **error);
;;;
;;; Adds a content type to the application information to indicate the
;;; application is capable of opening files with the given content type.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; content_type :
;;;     a string.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_add_supports_type" %app-info-add-supports-type)
    :boolean
  (info gobject:object)
  (content-type :string)
  (err :pointer))

(defun app-info-add-supports-type (info content-type)
  (glib:with-g-error (err)
    (%app-info-add-supports-type info content-type err)))

(export 'app-info-add-supports-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_can_remove_supports_type ()
;;;
;;; gboolean g_app_info_can_remove_supports_type (GAppInfo *appinfo);
;;;
;;; Checks if a supported content type can be removed from an application.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; Returns :
;;;     TRUE if it is possible to remove supported content types from a given
;;;     appinfo, FALSE if not.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_can_remove_supports_type"
                app-info-can-remove-supports-type) :boolean
  (info gobject:object))

(export 'app-info-can-remove-supports-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_remove_supports_type ()
;;;
;;; gboolean g_app_info_remove_supports_type (GAppInfo *appinfo,
;;;                                           const char *content_type,
;;;                                           GError **error);
;;;
;;; Removes a supported type from an application, if possible.
;;;
;;; appinfo :
;;;     a GAppInfo.
;;;
;;; content_type :
;;;     a string.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on success, FALSE on error.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_remove_supports_type" %app-info-remove-supports-type)
    :boolean
  (info gobject:object)
  (content-type :string)
  (err :pointer))

(defun app-info-remove-supports-type (info content-type)
  (glib:with-g-error (err)
    (%app-info-remove-supports-type info content-type err)))

(export 'app-info-remove-supports-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_supported_types ()
;;;
;;; const char ** g_app_info_get_supported_types (GAppInfo *appinfo);
;;;
;;; Retrieves the list of content types that app_info claims to support. If
;;; this information is not provided by the environment, this function will
;;; return NULL. This function does not take in consideration associations
;;; added with g_app_info_add_supports_type(), but only those exported directly
;;; by the application.
;;;
;;; appinfo :
;;;     a GAppInfo that can handle files
;;;
;;; Returns :
;;;     a list of content types.
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_supported_types" app-info-supported-types)
    glib:strv-t
  (info gobject:object))

(export 'app-info-supported-types)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_all ()
;;;
;;; GList * g_app_info_get_all (void);
;;;
;;; Gets a list of all of the applications currently registered on this system.
;;;
;;; For desktop files, this includes applications that have NoDisplay=true set
;;; or are excluded from display by means of OnlyShowIn or NotShowIn. See
;;; g_app_info_should_show(). The returned list does not include applications
;;; which have the Hidden key set.
;;;
;;; Returns :
;;;     a newly allocated GList of references to GAppInfos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_all" app-info-all) (glib:list-t gobject:object))

(export 'app-info-all)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_all_for_type ()
;;;
;;; GList * g_app_info_get_all_for_type (const char *content_type);
;;;
;;; Gets a list of all GAppInfos for a given content type, including the
;;; recommended and fallback GAppInfos. See
;;; g_app_info_get_recommended_for_type() and g_app_info_get_fallback_for_type().
;;;
;;; content_type :
;;;     the content type to find a GAppInfo for
;;;
;;; Returns :
;;;     GList of GAppInfos for given content_type or NULL on error
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_all_for_type" app-info-all-for-type)
    (glib:list-t gobject:object)
  (content-type :string))

(export 'app-info-all-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_default_for_type ()
;;;
;;; GAppInfo * g_app_info_get_default_for_type (const char *content_type,
;;;                                             gboolean must_support_uris);
;;;
;;; Gets the default GAppInfo for a given content type.
;;;
;;; content_type :
;;;     the content type to find a GAppInfo for
;;;
;;; must_support_uris :
;;;     if TRUE, the GAppInfo is expected to support URIs
;;;
;;; Returns :
;;;     GAppInfo for given content_type or NULL on error
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_default_for_type" app-info-default-for-type)
    (gobject:object app-info)
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
;;;
;;; GAppInfo * g_app_info_get_default_for_uri_scheme (const char *uri_scheme);
;;;
;;; Gets the default application for handling URIs with the given URI scheme. A
;;; URI scheme is the initial part of the URI, up to but not including the ':',
;;; e.g. "http", "ftp" or "sip".
;;;
;;; uri_scheme :
;;;     a string containing a URI scheme.
;;;
;;; Returns :
;;;     GAppInfo for given uri_scheme or NULL on error
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_default_for_uri_scheme"
                app-info-default-for-uri-scheme) (gobject:object app-info)
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
;;;
;;; GList * g_app_info_get_fallback_for_type (const gchar *content_type);
;;;
;;; Gets a list of fallback GAppInfos for a given content type, i.e. those
;;; applications which claim to support the given content type by MIME type
;;; subclassing and not directly.
;;;
;;; content_type :
;;;     the content type to find a GAppInfo for
;;;
;;; Returns :
;;;     GList of GAppInfos for given content_type or NULL on error
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_fallback_for_type" app-info-fallback-for-type)
    (glib:list-t gobject:object)
  (content-type :string))

(export 'app-info-fallback-for-type)

;;; ----------------------------------------------------------------------------
;;; g_app_info_get_recommended_for_type ()
;;;
;;; GList * g_app_info_get_recommended_for_type (const gchar *content_type);
;;;
;;; Gets a list of recommended GAppInfos for a given content type, i.e. those
;;; applications which claim to support the given content type exactly, and not
;;; by MIME type subclassing. Note that the first application of the list is the
;;; last used one, i.e. the last one for which
;;; g_app_info_set_as_last_used_for_type() has been called.
;;;
;;; content_type :
;;;     the content type to find a GAppInfo for
;;;
;;; Returns :
;;;     GList of GAppInfos for given content_type or NULL on error
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_info_get_recommended_for_type"
                app-info-recommended-for-type) (glib:list-t gobject:object)
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

(defun app-info-launch-default-for-uri (uri context)
 #+liber-documentation
 "@version{#2022-12-27}
  @argument[uri]{a string with the URI to show}
  @argument[context]{an optional @class{g:app-launch-context} object}
  @return{@em{True} on sucess, @em{false} on error.}
  @begin{short}
    Utility function that launches the default application registered to handle
    the specified URI.
  @end{short}
  Synchronous I/O is done on the URI to detect the type of the file if required.
  @see-class{g:app-info}
  @see-class{g:app-launch-context}"
  (glib:with-ignore-g-error (err)
    (%app-info-launch-default-for-uri uri context err)))

(export 'app-info-launch-default-for-uri)

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri_async ()
;;;
;;; void
;;; g_app_info_launch_default_for_uri_async (const char *uri,
;;;                                          GAppLaunchContext *context,
;;;                                          GCancellable *cancellable,
;;;                                          GAsyncReadyCallback callback,
;;;                                          gpointer user_data);
;;;
;;; Async version of g_app_info_launch_default_for_uri().
;;;
;;; This version is useful if you are interested in receiving error information
;;; in the case where the application is sandboxed and the portal may present
;;; an application chooser dialog to the user.
;;;
;;; This is also useful if you want to be sure that the D-Bus–activated
;;; applications are really started before termination and if you are
;;; interested in receiving error information from their activation.
;;;
;;; uri :
;;;     the uri to show
;;;
;;; context :
;;;     an optional GAppLaunchContext.
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
;;; Since 2.50
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_info_launch_default_for_uri_finish ()
;;;
;;; gboolean
;;; g_app_info_launch_default_for_uri_finish (GAsyncResult *result,
;;;                                           GError **error);
;;;
;;; Finishes an asynchronous launch-default-for-uri operation.
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     return location for an error, or NULL.
;;;
;;; Returns :
;;;     TRUE if the launch was successful, FALSE if error is set
;;;
;;; Since 2.50
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_new ()
;;;
;;; GAppLaunchContext * g_app_launch_context_new (void);
;;;
;;; Creates a new application launch context. This is not normally used,
;;; instead you instantiate a subclass of this, such as GdkAppLaunchContext.
;;;
;;; Returns :
;;;     a GAppLaunchContext.
;;; ----------------------------------------------------------------------------

(declaim (inline app-launch-context-new))

(defun app-launch-context-new ()
  (make-instance 'app-launch-context))

(export 'app-launch-context-new)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_setenv ()
;;;
;;; void g_app_launch_context_setenv (GAppLaunchContext *context,
;;;                                   const char *variable,
;;;                                   const char *value);
;;;
;;; Arranges for variable to be set to value in the child's environment when
;;; context is used to launch an application.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; variable :
;;;     the environment variable to set
;;;
;;; value :
;;;     the value for to set the variable to.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_setenv" app-launch-context-setenv) :void
  (context (gobject:object app-launch-context))
  (variable :string)
  (value :string))

(export 'app-launch-context-setenv)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_unsetenv ()
;;;
;;; void g_app_launch_context_unsetenv (GAppLaunchContext *context,
;;;                                     const char *variable);
;;;
;;; Arranges for variable to be unset in the child's environment when context
;;; is used to launch an application.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; variable :
;;;     the environment variable to remove
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_unsetenv" app-launch-context-unsetenv)
    :void
  (context (gobject:object app-launch-context))
  (variable :string))

(export 'app-launch-context-unsetenv)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_environment ()
;;;
;;; char ** g_app_launch_context_get_environment (GAppLaunchContext *context);
;;;
;;; Gets the complete environment variable list to be passed to the child
;;; process when context is used to launch an application. This is a
;;; NULL-terminated array of strings, where each string has the form KEY=VALUE.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; Returns :
;;;     the child's environment
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_get_environment"
                app-launch-context-environment) glib:strv-t
  (context (gobject:object app-launch-context)))

(export 'app-launch-context-environment)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_display ()
;;;
;;; char * g_app_launch_context_get_display (GAppLaunchContext *context,
;;;                                          GAppInfo *info,
;;;                                          GList *files);
;;;
;;; Gets the display string for the context. This is used to ensure new
;;; applications are started on the same display as the launching application,
;;; by setting the DISPLAY environment variable.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; info :
;;;     a GAppInfo
;;;
;;; files :
;;;     a GList of GFile objects
;;;
;;; Returns :
;;;     a display string for the display.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_get_display" app-launch-context-display)
    :string
  (context (gobject:object app-launch-context))
  (info gobject:object)
  (files (glib:list-t gobject:object)))

(export 'app-launch-context-display)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_get_startup_notify_id ()
;;;
;;; char *
;;; g_app_launch_context_get_startup_notify_id (GAppLaunchContext *context,
;;;                                             GAppInfo *info,
;;;                                             GList *files);
;;;
;;; Initiates startup notification for the application and returns the
;;; DESKTOP_STARTUP_ID for the launched operation, if supported.
;;;
;;; Startup notification IDs are defined in the FreeDesktop.Org Startup
;;; Notifications standard.
;;;
;;; context :
;;;     a GAppLaunchContext
;;;
;;; info :
;;;     a GAppInfo
;;;
;;; files :
;;;     a GList of of GFile objects
;;;
;;; Returns :
;;;     a startup notification ID for the application, or NULL if not supported.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_get_startup_notify_id"
                app-launch-context-startup-notify-id) :string
  (context (gobject:object app-launch-context))
  (info gobject:object)
  (files (glib:list-t gobject:object)))

(export 'app-launch-context-startup-notify-id)

;;; ----------------------------------------------------------------------------
;;; g_app_launch_context_launch_failed ()
;;;
;;; void
;;; g_app_launch_context_launch_failed (GAppLaunchContext *context,
;;;                                     const char *startup_notify_id);
;;;
;;; Called when an application has failed to launch, so that it can cancel the
;;; application startup notification started in
;;; g_app_launch_context_get_startup_notify_id().
;;;
;;; context :
;;;     a GAppLaunchContext.
;;;
;;; startup_notify_id :
;;;     the startup notification id that was returned by
;;;     g_app_launch_context_get_startup_notify_id().
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_app_launch_context_failed" app-launch-context-failed) :void
  (context (gobject:object app-launch-context))
  (startup-notify-id :string))

(export 'app-launch-context-failed)

;;; --- End of file gio.app-info.lisp ------------------------------------------
