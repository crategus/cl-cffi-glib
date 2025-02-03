;;; ----------------------------------------------------------------------------
;;; gio.application-command-line.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2025 Dieter Kaiser
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
;;; GApplicationCommandLine
;;;
;;;     A command-line invocation of an application
;;;
;;; Types and Values
;;;
;;;     GApplicationCommandLine
;;;
;;; Accessors
;;;
;;;     g_application_command_line_get_is_remote
;;;
;;; Functions
;;;
;;;     g_application_command_line_get_arguments
;;;     g_application_command_line_get_cwd
;;;     g_application_command_line_get_environ
;;;     g_application_command_line_get_options_dict
;;;     g_application_command_line_get_stdin                not implemented
;;;     g_application_command_line_create_file_for_arg
;;;     g_application_command_line_getenv
;;;     g_application_command_line_get_platform_data
;;;     g_application_command_line_set_exit_status
;;;     g_application_command_line_get_exit_status
;;;     g_application_command_line_print                    not implemented
;;;     g_application_command_line_printerr                 not implemented
;;;
;;; Properties
;;;
;;;     arguments
;;;     is-remote
;;;     options
;;;     platform-data
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GApplicationCommandLine
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GApplicationCommandLine
;;; ----------------------------------------------------------------------------

;; Note: The ARGUMENTS, OPTIONS and PLATFORM-DATA properties are not
;; directly accessible from the Lisp side, but through the corresponding
;; functions. The accessors are not exported.

(gobject:define-gobject "GApplicationCommandLine" application-command-line
  (:superclass gobject:object
   :export t
   :interfaces ()
   :type-initializer "g_application_command_line_get_type")
  ((arguments
    %application-command-line-arguments
    "arguments" "GVariant" nil nil)
   (is-remote
    application-command-line-is-remote
    "is-remote" "gboolean" t nil)
   (options
    %application-command-line-options
    "options" "GVariant" nil nil)
   (platform-data
    %application-command-line-platform-data
    "platform-data" "GVariant" nil nil)))

#+liber-documentation
(setf (documentation 'application-command-line 'type)
 "@version{2025-2-3}
  @begin{short}
    The @class{g:application-command-line} class represents a command line
    invocation of an application.
  @end{short}
  It is created by the @class{g:application} instance and emitted in the
  @code{\"command-line\"} signal and virtual function.

  The class contains the list of arguments that the program was invoked with.
  It is also possible to query if the command line invocation was local, that
  is, the current process is running in direct response to the invocation, or
  remote, that is, some other process forwarded the command line to this
  process.

  The @class{g:application-command-line} instance can provide the command line
  arguments for use with the @type{g:option-context} command line parsing API,
  with the @fun{g:application-command-line-arguments} function.

  The exit status of the originally invoked process may be set and messages can
  be printed to @code{stdout} or @code{stderr} of that process. The life cycle
  of the originally invoked process is tied to the life cycle of this object,
  that is, the process exits when the last reference is dropped.

  The main use for the @class{g:application-command-line} instance, and the
  @code{\"command-line\"} signal, is 'Emacs server' like use cases. You can set
  the @code{EDITOR} environment variable to have, for example @code{GIT}, use
  your favourite editor to edit commit messages, and if you already have an
  instance of the editor running, the editing will happen in the running
  instance, instead of opening a new one. An important aspect of this use case
  is that the process that gets started by @code{GIT} does not return until the
  editing is done.
  @begin[Examples]{dictionary}
    Normally, the command line is completely handled in the
    @code{\"command-line\"} signal handler. The launching instance exits once
    the signal handler in the primary instance has returned, and the return
    value of the signal handler becomes the exit status of the launching
    instance.
    @begin{pre}
(defun application-cmdline (&rest argv)
  (let ((app (make-instance 'g:application
                            :application-id
                            \"com.crategus.application-cmdline\"
                            :flags :handles-command-line))
        (argv (or argv (uiop:command-line-arguments))))
    ;; Print info about the application
    (format t \"Start application~%\")
    (format t \"       argv : ~a~%\" argv)
    (format t \"    prgname : ~a~%\" (g:prgname))
    ;; Signal handler \"command-line\"
    (g:signal-connect app \"command-line\"
        (lambda (application cmdline)
          (declare (ignore application))
          (let ((args (g:application-command-line-arguments cmdline)))
            (format t \"Signal handler COMMAND-LINE~%\")
            (format t \"  arguments : ~a~%\" args)
            ;; Return the exit status
            0)))
    ;; Run the application
    (g:application-run app argv)))
    @end{pre}
    This is the output, when executing the example from the Lisp prompt:
    @begin{pre}
(gio-example:application-cmdline \"file1\" \"file2\")
=> Start application
        argv : (file1 file2)
     prgname : sbcl
   Signal handler COMMAND-LINE
         arguments : (file1 file2)
   0
    @end{pre}
    A stand-alone executable for the example has the following output:
    @begin{pre}
./application-cmdline file1 file2
=> Start application
          argv : (file1 file2)
       prgname : application-cmdline
   Signal handler COMMAND-LINE
     arguments : (file1 file2)
    @end{pre}
  @end{dictionary}
  @see-slot{g:application-command-line-is-remote}
  @see-class{g:application}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:application-command-line-arguments -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "arguments"
                                               'application-command-line) t)
 "The @code{arguments} property of type @symbol{g:variant}
  (Write / Construct Only) @br{}
  The command line that caused the @code{\"command-line\"} signal emission.
  @br{}
  Note: This property is not directly accessible from the Lisp side, but
  through the corresponding @fun{g:application-command-line-arguments}
  function.")

;;; --- g:application-command-line-is-remote -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-remote"
                                               'application-command-line) t)
 "The @code{is-remote} property of type @code{:boolean} (Read) @br{}
  @em{True} if this is a remote command line. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'application-command-line-is-remote)
      "Accessor"
      (documentation 'application-command-line-is-remote 'function)
 "@version{#2024-12-27}
  @argument[object]{a @class{g:application-command-line} instance}
  @return{@em{True} if the invocation was remote.}
  @begin{short}
    Determines if the command line represents a remote invocation.
  @end{short}
  @see-class{g:application-command-line}")

;;; --- g:application-command-line-options -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "options"
                                               'application-command-line) t)
 "The @code{options} property of type @symbol{g:variant}
  (Write / Construct Only) @br{}
  The options sent along with the command line. @br{}
  Note: This property is not directly accessible from the Lisp side, but
  through the corresponding @fun{g:application-command-line-options-dict}
  function.")

(unexport 'application-command-line-options)

;;; --- g:application-command-line-platform-data -------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "platform-data"
                                               'application-command-line) t)
 "The @code{platform-data} property of type @symbol{g:variant}
  (Write / Construct Only) @br{}
  Platform-specific data for the command line. @br{}
  Note: This property is not directly accessible from the Lisp side, but
  through the corresponding @fun{g:application-command-line-platform-data}
  function.")

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_arguments
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_application_command_line_get_arguments"
                %application-command-line-get-arguments) glib:strv-t
  (cmdline (gobject:object application-command-line))
  (argc (:pointer :int)))

(defun application-command-line-arguments (cmdline)
 #+liber-documentation
 "@version{2025-2-3}
  @argument[cmdline]{a @class{g:application-command-line} instance}
  @return{The list of strings containing the command line arguments.}
  @begin{short}
    Gets the list of arguments that was passed on the command line.
  @end{short}
  The strings in the list may contain non-UTF-8 data on UNIX, such as
  filenames or arguments given in the system locale, but are always in UTF-8
  on Windows.

  If you wish to use the return value with the @type{g:option-context}
  implementation, you must use the @fun{g:option-context-parse-strv} function.
  @see-class{g:application-command-line}
  @see-type{g:option-context}
  @see-function{g:option-context-parse-strv}"
  (cffi:with-foreign-object (argc :int)
    (%application-command-line-get-arguments cmdline argc)))

(export 'application-command-line-arguments)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_cwd
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_application_command_line_get_cwd"
                application-command-line-cwd) :string
 "@version{2025-2-3}
  @argument[cmdline]{a @class{g:application-command-line} instance}
  @return{The string with the current directory, or @code{nil}.}
  @begin{short}
    Gets the working directory of the command line invocation.
  @end{short}
  The string may contain non UTF-8 data.

  It is possible that the remote application did not send a working directory,
  so this may be @code{nil}.
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar cmd (make-instance 'g:application-command-line)) => CMD
(g:application-command-line-cwd cmd) => \"/home/dieter/Lisp/lisp-projects\"
    @end{pre}
  @end{dictionary}
  @see-class{g:application-command-line}"
  (cmdline (gobject:object application-command-line)))

(export 'application-command-line-cwd)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_environ
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_application_command_line_get_environ"
                application-command-line-environ)
    (glib:strv-t :free-from-foreign nil)
 "@version{2025-2-3}
  @argument[cmdline]{a @class{g:application-command-line} instance}
  @return{The list of strings with the environment strings, or @code{nil} if they
    were not sent.}
  @begin{short}
    Gets the contents of the @code{environ} variable of the command line
    invocation, as would be returned by the @code{g_get_environ()} function.
  @end{short}
  Each item in the list is of the form @code{NAME} = @code{VALUE}. The strings
  may contain non UTF-8 data.

  The remote application usually does not send an environment. Use the
  @code{:send-enviroment} flag to affect that. Even with this flag set it is
  possible that the environment is still not available, due to invocation
  messages from other applications.

  See the @fun{g:application-command-line-getenv} function if you are only
  interested in the value of a single environment variable.
  @see-class{g:application-command-line}
  @see-function{g:application-command-line-getenv}"
  (cmdline (gobject:object application-command-line)))

(export 'application-command-line-environ)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_options_dict
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_application_command_line_get_options_dict"
                application-command-line-options-dict)
    (glib:boxed glib:variant-dict)
 #+liber-documentation
 "@version{#2025-2-3}
  @argument[cmdline]{a @class{g:application-command-line} instance}
  @return{The @class{g:variant-dict} instance with the options.}
  @begin{short}
    Gets the options that were passed to the @class{g:application-command-line}
    instance.
  @end{short}
  If you did not override the @code{local_command_line()} virtual function then
  these are the same options that were parsed according to the options entries
  added to the application with the @fun{g:application-add-main-option-entries}
  function and possibly modified from your @code{\"handle-local-options\"}
  signal handler.

  If no options were sent then an empty dictionary is returned so that you
  do not need to check for @code{nil}.
  @see-class{g:application-command-line}
  @see-class{g:variant-dict}
  @see-function{g:application-add-main-option-entries}"
  (cmdline (gobject:object application-command-line)))

(export 'application-command-line-options-dict)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_stdin
;;;
;;; Gets the stdin of the invoking process.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_create_file_for_arg
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_application_command_line_create_file_for_arg"
                application-command-line-create-file-for-arg)
    (gobject:object file)
 #+liber-documentation
 "@version{#2025-2-3}
  @argument[cmdline]{a @class{g:application-command-line} instance}
  @argument[arg]{a string for an argument from @arg{cmdline}}
  @return{The new @class{g:file} object.}
  @begin{short}
    Creates a @class{g:file} object corresponding to a filename that was given
    as part of the invocation of the command line.
  @end{short}
  This differs from the @fun{g:file-new-for-commandline-arg} function in that
  it resolves relative pathnames using the current working directory of the
  invoking process rather than the local process.
  @see-class{g:application-command-line}
  @see-class{g:file}
  @see-function{g:file-new-for-commandline-arg}"
  (cmdline (gobject:object application-command-line))
  (arg :string))

(export 'application-command-line-create-file-for-arg)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_getenv
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_application_command_line_getenv"
                application-command-line-getenv) :string
 "@version{#2025-2-3}
  @argument[cmdline]{a @class{g:application-command-line} instance}
  @argument[name]{a string for the environment variable to get}
  @return{The string with the value of the variable, or @code{nil} if unset or
    unsent.}
  @begin{short}
    Gets the value of a particular environment variable of the command line
    invocation, as would be returned by the @code{g_getenv()} function.
  @end{short}
  The strings may contain non UTF-8 data.

  The remote application usually does not send an environment. Use the
  @code{:send-enviroment} flag to affect that. Even with this flag set it is
  possible that the environment is still not available, due to invocation
  messages from other applications.
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar cmd (make-instance 'g:application-command-line)) => CMD
(g:application-command-line-getenv cmd \"HOME\") => \"/home/dieter\"
(g:application-command-line-getenv cmd \"unkown\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:application-command-line}"
  (cmdline (gobject:object application-command-line))
  (name :string))

(export 'application-command-line-getenv)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_platform_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_application_command_line_get_platform_data"
                application-command-line-platform-data)
    (:pointer (:struct glib:variant))
 "@version{2025-2-3}
  @argument[cmdline]{a @class{g:application-command-line} instance}
  @return{The @symbol{g:variant} dictionary with the platform data, or a
    @code{cffi:null-pointer} value.}
  @begin{short}
    Gets the platform data associated with the invocation of @arg{cmdline}.
  @end{short}
  This is a @symbol{g:variant} dictionary containing information about the
  context in which the invocation occurred. It typically contains information
  like the current working directory and the startup notification ID.

  For local invocation, it will be a @code{cffi:null-pointer} value.
  @see-class{g:application-command-line}
  @see-symbol{g:variant}"
  (cmdline (gobject:object application-command-line)))

(export 'application-command-line-platform-data)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_exit_status
;;; g_application_command_line_set_exit_status
;;; ----------------------------------------------------------------------------

(defun (setf application-command-line-exit-status) (status cmdline)
  (cffi:foreign-funcall "g_application_command_line_set_exit_status"
                        (gobject:object application-command-line) cmdline
                        :int status
                        :void)
  status)

(cffi:defcfun ("g_application_command_line_get_exit_status"
                application-command-line-exit-status) :int
 #+liber-documentation
 "@version{#2025-2-3}
  @syntax{(application-command-line-exit-status cmdline) => status}
  @syntax{(setf (application-command-line-exit-status cmdline) status)}
  @argument[cmdline]{a @class{g:application-command-line} instance}
  @argument[status]{an integer with the exit status}
  @begin{short}
    Accessor of the exit status of a @class{g:application-command-line}
    instance.
  @end{short}
  The @fun{g:application-command-line-exit-status} function gets the exit status
  of @arg{cmdline}. The @setf{g:application-command-line-exit-status} function
  sets the exit status that will be used when the invoking process exits.

  The return value of the @code{\"command-line\"} signal is passed to this
  function when the handler returns. This is the usual way of setting the exit
  status.

  In the event that you want the remote invocation to continue running and
  want to decide on the exit status in the future, you can use this call. For
  the case of a remote invocation, the remote process will typically exit when
  the last reference is dropped on @arg{cmdline}. The exit status of the remote
  process will be equal to the last value that was set with this function.

  In the case that the command line invocation is local, the situation is
  slightly more complicated. If the command line invocation results in the
  main loop running, that is, because the use-count of the application increased
  to a non-zero value, then the application is considered to have been
  'successful' in a certain sense, and the exit status is always zero. If the
  application use count is zero, though, the exit status of the local
  @class{g:application-command-line} instance is used.
  @see-class{g:application-command-line}"
  (cmdline (gobject:object application-command-line)))

(export 'application-command-line-exit-status)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_print
;;;
;;; Formats a message and prints it using the stdout print handler in the
;;; invoking process.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_printerr
;;;
;;; Formats a message and prints it using the stderr print handler in the
;;; invoking process.
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.application-command-line.lisp --------------------------
