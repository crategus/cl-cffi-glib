;;; ----------------------------------------------------------------------------
;;; glib.main-loop.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.76 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;
;;; The Main Event Loop
;;;
;;;     Manages all available sources of events
;;;
;;; Types and Values
;;;
;;;     G_PRIORITY_HIGH
;;;     G_PRIORITY_DEFAULT
;;;     G_PRIORITY_HIGH_IDLE
;;;     G_PRIORITY_DEFAULT_IDLE
;;;     G_PRIORITY_LOW
;;;     G_SOURCE_CONTINUE
;;;     G_SOURCE_REMOVE
;;;
;;;     GMainLoop
;;;
;;;     GMainContext
;;;     GMainContextPusher
;;;
;;;     GPid
;;;     G_PID_FORMAT
;;;     GPollFD
;;;     G_POLLFD_FORMAT
;;;
;;;     GSource
;;;     GSourceFuncs
;;;     GSourceCallbackFuncs
;;;
;;; Functions
;;;
;;;     g_main_loop_new
;;;     g_main_loop_ref
;;;     g_main_loop_unref
;;;     g_main_loop_run
;;;     g_main_loop_quit
;;;     g_main_loop_is_running
;;;     g_main_loop_get_context
;;;
;;;     g_main_context_new
;;;     g_main_context_ref
;;;     g_main_context_unref
;;;     g_main_context_default
;;;     g_main_context_iteration
;;;     g_main_context_pending
;;;     g_main_context_find_source_by_id
;;;     g_main_context_find_source_by_user_data
;;;     g_main_context_find_source_by_funcs_user_data
;;;     g_main_context_wakeup
;;;     g_main_context_acquire
;;;     g_main_context_release
;;;     g_main_context_is_owner
;;;     g_main_context_wait                                deprecated
;;;     g_main_context_prepare
;;;     g_main_context_query
;;;     g_main_context_check
;;;     g_main_context_dispatch
;;;     g_main_context_set_poll_func
;;;     g_main_context_get_poll_func
;;;
;;;     GPollFunc
;;;
;;;     g_main_context_add_poll
;;;     g_main_context_remove_poll
;;;     g_main_depth
;;;     g_main_current_source
;;;     g_main_set_poll_func                               deprecated
;;;     g_main_context_invoke
;;;     g_main_context_invoke_full
;;;
;;;     g_main_context_pusher_new
;;;     g_main_context_pusher_free
;;;
;;;     g_main_context_get_thread_default
;;;     g_main_context_ref_thread_default
;;;     g_main_context_push_thread_default
;;;     g_main_context_pop_thread_default
;;;
;;;     g_timeout_source_new
;;;     g_timeout_source_new_seconds
;;;     g_timeout_add
;;;     g_timeout_add_full
;;;     g_timeout_add_seconds
;;;     g_timeout_add_seconds_full
;;;
;;;     g_idle_source_new
;;;     g_idle_add
;;;     g_idle_add_full
;;;     g_idle_remove_by_data
;;;
;;;     GChildWatchFunc
;;;
;;;     g_child_watch_source_new
;;;     g_child_watch_add
;;;     g_child_watch_add_full
;;;
;;;     g_poll
;;;
;;;     GSourceDummyMarshal
;;;     GSourceDisposeFunc
;;;
;;;     g_source_new
;;;     g_source_ref
;;;     g_source_unref
;;;     g_source_set_funcs
;;;     g_source_set_dispose_function
;;;     g_source_attach
;;;     g_source_destroy
;;;     g_source_is_destroyed
;;;     g_source_set_priority
;;;     g_source_get_priority
;;;     g_source_set_can_recurse
;;;     g_source_get_can_recurse
;;;     g_source_get_id
;;;     g_source_get_name
;;;     g_source_set_name
;;;     g_source_set_name_by_id
;;;     g_source_get_context
;;;     g_source_set_callback
;;;
;;;     GSourceFunc
;;;     G_SOURCE_FUNC
;;;     g_source_set_callback_indirect
;;;
;;;     g_source_set_ready_time
;;;     g_source_get_ready_time
;;;     g_source_add_unix_fd
;;;     g_source_remove_unix_fd
;;;     g_source_modify_unix_fd
;;;     g_source_query_unix_fd
;;;
;;;     g_source_add_poll
;;;     g_source_remove_poll
;;;     g_source_add_child_source
;;;     g_source_remove_child_source
;;;     g_source_get_time
;;;     g_source_get_current_time                          deprecated
;;;     g_source_remove
;;;     g_source_remove_by_funcs_user_data
;;;     g_source_remove_by_user_data
;;;
;;;     GClearHandleFunc
;;;     g_clear_handle_id
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_HIGH
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-high+ -100
 #+liber-documentation
 "@version{2022-11-21}
  @variable-value{-100}
  @begin{short}
    Use this for high priority event sources.
  @end{short}
  It is not used within GLib or GTK.
  @see-variable{+g-priority-default+}
  @see-variable{+g-priority-low+}")

#+liber-documentation
(setf (liber:alias-for-variable '+g-priority-high+) "Constant")

(export '+g-priority-high+)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_DEFAULT
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-default+ 0
 #+liber-documentation
 "@version{2022-11-21}
  @variable-value{0}
  @begin{short}
    Use this for default priority event sources.
  @end{short}
  In GLib this priority is used when adding timeout functions with the
  @fun{g:timeout-add} function. In GDK this priority is used for events from
  the X server.
  @see-variable{+g-priority-high+}
  @see-variable{+g-priority-low+}
  @see-function{g:timeout-add}")

#+liber-documentation
(setf (liber:alias-for-variable '+g-priority-default+) "Constant")

(export '+g-priority-default+)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_HIGH_IDLE
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-high-idle+ 100
 #+liber-documentation
 "@version{2022-11-21}
  @variable-value{100}
  @begin{short}
    Use this for high priority idle functions.
  @end{short}
  GTK uses the @sym{+g-priority-high-idle+} + 10 value for resizing operations,
  and the @sym{+g-priority-high-idle+} + 20 value for redrawing operations. This
  is done to ensure that any pending resizes are processed before any pending
  redraws, so that widgets are not redrawn twice unnecessarily.
  @see-variable{+g-priority-default-idle+}")

#+liber-documentation
(setf (liber:alias-for-variable '+g-priority-high-idle+) "Constant")

(export '+g-priority-high-idle+)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_DEFAULT_IDLE
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-default-idle+ 200
 #+liber-documentation
 "@version{2022-11-21}
  @variable-value{200}
  @begin{short}
    Use this for default priority idle functions.
  @end{short}
  In GLib this priority is used when adding idle functions with the
  @fun{g:idle-add} function.
  @see-variable{+g-priority-high-idle+}
  @see-function{g:idle-add}")

#+liber-documentation
(setf (liber:alias-for-variable '+g-priority-default-idle+) "Constant")

(export '+g-priority-default-idle+)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_LOW
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-low+ 300
 #+liber-documentation
 "@version{2022-11-21}
  @variable-value{300}
  @begin{short}
    Use this for very low priority background tasks.
  @end{short}
  It is not used within GLib or GTK.
  @see-variable{+g-priority-default+}
  @see-variable{+g-priority-high+}")

#+liber-documentation
(setf (liber:alias-for-variable '+g-priority-low+) "Constant")

(export '+g-priority-low+)

;;; ----------------------------------------------------------------------------
;;; G_SOURCE_CONTINUE
;;; ----------------------------------------------------------------------------

(defconstant +g-source-continue+ t
 #+liber-documentation
 "@version{2022-11-21}
  @variable-value{@em{true}}
  @begin{short}
    Use this constant as the return value of a @symbol{g:source-func} callback
    function to leave the @type{g:source} instance in the main loop.
  @end{short}
  @see-type{g:source}
  @see-symbol{g:source-func}
  @see-variable{+g-source-remove+}")

#+liber-documentation
(setf (liber:alias-for-variable '+g-source-continue+) "Constant")

(export '+g-source-continue+)

;;; ----------------------------------------------------------------------------
;;; G_SOURCE_REMOVE
;;; ----------------------------------------------------------------------------

(defconstant +g-source-remove+ nil
 #+liber-documentation
 "@version{2022-11-21}
  @variable-value{@em{false}}
  @begin{short}
    Use this constant as the return value of a @symbol{g:source-func} callback
    function to remove the @type{g:source} instance from the main loop.
  @end{short}
  @see-type{g:source}
  @see-symbol{g:source-func}
  @see-variable{+g-source-continue+}")

#+liber-documentation
(setf (liber:alias-for-variable '+g-source-remove+) "Constant")

(export '+g-source-remove+)

;;; ----------------------------------------------------------------------------
;;; GMainLoop
;;; ----------------------------------------------------------------------------

(cffi:defcstruct main-loop)

#+liber-documentation
(setf (liber:alias-for-type 'main-loop)
      "CStruct"
      (documentation 'main-loop 'type)
 "@version{2022-11-21}
  @begin{short}
    The main event loop manages all the available sources of events for GLib
    and GTK applications. These events can come from any number of different
    types of sources such as file descriptors (plain files, pipes or sockets)
    and timeouts.
  @end{short}
  New types of event sources can also be added using the @fun{g:source-attach}
  function.

  To allow multiple independent sets of sources to be handled in different
  threads, each source is associated with a @type{g:main-context} instance. A
  @type{g:main-context} instance can only be running in a single thread, but
  sources can be added to it and removed from it from other threads. All
  functions which operate on a @type{g:main-context} instance or a built-in
  @type{g:source} instance are thread-safe.

  Each event source is assigned a priority. The @var{+g-priority-default+}
  default priority, is 0. Values less than 0 denote higher priorities.
  Values greater than 0 denote lower priorities. Events from high priority
  sources are always processed before events from lower priority sources.

  Idle functions can also be added, and assigned a priority. These will be run
  whenever no events with a higher priority are ready to be processed.

  The @sym{g:main-loop} data type represents a main event loop. A
  @sym{g:main-loop} instance is created with the @fun{g:main-loop-new} function.
  After adding the initial event sources, the @fun{g:main-loop-run} function is
  called. This continuously checks for new events from each of the event sources
  and dispatches them. Finally, the processing of an event from one of the
  sources leads to a call to the @fun{g:main-loop-quit} funcion to exit the
  main loop, and the @fun{g:main-loop-run} function returns.

  It is possible to create new instances of @sym{g:main-loop} instances
  recursively. This is often used in GTK applications when showing modal
  dialog boxes. Note that event sources are associated with a particular
  @type{g:main-context} instance, and will be checked and dispatched for all
  main loops associated with that @type{g:main-context} instance.
  @see-constructor{g:main-loop-new}
  @see-type{g:main-context}
  @see-type{g:source}")

(export 'main-loop)

;;; ----------------------------------------------------------------------------
;;; GMainContext
;;; ----------------------------------------------------------------------------

(cffi:defcstruct main-context)

#+liber-documentation
(setf (liber:alias-for-type 'main-context)
      "CStruct"
      (documentation 'main-context 'type)
 "@version{2022-11-21}
  @begin{short}
    The @sym{g:main-context} structure is an opaque data type representing a
    set of sources to be handled in a main loop.
  @end{short}
  @see-constructor{g:main-context-new}
  @see-type{g:main-loop}")

(export 'main-context)

;;; ----------------------------------------------------------------------------
;;; GMainContextPusher
;;;
;;; typedef void GMainContextPusher GLIB_AVAILABLE_TYPE_IN_2_64;
;;;
;;; Opaque type. See g_main_context_pusher_new() for details.
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GPid
;;;
;;; typedef int GPid;
;;;
;;; A type which is used to hold a process identification.
;;;
;;; On UNIX, processes are identified by a process id (an integer), while
;;; Windows uses process handles (which are pointers).
;;;
;;; GPid is used in GLib only for descendant processes spawned with the g_spawn
;;; functions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PID_FORMAT
;;;
;;; #define G_PID_FORMAT "i"
;;;
;;; A format specifier that can be used in printf()-style format strings when
;;; printing a GPid.
;;;
;;; Since 2.50
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GPollFD                                         not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct poll-fd
  (fd :int) ; TODO: #if defined (G_OS_WIN32) && GLIB_SIZEOF_VOID_P == 8
  (events :ushort)
  (revent :ushort))

#+liber-documentation
(setf (liber:alias-for-type 'poll-fd)
      "CStruct"
      (documentation 'poll-fd 'type)
 "@version{#2021-4-2}
  @begin{short}
    Represents a file descriptor, which events to poll for, and which events
    occurred.
  @end{short}
  @begin{pre}
(cffi:defcstruct poll-fd
  (fd :int)
  (events :ushort)
  (revent :ushort))
  @end{pre}
  @begin[code]{table}
    @entry[fd]{The file descriptor to poll (or a @code{HANDLE} on Win32).}
    @entry[events]{A bitwise combination from @code{GIOCondition},
      specifying which events should be polled for. Typically for reading from
      a file descriptor you would use @code{G_IO_IN | G_IO_HUP | G_IO_ERR}, and
      for writing you would use @code{G_IO_OUT | G_IO_ERR}.}
    @entry[revents]{A bitwise combination of flags from @code{GIOCondition},
      returned from the @code{poll()} function to indicate which events
      occurred.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; G_POLLFD_FORMAT
;;;
;;; #define G_POLLFD_FORMAT "%d"
;;;
;;; A format specifier that can be used in printf()-style format strings when
;;; printing the fd member of a GPollFD.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GSource
;;; ----------------------------------------------------------------------------

(cffi:defcstruct source
  ;; Private fields of the C structure
  (callback-data :pointer)
  (callback-funcs :pointer)
  (source-funcs :pointer)
  (ref-count :uint)
  (context :pointer)
  (priority :int)
  (flags :uint)
  (source-id :uint)
  (poll-fds :pointer)
  (prev :pointer)
  (next :pointer)
  (name :string)
  (priv :pointer))

#+liber-documentation
(setf (liber:alias-for-type 'source)
      "CStruct"
      (documentation 'source 'type)
 "@version{2022-11-21}
  @begin{short}
    The @sym{g:source} structure is an opaque data type representing an event
    source.
  @end{short}
  @see-type{g:main-loop}")

(export 'source)

;;; ----------------------------------------------------------------------------
;;; struct GSourceFuncs                                    not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct source-funcs
  (prepare :pointer)           ; lambda (source timeout)
  (check :pointer)             ; lambda (source)
  (dispatch :pointer)          ; lambda (source callback user-data)
  (finalize :pointer)          ; lambda (source) Can be NULL
  ;; Private data of the C structure
  (closure-callback :pointer)  ; no documentation
  (closure-marshal :pointer))  ; no documentation

#+liber-documentation
(setf (liber:alias-for-type 'source-funcs)
      "CStruct"
      (documentation 'source-funcs 'type)
 "@version{#2021-4-2}
  @begin{short}
    The @sym{source-funcs} structure contains a table of functions used to
    handle event sources in a generic manner.
  @end{short}

  For idle sources, the @code{prepare} and @code{check} functions always return
  @em{true} to indicate that the source is always ready to be processed. The
  @code{prepare} function also returns a timeout value of 0 to ensure that the
  @code{poll()} call does not block since that would be time wasted which could
  have been spent running the idle function.

  For timeout sources, the @code{prepare} and @code{check} functions both return
  @em{true} if the timeout interval has expired. The @code{prepare} function
  also returns a timeout value to ensure that the @code{poll()} call does not
  block too long and miss the next timeout.

  For file descriptor sources, the @code{prepare} function typically returns
  @code{nil}, since it must wait until @code{poll()} has been called before it
  knows whether any events need to be processed. It sets the returned timeout
  to -1 to indicate that it does not mind how long the @code{poll()} call
  blocks. In the @code{check} function, it tests the results of the
  @code{poll()} call to see if the required condition has been met, and returns
  @em{true} if so.
  @begin{pre}
(cffi:defcstruct source-funcs
  (prepare :pointer)
  (check :pointer)
  (dispatch :pointer)
  (finalize :pointer)
  (closure-callback :pointer)
  (closure-marshal :pointer))
  @end{pre}
  @begin[code]{table}
    @begin[prepare (source timeout)]{entry}
      Called before all the file descriptors are polled. If the source can
      determine that it is ready here, without waiting for the results of the
      @code{poll()} call, it should return @em{true}. It can also return a
      @code{timeout} value which should be the maximum timeout (in milliseconds)
      which should be passed to the @code{poll()} call. The actual timeout used
      will be -1 if all sources returned -1, or it will be the minimum of all
      the @code{timeout} values returned which were >= 0.
    @end{entry}
    @begin[check (source)]{entry}
      Called after all the file descriptors are polled. The source should
      return @em{true} if it is ready to be dispatched. Note that some time may
      have passed since the previous @code{prepare} function was called, so the
      source should be checked again here.
    @end{entry}
    @begin[dispatch (source callback user-data]{entry}
      Called to dispatch the event source, after it has returned @em{true} in
      either its @code{prepare} or its @code{check} function. The
      @code{dispatch} function is passed in a callback function and data. The
      callback function may be @code{NULL} if the source was never connected to
      a callback using @fun{source-set-callback}. The @code{dispatch} function
      should call the callback function with @arg{user-data} and whatever
      additional parameters are needed for this type of event source.
    @end{entry}
    @begin[finalize (source)]{entry}
      Called when the source is finalized.
    @end{entry}
  @end{table}
  @see-type{source}
  @see-function{source-set-callback}")

;;; ----------------------------------------------------------------------------
;;; struct GSourceCallbackFuncs                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct source-callback-funcs
  (ref :pointer)
  (unref :pointer)
  (get :pointer))

#+liber-documentation
(setf (documentation 'source-callback-funcs 'type)
 "@version{#2021-4-2}
  @begin{short}
    The @sym{source-callback-funcs} structure contains functions for managing
    callback objects.
  @end{short}
  @begin{pre}
(cffi:defcstruct source-callback-funcs
  (ref :pointer)
  (unref :pointer)
  (get :pointer))
  @end{pre}
  @begin[code]{table}
    @entry[ref]{Called when a reference is added to the callback object.}
    @entry[unref]{Called when a reference to the callback object is dropped.}
    @entry[get]{Called to extract the callback function and data from the
        callback object.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_main_loop_new ()
;;; ----------------------------------------------------------------------------

(defun main-loop-new (context is-running)
 #+liber-documentation
 "@version{2023-1-5}
  @argument[context]{a @type{g:main-context} instance, if @code{nil}, the
    default context will be used}
  @argument[is-running]{set to @em{true} to indicate that the main loop is
    running}
  @return{A new @type{g:main-loop} instance.}
  @short{Creates a new main loop.}
  @begin[Example]{dictionary}
    Create a running main loop with a default context and quit the main loop.
    @begin{pre}
(setq mainloop (g:main-loop-new nil t))
=> #.(SB-SYS:INT-SAP #X0808DF88)
(g:main-loop-is-running mainloop) => T
(g:main-loop-quit mainloop)
(g:main-loop-is-running mainloop) => NIL
    @end{pre}
  @end{dictionary}
  @see-type{g:main-loop}
  @see-type{g:main-context}
  @see-function{g:main-loop-run}"
  (let ((context (if context context (cffi:null-pointer))))
    (cffi:foreign-funcall "g_main_loop_new"
                          (:pointer (:struct main-context)) context
                          :boolean is-running
                          (:pointer (:struct main-loop)))))

(export 'main-loop-new)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_ref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_loop_ref" main-loop-ref) (:pointer (:struct main-loop))
 #+liber-documentation
 "@version{2022-11-21}
  @argument[loop]{a @type{g:main-loop} instance}
  @return{The @arg{loop} argument.}
  @short{Increases the reference count on a main loop by one.}
  @see-type{g:main-loop}
  @see-function{g:main-loop-unref}"
  (loop (:pointer (:struct main-loop))))

(export 'main-loop-ref)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_unref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_loop_unref" main-loop-unref) :void
 #+liber-documentation
 "@version{2022-11-21}
  @argument[loop]{a @type{g:main-loop} instance}
  @begin{short}
    Decreases the reference count on a main loop by one.
  @end{short}
  If the result is zero, free @arg{loop} and free all associated memory.
  @see-type{g:main-loop}
  @see-function{g:main-loop-ref}"
  (loop (:pointer (:struct main-loop))))

(export 'main-loop-unref)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_run ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_loop_run" main-loop-run) :void
 #+liber-documentation
 "@version{2022-11-21}
  @argument[loop]{a @type{g:main-loop} instance}
  @begin{short}
    Runs a main loop until the @fun{g:main-loop-quit} function is called on the
    main loop.
  @end{short}
  If this is called for the thread of the context of the main loop, it will
  process events from the main loop, otherwise it will simply wait.
  @see-type{g:main-loop}
  @see-function{g:main-loop-quit}"
  (loop (:pointer (:struct main-loop))))

(export 'main-loop-run)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_quit ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_loop_quit" main-loop-quit) :void
 #+liber-documentation
 "@version{2022-11-21}
  @argument[loop]{a @type{g:main-loop} instance}
  @begin{short}
    Stops a main loop from running.
  @end{short}
  Any calls to the @fun{g:main-loop-run} function for the main loop will return.
  Note that sources that have already been dispatched when the
  @sym{g:main-loop-quit} function is called will still be executed.
  @see-type{g:main-loop}
  @see-function{g:main-loop-run}"
  (loop (:pointer (:struct main-loop))))

(export 'main-loop-quit)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_is_running ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_loop_is_running" main-loop-is-running) :boolean
 #+liber-documentation
 "@version{2022-11-21}
  @argument[loop]{a @type{g:main-loop} instance}
  @return{@em{True} if the main loop is currently being run.}
  @begin{short}
    Checks to see if the main loop is currently being run via the
    @fun{g:main-loop-run} function.
  @end{short}
  @see-type{g:main-loop}
  @see-function{g:main-loop-run}"
  (loop (:pointer (:struct main-loop))))

(export 'main-loop-is-running)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_get_context () -> main-loop-context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_loop_get_context" main-loop-context)
    (:pointer (:struct main-context))
 #+liber-documentation
 "@version{2022-11-21}
  @argument[loop]{a @type{g:main-loop} instance}
  @return{The @type{g:main-context} instance of @arg{loop}.}
  @short{Returns the context of the main loop.}
  @see-type{g:main-loop}
  @see-type{g:main-context}"
  (loop (:pointer (:struct main-loop))))

(export 'main-loop-context)

;;; ----------------------------------------------------------------------------
;;; g_main_context_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_new" main-context-new)
    (:pointer (:struct main-context))
 #+liber-documentation
 "@version{2022-11-21}
  @return{The new @type{g:main-context} instance.}
  @short{Creates a new context.}
  @see-type{g:main-context}")

(export 'main-context-new)

;;; ----------------------------------------------------------------------------
;;; g_main_context_ref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_ref" main-context-ref)
    (:pointer (:struct main-context))
 #+liber-documentation
 "@version{2022-11-21}
  @argument[context]{a @type{g:main-context} instance}
  @return{The @arg{context} argument.}
  @short{Increases the reference count on a context by one.}
  @see-type{g:main-context}
  @see-function{g:main-context-unref}"
  (context (:pointer (:struct main-context))))

(export 'main-context-ref)

;;; ----------------------------------------------------------------------------
;;; g_main_context_unref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_unref" main-context-unref) :void
 #+liber-documentation
 "@version{2022-11-21}
  @argument[context]{a @type{g:main-context} instance}
  @begin{short}
    Decreases the reference count on a context by one.
  @end{short}
  If the result is zero, free @arg{context} and free all associated memory.
  @see-type{g:main-context}
  @see-function{g:main-context-ref}"
  (context (:pointer (:struct main-context))))

(export 'main-context-unref)

;;; ----------------------------------------------------------------------------
;;; g_main_context_default ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_default" main-context-default)
    (:pointer (:struct main-context))
 #+liber-documentation
 "@version{2022-11-21}
  @return{A @type{g:main-context} instance with the global default context.}
  @begin{short}
    Returns the global default context.
  @end{short}
  This is the context used for main loop functions when a main loop is not
  explicitly specified, and corresponds to the \"main\" main loop.
  @see-type{g:main-context}")

(export 'main-context-default)

;;; ----------------------------------------------------------------------------
;;; g_main_context_iteration ()
;;; ----------------------------------------------------------------------------

(defun main-context-iteration (context block)
 #+liber-documentation
 "@version{#2023-1-5}
  @argument[context]{a @type{g:main-context} instance, if @code{nil}, the
    default context will be used}
  @argument[block]{a boolean whether the call may block}
  @return{@em{True} if events were dispatched.}
  @begin{short}
    Runs a single iteration for the given main loop.
  @end{short}
  This involves checking to see if any event sources are ready to be processed,
  then if no events sources are ready and the @arg{block} argument is @em{true},
  waiting for a source to become ready, then dispatching the highest priority
  events sources that are ready. Otherwise, if the @arg{block} argument is
  @em{false} sources are not waited to become ready, only those highest priority
  events sources will be dispatched (if any), that are ready at this given
  moment without further waiting.

  Note that even when the @arg{block} argument is @em{true}, it is still
  possible for the @sym{g:main-context-iteration} function to return @em{false},
  since the wait may be interrupted for other reasons than an event source
  becoming ready.
  @see-type{g:main-context}"
  (let ((context (if context context (cffi:null-pointer))))
    (cffi:foreign-funcall "g_main_context_iteration"
                          (:pointer (:struct main-context)) context
                          :boolean block
                          :boolean)))

(export 'main-context-iteration)

;;; ----------------------------------------------------------------------------
;;; g_main_context_pending ()
;;; ----------------------------------------------------------------------------

(defun main-context-pending (context)
 #+liber-documentation
 "@version{#2023-1-5}
  @argument[context]{a @type{g:main-context} instance, if @code{nil}, the
    default context will be used}
  @return{@em{True} if events are pending.}
  @short{Checks if any sources have pending events for the given main context.}
  @see-type{g:main-context}"
  (let ((context (if context context (cffi:null-pointer))))
    (cffi:foreign-funcall "g_main_context_pending"
                          (:pointer (:struct main-context)) context
                          :boolean)))

(export 'main-context-pending)

;;; ----------------------------------------------------------------------------
;;; g_main_context_find_source_by_id ()
;;; ----------------------------------------------------------------------------

(defun main-context-find-source-by-id (context source)
 #+liber-documentation
 "@version{2023-1-5}
  @argument[context]{a @type{g:main-context} instance, if @code{nil}, the
    default context will be used}
  @argument[source]{an unsigned integer source ID, as returned by the
    @fun{g:source-id} function}
  @return{The @type{g:source} instance if found, otherwise @code{nil}.}
  @begin{short}
    Finds a source given a pair of context and ID.
  @end{short}
  It is a programmer error to attempt to look up a non-existent source. More
  specifically, source IDs can be reissued after a source has been destroyed and
  therefore it is never valid to use this function with a source ID which may
  have already been removed. An example is when scheduling an idle to run in
  another thread with the @fun{g:idle-add} function. The idle may already have
  run and been removed by the time this function is called on its (now invalid)
  source ID. This source ID may have been reissued, leading to the operation
  being performed against the wrong source.
  @see-type{g:main-context}
  @see-type{g:source}
  @see-function{g:source-id}
  @see-function{g:idle-add}"
  (let ((result nil)
        (context (if context context (cffi:null-pointer))))
    (setf result
          (cffi:foreign-funcall "g_main_context_find_source_by_id"
                                (:pointer (:struct main-context)) context
                                :uint source
                                (:pointer (:struct source))))
    (unless (cffi:null-pointer-p result)
      result)))

(export 'main-context-find-source-by-id)

;;; ----------------------------------------------------------------------------
;;; g_main_context_find_source_by_user_data ()             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_find_source_by_user_data"
                main-context-find-source-by-user-data)
    (:pointer (:struct source))
 #+liber-documentation
 "@version{#2013-4-9}
  @argument[context]{a @type{main-context} object}
  @argument[user-data]{the user data for the callback}
  @return{The source, if one was found, otherwise @code{null-pointer}.}
  @begin{short}
    Finds a source with the given user data for the callback.
  @end{short}
  If multiple sources exist with the same user data, the first one found will
  be returned."
  (context (:pointer (:struct main-context)))
  (user-data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_main_context_find_source_by_funcs_user_data ()       not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_find_source_by_funcs_user_data"
                main-context-find-source-by-funcs-user-data)
    (:pointer (:struct source))
 #+liber-documentation
 "@version{#2013-4-4}
  @argument[context]{a @type{main-context} object, if @code{null-pointer},
    the default context will be used}
  @argument[funcs]{the source funcs passed to @fun{source-new}}
  @argument[user-data]{the user data from the callback}
  @return{The source, if one was found, otherwise @code{null-pointer}.}
  @begin{short}
    Finds a source with the given source functions and user data.
  @end{short}
  If multiple sources exist with the same source function and user data, the
  first one found will be returned.
  @see-function{source-new}"
  (context (:pointer (:struct main-context)))
  (funcs (:pointer (:struct source-funcs)))
  (user-data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_main_context_wakeup ()                               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_wakeup" main-context-wakeup) :void
 #+liber-documentation
 "@version{#2021-12-10}
  @argument[context]{a @type{main-context} instance}
  @begin{short}
    If @arg{context} is currently blocking in a call of the
    @fun{main-context-iteration} function waiting for a source to become
    ready, cause it to stop blocking and return.
  @end{short}
  Otherwise, cause the next invocation of the @fun{main-context-iteration}
  function to return without blocking.

  This API is useful for low-level control over a main context. For example,
  integrating it with main loop implementations such as a @type{main-loop}
  implementation.

  Another related use for this function is when implementing a main loop with a
  termination condition, computed from multiple threads:
  @begin{pre}
#define NUM_TASKS 10
static volatile gint tasks_remaining = NUM_TASKS;
...

while (g_atomic_int_get (&tasks_remaining) != 0)
  g_main_context_iteration (NULL, TRUE);
Then in a thread:

perform_work();

if (g_atomic_int_dec_and_test (&tasks_remaining))
  g_main_context_wakeup (NULL);
  @end{pre}
  @see-type{main-context}
  @see-type{main-loop}
  @see-function{main-context-iteration}"
  (context (:pointer (:struct main-context))))

;;; ----------------------------------------------------------------------------
;;; g_main_context_acquire ()                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_acquire" main-context-acquire) :boolean
 #+liber-documentation
 "@version{#2021-12-10}
  @argument[context]{a @type{main-context} instance}
  @return{@em{True} if the operation succeeded, and this thread is now the
    owner of @arg{context}.}
  @begin{short}
    Tries to become the owner of the specified context.
  @end{short}
  If some other thread is the owner of the context, returns @em{false}
  immediately. Ownership is properly recursive: the owner can require ownership
  again and will release ownership when the @fun{main-context-release}
  function is called as many times as the @sym{main-context-acquire} function.
  @see-type{main-context}
  @see-function{main-context-release}"
  (context (:pointer (:struct main-context))))

;;; ----------------------------------------------------------------------------
;;; g_main_context_release ()                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_release" main-context-release) :void
 #+liber-documentation
 "@version{#2021-12-10}
  @argument[context]{a @type{main-context} instance}
  @begin{short}
    Releases ownership of a context previously acquired by this thread with
    the @fun{main-context-acquire} function.
  @end{short}
  If the context was acquired multiple times, the ownership will be released
  only when the @sym{main-context-release} function is called as many times
  as it was acquired.
  @see-type{main-context}
  @see-function{main-context-acquire}"
  (context (:pointer (:struct main-context))))

;;; ----------------------------------------------------------------------------
;;; g_main_context_is_owner ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_is_owner" main-context-is-owner) :boolean
 #+liber-documentation
 "@version{2022-11-22}
  @argument[context]{a @type{g:main-context} instance}
  @return{@em{True} if the current thread is owner of @arg{context}.}
  @begin{short}
    Determines whether this thread holds the (recursive) ownership of this
    context.
  @end{short}
  This is useful to know before waiting on another thread that may be blocking
  to get ownership of context.
  @see-type{g:main-context}"
  (context (:pointer (:struct main-context))))

(export 'main-context-is-owner)

;;; ----------------------------------------------------------------------------
;;; g_main_context_wait ()
;;;
;;; gboolean
;;; g_main_context_wait (GMainContext *context,
;;;                      GCond *cond,
;;;                      GMutex *mutex);
;;;
;;; g_main_context_wait has been deprecated since version 2.58 and should not
;;; be used in newly written code.
;;;
;;; Use g_main_context_is_owner() and separate locking instead.
;;;
;;; Tries to become the owner of the specified context, as with
;;; g_main_context_acquire(). But if another thread is the owner, atomically
;;; drop mutex and wait on cond until that owner releases ownership or until
;;; cond is signaled, then try again (once) to become the owner.
;;;
;;; context :
;;;     a GMainContext
;;;
;;; cond :
;;;     a condition variable
;;;
;;; mutex :
;;;     a mutex, currently held
;;;
;;; Returns :
;;;     TRUE if the operation succeeded, and this thread is now the owner of
;;;     context .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_prepare ()                              not exported
;;; ----------------------------------------------------------------------------

;; TODO: priority is a foreign integer, return this value

(cffi:defcfun ("g_main_context_prepare" main-context-prepare) :boolean
 #+liber-documentation
 "@version{#2021-4-9}
  @argument[context]{a @type{main-context} instance}
  @argument[priority]{location to store a foreign integer with the priority of
    highest priority source already ready}
  @return{@em{True} if some source is ready to be dispatched prior to polling.}
  @begin{short}
    Prepares to poll sources within a main loop.
  @end{short}
  The resulting information for polling is determined by calling
  the @fun{main-context-query} function.
  @see-type{main-context}
  @see-function{main-context-query}
  @see-function{main-context-check}
  @see-function{main-context-dispatch}"
  (context (:pointer (:struct main-context)))
  (priority (:pointer :int)))

;;; ----------------------------------------------------------------------------
;;; g_main_context_query ()                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_query" main-context-query) :int
 #+liber-documentation
 "@version{#2021-4-9}
  @argument[context]{a @type{main-context} instance}
  @argument[max-priority]{an integer with the maximum priority source to check}
  @argument[timeout]{location to store a foreign integer with the timeout to be
    used in polling}
  @argument[fds]{location to store an pointer to @type{poll-fd} records that
    need to be polled}
  @argument[n-fds]{length of @arg{fds}}
  @return{The number of records actually stored in @arg{fds}, or, if more than
  @arg{n-fds} records need to be stored, the number of records that need to be
    stored.}
  Determines information necessary to poll this main loop.
  @see-type{main-context}
  @see-function{main-context-check}
  @see-function{main-context-prepare}
  @see-function{main-context-dispatch}"
  (context (:pointer (:struct main-context)))
  (max-priority :int)
  (timeout (:pointer :int))
  (fds (:pointer (:struct poll-fd)))
  (n-fds :int))

;;; ----------------------------------------------------------------------------
;;; g_main_context_check ()                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_check" main-context-check) :int
 #+liber-documentation
 "@version{#2021-4-9}
  @argument[context]{a @type{main-context} instance}
  @argument[max-priority]{an integer with the maximum numerical priority of
    sources to check}
  @argument[fds]{a pointer with the location of an array of @type{poll-fd}'s
    that was passed to the last call to @fun{main-context-query}}
  @argument[n-fds]{return value of the @fun{main-context-query} function}
  @return{@em{True} if some sources are ready to be dispatched.}
  @begin{short}
    Passes the results of polling back to the main loop.
  @end{short}
  @see-type{main-context}
  @see-type{poll-fd}
  @see-function{main-context-query}
  @see-function{main-context-prepare}
  @see-function{main-context-dispatch}"
  (context (:pointer (:struct main-context)))
  (max-priority :int)
  (fds (:pointer (:struct poll-fd)))
  (n-fds :int))

;;; ----------------------------------------------------------------------------
;;; g_main_context_dispatch ()                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_dispatch" main-context-dispatch) :void
 #+liber-documentation
 "@version{#2021-12-10}
  @argument[context]{a @type{main-context} instance}
  @begin{short}
    Dispatches all pending sources.
  @end{short}
  You must have successfully acquired the context with the
  @fun{main-context-acquire} function before you may call this function.
  @see-type{main-context}
  @see-function{main-context-aquire}"
  (context (:pointer (:struct main-context))))

;;; ----------------------------------------------------------------------------
;;; g_main_context_set_poll_func ()                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_set_poll_func" main-context-set-poll-func) :void
 #+liber-documentation
 "@version{#2021-4-9}
  @argument[context]{a @type{main-context} instance}
  @argument[func]{the function to call to poll all file descriptors}
  @begin{short}
    Sets the function to use to handle polling of file descriptors.
  @end{short}
  It will be used instead of the @code{poll()} system call (or GLib's
  replacement function, which is used where @code{poll()} is not available).

  This function could possibly be used to integrate the GLib event loop with
  an external event loop.
  @see-type{main-context}
  @see-function{main-context-get-poll-func}"
  (context (:pointer (:struct main-context)))
  (func :pointer))

;;; ----------------------------------------------------------------------------
;;; g_main_context_get_poll_func ()                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_get_poll_func" main-context-get-poll-func)
    :pointer
 #+liber-documentation
 "@version{#2021-4-9}
  @argument[context]{a @type{main-context} instance}
  @return{The poll function.}
  @begin{short}
    Gets the poll function set by the @fun{main-context-set-poll-func} function.
  @end{short}
  @see-type{main-context}
  @see-function{main-context-set-poll-func}"
  (context (:pointer (:struct main-context))))

;;; ----------------------------------------------------------------------------
;;; GPollFunc ()
;;;
;;; gint (*GPollFunc) (GPollFD *ufds, guint nfsd, gint timeout_);
;;;
;;; Specifies the type of function passed to g_main_context_set_poll_func().
;;; The semantics of the function should match those of the poll() system call.
;;;
;;; ufds :
;;;     an array of GPollFD elements
;;;
;;; nfsd :
;;;     the number of elements in ufds
;;;
;;; timeout_ :
;;;     the maximum time to wait for an event of the file descriptors. A
;;;     negative value indicates an infinite timeout.
;;;
;;; Returns :
;;;     the number of GPollFD elements which have events or errors reported, or
;;;     -1 if an error occurred.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_add_poll ()                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_add_poll" main-context-add-poll) :void
 #+liber-documentation
 "@version{#2021-4-9}
  @argument[context]{a @type{main-context} instance or @code{null-pointer}
    for the default context}
  @argument[fd]{a @type{poll-fd} instance holding information about a file
    descriptor to watch}
  @argument[priority]{an integer with the priority for this file descriptor
    which should be the same as the priority used for the functin
    @fun{source-attach} to ensure that the file descriptor is polled whenever
    the results may be needed}
  @begin{short}
    Adds a file descriptor to the set of file descriptors polled for this
    context.
  @end{short}
  This will very seldom be used directly. Instead a typical event source will
  use the @fun{source-add-poll} function instead.
  @see-type{main-context}
  @see-type{poll-fd}
  @see-function{source-add-poll}
  @see-function{source-attach}"
  (context (:pointer (:struct main-context)))
  (fd (:pointer (:struct poll-fd)))
  (priority :int))

;;; ----------------------------------------------------------------------------
;;; g_main_context_remove_poll ()                          not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_remove_poll" main-context-remove-poll) :void
 #+liber-documentation
 "@version{#2021-4-9}
  @argument[context]{a @type{main-context} instance}
  @argument[fd]{a @type{poll-fd} descriptor previously added with the
    @fun{main-context-add-poll} function}
  @begin{short}
    Removes file descriptor from the set of file descriptors to be polled for a
    particular context.
  @end{short}
  @see-type{main-context}
  @see-type{poll-fd}
  @see-function{main-context-add-poll}"
  (context (:pointer (:struct main-context)))
  (fd (:pointer (:struct poll-fd))))

;;; ----------------------------------------------------------------------------
;;; g_main_depth ()                                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_depth" main-depth) :int
 #+liber-documentation
 "@version{#2021-12-10}
  @return{An integer with the main loop recursion level in the current thread.}
  @begin{short}
    Returns the depth of the stack of calls to the @fun{main-context-dispatch}
    function on any context in the current thread.
  @end{short}
  That is, when called from the toplevel, it gives 0. When called from within
  a callback from the @fun{main-context-iteration} function, or the
  @fun{main-loop-run} function, etc., it returns 1. When called from within a
  callback to a recursive call to the @fun{main-context-iteration} function,
  it returns 2. And so forth.

  There is a temptation to use the @sym{main-depth} function to solve problems
  with reentrancy. For instance, while waiting for data to be received from the
  network in response to a menu item, the menu item might be selected again.
  It might seem that one could make the callback of the menu item return
  immediately and do nothing if the @sym{main-depth} function returns a value
  greater than 1. However, this should be avoided since the user then sees
  selecting the menu item do nothing. Furthermore, you will find yourself adding
  these checks all over your code, since there are doubtless many things
  that the user could do. Instead, you can use the following techniques:
  @begin{itemize}
    @item{Use the @fun{gtk-widget-sensitive} function or modal dialogs to
      prevent the user from interacting with elements while the main loop is
      recursing.}
    @item{Avoid main loop recursion in situations where you cannot handle
      arbitrary callbacks. Instead, structure your code so that you simply
      return to the main loop and then get called again when there is more work
      to do.}
  @end{itemize}
  @see-type{main-context}
  @see-function{main-context-dispatch}
  @see-function{main-context-iteration}
  @see-function{main-loop-run}
  @see-function{gtk-widget-sensitive}")

;;; ----------------------------------------------------------------------------
;;; g_main_current_source ()                               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_current_source" main-current-source)
    (:pointer (:struct source))
 #+liber-documentation
 "@version{#2021-12-10}
  @return{The currently firing @type{source} instance or @code{null-pointer}.}
  @short{Returns the currently firing source for this thread.}
  @see-type{source}")

;;; ----------------------------------------------------------------------------
;;; g_main_set_poll_func()
;;;
;;; #define g_main_set_poll_func(func)
;;;
;;; Warning
;;;
;;; g_main_set_poll_func has been deprecated since version 2.2 and should not be
;;; used in newly written code. Use g_main_context_set_poll_func() again
;;;
;;; Sets the function to use for the handle polling of file descriptors for the
;;; default main context.
;;;
;;; func :
;;;     the function to call to poll all file descriptors
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_invoke ()
;;;
;;; void g_main_context_invoke (GMainContext *context,
;;;                             GSourceFunc function,
;;;                             gpointer data);
;;;
;;; Invokes a function in such a way that context is owned during the invocation
;;; of function.
;;;
;;; If context is NULL then the global default main context - as returned by
;;; g_main_context_default() - is used.
;;;
;;; If context is owned by the current thread, function is called directly.
;;; Otherwise, if context is the thread-default main context of the current
;;; thread and g_main_context_acquire() succeeds, then function is called and
;;; g_main_context_release() is called afterwards.
;;;
;;; In any other case, an idle source is created to call function and that
;;; source is attached to context (presumably to be run in another thread). The
;;; idle source is attached with G_PRIORITY_DEFAULT priority. If you want a
;;; different priority, use g_main_context_invoke_full().
;;;
;;; Note that, as with normal idle functions, function should probably return
;;; FALSE. If it returns TRUE, it will be continuously run in a loop (and may
;;; prevent this call from returning).
;;;
;;; context :
;;;     a GMainContext, or NULL
;;;
;;; function :
;;;     function to call
;;;
;;; data :
;;;     data to pass to function
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_invoke_full ()
;;;
;;; void g_main_context_invoke_full (GMainContext *context,
;;;                                  gint priority,
;;;                                  GSourceFunc function,
;;;                                  gpointer data,
;;;                                  GDestroyNotify notify);
;;;
;;; Invokes a function in such a way that context is owned during the invocation
;;; of function.
;;;
;;; This function is the same as g_main_context_invoke() except that it lets you
;;; specify the priority incase function ends up being scheduled as an idle and
;;; also lets you give a GDestroyNotify for data.
;;;
;;; notify should not assume that it is called from any particular thread or
;;; with any particular context acquired.
;;;
;;; context :
;;;     a GMainContext, or NULL
;;;
;;; priority :
;;;     the priority at which to run function
;;;
;;; function :
;;;     function to call
;;;
;;; data :
;;;     data to pass to function
;;;
;;; notify :
;;;     a function to call when data is no longer in use, or NULL
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_pusher_new ()
;;;
;;; GMainContextPusher *
;;; g_main_context_pusher_new (GMainContext *main_context);
;;;
;;; Push main_context as the new thread-default main context for the current
;;; thread, using g_main_context_push_thread_default(), and return a new
;;; GMainContextPusher. Pop with g_main_context_pusher_free(). Using
;;; g_main_context_pop_thread_default() on main_context while a
;;; GMainContextPusher exists for it can lead to undefined behaviour.
;;;
;;; Using two GMainContextPushers in the same scope is not allowed, as it leads
;;; to an undefined pop order.
;;;
;;; This is intended to be used with g_autoptr(). Note that g_autoptr() is only
;;; available when using GCC or clang, so the following example will only work
;;; with those compilers:
;;;
;;; typedef struct
;;; {
;;;   ...
;;;   GMainContext *context;
;;;   ...
;;; } MyObject;
;;;
;;; static void
;;; my_object_do_stuff (MyObject *self)
;;; {
;;;   g_autoptr(GMainContextPusher) pusher =
;;;       g_main_context_pusher_new (self->context);
;;;
;;;   // Code with main context as the thread default here
;;;
;;;   if (cond)
;;;     // No need to pop
;;;     return;
;;;
;;;   // Optionally early pop
;;;   g_clear_pointer (&pusher, g_main_context_pusher_free);
;;;
;;;   // Code with main context no longer the thread default here
;;; }
;;;
;;; main_context :
;;;     a main context to push.
;;;
;;; Returns :
;;;     a GMainContextPusher.
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_pusher_free ()
;;;
;;; void
;;; g_main_context_pusher_free (GMainContextPusher *pusher);
;;;
;;; Pop pusher ’s main context as the thread default main context. See
;;; g_main_context_pusher_new() for details.
;;;
;;; This will pop the GMainContext as the current thread-default main context,
;;; but will not call g_main_context_unref() on it.
;;;
;;; pusher :
;;;     a GMainContextPusher.
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_get_thread_default ()                   not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_get_thread_default" main-context-thread-default)
    (:pointer (:struct main-context))
 #+liber-documentation
 "@version{#2021-4-9}
  @begin{return}
    The thread default @type{main-context}, or @code{NULL} if the thread
    default context is the global default context.
  @end{return}
  @begin{short}
    Gets the thread default @type{main-context} for this thread.
  @end{short}
  Asynchronous operations that want to be able to be run in contexts other than
  the default one should call this method or the
  @fun{main-context-ref-thread-default} function to get a @type{main-context} to
  add their @type{source}s to. Note that even in single-threaded programs
  applications may sometimes want to temporarily push a non-default context, so
  it is not safe to assume that this will always return @code{NULL} if you are
  running in the default thread.

  If you need to hold a reference on the context, use the
  @fun{main-context-ref-thread-default} function instead.
  @see-type{main-context}
  @see-function{main-context-ref-thread-default}")

;;; ----------------------------------------------------------------------------
;;; g_main_context_ref_thread_default ()                   not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_main_context_ref_thread_default"
                main-context-ref-thread-default)
    (:pointer (:struct main-context))
 #+liber-documentation
 "@version{#2021-4-9}
  @begin{return}
    The thread-default @symbol{main-context}. Unref with the
    @fun{main-context-unref} function when you are done with it
  @end{return}
  @begin{short}
    Gets the thread-default @symbol{main-context} for this thread, as with
    the @fun{main-context-get-thread-default} function, but also adds a
    reference to it with the @fun{main-context-ref} function.
  @end{short}
  In addition, unlike the @fun{main-context-get-thread-default} function, if
  the thread-default context is the global default context, this will return
  that @symbol{main-context}, with a ref added to it, rather than returning
  @code{NULL}.
  @see-symbol{main-context}
  @see-function{main-context-ref}
  @see-function{main-context-unref}
  @see-function{main-context-thread-default}")

;;; ----------------------------------------------------------------------------
;;; g_main_context_push_thread_default ()
;;;
;;; void g_main_context_push_thread_default (GMainContext *context);
;;;
;;; Acquires context and sets it as the thread-default context for the current
;;; thread. This will cause certain asynchronous operations (such as most
;;; gio-based I/O) which are started in this thread to run under context and
;;; deliver their results to its main loop, rather than running under the global
;;; default context in the main thread. Note that calling this function changes
;;; the context returned by g_main_context_get_thread_default(), not the one
;;; returned by g_main_context_default(), so it does not affect the context used
;;; by functions like g_idle_add().
;;;
;;; Normally you would call this function shortly after creating a new thread,
;;; passing it a GMainContext which will be run by a GMainLoop in that thread,
;;; to set a new default context for all async operations in that thread. (In
;;; this case, you don't need to ever call g_main_context_pop_thread_default().)
;;; In some cases however, you may want to schedule a single operation in a
;;; non-default context, or temporarily use a non-default context in the main
;;; thread. In that case, you can wrap the call to the asynchronous operation
;;; inside a g_main_context_push_thread_default() /
;;; g_main_context_pop_thread_default() pair, but it is up to you to ensure that
;;; no other asynchronous operations accidentally get started while the
;;; non-default context is active.
;;;
;;; Beware that libraries that predate this function may not correctly handle
;;; being used from a thread with a thread-default context. Eg, see
;;; g_file_supports_thread_contexts().
;;;
;;; context :
;;;     a GMainContext, or NULL for the global default context
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_pop_thread_default ()
;;;
;;; void g_main_context_pop_thread_default (GMainContext *context);
;;;
;;; Pops context off the thread-default context stack (verifying that it was on
;;; the top of the stack).
;;;
;;; context :
;;;     a GMainContext object, or NULL
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_timeout_source_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_timeout_source_new" timeout-source-new)
    (:pointer (:struct source))
 #+liber-documentation
 "@version{2022-11-22}
  @argument[interval]{an integer with the timeout interval in milliseconds}
  @return{The newly created @type{g:source} timeout source.}
  @begin{short}
    Creates a new timeout source.
  @end{short}
  The source will not initially be associated with any context and must be added
  to one with the @fun{g:source-attach} function before it will be executed.
  @see-type{g:source}
  @see-function{g:source-attach}
  @see-function{g:timeout-source-new-seconds}"
  (interval :int))

(export 'timeout-source-new)

;;; ----------------------------------------------------------------------------
;;; g_timeout_source_new_seconds ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_timeout_source_new_seconds" timeout-source-new-seconds)
    (:pointer (:struct source))
 #+liber-documentation
 "@version{2023-1-5}
  @argument[interval]{an integer with the timeout interval in seconds}
  @return{The newly created @type{g:source} timeout source.}
  @begin{short}
    Creates a new timeout source.
  @end{short}
  The source will not initially be associated with any context and must be added
  to one with the @fun{g:source-attach} function before it will be executed.
  The scheduling granularity/accuracy of this timeout source will be in
  seconds.
  @see-type{g:source}
  @see-function{g:source-attach}
  @see-function{g:timeout-source-new}"
  (interval :int))

(export 'timeout-source-new-seconds)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add ()
;;; ----------------------------------------------------------------------------

(defun timeout-add (interval func &key (priority +g-priority-default+))
 #+liber-documentation
 "@version{2022-11-22}
  @argument[interval]{an integer with the time between calls to @arg{func},
    in milliseconds}
  @argument[func]{a @symbol{g:source-func} callback function to call}
  @argument[priority]{an integer with the priority of the timeout source,
    typically this will be in the range between the @var{+g-priority-default+}
    and @var{+g-priority-high+} values}
  @return{The unsigned integer ID greater than 0 of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals, with @arg{priority}.
  @end{short}
  The default value is @var{+g-priority-default+}. The function is called
  repeatedly until it returns @em{false}, at which point the timeout is
  automatically destroyed and the function will not be called again. The first
  call to the function will be at the end of the first interval.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval. It does not
  try to 'catch up' time lost in delays.

  If you want to have a timer in the \"seconds\" range and do not care about
  the exact time of the first call of the timer, use the
  @fun{g:timeout-add-seconds} function. This function allows for more
  optimizations and more efficient system power usage.

  This internally creates a main loop source using the
  @fun{g:timeout-source-new} function and attaches it to the main loop context
  using the @fun{g:source-attach} function. You can do these steps manually if
  you need greater control.
  @see-symbol{g:source-func}
  @see-function{g:timeout-add-seconds}
  @see-function{g:timeout-source-new}
  @see-function{g:source-attach}"
  (%timeout-add-full priority
                     interval
                     (cffi:callback source-func)
                     (allocate-stable-pointer func)
                     (cffi:callback stable-pointer-destroy-notify)))

(export 'timeout-add)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add_full ()                                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_timeout_add_full" %timeout-add-full) :uint
 #+liber-documentation
 "@version{#2013-7-20}
  @argument[priority]{the priority of the timeout source. Typically this will
    be in the range between @var{+g-priority-default+} and
    @var{+g-priority-high+}.}
  @argument[interval]{the time between calls to the function, in milliseconds
    (1/1000ths of a second)}
  @argument[function]{function to call}
  @argument[data]{data to pass to @arg{function}}
  @argument[notify]{function to call when the timeout is removed, or
    @code{null-pointer}}
  @return{the ID (greater than @code{0}) of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals, with the given priority.
  @end{short}
  The function is called repeatedly until it returns @code{nil}, at which point
  the timeout is automatically destroyed and the function will not be called
  again. The @arg{notify} function is called when the timeout is destroyed. The
  first call to the function will be at the end of the first @arg{interval}.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval (it does not
  try to 'catch up' time lost in delays).

  This internally creates a main loop source using @fun{timeout-source-new}
  and attaches it to the main loop context using @fun{source-attach}. You can
  do these steps manually if you need greater control.

  The interval given in terms of monotonic time, not wall clock time. See
  @code{g_get_monotonic_time()}."
  (priority :int)
  (interval-milliseconds :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

;;; ----------------------------------------------------------------------------
;;; g_timeout_add_seconds ()
;;; ----------------------------------------------------------------------------

(defun timeout-add-seconds (interval func
                            &key (priority +g-priority-default+))
 #+liber-documentation
 "@version{2023-1-6}
  @argument[interval]{an unsigned integer with the time between calls to
    @arg{func}, in seconds}
  @argument[func]{a @symbol{g:source-func} callback function to call}
  @argument[priority]{an integer with the priority of the timeout source,
    typically this will be in the range between @var{+g-priority-default+} and
    @var{+g-priority-high+} values}
  @return{The unsigned integer ID greater than 0 of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals with @arg{priority}.
  @end{short}
  The default value is @var{+g-priority-default+}. The function is called
  repeatedly until it returns @em{false}, at which point the timeout is
  automatically destroyed and the function will not be called again.

  This internally creates a main loop source using the
  @fun{g:timeout-source-new-seconds} function and attaches it to the main loop
  context using the @fun{g:source-attach} function. You can do these steps
  manually if you need greater control.

  Note that the first call of the timer may not be precise for timeouts of one
  second. If you need finer precision and have such a timeout, you may want to
  use the @fun{g:timeout-add} function instead.
  @see-symbol{g:source-func}
  @see-function{g:timeout-source-new-seconds}
  @see-function{g:source-attach}
  @see-function{g:timeout-add}"
  (%timeout-add-seconds-full priority
                             interval
                             (cffi:callback source-func)
                             (allocate-stable-pointer func)
                             (cffi:callback stable-pointer-destroy-notify)))

(export 'timeout-add-seconds)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add_seconds_full ()                          not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_timeout_add_seconds_full" %timeout-add-seconds-full) :uint
 #+liber-documentation
 "@version{#2013-01-17}
  @argument[priority]{the priority of the timeout source. Typically this will
    be in the range between @var{+g-priority-default+} and
    @var{+g-priority-high+}.}
  @argument[interval]{the time between calls to the function, in seconds}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @argument[notify]{function to call when the timeout is removed, or
    @code{null-pointer}}
  @return{the ID (greater than @code{0}) of the event source.}
  @begin{short}
    Sets a @arg{function} to be called at regular intervals, with priority.
  @end{short}
  The @arg{function} is called repeatedly until it returns @code{nil}, at which
  point the timeout is automatically destroyed and the function will not be
  called again.

  Unlike @fun{timeout-add}, this function operates at whole second
  granularity. The initial starting point of the timer is determined by the
  implementation and the implementation is expected to group multiple timers
  together so that they fire all at the same time. To allow this grouping, the
  interval to the first timer is rounded and can deviate up to one second from
  the specified @arg{interval}. Subsequent timer iterations will generally run
  at the specified interval.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval

  If you want timing more precise than whole seconds, use @fun{timeout-add}
  instead.

  The grouping of timers to fire at the same time results in a more power and
  CPU efficient behavior so if your timer is in multiples of seconds and you
  don't require the first timer exactly one second from now, the use of
  @fun{timeout-add-seconds} is preferred over @fun{timeout-add}.

  This internally creates a main loop source using
  @fun{timeout-source-new-seconds} and attaches it to the main loop context
  using @fun{source-attach}. You can do these steps manually if you need
  greater control.

  The interval given is in terms of monotonic time, not wall clock time. See
  @code{g_get_monotonic_time()}.

  Since 2.14"
  (priority :int)
  (interval-seconds :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

;;; ----------------------------------------------------------------------------
;;; g_idle_source_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_idle_source_new" idle-source-new) (:pointer (:struct source))
 #+liber-documentation
 "@version{2022-11-22}
  @return{The newly created @type{g:source} idle source.}
  @begin{short}
    Creates a new idle source.
  @end{short}
  The source will not initially be associated with any context and must be
  added to one with the @fun{g:source-attach} function before it will be
  executed. Note that the default priority for idle sources is
  @var{+g-priority-default-idle+}, as compared to other sources which have
  a default priority of @var{+g-priority-default+}.
  @see-type{g:source}
  @see-function{g:source-attach}")

(export 'idle-source-new)

;;; ----------------------------------------------------------------------------
;;; g_idle_add ()
;;; ----------------------------------------------------------------------------

(defun idle-add (func &key (priority +g-priority-default-idle+))
 #+liber-documentation
 "@version{2022-11-22}
  @argument[func]{a @symbol{g:source-func} callback function to call}
  @argument[priority]{an integer with the priority of the idle source, typically
    this will be in the range between @var{+g-priority-default-idle+} and
    @var{+g-priority-high-idle+}}
  @return{The unsigned integer ID greater than 0 of the event source.}
  @begin{short}
    Adds a function to be called whenever there are no higher priority events
    pending to the default main loop.
  @end{short}
  The @arg{priority} keyword argument has the @var{+g-priority-default-idle+}
  default value. If the function returns @em{false} it is automatically removed
  from the list of event sources and will not be called again.

  This internally creates a main loop source using the @fun{g:idle-source-new}
  function and attaches it to the main loop context using the
  @fun{g:source-attach} function. You can do these steps manually if you need
  greater control.
  @see-symbol{g:source-func}
  @see-function{g:idle-source-new}
  @see-function{g:source-attach}"
  (%idle-add-full priority
                  (cffi:callback source-func)
                  (allocate-stable-pointer func)
                  (cffi:callback stable-pointer-destroy-notify)))

(export 'idle-add)

;;; ----------------------------------------------------------------------------
;;; g_idle_add_full ()                                     not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_idle_add_full" %idle-add-full) :uint
 #+liber-documentation
 "@version{#2013-1-1}
  @argument[priority]{the priority of the idle source. Typically this will be
    in the range between G_PRIORITY_DEFAULT_IDLE and G_PRIORITY_HIGH_IDLE.}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @argument[notify]{function to call when the idle is removed, or NULL}
  @return{the ID (greater than 0) of the event source. Rename to: g_idle_add}
  @begin{short}
    Adds a function to be called whenever there are no higher priority events
    pending.
  @end{short}
  If the function returns FALSE it is automatically removed from the list of
  event sources and will not be called again.

  This internally creates a main loop source using g_idle_source_new() and
  attaches it to the main loop context using g_source_attach(). You can do
  these steps manually if you need greater control."
  (priority :uint)
  (function :pointer)
  (data :pointer)
  (notify :pointer))

;;; ----------------------------------------------------------------------------
;;; g_idle_remove_by_data ()                               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_idle_remove_by_data" idle-remove-by-data) :boolean
 #+liber-documentation
 "@version{#2013-1-1}
  @argument[data]{the data for the idle source's callback.}
  @return{TRUE if an idle source was found and removed.}
  @short{Removes the idle function with the given data.}"
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; GChildWatchFunc ()
;;;
;;; void (*GChildWatchFunc) (GPid pid, gint status, gpointer user_data);
;;;
;;; The type of functions to be called when a child exists.
;;;
;;; pid :
;;;     the process id of the child process
;;;
;;; status :
;;;     Status information about the child process, see waitpid(2) for more
;;;     information about this field
;;;
;;; user_data :
;;;     user data passed to g_child_watch_add()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_child_watch_source_new ()
;;;
;;; GSource * g_child_watch_source_new (GPid pid);
;;;
;;; Creates a new child_watch source.
;;;
;;; The source will not initially be associated with any GMainContext and must
;;; be added to one with g_source_attach() before it will be executed.
;;;
;;; Note that child watch sources can only be used in conjunction with
;;; g_spawn... when the G_SPAWN_DO_NOT_REAP_CHILD flag is used.
;;;
;;; Note that on platforms where GPid must be explicitly closed (see
;;; g_spawn_close_pid()) pid must not be closed while the source is still
;;; active. Typically, you will want to call g_spawn_close_pid() in the callback
;;; function for the source.
;;;
;;; Note further that using g_child_watch_source_new() is not compatible with
;;; calling waitpid(-1) in the application. Calling waitpid() for individual
;;; pids will still work fine.
;;;
;;; pid :
;;;     process to watch. On POSIX the pid of a child process. On Windows a
;;;     handle for a process (which does not have to be a child).
;;;
;;; Returns :
;;;     the newly-created child watch source
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_child_watch_add ()
;;;
;;; guint g_child_watch_add (GPid pid,
;;;                          GChildWatchFunc function,
;;;                          gpointer data);
;;;
;;; Sets a function to be called when the child indicated by pid exits, at a
;;; default priority, G_PRIORITY_DEFAULT.
;;;
;;; If you obtain pid from g_spawn_async() or g_spawn_async_with_pipes() you
;;; will need to pass G_SPAWN_DO_NOT_REAP_CHILD as flag to the spawn function
;;; for the child watching to work.
;;;
;;; Note that on platforms where GPid must be explicitly closed (see
;;; g_spawn_close_pid()) pid must not be closed while the source is still
;;; active. Typically, you will want to call g_spawn_close_pid() in the callback
;;; function for the source.
;;;
;;; GLib supports only a single callback per process id.
;;;
;;; This internally creates a main loop source using g_child_watch_source_new()
;;; and attaches it to the main loop context using g_source_attach(). You can do
;;; these steps manually if you need greater control.
;;;
;;; pid :
;;;     process id to watch. On POSIX the pid of a child process. On Windows a
;;;     handle for a process (which does not have to be a child).
;;;
;;; function :
;;;     function to call
;;;
;;; data :
;;;     data to pass to function
;;;
;;; Returns :
;;;     the ID (greater than 0) of the event source.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_child_watch_add_full ()
;;;
;;; guint g_child_watch_add_full (gint priority,
;;;                               GPid pid,
;;;                               GChildWatchFunc function,
;;;                               gpointer data,
;;;                               GDestroyNotify notify);
;;;
;;; Sets a function to be called when the child indicated by pid exits, at the
;;; priority priority.
;;;
;;; If you obtain pid from g_spawn_async() or g_spawn_async_with_pipes() you
;;; will need to pass G_SPAWN_DO_NOT_REAP_CHILD as flag to the spawn function
;;; for the child watching to work.
;;;
;;; Note that on platforms where GPid must be explicitly closed (see
;;; g_spawn_close_pid()) pid must not be closed while the source is still
;;; active. Typically, you will want to call g_spawn_close_pid() in the callback
;;; function for the source.
;;;
;;; GLib supports only a single callback per process id.
;;;
;;; This internally creates a main loop source using g_child_watch_source_new()
;;; and attaches it to the main loop context using g_source_attach(). You can
;;; do these steps manually if you need greater control.
;;;
;;; priority :
;;;     the priority of the idle source. Typically this will be in the range
;;;     between G_PRIORITY_DEFAULT_IDLE and G_PRIORITY_HIGH_IDLE.
;;;
;;; pid :
;;;     process to watch. On POSIX the pid of a child process. On Windows a
;;;     handle for a process (which does not have to be a child).
;;;
;;; function :
;;;     function to call
;;;
;;; data :
;;;     data to pass to function
;;;
;;; notify :
;;;     function to call when the idle is removed, or NULL
;;;
;;; Returns :
;;;     the ID (greater than 0) of the event source. Rename to:
;;;     g_child_watch_add
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_poll ()
;;;
;;; gint g_poll (GPollFD *fds, guint nfds, gint timeout);
;;;
;;; Polls fds, as with the poll() system call, but portably. (On systems that
;;; don't have poll(), it is emulated using select().) This is used internally
;;; by GMainContext, but it can be called directly if you need to block until
;;; a file descriptor is ready, but don't want to run the full main loop.
;;;
;;; Each element of fds is a GPollFD describing a single file descriptor to
;;; poll. The fd field indicates the file descriptor, and the events field
;;; indicates the events to poll for. On return, the revents fields will be
;;; filled with the events that actually occurred.
;;;
;;; On POSIX systems, the file descriptors in fds can be any sort of file
;;; descriptor, but the situation is much more complicated on Windows. If you
;;; need to use g_poll() in code that has to run on Windows, the easiest
;;; solution is to construct all of your GPollFDs with
;;; g_io_channel_win32_make_pollfd().
;;;
;;; fds :
;;;     file descriptors to poll
;;;
;;; nfds :
;;;     the number of file descriptors in fds
;;;
;;; timeout :
;;;     amount of time to wait, in milliseconds, or -1 to wait forever
;;;
;;; Returns :
;;;     the number of entries in fds whose revents fields were filled in, or 0
;;;     if the operation timed out, or -1 on error or if the call was
;;;     interrupted.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GSourceDummyMarshal ()
;;;
;;; void (*GSourceDummyMarshal) (void);
;;;
;;; This is just a placeholder for GClosureMarshal, which cannot be used here
;;; for dependency reasons.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GSourceDisposeFunc ()
;;;
;;; void
;;; (*GSourceDisposeFunc) (GSource *source);
;;;
;;; Dispose function for source . See g_source_set_dispose_function() for
;;; details.
;;;
;;; source :
;;;     GSource that is currently being disposed
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_new ()                                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_new" source-new) (:pointer (:struct source))
 #+liber-documentation
 "@version{#2021-4-10}
  @argument[source-funcs]{a @type{source-funcs} instance containing functions
    that implement the sources behavior}
  @argument[struct-size]{size of the @type{source} structure to create}
  @return{The newly-created @type{source} instance.}
  @begin{short}
    Creates a new source.
  @end{short}
  The size is specified to allow creating structures derived from a
  @type{source} structure that contain additional data. The size passed in
  must be at least @code{(cffi:foreign-type-size '(:struct source))}.

  The source will not initially be associated with any context and must be added
  to one with the @fun{source-attach} function before it will be executed.
  @see-type{source}
  @see-type{source-funcs}
  @see-function{source-attach}"
  (source-funcs (:pointer (:struct source-funcs)))
  (struct-size :uint))

;;; ----------------------------------------------------------------------------
;;; g_source_ref ()                                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_ref" source-ref) (:pointer (:struct source))
 #+liber-documentation
 "@version{#2021-12-10}
  @argument[source]{a @type{source} instance}
  @return{The @arg{source} argument.}
  @short{Increases the reference count on a source by one.}
  @see-type{source}
  @see-function{source-unref}"
  (source (:pointer (:struct source))))

;;; ----------------------------------------------------------------------------
;;; g_source_unref ()                                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_unref" source-unref) :void
 #+liber-documentation
 "@version{#2021-12-10}
  @argument[source]{a @type{source} instance}
  @begin{short}
    Decreases the reference count of @arg{source} by one.
  @end{short}
  If the resulting reference count is zero the source and associated memory
  will be destroyed.
  @see-type{source}
  @see-function{source-ref}"
  (source (:pointer (:struct source))))

;;; ----------------------------------------------------------------------------
;;; g_source_set_funcs ()                                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_set_funcs" source-set-funcs) :void
 #+liber-documentation
 "@version{#2013-7-21}
  @argument[source]{a @type{source} instance}
  @argument[funcs]{the new @type{source-funcs}}
  @begin{short}
    Sets the source functions of an unattached source.
  @end{short}
  This function can be used to override default implementations.

  Since 2.12"
  (source (:pointer (:struct source)))
  (funcs (:pointer (:struct source-funcs))))

;;; ----------------------------------------------------------------------------
;;; g_source_set_dispose_function ()
;;;
;;; void
;;; g_source_set_dispose_function (GSource *source,
;;;                                GSourceDisposeFunc dispose);
;;;
;;; Set dispose as dispose function on source . dispose will be called once the
;;; reference count of source reaches 0 but before any of the state of the
;;; source is freed, especially before the finalize function is called.
;;;
;;; This means that at this point source is still a valid GSource and it is
;;; allow for the reference count to increase again until dispose returns.
;;;
;;; The dispose function can be used to clear any "weak" references to the
;;; source in other data structures in a thread-safe way where it is possible
;;; for another thread to increase the reference count of source again while it
;;; is being freed.
;;;
;;; The finalize function can not be used for this purpose as at that point
;;; source is already partially freed and not valid anymore.
;;;
;;; This should only ever be called from GSource implementations.
;;;
;;; source :
;;;     A GSource to set the dispose function on
;;;
;;; dispose :
;;;     GSourceDisposeFunc to set on the source
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_attach ()
;;; ----------------------------------------------------------------------------

(defun source-attach (source context)
 #+liber-documentation
 "@version{2023-1-5}
  @argument[source]{a @type{g:source} instance}
  @argument[context]{a @type{g:main-context} instance, if @code{nil}, the
    default context will be used}
  @begin{return}
    The unsigned integer ID greater than 0 for the source within @arg{context}.
  @end{return}
  @begin{short}
    Adds a source to a context so that it will be executed within that context.
  @end{short}
  Remove it by calling the @fun{g:source-destroy} function.
  @see-type{g:source}
  @see-type{g:main-context}
  @see-function{g:source-destroy}"
  (let ((context (if context context (cffi:null-pointer))))
    (cffi:foreign-funcall "g_source_attach"
                          (:pointer (:struct source)) source
                          (:pointer (:struct main-context)) context
                          :uint)))

(export 'source-attach)

;;; ----------------------------------------------------------------------------
;;; g_source_destroy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_destroy" source-destroy) :void
 #+liber-documentation
 "@version{2022-11-22}
  @argument[source]{a @type{g:source} instance}
  @begin{short}
    Removes a source from its context, if any, and mark it as destroyed.
  @end{short}
  The source cannot be subsequently added to another context.
  @see-type{g:source}
  @see-function{g:source-attach}
  @see-function{g:source-is-destroyed}"
  (source (:pointer (:struct source))))

(export 'source-destroy)

;;; ----------------------------------------------------------------------------
;;; g_source_is_destroyed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_is_destroyed" source-is-destroyed) :boolean
 #+liber-documentation
 "@version{2022-11-22}
  @argument[source]{a @type{g:source} instance}
  @return{@em{True} if @arg{source} has been destroyed.}
  @begin{short}
    Returns whether the source has been destroyed.
  @end{short}
  This is important when you operate upon your objects from within idle
  handlers, but may have freed the object before the dispatch of your idle
  handler.
  @see-type{g:source}"
  (source (:pointer (:struct source))))

(export 'source-is-destroyed)

;;; ----------------------------------------------------------------------------
;;; g_source_get_priority ()
;;; g_source_set_priority () -> source-priority
;;; ----------------------------------------------------------------------------

(defun (setf source-priority) (priority source)
  (cffi:foreign-funcall "g_source_set_priority"
                        (:pointer (:struct source)) source
                        :int priority
                        :void)
  priority)

(cffi:defcfun ("g_source_get_priority" source-priority) :int
 #+liber-documentation
 "@version{2023-1-5}
  @syntax[]{(g:source-priority source) => priority}
  @syntax[]{(setf (g:source-priority source) => priority)}
  @argument[source]{a @type{g:source} instance}
  @argument[priority]{an integer with the priority}
  @begin{short}
    The @sym{g:source-priority} function gets the priority of the source.
  @end{short}
  The @sym{(setf g:source-priority)} function sets the priority. While the main
  loop is being run, a source will be dispatched if it is ready to be dispatched
  and no sources at a higher (numerically smaller) priority are ready to be
  dispatched.
  @see-type{g:source}"
  (source (:pointer (:struct source))))

(export 'source-priority)

;;; ----------------------------------------------------------------------------
;;; g_source_get_can_recurse ()
;;; g_source_set_can_recurse () -> source-can-recursive
;;; ----------------------------------------------------------------------------

(defun (setf source-can-recurse) (can-recurse source)
  (cffi:foreign-funcall "g_source_set_can_recurse"
                        (:pointer (:struct source)) source
                        :boolean can-recurse
                        :void)
  can-recurse)

(cffi:defcfun ("g_source_get_can_recurse" source-can-recurse) :boolean
 #+liber-documentation
 "@version{2023-1-5}
  @syntax[]{(g:source-can-recurse source) => can-recurse}
  @syntax[]{(setf g:source-can-recurse source) can-recurse)}
  @argument[source]{a @type{g:source} instance}
  @argument[can-recurse]{a boolean whether recursion is allowed for this source}
  @begin{short}
    The @sym{g:source-can-recurse} function checks whether the source is allowed
    to be called recursively.
  @end{short}
  The @sym{(setf g:source-can-recurse)} function sets whether the source can be
  called recursively. If the @arg{can-recurse} argument is @em{true}, then while
  the source is being dispatched then this source will be processed normally.
  Otherwise, all processing of this source is blocked until the dispatch
  function returns.
  @see-type{g:source}"
  (source (:pointer (:struct source))))

(export 'source-can-recurse)

;;; ----------------------------------------------------------------------------
;;; g_source_get_id () -> source-id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_get_id" source-id) :uint
 #+liber-documentation
 "@version{2022-11-22}
  @argument[source]{a @type{g:source} instance}
  @return{An unsigned integer with the ID greater than 0 for @arg{source}.}
  @begin{short}
    Returns the numeric ID for a particular source.
  @end{short}
  The ID of a source is a positive integer which is unique within a particular
  main loop context. The reverse mapping from ID to source is done by the
  @fun{g:main-context-find-source-by-id} function.
  @see-type{g:source}
  @see-function{g:main-context-find-source-by-id}"
  (source (:pointer (:struct source))))

(export 'source-id)

;;; ----------------------------------------------------------------------------
;;; g_source_get_name ()
;;; g_source_set_name () -> source-name
;;; ----------------------------------------------------------------------------

(defun (setf source-name) (name source)
  (let ((str (if name name (cffi:null-pointer))))
    (cffi:foreign-funcall "g_source_set_name"
                          (:pointer (:struct source)) source
                          :string str
                          :void))
  name)

(cffi:defcfun ("g_source_get_name" source-name) :string
 #+liber-documentation
 "@version{2023-1-5}
  @syntax[]{(g:source-name source) => name}
  @syntax[]{(setf (g:source-name source) name)}
  @argument[source]{a @type{g:source} instance}
  @argument[name]{a string with the debug name for the source}
  @begin{short}
    The @sym{g:source-name} function gets a name for the source, used in
    debugging and profiling.
  @end{short}
  The @sym{(setf g:source-name)} function sets a name for the source. The name
  may be @code{nil}. The source name should describe in a human readable way
  what the source does. For example, \"X11 event queue\" or \"GTK repaint idle
  handler\" or whatever it is.

  It is permitted to call this function multiple times, but is not recommended
  due to the potential performance impact. For example, one could change the
  name in the \"check\" function of a @code{GSourceFuncs} to include details
  like the event type in the source name.
  @see-type{g:source}"
  (source (:pointer (:struct source))))

(export 'source-name)

;;; ----------------------------------------------------------------------------
;;; g_source_set_name_by_id ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_set_name_by_id" source-set-name-by-id) :void
 #+liber-documentation
 "@version{2022-11-22}
  @argument[source]{an integer with the source ID}
  @argument[name]{a string with the debug name for the source}
  @begin{short}
    Sets the name of a source using its ID.
  @end{short}
  This is a convenience utility to set source names from the return value of
  the @fun{g:idle-add}, @fun{g:timeout-add} functions, etc.
  @see-type{g:source}
  @see-function{g:idle-add}
  @see-function{g:timeout-add}
  @see-function{g:source-name}"
  (source :uint)
  (name :string))

(export 'source-set-name-by-id)

;;; ----------------------------------------------------------------------------
;;; g_source_get_context () -> source-context
;;; ----------------------------------------------------------------------------

(defun source-context (source)
 #+liber-documentation
 "@version{2023-1-5}
  @argument[source]{a @type{g:source} instance}
  @return{The @type{g:main-context} instance with which the source is
    associated, or @code{nil} if the context has not yet been added to
    a source.}
  @begin{short}
    Gets the context with which the source is associated.
  @end{short}
  Calling this function on a destroyed source is an error.
  @see-type{g:source}
  @see-type{g:main-context}"
  (let ((result nil))
    (setf result
          (cffi:foreign-funcall "g_source_get_context"
                                (:pointer (:struct source)) source
                                (:pointer (:struct main-context))))
    (unless (cffi:null-pointer-p result)
      result)))

(export 'source-context)

;;; ----------------------------------------------------------------------------
;;; g_source_set_callback ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_set_callback" %source-set-callback) :void
  (source (:pointer (:struct source)))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun source-set-callback (source func)
 #+liber-documentation
 "@version{2023-1-5}
  @argument[source]{a @type{g:source} instance}
  @argument[func]{a @symbol{g:source-func} callback function}
  @begin{short}
    Sets the callback function for a source.
  @end{short}
  The callback function for a source is called from the dispatch function of
  the source. Typically, you will not use this function. Instead use functions
  specific to the type of source you are using.
  @see-type{g:source}
  @see-symbol{g:source-func}"
  (%source-set-callback source
                        (cffi:callback source-func)
                        (allocate-stable-pointer func)
                        (cffi:callback stable-pointer-destroy-notify)))

(export 'source-set-callback)

;;; ----------------------------------------------------------------------------
;;; GSourceFunc ()
;;; ----------------------------------------------------------------------------

(cffi:defcallback source-func :boolean
    ((data :pointer))
  (funcall (get-stable-pointer-value data)))

#+liber-documentation
(setf (liber:alias-for-symbol 'source-func)
      "Callback"
      (liber:symbol-documentation 'source-func)
 "@version{2022-11-22}
  @begin{short}
    Specifies the type of callback function passed to the @fun{g:timeout-add},
    and @fun{g:idle-add} functions.
  @end{short}
  @begin{pre}
lambda ()
  @end{pre}
  @begin[code]{table}
    @entry[Returns]{@em{False} if the source should be removed. The constants
      @var{+g-source-continue+} and @var{+g-source-remove+} are memorable names
      for the return value.}
  @end{table}
  @begin[Example]{dictionary}
    This example shows a timeout callback function, which runs 10 times and
    then quits the main loop.
    @begin{pre}
(let ((counter 0) (max 10))
  (defun timeout-callback (loop)
    (incf counter)
    (if (>= counter max)
        (progn
          ;; Reset the counter
          (setf counter 0)
          ;; Stop the main loop from running
          (g:main-loop-quit loop)
          ;; Stop the source
          +g-source-remove+)
        ;; Continue the source
        +g-source-continue+)))

(defun example-timeout-source ()
  (let* ((context (g:main-context-new))
         (mainloop (g:main-loop-new context nil))
         ;; Create a new timeout source
         (source (g:timeout-source-new 10)))
    ;; Attach source to context
    (g:source-attach source context)
    ;; Set the callback for source
    (g:source-set-callback source
                           (lambda ()
                             (timeout-callback mainloop)))
    ;; Start the main loop
    (g:main-loop-run mainloop)
    (g:main-loop-unref mainloop)))
    @end{pre}
  @end{dictionary}
  @see-function{g:timeout-add}
  @see-function{g:idle-add}")

(export 'source-func)

;;; ----------------------------------------------------------------------------
;;; g_source_set_callback_indirect ()
;;;
;;; void
;;; g_source_set_callback_indirect (GSource *source,
;;;                                 gpointer callback_data,
;;;                                 GSourceCallbackFuncs *callback_funcs);
;;;
;;; Sets the callback function storing the data as a refcounted callback
;;; "object". This is used internally. Note that calling
;;; g_source_set_callback_indirect() assumes an initial reference count on
;;; callback_data, and thus callback_funcs->unref will eventually be called
;;; once more than callback_funcs->ref.
;;;
;;; source :
;;;     the source
;;;
;;; callback_data :
;;;     pointer to callback data "object"
;;;
;;; callback_funcs :
;;;     functions for reference counting callback_data and getting the callback
;;;     and data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_set_ready_time ()
;;; g_source_get_ready_time () -> source-ready-time
;;; ----------------------------------------------------------------------------

(defun (setf source-ready-time) (time source)
  (cffi:foreign-funcall "g_source_set_ready_time"
                        (:pointer (:struct source)) source
                        :int64 time
                        :void)
  time)

(cffi:defcfun ("g_source_get_ready_time" source-ready-time) :int64
 #+liber-documentation
 "@version{2023-1-5}
  @syntax[]{(g:source-ready-time source) => time}
  @syntax[]{(setf (g:source-readey-time source) time)}
  @argument[source]{a @type{g:source} instance}
  @argument[time]{an integer with the monotonic time at which the source will
    be ready, 0 for \"immediately\", -1 for \"never\"}
  @begin{short}
    The @sym{g:source-ready-time} function gets the \"ready time\" of the
    source.
  @end{short}
  Any time before the current monotonic time, including 0, is an indication that
  the source will fire immediately. The @sym{(setf g:source-ready-time)}
  function sets a source to be dispatched when the given monotonic time is
  reached (or passed). If the monotonic time is in the past, as it always will
  be if @arg{time} is 0, then the source will be dispatched immediately. If
  @arg{time} is -1 then the source is never woken up on the basis of the
  passage of time. Dispatching the source does not reset the ready time. You
  should do so yourself, from the source dispatch function.

  Note that if you have a pair of sources where the ready time of one suggests
  that it will be delivered first but the priority for the other suggests that
  it would be delivered first, and the ready time for both sources is reached
  during the same main context iteration then the order of dispatch is
  undefined.
  @see-type{g:source}"
  (source (:pointer (:struct source))))

(export 'source-ready-time)

;;; ----------------------------------------------------------------------------
;;; g_source_add_unix_fd ()
;;;
;;; gpointer
;;; g_source_add_unix_fd (GSource *source,
;;;                       gint fd,
;;;                       GIOCondition events);
;;;
;;; Monitors fd for the IO events in events.
;;;
;;; The tag returned by this function can be used to remove or modify the
;;; monitoring of the fd using g_source_remove_unix_fd() or
;;; g_source_modify_unix_fd().
;;;
;;; It is not necessary to remove the fd before destroying the source; it will
;;; be cleaned up automatically.
;;;
;;; As the name suggests, this function is not available on Windows.
;;;
;;; source :
;;;     a GSource
;;;
;;; fd :
;;;     the fd to monitor
;;;
;;; events :
;;;     an event mask
;;;
;;; Returns :
;;;     an opaque tag
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_remove_unix_fd ()
;;;
;;; void g_source_remove_unix_fd (GSource *source, gpointer tag);
;;;
;;; Reverses the effect of a previous call to g_source_add_unix_fd().
;;;
;;; You only need to call this if you want to remove an fd from being watched
;;; while keeping the same source around. In the normal case you will just want
;;; to destroy the source.
;;;
;;; As the name suggests, this function is not available on Windows.
;;;
;;; source :
;;;     a GSource
;;;
;;; tag :
;;;     the tag from g_source_add_unix_fd()
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_modify_unix_fd ()
;;;
;;; void
;;; g_source_modify_unix_fd (GSource *source,
;;;                          gpointer tag,
;;;                          GIOCondition new_events);
;;;
;;; Updates the event mask to watch for the fd identified by tag.
;;;
;;; tag is the tag returned from g_source_add_unix_fd().
;;;
;;; If you want to remove a fd, don't set its event mask to zero. Instead, call
;;; g_source_remove_unix_fd().
;;;
;;; As the name suggests, this function is not available on Windows.
;;;
;;; source :
;;;     a GSource
;;;
;;; tag :
;;;     the tag from g_source_add_unix_fd()
;;;
;;; new_events :
;;;     the new event mask to watch
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_query_unix_fd ()
;;;
;;; GIOCondition g_source_query_unix_fd (GSource *source, gpointer tag);
;;;
;;; Queries the events reported for the fd corresponding to tag on source
;;; during the last poll.
;;;
;;; The return value of this function is only defined when the function is
;;; called from the check or dispatch functions for source.
;;;
;;; As the name suggests, this function is not available on Windows.
;;;
;;; source :
;;;     a GSource
;;;
;;; tag :
;;;     the tag from g_source_add_unix_fd()
;;;
;;; Returns :
;;;     the conditions reported on the fd
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_add_poll ()                                   not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_add_poll" source-add-poll) :void
 #+liber-documentation
 "@version{#2021-4-9}
  @argument[source]{a @type{source} instance}
  @argument[fd]{a @type{poll-fd} instance holding information about a file
    descriptor to watch}
  @begin{short}
    Adds a file descriptor to the set of file descriptors polled for this
    source.
  @end{short}
  This is usually combined with the @fun{source-new} function to add an event
  source. The event source's check function will typically test the revents
  field in the @type{poll-fd} instance and return @em{true} if events need
  to be processed.
  @see-type{source}
  @see-type{poll-fd}
  @see-function{source-new}"
  (source (:pointer (:struct source)))
  (fd (:pointer (:struct poll-fd))))

;;; ----------------------------------------------------------------------------
;;; g_source_remove_poll ()                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_remove_poll" source-remove-poll) :void
 #+liber-documentation
 "@version{#2013-5-13}
  @argument[source]{a @type{source} structure}
  @argument[fd]{a @type{poll-fd} structure previously passed to the
    @fun{source-add-poll} function}
  Removes a file descriptor from the set of file descriptors polled for this
  source."
  (source (:pointer (:struct source)))
  (fd (:pointer (:struct poll-fd))))

;;; ----------------------------------------------------------------------------
;;; g_source_add_child_source ()
;;;
;;; void g_source_add_child_source (GSource *source, GSource *child_source);
;;;
;;; Adds child_source to source as a "polled" source; when source is added to a
;;; GMainContext, child_source will be automatically added with the same
;;; priority, when child_source is triggered, it will cause source to dispatch
;;; (in addition to calling its own callback), and when source is destroyed, it
;;; will destroy child_source as well. (source will also still be dispatched if
;;; its own prepare/check functions indicate that it is ready.)
;;;
;;; If you don't need child_source to do anything on its own when it triggers,
;;; you can call g_source_set_dummy_callback() on it to set a callback that does
;;; nothing (except return TRUE if appropriate).
;;;
;;; source will hold a reference on child_source while child_source is attached
;;; to it.
;;;
;;; source :
;;;     a GSource
;;;
;;; child_source :
;;;     a second GSource that source should "poll"
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_remove_child_source ()
;;;
;;; void g_source_remove_child_source (GSource *source, GSource *child_source);
;;;
;;; Detaches child_source from source and destroys it.
;;;
;;; source :
;;;     a GSource
;;;
;;; child_source :
;;;     a GSource previously passed to g_source_add_child_source().
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_get_time () -> source-time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_get_time" source-time) :uint64
 #+liber-documentation
 "@version{2022-11-22}
  @argument[source]{a @type{g:source} instance}
  @return{An unsigned integer with the monotonic time in microseconds.}
  @begin{short}
    Gets the time to be used when checking this source.
  @end{short}
  The time here is the system monotonic time, if available, or some other
  reasonable alternative otherwise.
  @see-type{g:source}"
  (source (:pointer (:struct source))))

(export 'source-time)

;;; ----------------------------------------------------------------------------
;;; g_source_get_current_time ()
;;;
;;; void
;;; g_source_get_current_time (GSource *source, GTimeVal *timeval);
;;;
;;; g_source_get_current_time has been deprecated since version 2.28 and should
;;; not be used in newly written code.
;;;
;;; use g_source_get_time() instead
;;;
;;; This function ignores source and is otherwise the same as
;;; g_get_current_time().
;;;
;;; source :
;;;     a GSource
;;;
;;; timeval :
;;;     GTimeVal structure in which to store current time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_remove ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_remove" source-remove) :boolean
 #+liber-documentation
 "@version{2023-1-5}
  @argument[source]{an unsigned integer ID for the source to remove}
  @return{@em{True} if the source was found and removed.}
  @begin{short}
    Removes the source with the given ID from the default main context.
  @end{short}
  The ID of a @type{g:source} instance is given by the @fun{g:source-id}
  function, or will be returned by the @fun{g:source-attach}, @fun{g:idle-add},
  @fun{g:timeout-add} functions. You must use the @fun{g:source-destroy}
  function for sources added to a non-default main context.
  @see-type{g:source}
  @see-function{g:source-id}
  @see-function{g:source-attach}
  @see-function{g:idle-add}
  @see-function{g:timeout-add}
  @see-function{g:source-destroy}"
  (source :uint))

(export 'source-remove)

;;; ----------------------------------------------------------------------------
;;; g_source_remove_by_funcs_user_data ()                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_remove_by_funcs_user_data"
                source-remove-by-funcs-user-data) :boolean
 #+liber-documentation
 "@version{#2013-4-9}
  @argument[funcs]{The @arg{source-funcs} passed to @fun{source-new}}
  @argument[user-data]{the user data for the callback}
  @return{@em{True} if a source was found and removed.}
  @begin{short}
    Removes a source from the default main loop context given the source
    functions and user data.
  @end{short}
  If multiple sources exist with the same source functions and user data, only
  one will be destroyed.
  @see-function{source-new}"
  (funcs (:pointer (:struct source-funcs)))
  (user-data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_source_remove_by_user_data ()                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_source_remove_by_user_data" source-remove-by-user-data)
    :boolean
 #+liber-documentation
 "@version{#2013-4-9}
  @argument[user-data]{the @arg{user-data} for the callback}
  @return{@em{True} if a source was found and removed.}
  @begin{short}
    Removes a source from the default main loop context given the user data for
    the callback.
  @end{short}
  If multiple sources exist with the same user data, only one will be
  destroyed."
  (user-data :pointer))

;;; ----------------------------------------------------------------------------
;;; GClearHandleFunc ()
;;;
;;; void
;;; (*GClearHandleFunc) (guint handle_id);
;;;
;;; Specifies the type of function passed to g_clear_handle_id(). The
;;; implementation is expected to free the resource identified by handle_id ;
;;; for instance, if handle_id is a GSource ID, g_source_remove() can be used.
;;;
;;; handle_id :
;;;     the handle ID to clear
;;;
;;; Since 2.56
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_clear_handle_id ()
;;;
;;; void
;;; g_clear_handle_id (guint *tag_ptr, GClearHandleFunc clear_func);
;;;
;;; Clears a numeric handler, such as a GSource ID.
;;;
;;; tag_ptr must be a valid pointer to the variable holding the handler.
;;;
;;; If the ID is zero then this function does nothing. Otherwise, clear_func()
;;; is called with the ID as a parameter, and the tag is set to zero.
;;;
;;; A macro is also included that allows this function to be used without
;;; pointer casts.
;;;
;;; tag_ptr :
;;;     a pointer to the handler ID.
;;;
;;; clear_func :
;;;     the function to call to clear the handler.
;;;
;;; Since 2.56
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.main-loop.lisp  ---------------------------------------
