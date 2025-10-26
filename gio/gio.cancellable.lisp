;;; ----------------------------------------------------------------------------
;;; gio.cancellable.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 - 2025 Dieter Kaiser
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
;;; GCancellable
;;;
;;;     Thread-safe Operation Cancellation Stack
;;;
;;; Types and Values
;;;
;;;     GCancellable
;;;
;;; Functions
;;;
;;;     GCancellableSourceFunc
;;;
;;;     g_cancellable_new
;;;     g_cancellable_is_cancelled
;;;     g_cancellable_set_error_if_cancelled                not implemented
;;;     g_cancellable_get_fd                                not implemented
;;;     g_cancellable_make_pollfd                           not implemented
;;;     g_cancellable_release_fd                            not implemented
;;;     g_cancellable_source_new
;;;     g_cancellable_get_current
;;;     g_cancellable_pop_current
;;;     g_cancellable_push_current
;;;     g_cancellable_reset
;;;     g_cancellable_connect
;;;     g_cancellable_disconnect
;;;     g_cancellable_cancel
;;;
;;; Signals
;;;
;;;     cancelled
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GCancellable
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GCancellable
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GCancellable" cancellable
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_cancellable_get_type")
  nil)

#+liber-documentation
(setf (documentation 'cancellable 'type)
 "@version{2025-06-29}
  @begin{short}
    The @class{gio:cancellable} object allows operations to be cancelled.
  @end{short}
  The @class{g:cancellable} object is a thread-safe operation cancellation
  stack used throughout GIO to allow for cancellation of synchronous and
  asynchronous operations.
  @begin[Signal Details]{dictionary}
    @begin[cancellable::cancelled]{signal}
      @begin{pre}
lambda (cancellable)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[cancellable]{The @class{g:cancellable} object.}
      @end{simple-table}
      Emitted when the operation has been cancelled. Can be used by
      implementations of cancellable operations. If the operation is cancelled
      from another thread, the signal will be emitted in the thread that
      cancelled the operation, not the thread that is running the operation.

      Note that disconnecting from this signal, or any signal, in a
      multi-threaded program is prone to race conditions. For instance it is
      possible that a signal handler may be invoked even after a call to the
      @fun{g:signal-handler-disconnect} function for that handler has already
      returned.

      There is also a problem when cancellation happen right before connecting
      to the signal. If this happens the signal will unexpectedly not be
      emitted, and checking before connecting to the signal leaves a race
      condition where this is still happening.

      In order to make it safe and easy to connect handlers there are two
      helper functions: the @fun{g:cancellable-connect} and
      @fun{g:cancellable-disconnect} functions which protect against problems
      like this. An example of how to us this:
      @begin{pre}
;; Make sure we do not do any unnecessary work if already cancelled
(unless (g:cancellable-is-cancelled cancellable)
  (when cancellable
    (let ((id (g:cancellable-connect cancellable #'cancelled-handler)))

      ;; cancellable operation here ...

     (g:cancellable-disconnect cancellable id))))
      @end{pre}
      Note that the cancelled signal is emitted in the thread that the user
      cancelled from, which may be the main thread. So, the cancellable signal
      should not do something that can block.
    @end{signal}
  @end{dictionary}
  @see-constructor{g:cancellable-new}
  @see-function{g:cancellable-connect}
  @see-function{g:cancellable-disconnect}")

;;; ----------------------------------------------------------------------------
;;; g_cancellable_new
;;; ----------------------------------------------------------------------------

(declaim (inline cancellable-new))

(defun cancellable-new ()
 #+liber-documentation
 "@version{2025-05-27}
  @return{The newly created @class{g:cancellable} object.}
  @begin{short}
    Creates a new @class{g:cancellable} object.
  @end{short}
  Applications that want to start one or more operations that should be
  cancellable should create a @class{g:cancellable} object and pass it to the
  operations.

  One @class{g:cancellable} object can be used in multiple consecutive
  operations or in multiple concurrent operations.
  @see-class{g:cancellable}"
  (make-instance 'cancellable))

(export 'cancellable-new)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_is_cancelled
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_cancellable_is_cancelled" cancellable-is-cancelled) :boolean
 #+liber-documentation
 "@version{2025-05-27}
  @argument[cancellable]{a @class{g:cancellable} object, or @code{nil}}
  @begin{return}
    @em{True} if @arg{cancellable} is cancelled, @em{false} if called
    with @code{nil} or if not cancelled.
  @end{return}
  @begin{short}
    Checks if a cancellable job has been cancelled.
  @end{short}
  @see-class{g:cancellable}"
  (cancellable (gobject:object cancellable)))

(export 'cancellable-is-cancelled)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_set_error_if_cancelled ()
;;;
;;; gboolean g_cancellable_set_error_if_cancelled (GCancellable *cancellable,
;;;                                                GError **error);
;;;
;;; If the cancellable is cancelled, sets the error to notify that the operation
;;; was cancelled.
;;;
;;; cancellable :
;;;     a GCancellable or NULL
;;;
;;; error :
;;;     GError to append error state to
;;;
;;; Returns :
;;;     TRUE if cancellable was cancelled, FALSE if it was not
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_get_fd ()
;;;
;;; int g_cancellable_get_fd (GCancellable *cancellable);
;;;
;;; Gets the file descriptor for a cancellable job. This can be used to
;;; implement cancellable operations on Unix systems. The returned fd will turn
;;; readable when cancellable is cancelled.
;;;
;;; You are not supposed to read from the fd yourself, just check for readable
;;; status. Reading to unset the readable status is done with
;;; g_cancellable_reset().
;;;
;;; After a successful return from this function, you should use
;;; g_cancellable_release_fd() to free up resources allocated for the returned
;;; file descriptor.
;;;
;;; See also g_cancellable_make_pollfd().
;;;
;;; cancellable :
;;;     a GCancellable.
;;;
;;; Returns :
;;;     A valid file descriptor. -1 if the file descriptor is not supported, or
;;;     on errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_make_pollfd ()
;;;
;;; gboolean g_cancellable_make_pollfd (GCancellable *cancellable,
;;;                                     GPollFD *pollfd);
;;;
;;; Creates a GPollFD corresponding to cancellable; this can be passed to
;;; g_poll() and used to poll for cancellation. This is useful both for unix
;;; systems without a native poll and for portability to windows.
;;;
;;; When this function returns TRUE, you should use g_cancellable_release_fd()
;;; to free up resources allocated for the pollfd. After a FALSE return, do not
;;; call g_cancellable_release_fd().
;;;
;;; If this function returns FALSE, either no cancellable was given or resource
;;; limits prevent this function from allocating the necessary structures for
;;; polling. (On Linux, you will likely have reached the maximum number of file
;;; descriptors.) The suggested way to handle these cases is to ignore the
;;; cancellable.
;;;
;;; You are not supposed to read from the fd yourself, just check for readable
;;; status. Reading to unset the readable status is done with
;;; g_cancellable_reset().
;;;
;;; cancellable :
;;;     a GCancellable or NULL
;;;
;;; pollfd :
;;;     a pointer to a GPollFD
;;;
;;; Returns :
;;;     TRUE if pollfd was successfully initialized, FALSE on failure to prepare
;;;     the cancellable.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_release_fd ()
;;;
;;; void g_cancellable_release_fd (GCancellable *cancellable);
;;;
;;; Releases a resources previously allocated by g_cancellable_get_fd() or
;;; g_cancellable_make_pollfd().
;;;
;;; For compatibility reasons with older releases, calling this function is not
;;; strictly required, the resources will be automatically freed when the
;;; cancellable is finalized. However, the cancellable will block scarce file
;;; descriptors until it is finalized if this function is not called. This can
;;; cause the application to run out of file descriptors when many GCancellables
;;; are used at the same time.
;;;
;;; cancellable :
;;;     a GCancellable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GCancellableSourceFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback cancellable-source-func :boolean
    ((cancellable (gobject:object cancellable))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func cancellable)
      (return-from-cancellable-source-func () nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'cancellable-source-func)
      "Callback"
      (liber:symbol-documentation 'cancellable-source-func)
 "@version{#2025-10-26}
  @syntax{lambda (cancellable) => result}
  @argument[cancellable]{a @class{g:cancellable} object}
  @argument[result]{@em{false} if the source should be removed}
  @begin{short}
    This is the function type of the callback used for the @type{g:source}
    instance returned by the @fun{g:cancellable-source-new} function.
  @end{short}
  @see-class{g:cancellable}
  @see-type{g:source}
  @see-function{g:cancellable-source-new}")

(export 'cancellable-source-func)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_source_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_cancellable_soure_new" cancellable-source-new)
    (:pointer (:struct glib:source))
 #+liber-documentation
 "@version{#2025-05-27}
  @argument[cancellable]{a @class{g:cancellable} object}
  @return{The new @type{g:source} instance.}
  @begin{short}
    Creates a source that triggers if @arg{cancellable} is cancelled and calls
    its @symbol{g:cancellable-source-func} callback function.
  @end{short}
  This is primarily useful for attaching to another (non-cancellable) source
  with the @fun{g:source-add-child-source} function to add cancellability to it.

  For convenience, you can call this function with a @code{nil} value for
  @arg{cancellable}, in which case the source will never trigger.
  @see-class{g:cancellable}
  @see-type{g:source}
  @see-symbol{g:cancellable-source-func}
  @see-function{g:source-add-child-source}"
  (cancellable (gobject:object cancellable)))

(export 'cancellable-source-new)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_get_current
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_cancellable_get_current" cancellable-current)
    (gobject:object cancellable)
 #+liber-documentation
 "@version{#2025-05-27}
  @begin{return}
    The @class{g:cancellable} object from the top of the stack, or @code{nil}
    if the stack is empty.
  @end{return}
  @short{Gets the top cancellable from the stack.}
  @see-class{g:cancellable}")

(export 'cancellable-current)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_pop_current
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_cancellable_pop_current" cancellable-pop-current) :void
 #+liber-documentation
 "@version{#2025-05-27}
  @argument[cancellable]{a @class{g:cancellable} object}
  @begin{short}
    Pops @arg{cancellable} off the cancellable stack, verifying that cancellable
    is on the top of the stack.
  @end{short}
  @see-class{g:cancellable}"
  (cancellalbe (gobject:object cancellable)))

(export 'cancellable-pop-current)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_push_current
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_cancellable_push_current" cancellable-push-current) :void
 #+liber-documentation
 "@version{#2025-05-27}
  @argument[cancellable]{a @class{g:cancellable} object}
  @begin{short}
    Pushes @arg{cancellable} onto the cancellable stack.
  @end{short}
  The current cancellable can then be received using the
  @fun{g:cancellable-current} function.

  This is useful when implementing cancellable operations in code that does not
  allow you to pass down the cancellable object.

  This is typically called automatically by, for example @code{GFile}
  operations, so you rarely have to call this yourself.
  @see-class{g:cancellable}
  @see-function{g:cancellable-current}"
  (cancellable (gobject:object cancellable)))

(export 'cancellable-push-current)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_reset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_cancellable_reset" cancellable-reset) :void
 #+liber-documentation
 "@version{2025-05-27}
  @argument[cancellable]{a @class{g:cancellable} object}
  @begin{short}
    Resets @arg{cancellable} to its uncancelled state.
  @end{short}
  If @arg{cancellable} is currently in use by any cancellable operation then
  the behavior of this function is undefined.
  @see-class{g:cancellable}"
  (cancellabel (gobject:object cancellable)))

(export 'cancellable-reset)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_connect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_cancellable_connect" %cancellable-connect) :ulong
  (cancellable (gobject:object cancellable))
  (callback :pointer)
  (data :pointer)
  (notify :pointer))

(defun cancellable-connect (cancellable func)
 #+liber-documentation
 "@version{2025-10-26}
  @argument[cancellable]{a @class{g:cancellable} object}
  @argument[func]{a @symbol{g:callback} callback function}
  @begin{return}
    The unsigned integer for the ID of the signal handler or 0 if
    @arg{cancellable} has already been cancelled.
  @end{return}
  @begin{short}
    Convenience function to connect to the @sig[g:cancellable]{cancelled}
    signal.
  @end{short}
  Also handles the race condition that may happen if the cancellable is
  cancelled right before connecting.

  The @arg{func} function is called at most once, either directly at the time
  of the connect if cancellable is already cancelled, or when cancellable is
  cancelled in some thread.

  See the @sig[g:cancellable]{cancelled} signal for details on how to use this.
  @see-class{g:cancellable}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%cancellable-connect cancellable
                          (cffi:callback gobject:callback)
                          ptr
                          (cffi:callback glib:stable-pointer-destroy-notify))))

(export 'cancellable-connect)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_disconnect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_cancellable_disconnect" cancellable-disconnect) :void
 #+liber-documentation
 "@version{2025-10-26}
  @argument[cancellable]{a @class{g:cancellable} object}
  @argument[id]{an unsigned integer for the handler ID of the handler to be
    disconnected, or 0}
  @begin{short}
    Disconnects a handler from a cancellable instance similar to the
    @fun{g:signal-handler-disconnect} function.
  @end{short}
  Additionally, in the event that a signal handler is currently running, this
  call will block until the handler has finished. Calling this function from a
  @sig[g:cancellable]{cancelled} signal handler will therefore result in a
  deadlock.

  This avoids a race condition where a thread cancels at the same time as the
  cancellable operation is finished and the signal handler is removed. See the
  @sig[g:cancellable]{cancelled} signal handler for details on how to use this.

  If @arg{cancellable} is @code{nil} or @arg{ID} is 0 this function does
  nothing.
  @see-class{g:cancellable}
  @see-symbol{g:object}
  @see-function{g:signal-handler-disconnect}"
  (cancellable (gobject:object cancellable))
  (id :ulong))

(export 'cancellable-disconnect)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_cancel
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_cancellable_cancel" cancellable-cancel) :void
 #+liber-documentation
 "@version{2025-10-26}
  @argument[cancellable]{a @class{g:cancellable} object}
  @begin{short}
    Will set @arg{cancellable} to cancelled, and will emit the
    @sig[g:cancellable]{cancelled} signal.
  @end{short}
  However, see the warning about race conditions in the documentation for that
  signal if you are planning to connect to it.

  This function is thread-safe. In other words, you can safely call it from a
  thread other than the one running the operation that was passed the
  cancellable.

  The convention within GIO is that cancelling an asynchronous operation causes
  it to complete asynchronously. That is, if you cancel the operation from the
  same thread in which it is running, then the operation's
  @symbol{g:async-ready-callback} callback function will not be invoked until
  the application returns to the main loop.
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}"
  (cancellable (gobject:object cancellable)))

(export 'cancellable-cancel)

;;; --- End of file gio.cancellable.lisp ---------------------------------------
