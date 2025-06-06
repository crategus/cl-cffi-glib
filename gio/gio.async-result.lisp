;;; ----------------------------------------------------------------------------
;;; gio.async-result.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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
;;; GAsyncResult
;;;
;;;     Asynchronous Function Results
;;;
;;; Types and Values
;;;
;;;     GAsyncResult
;;;
;;; Functions
;;;
;;;     GAsyncReadyCallback
;;;
;;;     g_async_result_get_user_data
;;;     g_async_result_get_source_object
;;;     g_async_result_is_tagged
;;;     g_async_result_legacy_propagate_error               not implemented
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GAsyncResult
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GAsyncResult
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GAsyncResult" async-result
  (:export t
   :type-initializer "g_async_result_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'async-result)
      "Interface"
      (documentation 'async-result 'type)
 "@version{2025-05-27}
  @begin{short}
    Provides a base class for implementing asynchronous function results.
  @end{short}
  Asynchronous operations are broken up into two separate operations which are
  chained together by a @symbol{g:async-ready-callback} callback function. To
  begin an asynchronous operation, provide a @symbol{g:async-ready-callback}
  function to the asynchronous function. This callback will be triggered when
  the operation has completed, and must be run in a later iteration of the
  thread-default main context from where the operation was initiated. It will
  be passed a @class{g:async-result} instance filled with the details of the
  operation's success or failure, the object the asynchronous function was
  started for and any error codes returned. The asynchronous callback function
  is then expected to call the corresponding @sym{-finish} function, passing
  the object the function was called for, the @class{g:async-result} instance,
  and optionally an error to grab any error conditions that may have occurred.

  The @sym{-finish} function for an operation takes the generic result of
  type @class{g:async-result} and returns the specific result that the
  operation in question yields, for example a @code{GFileEnumerator} for a
  \"enumerate children\" operation. If the result or error status of the
  operation is not needed, there is no need to call the @sym{-finish} function.
  GIO will take care of cleaning up the result and error information after the
  @symbol{g:async-ready-callback} function returns. You can pass @code{nil} for
  the @symbol{g:async-ready-callback} function if you do not need to take any
  action at all after the operation completes. Applications may also take a
  reference to the @class{g:async-result} instance and call the @sym{-finish}
  function later. However, the @sym{-finish} function may be called at most
  once.

  Example of a typical asynchronous operation flow:
  @begin{pre}
void _theoretical_frobnitz_async (Theoretical         *t,
                                  GCancellable        *c,
                                  GAsyncReadyCallback  cb,
                                  gpointer             u);

gboolean _theoretical_frobnitz_finish (Theoretical   *t,
                                       GAsyncResult  *res,
                                       GError       **e);

static void
frobnitz_result_func (GObject      *source_object,
         GAsyncResult *res,
         gpointer      user_data)
{
  gboolean success = FALSE;

  success = _theoretical_frobnitz_finish (source_object, res, NULL);

  if (success)
    g_printf (\"Hurray!\n\");
  else
    g_printf (\"Uh oh!\n\");

  ...

@}

int main (int argc, void *argv[])
{
  ...

  _theoretical_frobnitz_async (theoretical_data,
                                NULL,
                                frobnitz_result_func,
                                NULL);

  ...
@}
  @end{pre}
  The callback for an asynchronous operation is called only once, and is always
  called, even in the case of a cancelled operation. On cancellation the result
  is a @code{G_IO_ERROR_CANCELLED} error.

  @subheading{I/O Priority}
  Many I/O-related asynchronous operations have a priority parameter, which is
  used in certain cases to determine the order in which operations are executed.
  They are not used to determine system-wide I/O scheduling. Priorities are
  integers, with lower numbers indicating higher priority. It is recommended to
  choose priorities between @code{G_PRIORITY_LOW} and @code{G_PRIORITY_HIGH},
  with @code{G_PRIORITY_DEFAULT} as a default.
  @see-class{g:task}")

;;; ----------------------------------------------------------------------------
;;; GAsyncReadyCallback
;;; ----------------------------------------------------------------------------

;; TODO: We free the allocated pointer in an UNWIND-PROTECT macro. Is this safe?

(cffi:defcallback async-ready-callback :void
    ((source gobject:object)
     (result (gobject:object async-result))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (unwind-protect
      (funcall func source result))
      (glib:free-stable-pointer data)))

#+liber-documentation
(setf (liber:alias-for-symbol 'async-ready-callback)
      "Callback"
      (liber:symbol-documentation 'async-ready-callback)
 "@version{#2025-05-27}
  @begin{declaration}
lambda (source result)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[source]{The @class{g:object} instance the asynchronous operation
        was started with.}
      @entry[result]{The @class{g:async-result} object.}
    @end{table}
  @end{values}
  @begin{short}
    Type definition for a function that will be called back when an asynchronous
    operation within GIO has been completed.
  @end{short}
  The @symbol{g:async-ready-callback} callbacks from the @class{g:task} object
  are guaranteed to be invoked in a later iteration of the thread-default main
  context where the @class{g:task} object was created. All other users of the
  @symbol{g:async-ready-callback} function must likewise call it asynchronously
  in a later iteration of the main context.
  @see-class{g:async-result}
  @see-class{g:object}
  @see-class{g:task}")

(export 'async-ready-callback)

;;; ----------------------------------------------------------------------------
;;; g_async_result_get_user_data
;;; ----------------------------------------------------------------------------

;; TODO: Is this function useful?!

(cffi:defcfun ("g_async_result_get_user_data" async-result-user-data) :pointer
 #+liber-documentation
 "@version{#2025-05-27}
  @argument[result]{a @class{g:async-result} instance}
  @return{The foreign pointer to the user data for @arg{result}.}
  @begin{short}
    Gets the user data from a @class{g:async-result} instance.
  @end{short}
  @see-class{g:async-result}"
  (result (gobject:object async-result)))

(export 'async-result-user-data)

;;; ----------------------------------------------------------------------------
;;; g_async_result_get_source_object
;;; ----------------------------------------------------------------------------

;; TODO: Check the memory management!

(cffi:defcfun ("g_async_result_get_source_object" async-result-source-object)
    gobject:object
 #+liber-documentation
 "@version{#2025-05-27}
  @argument[result]{a @class{g:async-result} instance}
  @begin{return}
    The @class{g:object} instance with the new reference to the source object
    for @arg{result}, or @code{nil} if there is none.
  @end{return}
  @begin{short}
    Gets the source object from a @class{g:async-result} instance.
  @end{short}
  @see-class{g:async-result}"
  (result (gobject:object async-result)))

(export 'async-result-source-object)

;;; ----------------------------------------------------------------------------
;;; g_async_result_is_tagged
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_async_result_is_tagged" async-result-is-tagged) :boolean
 #+liber-documentation
 "@version{#2025-05-27}
  @argument[result]{a @class{g:async-result} instance}
  @argument[tag]{a foreign pointer to an application defined tag}
  @begin{return}
    @em{True} if @arg{result} has the indicated @arg{tag}, @em{false} if not.
  @end{return}
  @begin{short}
    Checks if @arg{result} has the given @arg{tag}, generally a function
    pointer indicating the @arg{result} function was created by.
  @end{short}
  @see-class{g:async-result}"
  (result (gobject:object async-result))
  (tag :pointer))

(export 'async-result-is-tagged)

;;; ----------------------------------------------------------------------------
;;; g_async_result_legacy_propagate_error ()
;;;
;;; gboolean
;;; g_async_result_legacy_propagate_error (GAsyncResult *res,
;;;                                        GError **error);
;;;
;;; If res is a GSimpleAsyncResult, this is equivalent to
;;; g_simple_async_result_propagate_error(). Otherwise it returns FALSE.
;;;
;;; This can be used for legacy error handling in async *_finish() wrapper
;;; functions that traditionally handled GSimpleAsyncResult error returns
;;; themselves rather than calling into the virtual method. This should not be
;;; used in new code; GAsyncResult errors that are set by virtual methods
;;; should  also be extracted by virtual methods, to enable subclasses to chain
;;; up correctly.
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a location to propagate the error to.
;;;
;;; Returns :
;;;     TRUE if error is has been filled in with an error from res , FALSE if
;;;     not.
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.async-result.lisp --------------------------------------
