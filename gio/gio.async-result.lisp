;;; ----------------------------------------------------------------------------
;;; gio.async-result.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;;     g_async_result_legacy_propagate_error
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

(define-g-interface "GAsyncResult" async-result
  (:export t
   :type-initializer "g_async_result_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'async-result)
      "Interface"
      (documentation 'async-result 'type)
 "@version{#2023-5-6}
  @begin{short}
    Provides a base class for implementing asynchronous function results.
  @end{short}
  Asynchronous operations are broken up into two separate operations which are
  chained together by a @symbol{g:async-ready-callback} callback function. To
  begin an asynchronous operation, provide a @symbol{g:async-ready-callback}
  function to the asynchronous function. This callback will be triggered when
  the operation has completed, and must be run in a later iteration of the
  thread-default main context from where the operation was initiated. It will
  be passed a @sym{g:async-result} instance filled with the details of the
  operation's success or failure, the object the asynchronous function was
  started for and any error codes returned. The asynchronous callback function
  is then expected to call the corresponding @code{_finish()} function, passing
  the object the function was called for, the @sym{g:async-result} instance,
  and optionally an error to grab any error conditions that may have occurred.

  The @code{_finish()} function for an operation takes the generic result of
  type @sym{g:async-result} and returns the specific result that the operation
  in question yields, e.g. a @code{GFileEnumerator} for a \"enumerate children\"
  operation. If the result or error status of the operation is not needed, there
  is no need to call the @code{_finish()} function. GIO will take care of
  cleaning up the result and error information after the
  @symbol{g:async-ready-callback} function returns. You can pass @code{nil} for
  the @symbol{g:async-ready-callback} function if you do not need to take any
  action at all after the operation completes. Applications may also take a
  reference to the @sym{g:async-result} instance and call the @code{_finish()}
  function later. However, the @code{_finish()} function may be called at most
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
;;; GAsyncReadyCallback ()
;;;
;;; void
;;; (*GAsyncReadyCallback) (GObject *source_object,
;;;                         GAsyncResult *res,
;;;                         gpointer user_data);
;;;
;;;Type definition for a function that will be called back when an asynchronous operation within GIO has been completed. GAsyncReadyCallback callbacks from GTask are guaranteed to be invoked in a later iteration of the thread-default main context where the GTask was created. All other users of GAsyncReadyCallback must likewise call it asynchronously in a later iteration of the main context.

;;;Parameters
;;;source_object

;;;the object the asynchronous operation was started with.

;;;[nullable]
;;;res

;;;a GAsyncResult.

;;;
;;;user_data

;;;user data passed to the callback.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; g_async_result_get_user_data ()
;;;
;;; gpointer
;;; g_async_result_get_user_data (GAsyncResult *res);
;;;
;;; Gets the user data from a GAsyncResult.

;;;Parameters
;;;res

;;;a GAsyncResult.

;;;
;;;Returns
;;;the user data for res .

;;;[transfer full]

;;; ----------------------------------------------------------------------------
;;; g_async_result_get_source_object ()
;;;
;;; GObject *
;;; g_async_result_get_source_object (GAsyncResult *res);
;;;
;;; Gets the source object from a GAsyncResult.

;;;Parameters
;;;res

;;;a GAsyncResult

;;;
;;;Returns
;;;a new reference to the source object for the res , or NULL if there is none.


;;; ----------------------------------------------------------------------------
;;; g_async_result_is_tagged ()
;;;
;;; gboolean
;;; g_async_result_is_tagged (GAsyncResult *res,
;;;                           gpointer source_tag);
;;;
;;;Checks if res has the given source_tag (generally a function pointer indicating the function res was created by).

;;;Parameters
;;;res

;;;a GAsyncResult

;;;
;;;source_tag

;;;an application-defined tag

;;;
;;;Returns
;;;TRUE if res has the indicated source_tag , FALSE if not.

;;;Since: 2.34

;;; ----------------------------------------------------------------------------
;;; g_async_result_legacy_propagate_error ()
;;;
;;; gboolean
;;; g_async_result_legacy_propagate_error (GAsyncResult *res,
;;;                                        GError **error);
;;;
;;;If res is a GSimpleAsyncResult, this is equivalent to g_simple_async_result_propagate_error(). Otherwise it returns FALSE.

;;;This can be used for legacy error handling in async *_finish() wrapper functions that traditionally handled GSimpleAsyncResult error returns themselves rather than calling into the virtual method. This should not be used in new code; GAsyncResult errors that are set by virtual methods should also be extracted by virtual methods, to enable subclasses to chain up correctly.

;;;Parameters
;;;res

;;;a GAsyncResult

;;;
;;;error

;;;a location to propagate the error to.

;;;[out]
;;;Returns
;;;TRUE if error is has been filled in with an error from res , FALSE if not.

;;;Since: 2.34
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.async-result.lisp --------------------------------------