;;; ----------------------------------------------------------------------------
;;; gio.task.lisp
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
;;; GTask
;;;
;;;     Cancellable synchronous or asynchronous task and result
;;;
;;; Types and Values
;;;
;;;     GTask
;;;
;;; Accessors
;;;
;;;     g_task_get_completed
;;;
;;; Functions
;;;
;;;     g_task_new
;;;     g_task_set_task_data
;;;     g_task_get_task_data
;;;     g_task_set_priority
;;;     g_task_get_priority
;;;     g_task_set_check_cancellable
;;;     g_task_get_check_cancellable
;;;     g_task_set_return_on_cancel
;;;     g_task_get_return_on_cancel
;;;     g_task_set_source_tag
;;;     g_task_get_source_tag
;;;     g_task_set_name
;;;     g_task_get_name
;;;     g_taske_set_static_name                            Since 2.76
;;;     g_task_report_error
;;;     g_task_report_new_error
;;;     g_task_get_cancellable
;;;     g_task_get_context
;;;     g_task_get_source_object
;;;     g_task_return_boolean
;;;     g_task_return_int
;;;     g_task_return_pointer
;;;     g_task_return_value
;;;     g_task_return_error
;;;     g_task_return_new_error
;;;     g_task_return_error_if_cancelled
;;;     g_task_propagate_boolean
;;;     g_task_propagate_int
;;;     g_task_propagate_pointer
;;;     g_task_propagate_value
;;;     g_task_had_error
;;;     g_task_run_in_thread
;;;     g_task_run_in_thread_sync
;;;     GTaskThreadFunc
;;;     g_task_attach_source
;;;     g_task_is_valid
;;;
;;; Properties
;;;
;;;     completed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GTask
;;;
;;; Implemented Interfaces
;;;
;;;     GAsyncResult
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GTask
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GTask" task
  (:superclass gobject:object
   :export t
   :interfaces ("GAsyncResult")
   :type-initializer "g_task_get_type")
  ((completed
    task-completed
    "completed" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'task 'type)
 "@version{2023-5-7}
  @begin{short}
    A @sym{g:task} instance represents and manages a cancellable \"task\".
  @end{short}

  @subheading{Asynchronous operations}
  The most common usage of @sym{g:task} object is as a @class{g:async-result}
  object, to manage data during an asynchronous operation. You call the
  @fun{g:task-new} function in the \"start\" method, followed by the
  @fun{g:task-set-task-data} function and the like if you need to keep some
  additional data associated with the task, and then pass the task object around
  through your asynchronous operation. Eventually, you will call a method such
  as the @fun{g:task-return-pointer} or the @fun{g:task-return-error} function,
  which will save the value you give it and then invoke the task's callback
  function in the thread-default main context where it was created (waiting
  until the next iteration of the main loop first, if necessary). The caller
  will pass the @sym{g:task} object back to the operation's finish function (as
  a @class{g:async-result} object), and you can use the
  @fun{g:task-propagate-pointer} function or the like to extract the return
  value.

  Here is an example for using the @sym{g:task} object as a
  @class{g:async-result} object:
  @begin{pre}
typedef struct {
  CakeFrostingType frosting;
  char *message;
@} DecorationData;

static void
decoration_data_free (DecorationData *decoration)
{
  g_free (decoration->message);
  g_slice_free (DecorationData, decoration);
@}

static void
baked_cb (Cake     *cake,
          gpointer  user_data)
{
  GTask *task = user_data;
  DecorationData *decoration = g_task_get_task_data (task);
  GError *error = NULL;

  if (cake == NULL)
    {
      g_task_return_new_error (task, BAKER_ERROR, BAKER_ERROR_NO_FLOUR,
                               \"Go to the supermarket\");
      g_object_unref (task);
      return;
    @}

  if (!cake_decorate (cake, decoration->frosting, decoration->message, &error))
    {
      g_object_unref (cake);
      // g_task_return_error() takes ownership of error
      g_task_return_error (task, error);
      g_object_unref (task);
      return;
    @}

  g_task_return_pointer (task, cake, g_object_unref);
  g_object_unref (task);
@}

void
baker_bake_cake_async (Baker               *self,
                       guint                radius,
                       CakeFlavor           flavor,
                       CakeFrostingType     frosting,
                       const char          *message,
                       GCancellable        *cancellable,
                       GAsyncReadyCallback  callback,
                       gpointer             user_data)
{
  GTask *task;
  DecorationData *decoration;
  Cake  *cake;

  task = g_task_new (self, cancellable, callback, user_data);
  if (radius < 3)
    {
      g_task_return_new_error (task, BAKER_ERROR, BAKER_ERROR_TOO_SMALL,
                               \"%ucm radius cakes are silly\",
                               radius);
      g_object_unref (task);
      return;
    @}

  cake = _baker_get_cached_cake (self, radius, flavor, frosting, message);
  if (cake != NULL)
    {
      // _baker_get_cached_cake() returns a reffed cake
      g_task_return_pointer (task, cake, g_object_unref);
      g_object_unref (task);
      return;
    @}

  decoration = g_slice_new (DecorationData);
  decoration->frosting = frosting;
  decoration->message = g_strdup (message);
  g_task_set_task_data (task, decoration, (GDestroyNotify) decoration_data_free);

  _baker_begin_cake (self, radius, flavor, cancellable, baked_cb, task);
@}

Cake *
baker_bake_cake_finish (Baker         *self,
                        GAsyncResult  *result,
                        GError       **error)
{
  g_return_val_if_fail (g_task_is_valid (result, self), NULL);

  return g_task_propagate_pointer (G_TASK (result), error);
@}
  @end{pre}
  @subheading{Chained asynchronous operations}
  The @sym{g:task} object also tries to simplify asynchronous operations that
  internally chain together several smaller asynchronous operations. The
  @fun{g:task-get-cancellable}, @fun{g:task-get-context}, and
  @fun{g:task-get-priority} functions allow you to get back the task's
  @class{g:cancellable} object, @type{g:main-context} instance, and I/O priority
  when starting a new subtask, so you do not have to keep track of them
  yourself. The @fun{g:task-attach-source} function simplifies the case of
  waiting for a source to fire (automatically using the correct the
  @type{g:main-context} instance and priority).

  Here is an example for chained asynchronous operations:
  @begin{pre}
typedef struct {
  Cake *cake;
  CakeFrostingType frosting;
  char *message;
@} BakingData;

static void
decoration_data_free (BakingData *bd)
{
  if (bd->cake)
    g_object_unref (bd->cake);
  g_free (bd->message);
  g_slice_free (BakingData, bd);
@}

static void
decorated_cb (Cake         *cake,
              GAsyncResult *result,
              gpointer      user_data)
{
  GTask *task = user_data;
  GError *error = NULL;

  if (!cake_decorate_finish (cake, result, &error))
    {
      g_object_unref (cake);
      g_task_return_error (task, error);
      g_object_unref (task);
      return;
    @}

  // baking_data_free() will drop its ref on the cake, so we have to
  // take another here to give to the caller.
  g_task_return_pointer (task, g_object_ref (cake), g_object_unref);
  g_object_unref (task);
@}

static gboolean
decorator_ready (gpointer user_data)
{
  GTask *task = user_data;
  BakingData *bd = g_task_get_task_data (task);

  cake_decorate_async (bd->cake, bd->frosting, bd->message,
                       g_task_get_cancellable (task),
                       decorated_cb, task);

  return G_SOURCE_REMOVE;
@}

static void
baked_cb (Cake     *cake,
          gpointer  user_data)
{
  GTask *task = user_data;
  BakingData *bd = g_task_get_task_data (task);
  GError *error = NULL;

  if (cake == NULL)
    {
      g_task_return_new_error (task, BAKER_ERROR, BAKER_ERROR_NO_FLOUR,
                               \"Go to the supermarket\");
      g_object_unref (task);
      return;
    @}

  bd->cake = cake;

  // Bail out now if the user has already cancelled
  if (g_task_return_error_if_cancelled (task))
    {
      g_object_unref (task);
      return;
    @}

  if (cake_decorator_available (cake))
    decorator_ready (task);
  else
    {
      GSource *source;

      source = cake_decorator_wait_source_new (cake);
      // Attach @@source to @@task's GMainContext and have it call
      // decorator_ready() when it is ready.
      g_task_attach_source (task, source, decorator_ready);
      g_source_unref (source);
    @}
@}

void
baker_bake_cake_async (Baker               *self,
                       guint                radius,
                       CakeFlavor           flavor,
                       CakeFrostingType     frosting,
                       const char          *message,
                       gint                 priority,
                       GCancellable        *cancellable,
                       GAsyncReadyCallback  callback,
                       gpointer             user_data)
{
  GTask *task;
  BakingData *bd;

  task = g_task_new (self, cancellable, callback, user_data);
  g_task_set_priority (task, priority);

  bd = g_slice_new0 (BakingData);
  bd->frosting = frosting;
  bd->message = g_strdup (message);
  g_task_set_task_data (task, bd, (GDestroyNotify) baking_data_free);

  _baker_begin_cake (self, radius, flavor, cancellable, baked_cb, task);
@}

Cake *
baker_bake_cake_finish (Baker         *self,
                        GAsyncResult  *result,
                        GError       **error)
{
  g_return_val_if_fail (g_task_is_valid (result, self), NULL);

  return g_task_propagate_pointer (G_TASK (result), error);
@}
  @end{pre}
  @subheding{Asynchronous operations from synchronous ones}
  You can use the @fun{g:task-run-in-thread} function to turn a synchronous
  operation into an asynchronous one, by running it in a thread. When it
  completes, the result will be dispatched to the thread-default main context
  where the @sym{g:task} object was created.

  Running a task in a thread:
  @begin{pre}
typedef struct {
  guint radius;
  CakeFlavor flavor;
  CakeFrostingType frosting;
  char *message;
@} CakeData;

static void
cake_data_free (CakeData *cake_data)
{
  g_free (cake_data->message);
  g_slice_free (CakeData, cake_data);
@}

static void
bake_cake_thread (GTask         *task,
                  gpointer       source_object,
                  gpointer       task_data,
                  GCancellable  *cancellable)
{
  Baker *self = source_object;
  CakeData *cake_data = task_data;
  Cake *cake;
  GError *error = NULL;

  cake = bake_cake (baker, cake_data->radius, cake_data->flavor,
                    cake_data->frosting, cake_data->message,
                    cancellable, &error);
  if (cake)
    g_task_return_pointer (task, cake, g_object_unref);
  else
    g_task_return_error (task, error);
@}

void
baker_bake_cake_async (Baker               *self,
                       guint                radius,
                       CakeFlavor           flavor,
                       CakeFrostingType     frosting,
                       const char          *message,
                       GCancellable        *cancellable,
                       GAsyncReadyCallback  callback,
                       gpointer             user_data)
{
  CakeData *cake_data;
  GTask *task;

  cake_data = g_slice_new (CakeData);
  cake_data->radius = radius;
  cake_data->flavor = flavor;
  cake_data->frosting = frosting;
  cake_data->message = g_strdup (message);
  task = g_task_new (self, cancellable, callback, user_data);
  g_task_set_task_data (task, cake_data, (GDestroyNotify) cake_data_free);
  g_task_run_in_thread (task, bake_cake_thread);
  g_object_unref (task);
@}

Cake *
baker_bake_cake_finish (Baker         *self,
                        GAsyncResult  *result,
                        GError       **error)
{
  g_return_val_if_fail (g_task_is_valid (result, self), NULL);

  return g_task_propagate_pointer (G_TASK (result), error);
@}
  @end{pre}
  @subheading{Adding cancellability to uncancellable tasks}
  Finally, the @fun{g:task-run-in-thread} and @fun{g:task-run-in-thread-sync}
  functions can be used to turn an uncancellable operation into a cancellable
  one. If you call the @fun{g:task-set-return-on-cancel} function, passing
  @em{true}, then if the task's @clss{g:cancellable} object is cancelled, it
  will return control back to the caller immediately, while allowing the task
  thread to continue running in the background (and simply discarding its result
  when it finally does finish). Provided that the task thread is careful about
  how it uses locks and other externally-visible resources, this allows you to
  make \"GLib-friendly\" asynchronous and cancellable synchronous variants of
  blocking APIs.

  Cancelling a task:
  @begin{pre}
static void
bake_cake_thread (GTask         *task,
                  gpointer       source_object,
                  gpointer       task_data,
                  GCancellable  *cancellable)
{
  Baker *self = source_object;
  CakeData *cake_data = task_data;
  Cake *cake;
  GError *error = NULL;

  cake = bake_cake (baker, cake_data->radius, cake_data->flavor,
                    cake_data->frosting, cake_data->message,
                    &error);
  if (error)
    {
      g_task_return_error (task, error);
      return;
    @}

  // If the task has already been cancelled, then we don't want to add
  // the cake to the cake cache. Likewise, we don't  want to have the
  // task get cancelled in the middle of updating the cache.
  // g_task_set_return_on_cancel() will return %TRUE here if it managed
  // to disable return-on-cancel, or %FALSE if the task was cancelled
  // before it could.
  if (g_task_set_return_on_cancel (task, FALSE))
    {
      // If the caller cancels at this point, their
      // GAsyncReadyCallback won't be invoked until we return,
      // so we don't have to worry that this code will run at
      // the same time as that code does. But if there were
      // other functions that might look at the cake cache,
      // then we'd probably need a GMutex here as well.
      baker_add_cake_to_cache (baker, cake);
      g_task_return_pointer (task, cake, g_object_unref);
    @}
@}

void
baker_bake_cake_async (Baker               *self,
                       guint                radius,
                       CakeFlavor           flavor,
                       CakeFrostingType     frosting,
                       const char          *message,
                       GCancellable        *cancellable,
                       GAsyncReadyCallback  callback,
                       gpointer             user_data)
{
  CakeData *cake_data;
  GTask *task;

  cake_data = g_slice_new (CakeData);

  ...

  task = g_task_new (self, cancellable, callback, user_data);
  g_task_set_task_data (task, cake_data, (GDestroyNotify) cake_data_free);
  g_task_set_return_on_cancel (task, TRUE);
  g_task_run_in_thread (task, bake_cake_thread);
@}

Cake *
baker_bake_cake_sync (Baker               *self,
                      guint                radius,
                      CakeFlavor           flavor,
                      CakeFrostingType     frosting,
                      const char          *message,
                      GCancellable        *cancellable,
                      GError             **error)
{
  CakeData *cake_data;
  GTask *task;
  Cake *cake;

  cake_data = g_slice_new (CakeData);

  ...

  task = g_task_new (self, cancellable, NULL, NULL);
  g_task_set_task_data (task, cake_data, (GDestroyNotify) cake_data_free);
  g_task_set_return_on_cancel (task, TRUE);
  g_task_run_in_thread_sync (task, bake_cake_thread);

  cake = g_task_propagate_pointer (task, error);
  g_object_unref (task);
  return cake;
@}
  @end{pre}
  @subheading{Porting from GSimpleAsyncResult}
  The @sym{g:task} API attempts to be simpler than the
  @class{g:simple-async-result} API in several ways:
  @begin{itemize}
    @begin{item}
      You can save task-specific data with the @fun{g:task-set-task-data}
      function, and retrieve it later with the @fun{g;task-get-task-data}
      function. This replaces the abuse of the
      @fun{g:simple-async-result-set-op-res-gpointer} function for the same
      purpose with the @class{g:simple-async-result} object.
    @end{item}
    @begin{item}
      In addition to the task data, the @sym{g:task} object also keeps track of
      the priority, the @class{g:cancellable} object, and the
      @type{g:main-context} instance associated with the task, so tasks that
      consist of a chain of simpler asynchronous operations will have easy
      access to those values when starting each sub-task.
    @end{item}
    @begin{item}
      The @fun{g:task-return-error-if-cancelled} function provides simplified
      handling for cancellation. In addition, cancellation overrides any other
      @sym{g:task} return value by default, like the
      @class{g:simple-async-result} function does when the
      @fun{g:simple-async-result-set-check-cancellable} function is called. (You
      can use the @fun{g:task-set-check-cancellable} function to turn off that
      behavior.) On the other hand, the @fun{g:task-run-in-thread} function
      guarantees that it will always run your @code{task_func}, even if the
      task's @class{g:cancellable} object is already cancelled before the task
      gets a chance to run; you can start your @code{task_func} with a
      @fun{g:task-return-error-if-cancelled} check if you need the old behavior.
    @end{item}
    @begin{item}
      The \"return\" methods (e.g., the @fun{g:task-return-pointer} function)
      automatically cause the task to be \"completed\" as well, and there is no
      need to worry about the \"complete\" vs \"complete in idle\" distinction.
      (the @sym{g:task} object automatically figures out whether the task's
      callback can be invoked directly, or if it needs to be sent to another
      @type{g:main-context} instance, or delayed until the next iteration of the
      current @type{g:main-context} instance.)
    @end{item}
    @begin{item}
      The \"finish\" functions for the @sym{g:task} object based operations are
      generally much simpler than the @class{g:simple-async-result} object ones,
      normally consisting of only a single call to the
      @fun{g:task-propagate-pointer} function or the like. Since the
      @fun{g:task-propagate-pointer} function \"steals\" the return value from
      the @sym{g:task} object, it is not necessary to juggle pointers around to
      prevent it from being freed twice.
    @end{item}
    @begin{item}
      With the @class{g:simple-async-result} object, it was common to call
      the @fun{g:simple-async-result-propagate-error} function from the
      @code{_finish()} wrapper function, and have virtual method implementations
      only deal with successful returns. This behavior is deprecated, because it
      makes it difficult for a subclass to chain to a parent class's async
      methods. Instead, the wrapper function should just be a simple wrapper,
      and the virtual method should call an appropriate @code{g_task_propagate_}
      function. Note that wrapper methods can now use the
      @fun{g:async-result-legacy-propagate-error} function to do old-style
      @class{g:simple-async-result} error-returning behavior, and the
      @fun{g:async-result-is-tagged} function to check if a result is tagged as
      having come from the @code{_async()} wrapper function (for
      \"short-circuit\" results, such as when passing 0 to the
      @fun{g:input-stream-read-async} function).
    @end{item}
  @end{itemize}
  @see-constructor{g:task-new}
  @see-slot{g:task-completed}
  @see-class{g:async-result}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "completed" 'task) t)
 "The @code{completed} property of type @code{:boolean} (read) @br{}
  Whether the task has completed, meaning its callback (if set) has been
  invoked. This can only happen after the @fun{g:task-return-pointer} function,
  the @fun{g:task-return-error} function or one of the other return functions
  have been called on the task. This property is guaranteed to change from
  @em{false} to @em{true} exactly once. The \"notify\" signal for this change is
  emitted in the same main context as the task’s callback, immediately after
  that callback is invoked. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'task-completed)
      "Accessor"
      (documentation 'task-completed 'function)
 "@version{#2023-5-8}
  @syntax[]{(setf (g:task-completed object) completed)}
  @argument[object]{a @class{g:task} object}
  @argument[completed]{a boolean whether the task has completed}
  @begin{short}
    Gets the value of the @slot[g:task]{completed} property.
  @end{short}
  This changes from @em{false} to @em{true} after the task’s callback is
  invoked, and will return @em{false} if called from inside the callback.
  @see-class{g:task}")

;;; ----------------------------------------------------------------------------
;;; g_task_new ()
;;;
;;;
;;; Creates a GTask acting on source_object , which will eventually be used to
;;; invoke callback in the current thread-default main context.
;;;
;;; Call this in the "start" method of your asynchronous method, and pass the
;;; GTask around throughout the asynchronous operation. You can use
;;; g_task_set_task_data() to attach task-specific data to the object, which you
;;; can retrieve later via g_task_get_task_data().
;;;
;;; By default, if cancellable is cancelled, then the return value of the task
;;; will always be G_IO_ERROR_CANCELLED, even if the task had already completed
;;; before the cancellation. This allows for simplified handling in cases where
;;; cancellation may imply that other objects that the task depends on have been
;;; destroyed. If you do not want this behavior, you can use
;;; g_task_set_check_cancellable() to change it.
;;;
;;; source_object :
;;;     the GObject that owns this task, or NULL.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback.
;;;
;;; callback_data :
;;;     user data passed to callback .
;;;
;;; Returns :
;;;     a GTask.
;;; ----------------------------------------------------------------------------

;; TODO: We allocate a stable pointer, but we do not free the pointer.
;; Improve this!?

(cffi:defcfun ("g_task_new" %task-new) (gobject:object task)
  (source :pointer)
  (cancellable (gobject:object cancellable))
  (callback :pointer)
  (data :pointer))

(defun task-new (source cancellable func)
  (%task-new (gobject:object-pointer source)
             cancellable
             (cffi:callback asyn-ready-callback)
             (glib:allocate-stable-pointer func)))

(export 'task-new)

;;; ----------------------------------------------------------------------------
;;; g_task_set_task_data ()
;;;
;;; void
;;; g_task_set_task_data (GTask *task,
;;;                       gpointer task_data,
;;;                       GDestroyNotify task_data_destroy);
;;;
;;; Sets task 's task data (freeing the existing task data, if any).
;;;
;;; task :
;;;     the GTask
;;;
;;; task_data :
;;;     task-specific data.
;;;
;;; task_data_destroy :
;;;     GDestroyNotify for task_data .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_get_task_data ()
;;;
;;; gpointer
;;; g_task_get_task_data (GTask *task);
;;;
;;; Gets task 's task_data.
;;;
;;; task :
;;;     a GTask
;;;
;;; Returns :
;;;     task 's task_data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_set_priority ()
;;;
;;; void
;;; g_task_set_priority (GTask *task,
;;;                      gint priority);
;;;
;;; Sets task 's priority. If you do not call this, it will default to
;;; G_PRIORITY_DEFAULT.
;;;
;;; This will affect the priority of GSources created with
;;; g_task_attach_source() and the scheduling of tasks run in threads, and can
;;; also be explicitly retrieved later via g_task_get_priority().
;;;
;;; task :
;;;     the GTask
;;;
;;; priority :
;;;     the priority of the request
;;; ----------------------------------------------------------------------------

(defun (setf task-priority) (priority task)
  (cffi:foreign-funcall "g_task_set_priority"
                        (gobject:object task) task
                        :int priority)
  priority)

;;; ----------------------------------------------------------------------------
;;; g_task_get_priority ()
;;;
;;; gint
;;; g_task_get_priority (GTask *task);
;;;
;;; Gets task 's priority
;;;
;;; task :
;;;     a GTask
;;;
;;; Returns :
;;;     task 's priority
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_task_get_priority" task-priority) :int
  (task (gobject:object task)))

(export 'task-priority)

;;; ----------------------------------------------------------------------------
;;; g_task_set_check_cancellable ()
;;;
;;; void
;;; g_task_set_check_cancellable (GTask *task,
;;;                               gboolean check_cancellable);
;;;
;;; Sets or clears task 's check-cancellable flag. If this is TRUE (the
;;; default), then g_task_propagate_pointer(), etc, and g_task_had_error() will
;;; check the task's GCancellable first, and if it has been cancelled, then they
;;; will consider the task to have returned an "Operation was cancelled" error
;;; (G_IO_ERROR_CANCELLED), regardless of any other error or return value the
;;; task may have had.
;;;
;;; If check_cancellable is FALSE, then the GTask will not check the cancellable
;;; itself, and it is up to task 's owner to do this (eg, via
;;; g_task_return_error_if_cancelled()).
;;;
;;; If you are using g_task_set_return_on_cancel() as well, then you must leave
;;; check-cancellable set TRUE.
;;;
;;; task :
;;;     the GTask
;;;
;;; check_cancellable :
;;;     whether GTask will check the state of its GCancellable for you.
;;; ----------------------------------------------------------------------------

(defun (setf task-check-cancellable) (check task)
  (cffi:foreign-funcall "g_task_set_check_cancellable"
                        (gobject:object task) task
                        :boolean check)
  check)

;;; ----------------------------------------------------------------------------
;;; g_task_get_check_cancellable ()
;;;
;;; gboolean
;;; g_task_get_check_cancellable (GTask *task);
;;;
;;; Gets task 's check-cancellable flag. See g_task_set_check_cancellable() for
;;; more details.
;;;
;;; task :
;;;     the GTask
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_task_get_check_cancellable" task-check-cancellable) :boolean
  (task (gobject:object task)))

(export 'task-check-cancellable)

;;; ----------------------------------------------------------------------------
;;; g_task_set_return_on_cancel ()
;;;
;;; gboolean
;;; g_task_set_return_on_cancel (GTask *task,
;;;                              gboolean return_on_cancel);
;;;
;;; Sets or clears task 's return-on-cancel flag. This is only meaningful for
;;; tasks run via g_task_run_in_thread() or g_task_run_in_thread_sync().
;;;
;;; If return_on_cancel is TRUE, then cancelling task 's GCancellable will
;;; immediately cause it to return, as though the task's GTaskThreadFunc had
;;; called g_task_return_error_if_cancelled() and then returned.
;;;
;;; This allows you to create a cancellable wrapper around an uninterruptible
;;; function. The GTaskThreadFunc just needs to be careful that it does not
;;; modify any externally-visible state after it has been cancelled. To do that,
;;; the thread should call g_task_set_return_on_cancel() again to (atomically)
;;; set return-on-cancel FALSE before making externally-visible changes; if the
;;; task gets cancelled before the return-on-cancel flag could be changed,
;;; g_task_set_return_on_cancel() will indicate this by returning FALSE.
;;;
;;; You can disable and re-enable this flag multiple times if you wish. If the
;;; task's GCancellable is cancelled while return-on-cancel is FALSE, then
;;; calling g_task_set_return_on_cancel() to set it TRUE again will cause the
;;; task to be cancelled at that point.
;;;
;;; If the task's GCancellable is already cancelled before you call
;;; g_task_run_in_thread()/g_task_run_in_thread_sync(), then the GTaskThreadFunc
;;; will still be run (for consistency), but the task will also be completed
;;; right away.
;;;
;;; task :
;;;     the GTask
;;;
;;; return_on_cancel :
;;;     whether the task returns automatically when it is cancelled.
;;;
;;; Returns :
;;;     TRUE if task 's return-on-cancel flag was changed to match
;;;     return_on_cancel . FALSE if task has already been cancelled.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_get_return_on_cancel ()
;;;
;;; gboolean
;;; g_task_get_return_on_cancel (GTask *task);
;;;
;;; Gets task 's return-on-cancel flag. See g_task_set_return_on_cancel() for
;;; more details.
;;;
;;; task :
;;;     the GTask
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_set_source_tag ()
;;;
;;; void
;;; g_task_set_source_tag (GTask *task,
;;;                        gpointer source_tag);
;;;
;;; Sets task 's source tag. You can use this to tag a task return value with a
;;; particular pointer (usually a pointer to the function doing the tagging) and
;;; then later check it using g_task_get_source_tag() (or
;;; g_async_result_is_tagged()) in the task's "finish" function, to figure out
;;; if the response came from a particular place.
;;;
;;; task :
;;;     the GTask
;;;
;;; source_tag :
;;;     an opaque pointer indicating the source of this task
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_get_source_tag ()
;;;
;;; gpointer
;;; g_task_get_source_tag (GTask *task);
;;;
;;; Gets task 's source tag. See g_task_set_source_tag().
;;;
;;; task :
;;;     a GTask
;;;
;;; Returns :
;;;     task 's source tag.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_set_name ()
;;;
;;; void
;;; g_task_set_name (GTask *task,
;;;                  const gchar *name);
;;;
;;; Sets task ’s name, used in debugging and profiling. The name defaults to
;;; NULL.
;;;
;;; The task name should describe in a human readable way what the task does.
;;; For example, ‘Open file’ or ‘Connect to network host’. It is used to set the
;;; name of the GSource used for idle completion of the task.
;;;
;;; This function may only be called before the task is first used in a thread
;;; other than the one it was constructed in.
;;;
;;; task :
;;;     a GTask
;;;
;;; name :
;;;     a human readable name for the task, or NULL to unset it.
;;;
;;; Since 2.60
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_get_name ()
;;;
;;; const gchar *
;;; g_task_get_name (GTask *task);
;;;
;;; Gets task ’s name. See g_task_set_name().
;;;
;;; task :
;;;     a GTask
;;;
;;; Returns :
;;;     task ’s name, or NULL.
;;;
;;; Since 2.60
;;; ----------------------------------------------------------------------------

#|
GioTaskset_static_name
since: 2.76

[−]
Declaration
void
g_task_set_static_name (
  GTask* task,
  const gchar* name
)

[−]
Description
Sets task’s name, used in debugging and profiling.

This is a variant of g_task_set_name() that avoids copying name.

Available since: 2.76

[−]
Parameters
name
Type: const gchar*

A human readable name for the task. Must be a string literal.

The argument can be NULL.
The data is owned by the caller of the function.
The value is a NUL terminated UTF-8 string.
|#

;;; ----------------------------------------------------------------------------
;;; g_task_report_error ()
;;;
;;; void
;;; g_task_report_error (gpointer source_object,
;;;                      GAsyncReadyCallback callback,
;;;                      gpointer callback_data,
;;;                      gpointer source_tag,
;;;                      GError *error);
;;;
;;; Creates a GTask and then immediately calls g_task_return_error() on it. Use
;;; this in the wrapper function of an asynchronous method when you want to
;;; avoid even calling the virtual method. You can then use
;;; g_async_result_is_tagged() in the finish method wrapper to check if the
;;; result there is tagged as having been created by the wrapper method, and
;;; deal with it appropriately if so.
;;;
;;; See also g_task_report_new_error().
;;;
;;; source_object :
;;;     the GObject that owns this task, or NULL.
;;;
;;; callback :
;;;     a GAsyncReadyCallback.
;;;
;;; callback_data :
;;;     user data passed to callback .
;;;
;;; source_tag :
;;;     an opaque pointer indicating the source of this task
;;;
;;; error :
;;;     error to report.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_report_new_error ()
;;;
;;; void
;;; g_task_report_new_error (gpointer source_object,
;;;                          GAsyncReadyCallback callback,
;;;                          gpointer callback_data,
;;;                          gpointer source_tag,
;;;                          GQuark domain,
;;;                          gint code,
;;;                          const char *format,
;;;                          ...);
;;;
;;; Creates a GTask and then immediately calls g_task_return_new_error() on it.
;;; Use this in the wrapper function of an asynchronous method when you want to
;;; avoid even calling the virtual method. You can then use
;;; g_async_result_is_tagged() in the finish method wrapper to check if the
;;; result there is tagged as having been created by the wrapper method, and
;;; deal with it appropriately if so.
;;;
;;; See also g_task_report_error().
;;;
;;; source_object :
;;;     the GObject that owns this task, or NULL.
;;;
;;; callback :
;;;     a GAsyncReadyCallback.
;;;
;;; callback_data :
;;;     user data passed to callback .
;;;
;;; source_tag :
;;;     an opaque pointer indicating the source of this task
;;;
;;; domain :
;;;     a GQuark.
;;;
;;; code :
;;;     an error code.
;;;
;;; format :
;;;     a string with format characters.
;;;
;;; ... :
;;;     a list of values to insert into format .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_get_cancellable ()
;;;
;;; GCancellable *
;;; g_task_get_cancellable (GTask *task);
;;;
;;; Gets task 's GCancellable
;;;
;;; task :
;;;     a GTask
;;;
;;; Returns :
;;;     task 's GCancellable.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_tast_get_cancellable" task-canellable)
    (gobject:object cancellable)
  (task (gobject:object task)))

(export 'task-cancellable)

;;; ----------------------------------------------------------------------------
;;; g_task_get_context ()
;;;
;;; GMainContext *
;;; g_task_get_context (GTask *task);
;;;
;;; Gets the GMainContext that task will return its result in (that is, the
;;; context that was the thread-default main context at the point when task was
;;; created).
;;;
;;; This will always return a non-NULL value, even if the task's context is the
;;; default GMainContext.
;;;
;;; task :
;;;     a GTask
;;;
;;; Returns :
;;;     task 's GMainContext.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_task_get_context" task-context)
    (:pointer (:struct glib:main-context))
  (task (gobject:object task)))

(export 'task-context)

;;; ----------------------------------------------------------------------------
;;; g_task_get_source_object ()
;;;
;;; gpointer
;;; g_task_get_source_object (GTask *task);
;;;
;;; Gets the source object from task . Like g_async_result_get_source_object(),
;;; but does not ref the object.
;;;
;;; task :
;;;     a GTask
;;;
;;; Returns :
;;;     task 's source object, or NULL.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_task_get_source_object" task-source-object) :pointer
  (task (gobject:object task)))

(export 'task-source-object)

;;; ----------------------------------------------------------------------------
;;; g_task_return_boolean ()
;;;
;;; void
;;; g_task_return_boolean (GTask *task,
;;;                        gboolean result);
;;;
;;; Sets task 's result to result and completes the task (see
;;; g_task_return_pointer() for more discussion of exactly what this means).
;;;
;;; task :
;;;     a GTask.
;;;
;;; result :
;;;     the gboolean result of a task function.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_task_return_boolean" task-return-boolean) :void
  (task (gobject:object task))
  (result :boolean))

(export 'task-return-boolean)

;;; ----------------------------------------------------------------------------
;;; g_task_return_int ()
;;;
;;; void
;;; g_task_return_int (GTask *task,
;;;                    gssize result);
;;;
;;; Sets task 's result to result and completes the task (see
;;; g_task_return_pointer() for more discussion of exactly what this means).
;;;
;;; task :
;;;     a GTask.
;;;
;;; result :
;;;     the integer (gssize) result of a task function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_return_pointer ()
;;;
;;; void
;;; g_task_return_pointer (GTask *task,
;;;                        gpointer result,
;;;                        GDestroyNotify result_destroy);
;;;
;;; Sets task 's result to result and completes the task. If result is not NULL,
;;; then result_destroy will be used to free result if the caller does not take
;;; ownership of it with g_task_propagate_pointer().
;;;
;;; "Completes the task" means that for an ordinary asynchronous task it will
;;; either invoke the task's callback, or else queue that callback to be invoked
;;; in the proper GMainContext, or in the next iteration of the current
;;; GMainContext. For a task run via g_task_run_in_thread() or
;;; g_task_run_in_thread_sync(), calling this method will save result to be
;;; returned to the caller later, but the task will not actually be completed
;;; until the GTaskThreadFunc exits.
;;;
;;; Note that since the task may be completed before returning from
;;; g_task_return_pointer(), you cannot assume that result is still valid after
;;; calling this, unless you are still holding another reference on it.
;;;
;;; task :
;;;     a GTask
;;;
;;; result :
;;;     the pointer result of a task function.
;;;
;;; result_destroy :
;;;     a GDestroyNotify function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_return_value ()
;;;
;;; void
;;; g_task_return_value (GTask *task,
;;;                      GValue *result);
;;;
;;; Sets task 's result to result (by copying it) and completes the task.
;;;
;;; If result is NULL then a GValue of type G_TYPE_POINTER with a value of NULL
;;; will be used for the result.
;;;
;;; This is a very generic low-level method intended primarily for use by
;;; language bindings; for C code, g_task_return_pointer() and the like will
;;; normally be much easier to use.
;;;
;;; task :
;;;     a GTask
;;;
;;; result :
;;;     the GValue result of a task function.
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_return_error ()
;;;
;;; void
;;; g_task_return_error (GTask *task,
;;;                      GError *error);
;;;
;;; Sets task 's result to error (which task assumes ownership of) and completes
;;; the task (see g_task_return_pointer() for more discussion of exactly what
;;; this means).
;;;
;;; Note that since the task takes ownership of error , and since the task may
;;; be completed before returning from g_task_return_error(), you cannot assume
;;; that error is still valid after calling this. Call g_error_copy() on the
;;; error if you need to keep a local copy as well.
;;;
;;; See also g_task_return_new_error().
;;;
;;; task :
;;;     a GTask.
;;;
;;; error :
;;;     the GError result of a task function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_return_new_error ()
;;;
;;; void
;;; g_task_return_new_error (GTask *task,
;;;                          GQuark domain,
;;;                          gint code,
;;;                          const char *format,
;;;                          ...);
;;;
;;; Sets task 's result to a new GError created from domain , code , format ,
;;; and the remaining arguments, and completes the task (see
;;; g_task_return_pointer() for more discussion of exactly what this means).
;;;
;;; See also g_task_return_error().
;;;
;;; task :
;;;     a GTask.
;;;
;;; domain :
;;;     a GQuark.
;;;
;;; code :
;;;     an error code.
;;;
;;; format :
;;;     a string with format characters.
;;;
;;; ... :
;;;     a list of values to insert into format .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_return_error_if_cancelled ()
;;;
;;; gboolean
;;; g_task_return_error_if_cancelled (GTask *task);
;;;
;;; Checks if task 's GCancellable has been cancelled, and if so, sets task 's
;;; error accordingly and completes the task (see g_task_return_pointer() for
;;; more discussion of exactly what this means).
;;;
;;; task :
;;;     a GTask
;;;
;;; Returns :
;;;     TRUE if task has been cancelled, FALSE if not
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_propagate_boolean ()
;;;
;;; gboolean
;;; g_task_propagate_boolean (GTask *task,
;;;                           GError **error);
;;;
;;; Gets the result of task as a gboolean.
;;;
;;; If the task resulted in an error, or was cancelled, then this will instead
;;; return FALSE and set error .
;;;
;;; Since this method transfers ownership of the return value (or error) to the
;;; caller, you may only call it once.
;;;
;;; task :
;;;     a GTask.
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the task result, or FALSE on error
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_propagate_int ()
;;;
;;; gssize
;;; g_task_propagate_int (GTask *task,
;;;                       GError **error);
;;;
;;; Gets the result of task as an integer (gssize).
;;;
;;; If the task resulted in an error, or was cancelled, then this will instead
;;; return -1 and set error .
;;;
;;; Since this method transfers ownership of the return value (or error) to the
;;; caller, you may only call it once.
;;;
;;; task :
;;;     a GTask.
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the task result, or -1 on error
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_propagate_pointer ()
;;;
;;; gpointer
;;; g_task_propagate_pointer (GTask *task,
;;;                           GError **error);
;;;
;;; Gets the result of task as a pointer, and transfers ownership of that value
;;; to the caller.
;;;
;;; If the task resulted in an error, or was cancelled, then this will instead
;;; return NULL and set error .
;;;
;;; Since this method transfers ownership of the return value (or error) to the
;;; caller, you may only call it once.
;;;
;;; task :
;;;     a GTask
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the task result, or NULL on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_propagate_value ()
;;;
;;; gboolean
;;; g_task_propagate_value (GTask *task,
;;;                         GValue *value,
;;;                         GError **error);
;;;
;;; Gets the result of task as a GValue, and transfers ownership of that value
;;; to the caller. As with g_task_return_value(), this is a generic low-level
;;; method; g_task_propagate_pointer() and the like will usually be more useful
;;; for C code.
;;;
;;; If the task resulted in an error, or was cancelled, then this will instead
;;; set error and return FALSE.
;;;
;;; Since this method transfers ownership of the return value (or error) to the
;;; caller, you may only call it once.
;;;
;;; task :
;;;     a GTask
;;;
;;; value :
;;;     return location for the GValue.
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     TRUE if task succeeded, FALSE on error.
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_had_error ()
;;;
;;; gboolean
;;; g_task_had_error (GTask *task);
;;;
;;; Tests if task resulted in an error.
;;;
;;; task :
;;;     a GTask.
;;;
;;; Returns :
;;;     TRUE if the task resulted in an error, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_run_in_thread ()
;;;
;;; void
;;; g_task_run_in_thread (GTask *task,
;;;                       GTaskThreadFunc task_func);
;;;
;;; Runs task_func in another thread. When task_func returns, task 's
;;; GAsyncReadyCallback will be invoked in task 's GMainContext.
;;;
;;; This takes a ref on task until the task completes.
;;;
;;; See GTaskThreadFunc for more details about how task_func is handled.
;;;
;;; Although GLib currently rate-limits the tasks queued via
;;; g_task_run_in_thread(), you should not assume that it will always do this.
;;; If you have a very large number of tasks to run, but don't want them to all
;;; run at once, you should only queue a limited number of them at a time.
;;;
;;; task :
;;;     a GTask
;;;
;;; task_func :
;;;     a GTaskThreadFunc.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_run_in_thread_sync ()
;;;
;;; void
;;; g_task_run_in_thread_sync (GTask *task,
;;;                            GTaskThreadFunc task_func);
;;;
;;; Runs task_func in another thread, and waits for it to return or be
;;; cancelled. You can use g_task_propagate_pointer(), etc, afterward to get
;;; the result of task_func .
;;;
;;; See GTaskThreadFunc for more details about how task_func is handled.
;;;
;;; Normally this is used with tasks created with a NULL callback, but note that
;;; even if the task does have a callback, it will not be invoked when task_func
;;; returns. “completed” will be set to TRUE just before this function returns.
;;;
;;; Although GLib currently rate-limits the tasks queued via
;;; g_task_run_in_thread_sync(), you should not assume that it will always do
;;; this. If you have a very large number of tasks to run, but don't want them
;;; to all run at once, you should only queue a limited number of them at a
;;; time.
;;;
;;; task :
;;;     a GTask
;;;
;;; task_func :
;;;     a GTaskThreadFunc.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTaskThreadFunc ()
;;;
;;; void
;;; (*GTaskThreadFunc) (GTask *task,
;;;                     gpointer source_object,
;;;                     gpointer task_data,
;;;                     GCancellable *cancellable);
;;;
;;; The prototype for a task function to be run in a thread via
;;; g_task_run_in_thread() or g_task_run_in_thread_sync().
;;;
;;; If the return-on-cancel flag is set on task , and cancellable gets
;;; cancelled, then the GTask will be completed immediately (as though
;;; g_task_return_error_if_cancelled() had been called), without waiting for the
;;; task function to complete. However, the task function will continue running
;;; in its thread in the background. The function therefore needs to be careful
;;; about how it uses externally-visible state in this case. See
;;; g_task_set_return_on_cancel() for more details.
;;;
;;; Other than in that case, task will be completed when the GTaskThreadFunc
;;; returns, not when it calls a g_task_return_ function.
;;;
;;; task :
;;;     the GTask
;;;
;;; source_object :
;;;     task 's source object.
;;;
;;; task_data :
;;;     task 's task data
;;;
;;; cancellable :
;;;     task 's GCancellable, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_attach_source ()
;;;
;;; void
;;; g_task_attach_source (GTask *task,
;;;                       GSource *source,
;;;                       GSourceFunc callback);
;;;
;;; A utility function for dealing with async operations where you need to wait
;;; for a GSource to trigger. Attaches source to task 's GMainContext with
;;; task's priority, and sets source 's callback to callback , with task as the
;;; callback's user_data.
;;;
;;; It will set the source ’s name to the task’s name (as set with
;;; g_task_set_name()), if one has been set.
;;;
;;; This takes a reference on task until source is destroyed.
;;;
;;; task :
;;;     a GTask
;;;
;;; source :
;;;     the source to attach
;;;
;;; callback :
;;;     the callback to invoke when source triggers
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_task_is_valid ()
;;;
;;; gboolean
;;; g_task_is_valid (gpointer result,
;;;                  gpointer source_object);
;;;
;;; Checks that result is a GTask, and that source_object is its source object
;;; (or that source_object is NULL and result has no source object). This can be
;;; used in g_return_if_fail() checks.
;;;
;;; result :
;;;     A GAsyncResult.
;;;
;;; source_object :
;;;     the source object expected to be associated with the task.
;;;
;;; Returns :
;;;     TRUE if result and source_object are valid, FALSE if not
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.task.lisp ----------------------------------------------
