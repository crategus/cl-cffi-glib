(in-package :glib-test)

(def-suite glib-main-loop :in glib-suite)
(in-suite glib-main-loop)

(defvar *verbose-glib-main-loop* nil)

;;; A callback function for use as a source function

(let ((counter 0) (max 3))
  (defun timeout-callback (loop)
    (incf counter)
    (when *verbose-glib-main-loop*
      (format t "~&timout-callback called ~d times~%" counter))
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

;;;     G_PRIORITY_HIGH
;;;     G_PRIORITY_DEFAULT
;;;     G_PRIORITY_HIGH_IDLE
;;;     G_PRIORITY_DEFAULT_IDLE
;;;     G_PRIORITY_LOW

(test priority-constants
  (is (= -100 +g-priority-high+))
  (is (=    0 +g-priority-default+))
  (is (=  100 +g-priority-high-idle+))
  (is (=  200 +g-priority-default-idle+))
  (is (=  300 +g-priority-low+)))

;;;     G_SOURCE_CONST
;;;     G_SOURCE_REMOVE

(test source-constants
  (is-true +g-source-continue+)
  (is-false +g-source-remove+))

;;;   GMainLoop

(defvar *main-loop* nil)
(defvar *main-thread* nil)
(defvar *main-thread-level* nil)
(defvar *main-thread-lock* (bt:make-lock "main-thread lock"))

(test main-loop
  ;; Start a main loop
  (bt:with-lock-held (*main-thread-lock*)
    (when (and *main-thread* (not (bt:thread-alive-p *main-thread*)))
      (setf *main-thread* nil))
    ;; Start the main loop in a thread.
    (unless *main-thread*
      (setf *main-thread*
            (bt:make-thread (lambda ()
                              (setf *main-loop*
                                    (g:main-loop-new nil nil))
                              (g:main-loop-run *main-loop*))
                            :name "rtest-glib-thread")
            *main-thread-level* 0))
    (incf *main-thread-level*))
  (sleep 1)
  ;; At this point a thread with a main loop is started.
  ;; We do some checks for the running main loop.
  (is-true (bt:thread-alive-p *main-thread*))
  (is (= 1 *main-thread-level*))
  (is-true (cffi:pointerp *main-loop*))
  (is-true (g:main-loop-is-running *main-loop*))
  (is-true (cffi:pointer-eq *main-loop*
                            (g:main-loop-ref *main-loop*)))
  (g:main-loop-unref *main-loop*)
  (is-true (cffi:pointer-eq (g:main-context-default)
                            (g:main-loop-context *main-loop*)))
  (is-false (g:main-context-is-owner (g:main-context-default)))
  ;; Stop the main loop.
  (bt:with-lock-held (*main-thread-lock*)
    (decf *main-thread-level*)
    (when (zerop *main-thread-level*)
      ;; instead of gtk-main-quit
      (g:main-loop-quit *main-loop*)))
  (sleep 1)
  ;; The main loop is stopped. We do some checks.
  (is-false (bt:thread-alive-p *main-thread*))
  (is (= 0 *main-thread-level*))
  (is-true (cffi:pointerp *main-loop*))
  (is-false (g:main-loop-is-running *main-loop*)))

;;;   GMainContext

(test main-context
  (let ((context (g:main-context-new)))
    (is-true (cffi:pointerp context))
    (is-true (cffi:pointer-eq context (g:main-context-ref context)))
    (g:main-context-unref context)
    (g:main-context-unref context)))

;;;   g_main_loop_new
;;;   g_main_loop_ref
;;;   g_main_loop_unref
;;;   g_main_loop_quit
;;;   g_main_loop_is_running
;;;   g_main_loop_get_context

(test main-loop-new.1
  (let ((loop (g:main-loop-new nil t)))
    (is-true (cffi:pointerp loop))
    (is-true (not (cffi:null-pointer-p loop)))
    (is-true (g:main-loop-is-running loop))
    (is-true (cffi:pointer-eq loop (g:main-loop-ref loop)))
    (g:main-loop-unref loop)
    (is-true (cffi:pointer-eq (g:main-context-default)
                              (g:main-loop-context loop)))
    (g:main-loop-quit loop)
    (is-false (g:main-loop-is-running loop))
    (g:main-loop-unref loop)))

(test main-loop-new.2
  (let ((loop (g:main-loop-new (cffi:null-pointer) t)))
    (is-true (cffi:pointerp loop))
    (is-true (not (cffi:null-pointer-p loop)))
    (is-true (g:main-loop-is-running loop))
    (is-true (cffi:pointer-eq loop (g:main-loop-ref loop)))
    (g:main-loop-unref loop)
    (is-true (cffi:pointer-eq (g:main-context-default)
                              (g:main-loop-context loop)))
    (g:main-loop-quit loop)
    (is-false (g:main-loop-is-running loop))
    (g:main-loop-unref loop)))

;;;   g_main_context_new
;;;   g_main_context_ref
;;;   g_main_context_unref
;;;   g_main_context_default

(test main-context-new
  (let ((context (g:main-context-new)))
    (is-true (cffi:pointerp context))
    (is-true (cffi:pointer-eq context (g:main-context-ref context)))
    (g:main-context-unref context)
    (g:main-context-unref context)
    (is-true (not (cffi:pointer-eq context (g:main-context-default))))))

;;;     g_main_context_iteration
;;;     g_main_context_pending

;;;   g_main_context_find_source_by_id

(test main-context-find-source-by-id
  (let* ((source (g:timeout-source-new 500))
         (context (g:main-context-new))
         (id (g:source-attach source context)))
    (is-true (g:main-context-find-source-by-id context id))
    (is-false (g:main-context-find-source-by-id context 9999))
    (is-true (cffi:pointer-eq source
                              (g:main-context-find-source-by-id context id)))))

;;;     g_main_loop_run
;;;     g_timeout_source_new
;;;     g_source_attach
;;;     g_source_set_callback
;;;     g_source_get_context

;; Run a timeout callback
(test timeout-source-new.1
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

;; Do some more tests
(test timeout-source-new.2
  (let* ((context (g:main-context-new))
         (mainloop (g:main-loop-new context nil))
         ;; Create a new timeout source
         (source (g:timeout-source-new 10)))
    ;; Attach source to context
    (is (integerp (g:source-attach source context)))
    ;; Set the callback for source
    (is-false (g:source-set-callback source
                                     (lambda ()
                                       (timeout-callback mainloop))))
    ;; Do tests for some functions
    (is-false (g:source-is-destroyed source))
    (is (eq +g-priority-default+
            (g:source-priority source)))
    (is-false (g:source-can-recurse source))
    (is (string= "timeout"
                 (setf (g:source-name source) "timeout")))
    (is (string= "timeout" (g:source-name source)))
    (is (cffi:pointer-eq context
                         (g:source-context source)))
    ;; Start the main loop
    (is-false (g:main-loop-run mainloop))
    (is-false (g:main-loop-unref mainloop))))

;;;     g_timeout_source_new_seconds

(test timeout-source-new-seconds
  (let* ((context (g:main-context-new))
         (mainloop (g:main-loop-new context nil))
         ;; Create a new timeout source
         (source (g:timeout-source-new-seconds 1)))
    ;; Attach source to context
    (g:source-attach source context)
    ;; Set the callback for source
    (g:source-set-callback source
                           (lambda ()
                             (timeout-callback mainloop)))
    ;; Start the main loop
    (g:main-loop-run mainloop)
    (g:main-loop-unref mainloop)))

;;;     g_timeout_add

(test timeout-add
  (let* ((context (g:main-context-default))
         (mainloop (g:main-loop-new context nil)))
    ;; Create a new timeout source
    (is (integerp (g:timeout-add 10
                                 (lambda ()
                                   (timeout-callback mainloop)))))
    ;; Start the main loop
    (g:main-loop-run mainloop)
    (g:main-loop-unref mainloop)))

;;;     g_timeout_add_seconds

(test timeout-add-seconds
  (let* ((context (g:main-context-default))
         (mainloop (g:main-loop-new context nil)))
         ;; Create a new timeout source
    (is (integerp (g:timeout-add-seconds 1
                                         (lambda ()
                                           (timeout-callback mainloop)))))
    ;; Start the main loop
    (g:main-loop-run mainloop)
    (g:main-loop-unref mainloop)))

;;;     g_idle_source_new

(test idle-source-new
  (let* ((context (g:main-context-new))
         (mainloop (g:main-loop-new context nil))
         ;; Create a new timeout source
         (source (g:idle-source-new)))
    ;; Attach source to context
    (g:source-attach source context)
    ;; Set the callback for source
    (g:source-set-callback source
                           (lambda ()
                             (timeout-callback mainloop)))
    ;; Start the main loop
    (g:main-loop-run mainloop)
    (g:main-loop-unref mainloop)))

;;;     g_idle_add

(test idle-add
  (let* ((context (g:main-context-default))
         (mainloop (g:main-loop-new context nil)))
    ;; Create a new timeout source
    (is (integerp (g:idle-add (lambda ()
                                (timeout-callback mainloop)))))
    ;; Start the main loop
    (g:main-loop-run mainloop)
    (g:main-loop-unref mainloop)))

;;;     g_source_destroy
;;;     g_source_is_destroyed

(test source-destroy
  (let ((source (g:timeout-source-new 10)))
    (is-false (g:source-is-destroyed source))
    (is-false (g:source-destroy source))
    (is-true (g:source-is-destroyed source))
    (is-false (g:source-destroy source))))

;;;     g_source_set_priority
;;;     g_source_get_priority

(test source-priority
  (let ((source (g:timeout-source-new 10)))
    (is (eq +g-priority-high+
            (setf (g:source-priority source ) +g-priority-high+)))
    (is (eq +g-priority-high+ (g:source-priority source)))
    (is-false (g:source-destroy source))))

;;;     g_source_set_can_recurse
;;;     g_source_get_can_recurse

(test source-can-recurse
  (let ((source (g:timeout-source-new 10)))
    (is-true (setf (g:source-can-recurse source) t))
    (is-true (g:source-can-recurse source))
    (is-false (g:source-destroy source))))

;;;     g_source_get_id

(test source-id.1
  (let ((source (g:timeout-source-new 10)))
    (is (integerp (g:source-attach source (g:main-context-default))))
    (is (integerp (g:source-id source)))
    (is-false (g:source-destroy source))))

(test source-id.2
  (let ((source (g:timeout-source-new 10)))
    (is (integerp (g:source-attach source nil)))
    (is (integerp (g:source-id source)))
    (is-false (g:source-destroy source))))

;;;     g_source_get_name
;;;     g_source_set_name

(test source-name.1
  (let ((source (g:timeout-source-new 10)))
    (is (string= "timeout"
                 (setf (g:source-name source) "timeout")))
    (is (string= "timeout" (g:source-name source)))
    (is-false (g:source-destroy source))))

(test source-name.2
  (let ((source (g:timeout-source-new 10)))
    (is (cffi:pointer-eq (cffi:null-pointer)
                         (setf (g:source-name source) (cffi:null-pointer))))
    (is-false (g:source-name source))
    (is-false (g:source-destroy source))))

(test source-name.3
  (let ((source (g:timeout-source-new 10)))
    (is-false (setf (g:source-name source) nil))
    (is-false (g:source-name source))
    (is-false (g:source-destroy source))))

;;;     g_source_set_name_by_id
;;;     g_source_remove

(test source-set-name-by-id.1
  (let* ((id (g:timeout-add 10 #'(lambda ()) :priority +g-priority-default+))
         (source (g:main-context-find-source-by-id nil id)))
    (is-false (g:source-set-name-by-id id "timeout"))
    (is (string= "timeout" (g:source-name source)))
    (is-true (g:source-remove id))))

(test source-set-name-by-id.2
  (let* ((id (g:timeout-add 10 #'(lambda ()) :priority +g-priority-default+))
         (source (g:main-context-find-source-by-id (cffi:null-pointer) id)))
    (is-false (g:source-set-name-by-id id "timeout"))
    (is (string= "timeout" (g:source-name source)))
    (is-true (g:source-remove id))))

;;;     g_source_get_context

(test source-context
  (let ((source (g:timeout-source-new 10))
        (context (g:main-context-new)))
    (is-false (g:source-context source))
    (is (integerp (g:source-attach source context)))
    (is (cffi:pointer-eq context
                         (g:source-context source)))))

;;;     g_source_set_ready_time
;;;     g_source_get_ready_time
;;;     g_source_get_time

(test source-ready-time
  (let ((source (g:timeout-source-new 10)))
    (is (integerp (g:source-attach source (g:main-context-default))))
    (is (integerp (g:source-time source)))
    (is (= 100 (setf (g:source-ready-time source) 100)))
    (is (= 100 (g:source-ready-time source)))
    (is-false (g:source-destroy source))))

;;; --- 2023-1-5 ---------------------------------------------------------------
