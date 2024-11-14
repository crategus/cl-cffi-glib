(in-package :glib-test)

(def-suite gio-task :in gio-suite)
(in-suite gio-task)

;;; --- Types and Values -------------------------------------------------------

;;;     GTask

(test g-task-class
  ;; Check type
  (is (g:type-is-object "GTask"))
  ;; Check registered symbol
  (is (eq 'g:task
          (glib:symbol-for-gtype "GTask")))
  ;; Check type initializer
  (is (eq (g:gtype "GTask")
          (g:gtype (cffi:foreign-funcall "g_task_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GTask")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GTask")))
  ;; Check interfaces
  (is (equal '("GAsyncResult")
             (glib-test:list-interfaces "GTask")))
  ;; Check class properties
  (is (equal '("completed")
             (glib-test:list-properties "GTask")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GTask")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GTask" GIO:TASK
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GAsyncResult")
                        :TYPE-INITIALIZER "g_task_get_type")
                       ((COMPLETED TASK-COMPLETED "completed" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GTask"))))

;;; --- Properties -------------------------------------------------------------

;;;     completed

;;; --- Functions --------------------------------------------------------------

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

;;; 2024-10-23
