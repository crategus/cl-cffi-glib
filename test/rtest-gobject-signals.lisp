(in-package :glib-test)

(def-suite gobject-signals :in gobject-suite)
(in-suite gobject-signals)

(defvar *verbose-gobject-signals* nil)

;; TODO: These tests only works after performing the tests for GApplication
;; and GSimpleAction.

;;; --- Types and Values -------------------------------------------------------

;;;     GSignalInvocationHint
;;;     GSignalCMarshaller
;;;     GSignalCVaMarshaller
;;;     GSignalFlags
;;;     GSignalMatchType
;;;     GSignalQuery
;;;     GConnectFlags

;;;     G_SIGNAL_TYPE_STATIC_SCOPE
;;;     G_SIGNAL_MATCH_MASK
;;;     G_SIGNAL_FLAGS_MASK

;;; --- Functions --------------------------------------------------------------

;;;     g_signal_new
;;;     g_signal_newv
;;;     g_signal_new_valist
;;;     g_signal_set_va_marshaller

;;;     g-signal-query

(test g-signal-query
  (let* ((signal-id (g:signal-lookup "activate" "GSimpleAction"))
         (query (g:signal-query signal-id)))
      (is (= signal-id (g:signal-query-signal-id query)))
      (is (string= "activate" (g:signal-query-signal-name query)))
      (is (string= "GSimpleAction"
                   (g:type-name (g:signal-query-owner-type query))))
      (is (equal '(:must-collect :run-last)
                 (sort (g:signal-query-signal-flags query) #'string-lessp)))
      (is (string= "void" (g:type-name (g:signal-query-return-type query))))
      (is (equal '("GVariant")
                 (mapcar #'g:type-name (g:signal-query-param-types query))))
      (is-false (g:signal-query-signal-detail query))))

;;;     g_signal_lookup

(test g-signal-lookup
  (is (integerp (g:signal-lookup "activate" "GSimpleAction"))))

;;;     g_signal_name

(test g-signal-name
  (is (string= "activate"
               (g:signal-name (g:signal-lookup "activate" "GSimpleAction"))))
  (is (string= "change-state"
               (g:signal-name (g:signal-lookup "change-state"
                                               "GSimpleAction")))))

;;;     g_signal_list_ids

(test g-signal-list-ids
  (is (equal '()
             (mapcar #'g:signal-name (g:signal-list-ids "gboolean"))))
  (is (equal '()
             (sort (mapcar #'g:signal-name (g:signal-list-ids "GAction"))
                   #'string<)))
  (is (equal '("activate" "change-state")
             (sort (mapcar #'g:signal-name (g:signal-list-ids "GSimpleAction"))
                   #'string<)))
  ;; A workaround to get the correct result for the following test
  (is (typep (make-instance 'g:list-store) 'g:list-store))
  (is (equal '("items-changed")
             (mapcar #'g:signal-name (g:signal-list-ids "GListModel"))))
  (is (equal '("activate" "command-line" "handle-local-options" "name-lost"
               "open" "shutdown" "startup")
             (sort (mapcar #'g:signal-name (g:signal-list-ids "GApplication"))
                   #'string<))))

;;;     g_signal_emit

(test g-signal-emit.1
  (let* ((message nil)
         (action (make-instance 'g:simple-action))
         ;; Connect a signal handler
         (handler-id (g:signal-connect action "activate"
                       (lambda (action parameter)
                         (when *verbose-gobject-signals*
                           (format t "~&Signal activate for action.~%")
                           (format t "      action: ~a~%" action)
                           (format t "   parameter: ~a~%" parameter))
                         (setf message "Signal activate for action")
                         t))))
    ;; The signal handler writes a message in the variable MESSAGE.
    ;; We emit the signal and check the value of MESSAGE.
    (is-true (integerp handler-id))
    (is-false (setf message nil))
    (is-false (g:signal-emit action "activate" (g:variant-new-boolean t)))
    (is (string= "Signal activate for action" message))))

(test g-signal-emit.2
  (let* ((message nil)
         (action (make-instance 'g:simple-action))
         ;; Connect a signal handler
         (handler-id (g:signal-connect action "notify::enabled"
                       (lambda (widget pspec)
                         (declare (ignore widget))
                         (when *verbose-gobject-signals*
                           (format t "~&Signal notify::enabled for action~%"))
                         (setf message "Signal notify::enabled for action")
                         (is (g:is-param-spec pspec))
                         (is (eq (g:gtype "GParamBoolean")
                                 (g:param-spec-type pspec)))
                         (is (eq (g:gtype "gboolean")
                                 (g:param-spec-value-type pspec)))
                         (is (string= "myBoolean" (g:param-spec-name pspec)))
                         (is (string= "GParamBoolean"
                                      (g:param-spec-type-name pspec)))
                         (is-true (g:param-spec-default-value pspec))
                         t))))
    ;; The signal handler writes a message in the variable MESSAGE.
    ;; We emit the signal and check the value of MESSAGE.
    (is-true (integerp handler-id))
    (is-false (setf message nil))
    (is-false (g:signal-emit action "notify::enabled"
                             (g:param-spec-boolean "myBoolean"
                                                   "myBool"
                                                   "Doku"
                                                   t
                                                   '(:readable :writable))))
    (is (string= "Signal notify::enabled for action" message))))

;; This test does not emit the signal, but sets the property "can-default".

(test g-signal-emit.3
  (let* ((message nil)
         (action (make-instance 'g:simple-action))
         ;; Connect a signal handler
         (handler-id (g:signal-connect action "notify::enabled"
                       (lambda (widget pspec)
                         (declare (ignore widget))
                         (when *verbose-gobject-signals*
                           (format t "~&Signal notify::enabled for action~%"))
                         (setf message "Signal notify::enabled for action")
                         (is (g:is-param-spec pspec))
                         (is (eq (g:gtype "GParamBoolean")
                                 (g:param-spec-type pspec)))
                         (is (eq (g:gtype "gboolean")
                                 (g:param-spec-value-type pspec)))
                         (is (string= "enabled" (g:param-spec-name pspec)))
                         (is (string= "GParamBoolean"
                                      (g:param-spec-type-name pspec)))
                         (is-true (g:param-spec-default-value pspec))
                         t))))
    ;; The signal handler writes a message in the variable MESSAGE.
    ;; We emit the signal and check the value of MESSAGE.
    (is-true (integerp handler-id))
    (is-false (setf message nil))
    (is-true (setf (g:simple-action-enabled action) t))
    (is (string= "Signal notify::enabled for action" message))))

;;;     g_signal_emit_by_name
;;;     g_signal_emitv
;;;     g_signal_emit_valist
;;;     g_signal_connect
;;;     g_signal_connect_after
;;;     g_signal_connect_swapped
;;;     g_signal_connect_object

;;;     g_signal_connect_data
;;;     g_signal_connect_closure
;;;     g_signal_connect_closure_by_id

;;;     g-signal-handler-block
;;;     g-signal-handler-unblock

(test g-signal-handler-block/unblock
  (let* ((action (make-instance 'g:simple-action))
         (signal-id (g:signal-lookup "activate" "GSimpleAction"))
         (handler-id (g:signal-connect action "activate"
                         (lambda (widget)
                           (declare (ignore widget))
                           t))))
    ;; Block the handler
    (g:signal-handler-block action handler-id)
    (is-false (g:signal-has-handler-pending action
                                            signal-id
                                            (cffi:null-pointer)
                                            nil))
    (is-true (g:signal-has-handler-pending action
                                           signal-id
                                           (cffi:null-pointer)
                                           t))
    ;; Unblock the handler
    (g:signal-handler-unblock action handler-id)
    (is-true (g:signal-has-handler-pending action
                                           signal-id
                                           (cffi:null-pointer)
                                           nil))
    (is-true (g:signal-has-handler-pending action
                                           signal-id
                                           (cffi:null-pointer)
                                           t))))

;;;     g-signal-handler-disconnect

;; TODO: Implement the g:signal-handler-disconnect function

(test g-signal-handler-disconnect
  (let* ((action (make-instance 'g:simple-action))
         (handler-id (g:signal-connect action "activate"
                       (lambda (object)
                         (declare (ignore object))
                         t))))
    (is-true (g:signal-handler-is-connected action handler-id))
    (is-false (g:signal-handler-disconnect action handler-id))
    (is-false (g:signal-handler-is-connected action handler-id))))

;;;     g-signal-handler-find

(test g-signal-handler-find
  (let* ((action (make-instance 'g:simple-action))
         (signal-id (g:signal-lookup "activate" "GSimpleAction"))
         (handler-id (g:signal-connect action "activate"
                       (lambda (widget)
                         (declare (ignore widget))
                         t))))
    (is (= handler-id (g:signal-handler-find action signal-id)))))

;;;     g_signal_handlers_block_matched
;;;     g_signal_handlers_unblock_matched
;;;     g_signal_handlers_disconnect_matched

;;;     g-signal-handler-is-connected

(test g-signal-handler-is-connected
  (let* ((action (make-instance 'g:simple-action))
         ;; Connect a signal handler
         (handler-id (g:signal-connect action "activate"
                       (lambda (widget)
                         (declare (ignore widget))
                         t))))
    (is-true (g:signal-handler-is-connected action handler-id))))

;;;     g_signal_handlers_block_by_func
;;;     g_signal_handlers_unblock_by_func
;;;     g_signal_handlers_disconnect_by_func
;;;     g_signal_handlers_disconnect_by_data

;;;     g-signal-has-handler-pending

(test g-signal-has-handler-pending
  (let* ((action (make-instance 'g:simple-action))
         (signal-id (g:signal-lookup "activate" "GSimpleAction"))
         (handler-id (g:signal-connect action "activate"
                       (lambda (widget)
                         (declare (ignore widget))
                         t))))
    (is (integerp handler-id))
    ;; We have a signal handler for the signal "activate"
    (is-true (g:signal-has-handler-pending action
                                           signal-id
                                           (cffi:null-pointer)
                                           t))
    (is-true (g:signal-has-handler-pending action
                                           signal-id
                                           (cffi:null-pointer)
                                           nil))
    ;; We have no signal handler for the signal "change-state"
    (is-false (g:signal-has-handler-pending action
                                            (g:signal-lookup "change-state"
                                                             "GSimpleAction")
                                            (cffi:null-pointer)
                                            t))
    (is-false (g:signal-has-handler-pending action
                                            (g:signal-lookup "change-state"
                                            "GSimpleAction")
                                            (cffi:null-pointer)
                                            nil))))

;;;     g_signal_stop_emission
;;;     g_signal_stop_emission_by_name
;;;     g_signal_override_class_closure
;;;     g_signal_chain_from_overridden
;;;     g_signal_new_class_handler
;;;     g_signal_override_class_handler
;;;     g_signal_chain_from_overridden_handler
;;;     g_signal_add_emission_hook
;;;     g_signal_remove_emission_hook
;;;     g_signal_is_valid_name
;;;     g_signal_parse_name
;;;     g_signal_get_invocation_hint
;;;     g_signal_type_cclosure_new
;;;     g_signal_accumulator_first_wins
;;;     g_signal_accumulator_true_handled
;;;     g_clear_signal_handler

;;; --- 2023-7-9 ---------------------------------------------------------------
