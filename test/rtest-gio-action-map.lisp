(in-package :glib-test)

(def-suite gio-action-map :in gio-suite)
(in-suite gio-action-map)

;;; --- Types and Values -------------------------------------------------------

;;;     GActionMap

(test g-action-map-interface
  ;; Check type
  (is-true (g:type-is-interface "GActionMap"))
  ;; Check registered symbol
  (is (eq 'g:action-map
          (glib:symbol-for-gtype "GActionMap")))
  ;; Check type initializer
  (is (eq (g:gtype "GActionMap")
          (g:gtype (cffi:foreign-funcall "g_action_map_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GActionMap")))
  ;; Check interface properties.
  (is (equal '()
             (glib-test:list-interface-properties "GActionMap")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GActionMap")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GActionMap" GIO:ACTION-MAP
                                          (:EXPORT T
                                           :TYPE-INITIALIZER
                                           "g_action_map_get_type"))
             (gobject:get-gtype-definition "GActionMap"))))

;;; --- Functions --------------------------------------------------------------

(defun activate-quit (action parameter)
  (declare (ignore action parameter)))

(defun activate-print (action parameter)
  (declare (ignore action parameter)))

(defun create-action-group ()
  (let ((entries (list (list "quit"
                             #'activate-quit)
                       (list "print"
                             #'activate-print
                             "s")))
        (group (g:simple-action-group-new)))
    (g:action-map-add-action-entries group entries)
    group))

;;;     g_action_map_lookup_action

(test g-action-map-lookup-action
  (let ((group (create-action-group)))
    (is (typep (g:action-map-lookup-action group "quit") 'g:simple-action))
    (is (typep (g:action-map-lookup-action group "print") 'g:simple-action))
    (is-false (g:action-map-lookup-action group "unknown"))))

;;;     g_action_map_add_action_entries

;; Example in the documentation of g:action-map-add-action-entries

(test g-action-map-add-action-entries
  (let* ((group (create-action-group))
         (action-quit (g:action-map-lookup-action group "quit"))
         (action-print (g:action-map-lookup-action group "print")))
    ;; Check action QUIT
    (is (typep action-quit 'g:simple-action))
    (is (string= "quit" (g:action-name action-quit)))
    ;; Slot parameter-type is not initialized
    (is-false (g:action-parameter-type action-quit))
    ;; Check action PRINT
    (is (typep action-print 'g:simple-action))
    (is (string= "print" (g:action-name action-print)))
    ;; Slot parameter-type is initialized with type "s"
    (is (typep (g:action-parameter-type action-print) 'g:variant-type))))

;;;     g_action_map_add_action
;;;     g_action_map_remove_action

(test g-action-map-add-action
  (let ((group (g:simple-action-group-new)))
    (g:action-map-add-action group (g:simple-action-new "quit" nil))
    (is (string= "quit"
                 (g:action-name (g:action-map-lookup-action group "quit"))))
    (g:action-map-add-action group (g:simple-action-new "close" nil))
    (is (string= "close"
                 (g:action-name (g:action-map-lookup-action group "close"))))
    (g:action-map-remove-action group "quit")
    (is-false (g:action-map-lookup-action group "quit"))))

;;; 2024-9-17
