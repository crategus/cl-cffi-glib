(in-package :glib-test)

(def-suite gio-simple-action-group :in gio-suite)
(in-suite gio-simple-action-group)

;;;   GSimpleActionGroup

(test g-simple-action-group-class
  ;; Check type
  (is (g:type-is-object "GSimpleActionGroup"))
  ;; Check registered symbol
  (is (eq 'g:simple-action-group
          (glib:symbol-for-gtype "GSimpleActionGroup")))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GSimpleActionGroup")))
  ;; Check children
;  TODO: In a second run we have the child "GApplicationExportedActions
;  (is (equal '()
;             (mapcar #'g:type-name (g:type-children "GSimpleActionGroup"))))
  ;; Check interfaces
  (is (equal '("GActionGroup" "GActionMap")
             (glib-test:list-interfaces "GSimpleActionGroup")))
  ;; Check class properties
  (is (equal '()
              (glib-test:list-properties "GSimpleActionGroup")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GSimpleActionGroup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GSimpleActionGroup"
                                      GIO:SIMPLE-ACTION-GROUP
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GActionGroup" "GActionMap")
                        :TYPE-INITIALIZER "g_simple_action_group_get_type")
                       NIL)
             (gobject:get-gtype-definition "GSimpleActionGroup"))))

;;; --- Functions --------------------------------------------------------------

;;;   g_simple_action_group_new

(test g-simple-action-group-new
  (glib-test:with-check-memory ()
    (is (eq 'g:simple-action-group (type-of (g:simple-action-group-new))))))

;;;   g_simple_action_group_lookup

(test g-simple-action-group-lookup
  (glib-test:with-check-memory (group)
    (setf group (g:simple-action-group-new))
    (is-false (g:simple-action-group-lookup group "action"))))

;;;   g_simple_action_group_insert

(test g-simple-action-group-insert
  (glib-test:with-check-memory (group)
    (setf group (g:simple-action-group-new))
    (g:simple-action-group-insert group
                                  (g:simple-action-new "simple"
                                                       (g:variant-type-new "b")))
    (is (eq 'g:simple-action
            (type-of (g:simple-action-group-lookup group "simple"))))))

;;;   g_simple_action_group_remove

(test g-simple-action-group-insert
  (glib-test:with-check-memory (group)
    (setf group (g:simple-action-group-new))
    (g:simple-action-group-insert group
                                  (g:simple-action-new "simple"
                                                       (g:variant-type-new "b")))
    (is (eq 'g:simple-action
            (type-of (g:simple-action-group-lookup group "simple"))))
    (g:simple-action-group-remove group "simple")
    (is-false (g:simple-action-group-lookup group "simple"))))

;;;   g_simple_action_group_add_entries

(test g-simple-action-group-add-entries
  (glib-test:with-check-memory (group)
    (let ((entries '(("copy"       ; name
                      nil          ; activate callback
                      nil          ; g:variant-type string
                      nil          ; g:variant
                      nil          ; change-state callback
                     )
                     ("paste" nil nil nil nil))))
      (setf group (g:simple-action-group-new))
      (g:simple-action-group-add-entries group entries)
      (is (typep (g:simple-action-group-lookup group "copy") 'g:simple-action))
      (is (typep (g:simple-action-group-lookup group "paste") 'g:simple-action))
      ;; Remove references
      (is-false (map nil (lambda (x)
                           (g:action-map-remove-action group x))
                         (g:action-group-list-actions group)))
      (is-false (g:action-group-list-actions group)))))

;;; 2024-12-19
