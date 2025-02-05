(in-package :glib-test)

(def-suite gio-notification :in gio-suite)
(in-suite gio-notification)

;;; Types and Values

;;;     GNotificationPriority

(test g-notification-priority
  ;; Check type
  (is (g:type-is-enum "GNotificationPriority"))
  ;; Check type initializer
  (is (eq (g:gtype "GNotificationPriority")
          (g:gtype (cffi:foreign-funcall "g_notification_priority_get_type"
                                         :size))))
  ;; Check registered symbol
  (is (eq 'gio:notification-priority
          (glib:symbol-for-gtype "GNotificationPriority")))
  ;; Check names
  (is (equal '("G_NOTIFICATION_PRIORITY_NORMAL" "G_NOTIFICATION_PRIORITY_LOW"
               "G_NOTIFICATION_PRIORITY_HIGH" "G_NOTIFICATION_PRIORITY_URGENT")
             (glib-test:list-enum-item-names "GNotificationPriority")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GNotificationPriority")))
  ;; Check nick names
  (is (equal '("normal" "low" "high" "urgent")
             (glib-test:list-enum-item-nicks "GNotificationPriority")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GNotificationPriority"
                                    GIO:NOTIFICATION-PRIORITY
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "g_notification_priority_get_type")
                                    (:NORMAL 0)
                                    (:LOW 1)
                                    (:HIGH 2)
                                    (:URGENT 3))
             (gobject:get-gtype-definition "GNotificationPriority"))))

;;;     GNotification

(test g-notification-class
  ;; Check type
  (is (g:type-is-object "GNotification"))
  ;; Check registered symbol
  (is (eq 'gio:notification
          (glib:symbol-for-gtype "GNotification")))
  ;; Check type initializer
  (is (eq (g:gtype "GNotification")
          (g:gtype (cffi:foreign-funcall "g_notification_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GNotification")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GNotification")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GNotification")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GNotification")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GNotification")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GNotification" GIO:NOTIFICATION
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "g_notification_get_type")
                      NIL)
             (gobject:get-gtype-definition "GNotification"))))

;;; Functions

;;;     g_notification_new

(test g-notification-new
  (glib-test:with-check-memory (notification)
    (is (typep (setf notification
                     (g:notification-new "notification")) 'g:notification))))

;;;     g_notification_set_title
;;;     g_notification_set_body
;;;     g_notification_set_icon
;;;     g_notification_set_priority

(test g-notification-set-title
  (when *first-run-testsuite*
    (glib-test:with-check-memory (notification (icon 2) :strong 1)
      (is (typep (setf icon
                       (g:themed-icon-new "gnome-dev-cdrom")) 'g:themed-icon))
      (is (typep (setf notification
                       (g:notification-new "notification")) 'g:notification))
      (is-false (g:notification-set-title notification "new title"))
      (is-false (g:notification-set-body notification "body"))
      (is-false (g:notification-set-icon notification icon))
      (is-false (g:notification-set-priority notification :urgent)))))

;;;     g_notification_set_urgent                           not implemented

;;;     g_notification_set_default_action

(test g-notification-set-default-action
  (glib-test:with-check-memory (notification)
    (is (typep (setf notification
                     (g:notification-new "notification")) 'g:notification))
    (is-false (g:notification-set-default-action notification "app.default"))))

;;;     g_notification_set_default_action_and_target        not implemented
;;;     g_notification_set_default_action_and_target_value  not implemented

;;;     g_notification_add_button

(test g-notification-add-button
  (glib-test:with-check-memory (notification)
    (is (typep (setf notification
                     (g:notification-new "notification")) 'g:notification))
    (is-false (g:notification-add-button notification "label" "app.action"))))

;;;     g_notification_add_button_with_target               not implemented
;;;     g_notification_add_button_with_target_value         not implemented

;;; 2024-12-28
