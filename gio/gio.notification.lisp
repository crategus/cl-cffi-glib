;;; ----------------------------------------------------------------------------
;;; gio.notification.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2021- 2023 Dieter Kaiser
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
;;; GNotification
;;;
;;;     User Notifications (pop up messages)
;;;
;;; Types and Values
;;;
;;;     GNotification
;;;     GNotificationPriority
;;;
;;; Functions
;;;
;;;     g_notification_new
;;;     g_notification_set_title
;;;     g_notification_set_body
;;;     g_notification_set_icon
;;;     g_notification_set_priority
;;;     g_notification_set_urgent
;;;     g_notification_set_default_action
;;;     g_notification_set_default_action_and_target
;;;     g_notification_set_default_action_and_target_value
;;;     g_notification_add_button
;;;     g_notification_add_button_with_target
;;;     g_notification_add_button_with_target_value
;;;
;;; Object Hierarchy
;;;
;;;     GEnum
;;;     ╰── GNotificationPriority
;;;
;;;     GObject
;;;     ╰── GNotification
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GNotificationPriority
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GNotificationPriority" notification-priority
  (:export t
   :type-initializer "g_notification_priority_get_type")
  (:normal 0)
  (:low 1)
  (:high 2)
  (:urgent 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'notification-priority)
      "GEnum"
      (liber:symbol-documentation 'notification-priority)
 "@version{#2022-12-30}
  @begin{declaration}
(gobject:define-g-enum \"GNotificationPriority\" notification-priority
  (:export t
   :type-initializer \"g_notification_priority_get_type\")
  (:normal 0)
  (:low 1)
  (:high 2)
  (:urgent 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:normal]{The default priority, to be used for the majority of
        notifications, for example email messages, software updates, completed
        download/sync operations.}
      @entry[:low]{For notifications that do not require immediate attention -
        typically used for contextual background information, such as contact
        birthdays or local weather.}
      @entry[:high]{For events that require more attention, usually because
        responses are time-sensitive, for example chat and SMS messages or
        alarms.}
      @entry[:urgent]{For urgent notifications, or notifications that require a
        response in a short space of time, for example phone calls or emergency
        warnings.}
    @end{table}
  @end{values}
  @begin{short}
    Priority levels for @class{g:notification} objects.
  @end{short}
  @see-class{g:notification}")

;;; ----------------------------------------------------------------------------
;;; GNotification
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GNotification" notification
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_notification_get_type")
  nil)

#+liber-documentation
(setf (documentation 'notification 'type)
 "@version{#2022-12-30}
  @begin{short}
    The @sym{g:notification} class is a mechanism for creating a notification to
    be shown to the user -- typically as a pop-up notification presented by the
    desktop environment shell.
  @end{short}

  The key difference between the @sym{g:notification} implementation and other
  similar APIs is that, if supported by the desktop environment, notifications
  sent with the @sym{g:notification} class will persist after the application
  has exited, and even across system reboots.

  Since the user may click on a notification while the application is not
  running, applications using the @sym{g:notification} class should be able to
  be started as a D-Bus service, using the @class{g:application} class.

  User interaction with a notification, either the default action, or buttons,
  must be associated with actions on the application, i.e. \"app.\" actions. It
  is not possible to route user interaction through the notification itself,
  because the object will not exist if the application is autostarted as a
  result of a notification being clicked.

  A notification can be sent with the @fun{g:application-send-notification}
  function.
  @see-class{g:application}")

;;; ----------------------------------------------------------------------------
;;; g_notification_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_notification_new" notification-new)
    (gobject:object notification)
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[title]{a string with the title of the notification}
  @return{A new @class{g:notification} instance.}
  @begin{short}
    Creates a new notification with @arg{title} as its title.
  @end{short}
  After populating the notification with more details, it can be sent to the
  desktop shell with the @fun{g:application-send-notification} function.
  Changing any properties after this call will not have any effect until
  resending the notification.
  @see-class{g:notification}
  @see-function{g:application-send-notification}"
  (title :string))

(export 'notification-new)

;;; ----------------------------------------------------------------------------
;;; g_notification_set_title ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_notification_set_title" notification-set-title) :void
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[notification]{a @class{g:notification} instance}
  @argument[title]{a string with the new title for the notification}
  @begin{short}
    Sets the title of the notification.
  @end{short}
  @see-class{g:notification}"
  (notification (gobject:object notification))
  (title :string))

(export 'notification-set-title)

;;; ----------------------------------------------------------------------------
;;; g_notification_set_body ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_notification_set_body" notification-set-body) :void
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[notification]{a @class{g:notification} instance}
  @argument[body]{a string with the body for the notification, or @code{nil}}
  @begin{short}
    Sets the body of the notification.
  @end{short}
  @see-class{g:notification}"
  (notification (gobject:object notification))
  (body :string))

(export 'notification-set-body)

;;; ----------------------------------------------------------------------------
;;; g_notification_set_icon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_notification_set_icon" notification-set-icon) :void
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[notification]{a @class{g:notification} instance}
  @argument[icon]{a @class{g:icon} icon to be shown in the notification}
  @begin{short}
    Sets the icon of the notification.
  @end{short}
  @see-class{g:notification}
  @see-class{g:icon}"
  (notification (gobject:object notification))
  (icon (gobject:object icon)))

(export 'notification-set-icon)

;;; ----------------------------------------------------------------------------
;;; g_notification_set_priority ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_notification_set_priority" notification-set-priority) :void
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[notification]{a @class{g:notification} instance}
  @argument[priority]{a @symbol{g:notification-priority} value}
  @begin{short}
    Sets the priority of the notification.
  @end{short}
  See the @symbol{g:notification-priority} enumeration for possible values.
  @see-class{g:notification}
  @see-symbol{g:notification-priority}"
  (notification (gobject:object notification))
  (priority notification-priority))

(export 'notification-set-priority)

;;; ----------------------------------------------------------------------------
;;; g_notification_set_urgent ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_notification_set_urgent" notification-set-urgent) :void
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[notification]{a @class{g:notification} instance}
  @argument[urgent]{@em{true} if the notification is urgent}
  @begin{short}
    Deprecated in favor of the @fun{g:notification-set-priority} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{g:notification-set-urgent} function has been deprecated since
    version 2.42 and should not be used in newly written code.
  @end{dictionary}
  @see-class{g:notification}
  @see-function{g:notification-set-priority}"
  (notification (gobject:object notification))
  (urgent :boolean))

(export 'notification-set-urgent)

;;; ----------------------------------------------------------------------------
;;; g_notification_set_default_action ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_notification_set_default_action"
                notification-set-default-action) :void
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[notifiaction]{a @class{g:notification} instance}
  @argument[action]{a string with a detailed action name}
  @begin{short}
    Sets the default action of the notification to @arg{action}.
  @end{short}
  The action is activated when the notification is clicked on.

  The action in @arg{action} must be an application wide action, it must start
  with \"app.\". If the @arg{action} argument contains a target, the given
  action will be activated with that target as its parameter. See the
  @fun{g:action-parse-detailed-name} function for a description of the format
  for @arg{action}.

  When no default action is set, the application that the notification was sent
  on is activated.
  @see-class{g:notification}
  @see-function{g:action-parse-detailed-name}"
  (notification (gobject:object notification))
  (action :string))

(export 'notification-set-default-action)

;;; ----------------------------------------------------------------------------
;;; g_notification_set_default_action_and_target ()
;;;
;;; void
;;; g_notification_set_default_action_and_target
;;;                                (GNotification *notification,
;;;                                 const gchar *action,
;;;                                 const gchar *target_format,
;;;                                 ...);
;;;
;;; Sets the default action of notification to action . This action is activated
;;; when the notification is clicked on. It must be an application-wide action
;;; (it must start with "app.").
;;;
;;; If target_format is given, it is used to collect remaining positional
;;; parameters into a GVariant instance, similar to g_variant_new(). action will
;;; be activated with that GVariant as its parameter.
;;;
;;; When no default action is set, the application that the notification was
;;; sent on is activated.
;;;
;;; notification :
;;;     a GNotification
;;;
;;; action :
;;;     an action name
;;;
;;; target_format :
;;;     a GVariant format string, or NULL.
;;;
;;; ... :
;;;     positional parameters, as determined by target_format
;;;
;;;Since: 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_notification_set_default_action_and_target_value ()
;;;
;;; void
;;; g_notification_set_default_action_and_target_value
;;;                                (GNotification *notification,
;;;                                 const gchar *action,
;;;                                 GVariant *target);
;;;
;;; Sets the default action of notification to action . This action is activated
;;; when the notification is clicked on. It must be an application-wide action
;;; (start with "app.").
;;;
;;; If target is non-NULL, action will be activated with target as its
;;; parameter.
;;;
;;; When no default action is set, the application that the notification was
;;; sent on is activated.
;;;
;;; [rename-to g_notification_set_default_action_and_target]
;;;
;;; notification :
;;;     a GNotification
;;;
;;; action :
;;;     an action name
;;;
;;; target :
;;;     a GVariant to use as action 's parameter, or NULL.
;;;
;;; Since: 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;g_notification_add_button ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_notification_add_button" notification-add-button) :void
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[notification]{a @class{g:notification} instance}
  @argument[label]{a string with the label of the button}
  @argument[action]{a string with the detailed action name}
  @begin{short}
    Adds a button to the notification that activates the action in
    @arg{action} when clicked.
  @end{short}
  That action must be an application wide action, starting with \"app.\".
  If @arg{action} contains a target, the action will be activated with that
  target as its parameter. See the @fun{g:action-parse-detailed-name} function
  for a description of the format for @arg{action}.
  @see-class{g:notification}
  @see-class{g:action}
  @see-function{g:action-parse-detailed-name}"
  (notification (gobject:object notification))
  (label :string)
  (action :string))

(export 'notification-add-button)

;;; ----------------------------------------------------------------------------
;;; g_notification_add_button_with_target ()
;;;
;;; void
;;; g_notification_add_button_with_target (GNotification *notification,
;;;                                        const gchar *label,
;;;                                        const gchar *action,
;;;                                        const gchar *target_format,
;;;                                        ...);
;;;
;;; Adds a button to notification that activates action when clicked. action
;;; must be an application-wide action (it must start with "app.").
;;;
;;; If target_format is given, it is used to collect remaining positional
;;; parameters into a GVariant instance, similar to g_variant_new(). action will
;;; be activated with that GVariant as its parameter.
;;;
;;; notification :
;;;     a GNotification
;;;
;;; label :
;;;     label of the button
;;;
;;; action :
;;;     an action name
;;;
;;; target_format :
;;;     a GVariant format string, or NULL.
;;;
;;; ... :
;;;     positional parameters, as determined by target_format
;;;
;;; Since: 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_notification_add_button_with_target_value ()
;;;
;;; void
;;; g_notification_add_button_with_target_value (GNotification *notification,
;;;                                              const gchar *label,
;;;                                              const gchar *action,
;;;                                              GVariant *target);
;;;
;;; Adds a button to notification that activates action when clicked. action
;;; must be an application-wide action (it must start with "app.").
;;;
;;; If target is non-NULL, action will be activated with target as its
;;; parameter.
;;;
;;; [rename-to g_notification_add_button_with_target]
;;;
;;;
;;; notification :
;;;     a GNotification
;;;
;;; label :
;;;     label of the button
;;;
;;; action :
;;;     an action name
;;;
;;; target ;
;;;     a GVariant to use as action 's parameter, or NULL.
;;;
;;; Since: 2.40
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.notification.lisp --------------------------------------
