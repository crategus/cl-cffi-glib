;;; ----------------------------------------------------------------------------
;;; gio.action-group.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; GActionGroup
;;;
;;;     A group of actions
;;;
;;; Types and Values
;;;
;;;     GActionGroup
;;;
;;; Functions
;;;
;;;     g_action_group_list_actions
;;;     g_action_group_query_action                         not implemented
;;;     g_action_group_has_action
;;;     g_action_group_get_action_enabled
;;;     g_action_group_get_action_parameter_type
;;;     g_action_group_get_action_state_type
;;;     g_action_group_get_action_state_hint
;;;     g_action_group_get_action_state
;;;     g_action_group_change_action_state
;;;     g_action_group_activate_action
;;;     g_action_group_action_added
;;;     g_action_group_action_removed
;;;     g_action_group_action_enabled_changed
;;;     g_action_group_action_state_changed
;;;
;;; Signals
;;;
;;;     action-added
;;;     action-enabled-changed
;;;     action-removed
;;;     action-state-changed
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GActionGroup
;;;
;;; Prerequisites
;;;
;;;     GActionGroup requires GObject.
;;;
;;; Known Derived Interfaces
;;;
;;;     GActionGroup is required by GRemoteActionGroup.
;;;
;;; Known Implementations
;;;
;;;     GActionGroup is implemented by GApplication, GDBusActionGroup and
;;;     GSimpleActionGroup.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GActionGroup
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GActionGroup" action-group
  (:export t
   :type-initializer "g_action_group_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'action-group)
      "Interface"
      (documentation 'action-group 'type)
 "@version{2025-06-21}
  @begin{short}
    The @class{g:action-group} interface represents a group of actions.
  @end{short}
  Actions can be used to expose functionality in a structured way, either from
  one part of a program to another, or to the outside world. Action groups are
  often used together with a @class{g:menu-model} object that provides
  additional representation data for displaying the actions to the user, for
  example in a menu.

  The main way to interact with the actions in a @class{g:action-group} instance
  is to activate them with the @fun{g:action-group-activate-action} function.
  Activating an action may require a @symbol{g:variant} parameter. The required
  type of the parameter can be inquired with the
  @fun{g:action-group-action-parameter-type} function. Actions may be disabled,
  see the @fun{g:action-group-action-enabled} function. Activating a disabled
  action has no effect.

  Actions may optionally have a state in the form of a @symbol{g:variant}
  parameter. The current state of an action can be inquired with the
  @fun{g:action-group-action-state} function. Activating a stateful action may
  change its state, but it is also possible to set the state by calling
  the @fun{g:action-group-change-action-state} function.

  As typical example, consider a text editing application which has an option
  to change the current font to 'bold'. A good way to represent this would be
  a stateful action, with a boolean state. Activating the action would toggle
  the state.

  Each action in the group has a unique name which is a string. All method
  calls, except the @fun{g:action-group-list-actions} function take the name
  of an action as an argument.

  The @class{g:action-group} API is meant to be the 'public' API to the action
  group. The calls here are exactly the interaction that 'external forces',
  for example UI, incoming D-Bus messages, and so on, are supposed to have with
  actions. 'Internal' APIs, that is, ones meant only to be accessed by the
  action group implementation, are found on subclasses. This is why you will
  find, for example, the @fun{g:action-group-action-enabled} function but not
  an equivalent setter function.

  Signals are emitted on the action group in response to state changes on
  individual actions.

  Implementations of the @class{g:action-group} interface should provide
  implementations for the @fun{g:action-group-list-actions} and
  @fun{g:action-group-query-action} virtual functions. The other virtual
  functions should not be implemented - their \"wrappers\" are actually
  implemented with calls to the @fun{g:action-group-query-action} function.



  @begin[Signal Details]{dictionary}
    @begin[action-group::action-added]{signal}
      @begin{pre}
lambda (group name)    :detailed
      @end{pre}
      @begin[code]{table}
        @entry[group]{The @class{g:action-group} instance that changed.}
        @entry[name]{The string with the name of the action.}
      @end{table}
      Signals that a new action was just added to the group. The signal is
      emitted after the action has been added and is now visible.
    @end{signal}
    @begin[action-group::action-enabled-changed]{signal}
      @begin{pre}
lambda (group name enabled)    :detailed
      @end{pre}
      @begin[code]{table}
        @entry[group]{The @class{g:action-group} instance that changed.}
        @entry[name]{The string with the name of the action.}
        @entry[enabled]{The boolean whether the action is enabled or not.}
      @end{table}
      Signals that the enabled status of the named action has changed.
    @end{signal}
    @begin[action-group::action-removed]{signal}
      @begin{pre}
lambda (group name)    :detailed
      @end{pre}
      @begin[code]{table}
        @entry[group]{The @class{g:action-group} instance that changed.}
        @entry[name]{The string with the name of the action.}
      @end{table}
      Signals that an action is just about to be removed from the group. This
      signal is emitted before the action is removed, so the action is still
      visible and can be queried from the signal handler.
    @end{signal}
    @begin[action-group::action-state-changed]{signal}
      @begin{pre}
lambda (group name parameter)    :detailed
      @end{pre}
      @begin[code]{table}
        @entry[group]{The @class{g:action-group} instance that changed.}
        @entry[name]{The string with the name of the action.}
        @entry[parameter]{The new @symbol{g:variant} parameter for the state.}
      @end{table}
      Signals that the state of the named action has changed.
    @end{signal}
  @end{dictionary}
  @see-class{g:simple-action-group}")

;;; ----------------------------------------------------------------------------
;;; g_action_group_list_actions
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_list_actions" action-group-list-actions)
    glib:strv-t
 #+liber-documentation
 "@version{2024-12-27}
  @argument[group]{a @class{g:action-group} instance}
  @return{The list of strings with the names of the actions in the group.}
  @begin{short}
    Lists the actions contained within the action group.
  @end{short}
  @see-class{g:action-group}"
  (group (gobject:object action-group)))

(export 'action-group-list-actions)

;;; ----------------------------------------------------------------------------
;;; g_action_group_query_action
;;;
;;; Queries all aspects of the named action within an action_group.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_action_group_has_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_has_action" action-group-has-action) :boolean
 #+liber-documentation
 "@version{2025-02-03}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of the action to check for}
  @return{The boolean whether the named action exists.}
  @begin{short}
    Checks if the named action exists within the action group.
  @end{short}
  @see-class{g:action-group}"
  (group (gobject:object action-group))
  (name :string))

(export 'action-group-has-action)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_enabled
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_get_action_enabled" action-group-action-enabled)
    :boolean
 #+liber-documentation
 "@version{2025-02-03}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of the action to query}
  @return{The boolean whether or not the action is currently enabled.}
  @begin{short}
    Checks if the named action within the action group is currently enabled.
  @end{short}
  An action must be enabled in order to be activated or in order to have its
  state changed from outside callers.
  @see-class{g:action-group}"
  (group (gobject:object action-group))
  (name :string))

(export 'action-group-action-enabled)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_parameter_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_get_action_parameter_type"
                action-group-action-parameter-type)
    (glib:boxed glib:variant-type)
 #+liber-documentation
 "@version{2025-02-03}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of the action to query}
  @return{The @class{g:variant-type} parameter type.}
  @begin{short}
    Queries the type of the parameter that must be given when activating the
    named action within the action group.
  @end{short}
  When activating the action using the @fun{g:action-group-activate-action}
  function, the @class{g:variant-type} parameter type given to that function
  must be of the type returned by this function.

  In the case that this function returns @code{nil}, you must not give any
  @symbol{g:variant} parameter, but @code{nil} instead.

  The parameter type of a particular action will never change but it is
  possible for an action to be removed and for a new action to be added with
  the same name but a different parameter type.
  @see-class{g:action-group}
  @see-class{g:variant-type}
  @see-symbol{g:variant}
  @see-function{g:action-group-activate-action}
  @see-function{g:action-map-lookup-action}
  @see-function{g:action-parameter-type}"
  (group (gobject:object action-group))
  (name :string))

(export 'action-group-action-parameter-type)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_state_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_get_action_state_type"
                action-group-action-state-type) (glib:boxed glib:variant-type)
 #+liber-documentation
 "@version{2025-02-03}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of the action to query}
  @return{The @class{g:variant-type} state type, if the action is stateful.}
  @begin{short}
    Queries the type of the state of the named action within the action group.
  @end{short}
  If the action is stateful then this function returns the
  @class{g:variant-type} state type of the state. All calls to the
  @fun{g:action-group-change-action-state} function must give a
  @symbol{g:variant} parameter of this type and the
  @fun{g:action-group-action-state} function will return a @symbol{g:variant}
  parameter of the same type.

  If the action is not stateful then this function will return @code{nil}. In
  that case, the @fun{g:action-group-action-state} function will return
  @code{nil} and you must not call the @fun{g:action-group-change-action-state}
  function.

  The state type of a particular action will never change but it is possible
  for an action to be removed and for a new action to be added with the same
  name but a different state type.
  @see-class{g:action-group}
  @see-class{g:variant-type}
  @see-symbol{g:variant}
  @see-function{g:action-group-change-action-state}
  @see-function{g:action-group-action-state}
  @see-function{g:action-map-lookup-action}
  @see-function{g:action-state-type}"
  (group (gobject:object action-group))
  (name :string))

(export 'action-group-action-state-type)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_state_hint
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_get_action_state_hint"
                action-group-action-state-hint)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{2025-06-21}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of the action to query}
  @begin{return}
    The @symbol{g:variant} parameter with the state range hint, or
    @code{cffi:null-pointer}.
  @end{return}
  @begin{short}
    Requests a hint about the valid range of values for the state of the named
    action within the action group.
  @end{short}

  If a @code{cffi:null-pointer} is returned it either means that the action is
  not stateful or that there is no hint about the valid range of values for the
  state of the action.

  If a @symbol{g:variant} parameter array is returned then each item in the
  array is a possible value for the state. If a @symbol{g:variant} parameter
  pair, that is two-tuple, is returned then the tuple specifies the inclusive
  lower and upper bound of valid values for the state.

  In any case, the information is merely a hint. It may be possible to have a
  state value outside of the hinted range and setting a value within the range
  may fail.
  @see-class{g:action-group}
  @see-symbol{g:variant}"
  (group (gobject:object action-group))
  (name :string))

(export 'action-group-action-state-hint)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_get_action_state" action-group-action-state)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{2025-06-21}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of the action to query}
  @begin{return}
    The current @symbol{g:variant} parameter with the state of the action,
    or a @code{cffi:null-pointer}.
  @end{return}
  @begin{short}
    Queries the current state of the named action within the action group.
  @end{short}
  If the action is not stateful then a @code{cffi:null-pointer} will be
  returned. If the action is stateful then the type of the return value is the
  type given by the @fun{g:action-group-action-state-type} function.
  @see-class{g:action-group}
  @see-symbol{g:variant}
  @see-function{g:action-group-action-state-type}"
  (group (gobject:object action-group))
  (name :string))

(export 'action-group-action-state)

;;; ----------------------------------------------------------------------------
;;; g_action_group_change_action_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_change_action_state"
                action-group-change-action-state) :void
 #+liber-documentation
 "@version{2025-02-03}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of the action to request the change on}
  @argument[parameter]{a new @symbol{g:variant} parameter for the state}
  @begin{short}
    Request for the state of the named action within the action group to be
    changed to the @arg{parameter} argument.
  @end{short}
  The action must be stateful and @arg{parameter} must be of the correct type,
  see the @fun{g:action-group-action-state-type} function. This call merely
  requests a change. The action may refuse to change its state or may change
  its state to something other than value, see the
  @fun{g:action-group-action-state-hint} function.
  @see-class{g:action-group}
  @see-symbol{g:variant}
  @see-function{g:action-group-action-state-type}
  @see-function{g:action-group-action-state-hint}"
  (group (gobject:object action-group))
  (name :string)
  (parameter (:pointer (:struct glib:variant))))

(export 'action-group-change-action-state)

;;; ----------------------------------------------------------------------------
;;; g_action_group_activate_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_activate_action" action-group-activate-action)
    :void
 #+liber-documentation
 "@version{2025-02-03}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of the action to activate}
  @argument[parameter]{a @symbol{g:variant} parameter to the activation}
  @begin{short}
    Activate the named action within the action group.
  @end{short}
  If the action is expecting a parameter, then the correct type of the parameter
  must be given as @arg{parameter}. If the action is expecting no parameters
  then the @arg{parameter} argument must be a @code{cffi:null-pointer}, see the
  @fun{g:action-group-action-parameter-type} function.
  @see-class{g:action-group}
  @see-symbol{g:variant}
  @see-function{g:action-group-action-parameter-type}"
  (group (gobject:object action-group))
  (name :string)
  (parameter :pointer))

(export 'action-group-activate-action)

;;; ----------------------------------------------------------------------------
;;; g_action_group_action_added
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_action_added" action-group-action-added) :void
 #+liber-documentation
 "@version{#2025-06-22}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of an action in the group}
  @begin{short}
    Emits the @sig[g:action-group]{action-added} signal on the action group.
  @end{short}
  This function should only be called by @class{g:action-group} implementations.
  @see-class{g:action-group}"
  (group (gobject:object action-group))
  (name :string))

(export 'action-group-action-added)

;;; ----------------------------------------------------------------------------
;;; g_action_group_action_removed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_action_removed" action-group-action-removed)
    :void
 #+liber-documentation
 "@version{#2025-06-22}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of an action in the action group}
  @begin{short}
    Emits the @sig[g:action-group]{action-removed} signal on the action group.
  @end{short}
  This function should only be called by @class{g:action-group} implementations.
  @see-class{g:action-group}"
  (group (gobject:object action-group))
  (name :string))

(export 'action-group-action-removed)

;;; ----------------------------------------------------------------------------
;;; g_action_group_action_enabled_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_action_enabled_changed"
                action-group-action-enabled-changed) :void
 #+liber-documentation
 "@version{#2025-06-22}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of an action in the action group}
  @argument[enabled]{a boolean whether or not the action is now enabled}
  @begin{short}
    Emits the @sig[g:action-group]{action-enabled-changed} signal on the action
    group.
  @end{short}
  This function should only be called by @class{g:action-group}
  implementations.
  @see-class{g:action-group}"
  (group (gobject:object action-group))
  (name :string)
  (enabled :boolean))

(export 'action-group-action-enabled-changed)

;;; ----------------------------------------------------------------------------
;;; g_action_group_action_state_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_group_action_state_changed"
                action-group-action-state-changed) :void
 #+liber-documentation
 "@version{#2025-02-03}
  @argument[group]{a @class{g:action-group} instance}
  @argument[name]{a string for the name of an action in the group}
  @argument[state]{a new @symbol{g:variant} parameter for the state of the
    named action}
  @begin{short}
    Emits the @sig[g:action-group]{action-state-changed} signal on the action
    group.
  @end{short}
  This function should only be called by @class{g:action-group} implementations.
  @see-class{g:action-group}
  @see-symbol{g:variant}"
  (group (gobject:object action-group))
  (name :string)
  (state (:pointer (:struct glib:variant))))

(export 'action-group-action-state-changed)

;;; --- End of file gio.action-group.lisp --------------------------------------
