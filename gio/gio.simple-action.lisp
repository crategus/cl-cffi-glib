;;; ----------------------------------------------------------------------------
;;; gio.simple-action.lisp
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
;;; GSimpleAction
;;;
;;;     A simple GAction implementation
;;;
;;; Types and Values
;;;
;;;     GSimpleAction
;;;
;;; Functions
;;;
;;;     g_simple_action_new
;;;     g_simple_action_new_stateful
;;;
;;;     g_simple_action_set_enabled
;;;     g_simple_action_set_state
;;;     g_simple_action_set_state_hint
;;;
;;; Properties
;;;
;;;     enabled
;;;     name
;;;     parameter-type
;;;     state
;;;     state-type
;;;
;;; Signals
;;;
;;;     activate
;;;     change-state
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GSimpleAction
;;;
;;; Implemented Interfaces
;;;
;;;      GAction
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GSimpleAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GSimpleAction" simple-action
  (:superclass gobject:object
   :export t
   :interfaces ("GAction")
   :type-initializer "g_simple_action_get_type")
  ((enabled
    simple-action-enabled
    "enabled" "gboolean" t t)
   (name
    simple-action-name
    "name" "gchararray" t nil)
   (parameter-type
    simple-action-parameter-type
    "parameter-type" "GVariantType" t nil)
   (state
    simple-action-state
    "state" "GVariant" t t)
   (state-type
    simple-action-state-type
    "state-type" "GVariantType" t nil)))

#+liber-documentation
(setf (documentation 'simple-action 'type)
 "@version{2025-06-29}
  @begin{short}
    The @class{g:simple-action} class is the obvious simple implementation of
    the @class{g:action} interface.
  @end{short}
  This is the easiest way to create an action for purposes of adding it to a
  @class{g:simple-action-group} object.
  @begin[Signal Details]{dictionary}
    @begin[simple-action::activate]{signal}
      @begin{pre}
lambda (action parameter)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[action]{The @class{g:simple-action} object.}
        @entry[parameter]{The @symbol{g:variant} parameter to the activation.}
      @end{simple-table}
      Indicates that the action was just activated. The @arg{parameter} argument
      will always be of the expected type. In the event that an incorrect type
      was given, no signal will be emitted.
    @end{signal}
    @begin[simple-action::change-state]{signal}
      @begin{pre}
lambda (action value)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[action]{The @sym{g:simple-action} object.}
        @entry[value]{The requested @symbol{g:variant} parameter for the state.}
      @end{simple-table}
      Indicates that the action just received a request to change its state.
      The @arg{value} argument will always be of the correct state type. In the
      event that an incorrect type was given, no signal will be emitted.
      If no handler is connected to this signal then the default behaviour is
      to call the @fun{g:simple-action-state} function to set the state to the
      requested value. If you connect a signal handler then no default action
      is taken. If the state should change then you must call the
      @fun{g:simple-action-state} function from the handler. @break{}
      @b{Examples:} Implementation of a @code{\"change-state\"} signal handler:
      @begin{pre}
(g:signal-connect action \"change-state\"
                  (lambda (simple value)
                    (let ((requested (g:variant-int32 value)))
                      ;; Volume only goes from 0 to 10
                      (when (and (>= requested 0) (<= requested 10))
                        (setf (g:simple-action-state simple) value)))))
      @end{pre}
      The handler need not set the state to the requested value. It could set
      it to any value at all, or take some other action.
    @end{signal}
  @end{dictionary}
  @see-constructor{g:simple-action-new}
  @see-constructor{g:simple-action-new-stateful}
  @see-slot{g:simple-action-enabled}
  @see-slot{g:simple-action-name}
  @see-slot{g:simple-action-parameter-type}
  @see-slot{g:simple-action-state}
  @see-slot{g:simple-action-state-type}
  @see-class{g:action}
  @see-class{g:simple-action-group}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:simple-action-enabled ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enabled" 'simple-action) t)
 "The @code{enabled} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action is currently enabled. If the action is disabled then calls
  to the @fun{g:action-activate} and @fun{g:action-change-state} functions have
  no effect. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'simple-action-enabled)
      "Accessor"
      (documentation 'simple-action-enabled 'function)
 "@version{2025-09-27}
  @syntax{(g:simple-action-enabled object) => enabled}
  @syntax{(setf (g:simple-action-enabled object) enabled)}
  @argument[object]{a @class{g:simple-action} object}
  @argument[enabled]{a boolean whether the action is enabled}
  @begin{short}
    The accessor for the @slot[g:simple-action]{enabled} slot of the
    @class{g:simple-action} class gets or sets whether the action is enabled.
  @end{short}
  An action must be enabled in order to be activated or in order to have its
  state changed from outside callers. This should only be called by the
  implementor of the action. Users of the action should not attempt to modify
  its enabled flag.
  @see-class{g:simple-action}")

;;; --- g:simple-action-name ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'simple-action) t)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The name of the action. This is mostly meaningful for identifying the action
  once it has been added to a @class{g:action-map} instance. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'simple-action-name)
      "Accessor"
      (documentation 'simple-action-name 'function)
 "@version{2025-09-27}
  @syntax{(g:simple-action-name object) => name}
  @argument[object]{a @class{g:simple-action} object}
  @argument[name]{a string for the name of the action}
  @begin{short}
    The accessor for the @slot[g:simple-action]{name} slot of the
    @class{g:simple-action} class returns the name of the action.
  @end{short}
  This is mostly meaningful for identifying the action once it has been added
  to a @class{g:action-map} instance.
  @see-class{g:simple-action}
  @see-class{g:action-map}")

;;; --- g:simple-action-parameter-type -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "parameter-type"
                                               'simple-action) t)
 "The @code{parameter-type} property of type @class{g:variant-type}
  (Read / Write / Construct Only) @br{}
  The type of the parameter that must be given when activating the action.")

#+liber-documentation
(setf (liber:alias-for-function 'simple-action-parameter-type)
      "Accessor"
      (documentation 'simple-action-parameter-type 'function)
 "@version{2025-09-27}
  @syntax{(g:simple-action-parameter-type object) => vtype}
  @argument[object]{a @class{g:simple-action} object}
  @argument[vtype]{a @class{g:variant-type} parameter type}
  @begin{short}
    The accessor for the @slot[g:simple-action]{parameter-type} slot of the
    @class{g:simple-action} class returns the type of the parameter that must
    be given when activating the action.
  @end{short}
  @see-class{g:simple-action}
  @see-class{g:variant-type}")

;;; --- g:simple-action-state --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "state" 'simple-action) t)
 "The @code{state} property of type @symbol{g:variant}
  (Read / Write / Construct) @br{}
  The state of the action, or the @code{cffi:null-pointer} value if the action
  is stateless.")

#+liber-documentation
(setf (liber:alias-for-function 'simple-action-state)
      "Accessor"
      (documentation 'simple-action-state 'function)
 "@version{2025-09-27}
  @syntax{(g:simple-action-state object) => value}
  @syntax{(setf (g:simple-action-state object) value)}
  @argument[object]{a @class{g:simple-action} object}
  @argument[value]{a @symbol{g:variant} parameter for the state}
  @begin{short}
    The accessor for the @slot[g:simple-action]{state} slot of the
    @class{g:simple-action} class gets or sets the state of the action.
  @end{short}
  This directly updates the @slot[g:simple-action]{state} property to the given
  @arg{value}. This should only be called by the implementor of the action.
  Users of the action should not attempt to directly modify the
  @slot[g:simple-action]{state} property. Instead, they should call the
  @fun{g:action-change-state} function to request the change.
  @see-class{g:simple-action}
  @see-symbol{g:variant}
  @see-function{g:action-change-state}")

;;; --- g:simple-action-state-type ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "state-type" 'simple-action) t)
 "The @code{state-type} property of type @class{g:variant-type} (Read) @br{}
  The type of the state that the action has, or @code{nil} if the action is
  stateless.")

#+liber-documentation
(setf (liber:alias-for-function 'simple-action-state-type)
      "Accessor"
      (documentation 'simple-action-state-type 'function)
 "@version{2025-09-27}
  @syntax{(g:simple-action-state-type object) => vtype}
  @argument[object]{a @class{g:simple-action} object}
  @argument[vtype]{a @class{g:variant-type} parameter type}
  @begin{short}
    The accessor for the @slot[g:simple-action]{state-type} slot of the
    @class{g:simple-action} class returns the type of the state that the action
    has, or @code{nil} if the action is stateless.
  @end{short}
  @see-class{g:simple-action}
  @see-class{g:variant-type}")

;;; ----------------------------------------------------------------------------
;;; g_simple_action_new
;;; ----------------------------------------------------------------------------

(defun simple-action-new (name vtype)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[name]{a string for the name of the action}
  @argument[vtype]{a @class{g:variant-type} parameter type or a type string for
    the parameter to the activate function}
  @return{The new @class{g:simple-action} object.}
  @begin{short}
    Creates a new action.
  @end{short}
  The created action is stateless. See the @fun{g:simple-action-new-stateful}
  function for a stateful action.
  @begin[Notes]{dictionary}
    A type string for the @arg{vtype} argument is converted to the
    @class{g:variant-type} parameter type with the @fun{g:variant-type-new}
    function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    A simple action with no parameter type.
    @begin{pre}
(defvar action (g:simple-action-new \"clear\" nil)) => ACTION
(g:action-name action) => \"clear\"
(g:action-enabled action) => T
(g:action-parameter-type action) => NIL
(g:action-state action) => #.(SB-SYS:INT-SAP #X00000000)
(g:action-state-type action) => NIL
    @end{pre}
    A simple action with a string parameter type.
    @begin{pre}
(setf action (g:simple-action-new \"check\" \"s\"))
=> #<G-SIMPLE-ACTION {10022B4AF3@}>
(g:action-name action) => \"check\"
(g:action-enabled action) => T
(g:action-parameter-type action) => #<G-VARIANT-TYPE {10022B5AB3@}>
(g:action-state action) => #.(SB-SYS:INT-SAP #X00000000)
(g:action-state-type action) => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:simple-action}
  @see-class{g:variant-type}
  @see-function{g:simple-action-new-stateful}
  @see-function{g:variant-type-new}"
  (let ((vtype1 (if (stringp vtype)
                    (glib:variant-type-new vtype)
                    vtype)))
    (make-instance 'simple-action
                   :name name
                   :parameter-type vtype1)))

(export 'simple-action-new)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_new_stateful
;;; ----------------------------------------------------------------------------

(defun simple-action-new-stateful (name vtype state)
 #+liber-documentation
 "@version{2025-10-08}
  @argument[name]{a string for the name of the action}
  @argument[vtype]{a @class{g:variant-type} parameter type or a type string for
    the parameter to the activate function}
  @argument[state]{a @symbol{g:variant} parameter for the initial state of the
    action}
  @return{The new @class{g:simple-action} object.}
  @begin{short}
    Creates a new stateful action.
  @end{short}
  The @arg{state} argument is the initial state of the action. All future state
  values must have the same @class{g:variant-type} parameter type as the initial
  state.
  @begin[Notes]{dictionary}
    A type string for the @arg{vtype} argument is converted to the
    @class{g:variant-type} parameter type with the @fun{g:variant-type-new}
    function.
  @end{dictionary}
  @see-class{g:simple-action}
  @see-symbol{g:variant}
  @see-class{g:variant-type}
  @see-function{g:simple-action-new}
  @see-function{g:variant-type-new}"
  (let ((vtype1 (if (stringp vtype)
                    (glib:variant-type-new vtype)
                    vtype)))
    (make-instance 'simple-action
                   :name name
                   :parameter-type vtype1
                   :state state)))

(export 'simple-action-new-stateful)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_set_state_hint
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_simple_action_set_state_hint" simple-action-set-state-hint)
    :void
 #+liber-documentation
 "@version{#2024-12-29}
  @argument[action]{a @class{g:simple-action} object}
  @argument[hint]{a @symbol{g:variant} parameter representing the state hint}
  @begin{short}
    Sets the state hint for the action.
  @end{short}
  See the @fun{g:action-state-hint} function for more information about action
  state hints.
  @see-class{g:simple-action}
  @see-symbol{g:variant}
  @see-function{g:action-state-hint}"
  (action (gobject:object simple-action))
  (hint (:pointer (:struct glib:variant))))

(export 'simple-action-set-state-hint)

;;; --- End of file gio.simple-action.lisp -------------------------------------
