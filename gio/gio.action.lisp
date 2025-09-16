;;; ----------------------------------------------------------------------------
;;; gio.action.lisp
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
;;; GAction
;;;
;;;     An action interface
;;;
;;; Types and Values
;;;
;;;     GAction
;;;
;;; Accessors
;;;
;;;     g_action_get_name
;;;     g_action_get_parameter_type
;;;     g_action_get_state_type
;;;     g_action_get_enabled
;;;     g_action_get_state
;;;
;;; Functions
;;;
;;;     g_action_name_is_valid
;;;     g_action_get_state_hint
;;;     g_action_change_state
;;;     g_action_activate
;;;     g_action_parse_detailed_name
;;;     g_action_print_detailed_name
;;;
;;; Properties
;;;
;;;     enabled
;;;     name
;;;     parameter-type
;;;     state
;;;     state-type
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GAction
;;;
;;; Prerequisites
;;;
;;;     GAction requires GObject
;;;
;;; Known Implementations
;;;
;;;     GAction is implemented by GPropertyAction and GSimpleAction
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GAction
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GAction" action
  (:export t
   :type-initializer "g_action_get_type")
  ((enabled
    action-enabled
    "enabled" "gboolean" t t)
   (name
    action-name
    "name" "gchararray" t nil)
   (parameter-type
    action-parameter-type
    "parameter-type" "GVariantType" t nil)
   (state
    action-state
    "state" "GVariant" t nil)
   (state-type
    action-state-type
    "state-type" "GVariantType" t nil)))

#+liber-documentation
(setf (liber:alias-for-class 'action)
      "Interface"
      (documentation 'action 'type)
 "@version{2025-08-27}
  @begin{short}
    The @class{g:action} interface represents a single named action.
  @end{short}
  The main interface to an action is that it can be activated with the
  @fun{g:action-activate} function. This results in the @code{\"activate\"}
  signal being emitted. An activation has a @sym{g:variant} parameter, which
  may be @code{nil}. The correct type for the parameter is determined by a
  static parameter type, which is given at construction time.

  An action may optionally have a state, in which case the state may be set
  with the @fun{g:action-change-state} function. This call takes a
  @sym{g:variant} parameter. The correct type for the state is determined by
  a static state type, which is given at construction time. The state may have
  a hint associated with it, specifying its valid range.

  The @class{g:action} interface is merely the interface to the concept of an
  action, as described above. Various implementations of actions exist,
  including the @class{g:simple-action} class.

  In all cases, the implementing class is responsible for storing the name of
  the action, the parameter type, the enabled state, the optional state type
  and the state and emitting the appropriate signals when these change. The
  implementor responsible for filtering calls to the @fun{g:action-activate}
  and @fun{g:action-change-state} functions for type safety and for the state
  being enabled.

  Probably the only useful thing to do with a @class{g:action} object is to put
  it inside of a @class{g:simple-action-group} object.
  @see-slot{g:action-enabled}
  @see-slot{g:action-name}
  @see-slot{g:action-parameter-type}
  @see-slot{g:action-state}
  @see-slot{g:action-state-type}
  @see-class{g:simple-action}
  @see-class{g:simple-action-group}
  @see-function{g:action-activate}
  @see-function{g:action-change-state}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:action-enabled -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enabled" 'action) t)
 "The @code{enabled} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action is currently enabled. If the action is disabled then calls
  to the @fun{g:action-activate} and @fun{g:action-change-state} functions
  have no effect. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-enabled)
      "Accessor"
      (documentation 'action-enabled 'function)
 "@version{2025-08-27}
  @syntax{(g:action-enabled object) => enabled}
  @syntax{(setf (g:action-enabled object) enabled)}
  @argument[object]{a @class{g:action} object}
  @argument[enabled]{a boolean whether the action is enabled}
  @begin{short}
    The accessor for the @slot[g:action]{enabled} slot of the @class{g:action}
    class gets or sets whether the action is enabled.
  @end{short}
  An action must be enabled in order to be activated or in order to have its
  state changed from outside callers.
  @see-class{g:action}")

;;; --- g:action-name ----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'action) t)
 "The @code{name} property of type @code{:string} (Read) @br{}
  The name of the action. This is mostly meaningful for identifying the action
  once it has been added to a @class{g:action-map} instance. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'action-name)
      "Accessor"
      (documentation 'action-name 'function)
 "@version{2025-08-27}
  @syntax{(g:action-name object) => name}
  @argument[action]{a @class{g:action} object}
  @argument[name]{a string for the name of the action}
  @begin{short}
    The accessor for the @slot[g:action]{name} slot of the @class{g:action}
    class queries the name of the action.
  @end{short}
  This is mostly meaningful for identifying the action once it has been added
  to a @class{g:action-map} instance.
  @see-class{g:action}
  @see-class{g:action-map}")

;;; --- g:action-parameter-type ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "parameter-type" 'action) t)
 "The @code{parameter-type} property of type @class{g:variant-type} (Read) @br{}
  The type of the parameter that must be given when activating the action.")

#+liber-documentation
(setf (liber:alias-for-function 'action-parameter-type)
      "Accessor"
      (documentation 'action-parameter-type 'function)
 "@version{2025-08-27}
  @syntax{(g:action-parameter-type object) => vtype}
  @argument[object]{a @class{g:action} object}
  @argument[vtype]{a @class{g:variant-type} parameter type}
  @begin{short}
    The accessor for the @slot[g:action]{parameter-type} slot of the
    @class{g:action} class queries the type of the parameter that must be given
    when activating the action.
  @end{short}
  When activating the action using the @fun{g:action-activate} function, the
  @sym{g:variant} parameter given to that function must be of the type returned
  by this function.

  In the case that this function returns a @code{nil} value, you must not give
  any @sym{g:variant} parameter, but a @code{cffi:null-pointer} value instead.
  @see-class{g:action}
  @see-class{g:variant-type}
  @see-symbol{g:variant}
  @see-function{g:action-activate}")

;;; --- g:action-state ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "state" 'action) t)
 "The @code{state} property of type @sym{g:variant} (Read) @br{}
  The state of the action, or the @code{cffi:null-pointer} value if the action
  is stateless.")

#+liber-documentation
(setf (liber:alias-for-function 'action-state)
      "Accessor"
      (documentation 'action-state 'function)
 "@version{2025-08-27}
  @syntax{(g:action-state object) => state}
  @argument[object]{a @class{g:action} object}
  @argument[state]{a @sym{g:variant} parameter for the state of the action}
  @begin{short}
    The accessor for the @slot[g:action]{state} slot of the @class{g:action}
    class queries the current state of the action.
  @end{short}
  If the action is not stateful then a @code{cffi:null-pointer} value will be
  returned. If the action is stateful then the type of the return value is the
  type given by the @fun{g:action-state-type} function.
  @begin[Examples]{dictionary}
    A stateful action with an integer.
    @begin{pre}
(defvar state (g:variant-new-int32 123)) => STATE
(defvar action (g:simple-action-new-stateful \"stateful\" nil state)) => ACTION
(g:action-state action) => #.(SB-SYS:INT-SAP #X560FD04EFE10)
(g:variant-int32 *) => 123
(setf (g:action-state action) (g:variant-new-int32 999))
=> #.(SB-SYS:INT-SAP #X560FD04F2C40)
(g:action-state action) => #.(SB-SYS:INT-SAP #X560FD04F2C40)
(g:variant-int32 *) => 999
    @end{pre}
    A simple action with no state returns a @code{cffi:null-pointer} value.
    @begin{pre}
(setf action (g:simple-action-new \"simple\" nil))
=>  #<G-SIMPLE-ACTION {1004B2CE73@}>
(g:action-state *) => #.(SB-SYS:INT-SAP #X00000000)
    @end{pre}
  @end{dictionary}
  @see-class{g:action}
  @see-symbol{g:variant}
  @see-function{g:action-state-type}")

;;; --- g:action-state-type ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "state-type" 'action) t)
 "The @code{state-type} property of type @class{g:variant-type} (Read) @br{}
  The parameter type of the state that the action has, or @code{nil} if the
  action is stateless.")

#+liber-documentation
(setf (liber:alias-for-function 'action-state-type)
      "Accessor"
      (documentation 'action-state-type 'function)
 "@version{2025-08-27}
  @syntax{(g:action-state-type object) => vtype}
  @argument[object]{a @class{g:action} object}
  @argument[vtype]{a @class{g:variant-type} parameter type, for the state type
    if the action is stateful}
  @begin{short}
    The accessor for the @slot[g:action]{state-type} slot of the @class{action}
    class queries the type of the state of the action.
  @end{short}
  If the action is stateful, for example created with the
  @fun{g:simple-action-new-stateful} function, then this function returns the
  @class{g:variant-type} parameter type of the state. This is the type of the
  initial value given as the state. All calls to the @fun{g:action-change-state}
  function must give a @sym{g:variant} parameter of this type and the
  @fun{g:action-state} function will return a @sym{g:variant} parameter of the
  same type.

  If the action is not stateful, for example created with the
  @fun{g:simple-action-new} function, then this function will return @code{nil}.
  In that case, the @fun{g:action-state} function will return a
  @code{cffi:null-pointer} value and you must not call the
  @fun{g:action-change-state} function.
  @see-class{g:action}
  @see-class{g:variant-type}
  @see-symbol{g:variant}
  @see-function{g:simple-action-new}
  @see-function{g:simple-action-new-stateful}
  @see-function{g:action-change-state}
  @see-function{g:action-state}")

;;; ----------------------------------------------------------------------------
;;; g_action_name_is_valid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_name_is_valid" action-name-is-valid) :boolean
 #+liber-documentation
 "@version{2025-02-03}
  @argument[name]{a string for an action name}
  @return{@em{True} if the @arg{name} argument is a valid action name.}
  @begin{short}
    Checks if the action name is valid.
  @end{short}
  The action name is valid if it consists only of alphanumeric characters, plus
  '-' and '.'. The empty string is not a valid action name. It is an error to
  call this function with a non UTF-8 action name.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:action-name-is-valid \"action\") => T
(g:action-name-is-valid \"win.action\") => T
(g:action-name-is-valid \"win-action\") => T
(g:action-name-is-valid \"win-action!\") NIL
(g:action-name-is-valid \"\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:action}"
  (name :string))

(export 'action-name-is-valid)

;;; ----------------------------------------------------------------------------
;;; g_action_get_state_hint
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_get_state_hint" action-state-hint)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{2025-08-27}
  @argument[action]{a @class{g:action} object}
  @return{The @sym{g:variant} parameter for the state range hint.}
  @begin{short}
    Requests a hint about the valid range of values for the state of the action.
  @end{short}
  If a @code{cffi:null-pointer} value is returned it either means that the
  action is not stateful or that there is no hint about the valid range of
  values for the state of the action.

  If a @sym{g:variant} parameter array is returned then each item in the array
  is a possible value for the state. If a @sym{g:variant} parameter pair, for
  example two-tuple, is returned then the tuple specifies the inclusive lower
  and upper bound of valid values for the state.

  In any case, the information is merely a hint. It may be possible to have a
  state value outside of the hinted range and setting a value within the range
  may fail.
  @see-class{g:action}
  @see-symbol{g:variant}"
  (action (gobject:object action)))

(export 'action-state-hint)

;;; ----------------------------------------------------------------------------
;;; g_action_change_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_change_state" action-change-state) :void
 #+liber-documentation
 "@version{2025-08-27}
  @argument[action]{a @class{g:action} object}
  @argument[value]{a @sym{g:variant} parameter for the new state}
  @begin{short}
    Request for the state of the action to be changed to @arg{value}.
  @end{short}
  The action must be stateful and the @arg{value} argument must be of the
  correct type. See the @fun{g:action-state-type} function.

  This call merely requests a change. The action may refuse to change its
  state or may change its state to something other than @arg{value}. See the
  @fun{g:action-state-hint} function.
  @see-class{g:action}
  @see-symbol{g:variant}
  @see-function{g:action-state-type}
  @see-function{g:action-state-hint}"
  (action (gobject:object action))
  (value (:pointer (:struct glib:variant))))

(export 'action-change-state)

;;; ----------------------------------------------------------------------------
;;; g_action_activate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_activate" %action-activate) :void
  (action :pointer)
  (parameter :pointer))

(defun action-activate (action &optional (parameter nil))
 #+liber-documentation
 "@version{2025-08-27}
  @argument[action]{a @class{g:action} object}
  @argument[parameter]{an optional @sym{g:variant} parameter to the
    activation}
  @begin{short}
    Activates the action.
  @end{short}
  The optional @arg{parameter} argument must be the correct type of the
  parameter for the action, for example the parameter type given at construction
  time. If the parameter type was a @code{cffi:null-pointer} value then the
  @arg{parameter} argument must be @code{nil}, this is the default value.
  @see-class{g:action}
  @see-symbol{g:variant}"
  (let ((parameter (or parameter (cffi:null-pointer))))
    (%action-activate (gobject:object-pointer action) parameter)))

(export 'action-activate)

;;; ----------------------------------------------------------------------------
;;; g_action_parse_detailed_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_parse_detailed_name" %action-parse-detailed-name)
    :boolean
  (detailed :string)
  (name :string)
  (value (:pointer (:struct glib:variant)))
  (err :pointer))

(defun action-parse-detailed-name (detailed)
 #+liber-documentation
 "@version{2025-08-27}
  @syntax{(g:action-parse-detailed-name detailed) => name, value}
  @argument[detailed]{a string for a detailed action name}
  @argument[name]{a string for the action name}
  @argument[value]{a @sym{g:variant} parameter for the target value, or a
    @code{cffi:null-pointer} value for no target}
  @begin{short}
    Parses a detailed action name into its separate name and target components.
  @end{short}
  Detailed action names can have three formats.

  The first format is used to represent an action name with no target value and
  consists of just an action name containing no whitespace nor the characters
  ':', '(' or ')'. For example: @code{\"app.action\"}.

  The second format is used to represent an action with a target value that is
  a non-empty string consisting only of alphanumerics, plus '-' and '.'. In
  that case, the action name and target value are separated by a double colon
  (\"::\"). For example: @code{\"app.action::target\"}.

  The third format is used to represent an action with any type of target value,
  including strings. The target value follows the action name, surrounded in
  parens. For example: @code{\"app.action(42)\"}. The target value is parsed
  using the @fun{g:variant-parse} function. If a tuple-typed value is desired,
  it must be specified in the same way, resulting in two sets of parens, for
  example: @code{\"app.action((1,2,3))\"}. A string target can be specified this
  way as well: @code{\"app.action('target')\"}. For strings, this third format
  must be used if *target value is empty or contains characters other than
  alphanumerics, '-' and '.'.
  @begin[Examples]{dictionary}
    @begin{pre}
;; First format
(g:action-parse-detailed-name \"app.action\")
=> \"app.action\"
=> #.(SB-SYS:INT-SAP #X00000000)
;; Second format
(g:action-parse-detailed-name \"app.action::target\")
=> \"app.action\"
=> #.(SB-SYS:INT-SAP #X7F5B7000E8D0)
(g:variant-string
    (second (multiple-value-list
        (g:action-parse-detailed-name \"app.action::target\"))))
=> \"target\"
;; Third format
(g:action-parse-detailed-name \"app.action(42)\")
=> \"app.action\"
=> #.(SB-SYS:INT-SAP #X7F5B7000E870)
(g:variant-int32
    (second (multiple-value-list
        (g:action-parse-detailed-name \"app.action(42)\"))))
=> 42
    @end{pre}
  @end{dictionary}
  @see-class{g:action}
  @see-symbol{g:variant}
  @see-function{g:variant-parse}"
  (glib:with-error (err)
    (cffi:with-foreign-objects ((name :string)
                                (value '(:pointer (:struct glib:variant))))
      (when (%action-parse-detailed-name detailed name value err)
        (values (cffi:mem-ref name :string)
                (cffi:mem-ref value '(:pointer (:struct glib:variant))))))))

(export 'action-parse-detailed-name)

;;; ----------------------------------------------------------------------------
;;; g_action_print_detailed_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_action_print_detailed_name" %action-print-detailed-name)
    :string
  (name :string)
  (value (:pointer (:struct glib:variant))))

(defun action-print-detailed-name (name &optional (value nil))
 #+liber-documentation
 "@version{2025-08-27}
  @argument[name]{a string for a valid action name}
  @argument[value]{an optional @sym{g:variant} parameter for the target value}
  @begin{short}
    Formats a detailed action name from an action name and a target value.
  @end{short}
  It is an error to call this function with an invalid action name. This
  function is the opposite of the @fun{g:action-parse-detailed-name} function.
  It will produce a string that can be parsed back to the action name and
  target value by that function. See that function for the types of strings
  that will be printed by this function.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:action-print-detailed-name \"action\")
=> \"action\"
(g:action-print-detailed-name \"action\" (g:variant-new-boolean \"t\"))
=> \"action(true)\"
(g:action-print-detailed-name \"action\" (g:variant-new-int32 42))
=> \"action(42)\"
    @end{pre}
  @end{dictionary}
  @see-class{g:action}
  @see-symbol{g:variant}
  @see-function{g:action-parse-detailed-name}"
  (%action-print-detailed-name name (or value (cffi:null-pointer))))

(export 'action-print-detailed-name)

;;; --- End of file gio.action.lisp --------------------------------------------
