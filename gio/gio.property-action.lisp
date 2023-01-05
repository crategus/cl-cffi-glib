;;; ----------------------------------------------------------------------------
;;; gio.property-action.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.64 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GPropertyAction
;;;
;;;     A GAction reflecting a GObject property
;;;
;;; Types and Values
;;;
;;;     GPropertyAction
;;;
;;; Functions
;;;
;;;     g_property_action_new
;;;
;;; Properties
;;;
;;;     enabled
;;;     invert-boolean
;;;     name
;;;     object
;;;     parameter-type
;;;     property-name
;;;     state
;;;     state-type
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GPropertyAction
;;;
;;; Implemented Interfaces
;;;
;;;     GPropertyAction implements GAction.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GPropertyAction
;;; ----------------------------------------------------------------------------

(define-g-object-class "GPropertyAction" property-action
  (:superclass gobject:object
   :export t
   :interfaces ("GAction")
   :type-initializer "g_property_action_get_type")
  ((enabled
    property-action-enabled
    "enabled" "gboolean" t nil)
   (invert-boolean
    property-action-invert-boolean
    "invert-boolean" "gboolean" t nil)
   (name
    property-action-name
    "name" "gchararray" t nil)
   (object
    property-action-object
    "object" "GObject" nil nil)
   (parameter-type
    property-action-parameter-type
    "parameter-type" "GVariantType" t nil)
   (property-name
    property-action-property-name
    "property-name" "gchararray" nil nil)
   (state
    property-action-state
    "state" "GVariant" t nil)
   (state-type
    property-action-state-type
    "state-type" "GVariantType" t nil)))

#+liber-documentation
(setf (documentation 'property-action 'type)
 "@version{2022-12-26}
  @begin{short}
    A @sym{g:property-action} object is a way to get a @class{g:action} object
    with a state value reflecting and controlling the value of a
    @class{g:object} property.
  @end{short}
  The state of the action will correspond to the value of the property.
  Changing it will change the property, assuming the requested value matches
  the requirements as specified in the @symbol{g:param-spec} instance.

  Only the most common types are presently supported. Booleans are mapped to
  booleans, strings to strings, signed/unsigned integers to int32/uint32 and
  floats and doubles to doubles. If the property is an enumeration then the
  state will be string-typed and conversion will automatically be performed
   between the enumeration value and \"nick\" string.

   Flags types are not currently supported. Properties of object types, boxed
   types and pointer types are not supported and probably never will be.
   Properties of @type{g:variant} types are not currently supported.

  If the property is boolean-valued then the action will have a NULL parameter
  type, and activating the action with no parameter will toggle the value of
  the property. In all other cases, the parameter type will correspond to the
  type of the property.

  The general idea here is to reduce the number of locations where a particular
  piece of state is kept and therefore has to be synchronised between. The
  @sym{g:property-action} object does not have a separate state that is kept
  in sync with the property value -- its state is the property value.

  For example, it might be useful to create a @class{g:action} object
  corresponding to the \"visible-child-name\" property of a @class{gtk:stack}
  widget so that the current page can be switched from a menu. The active radio
  indication in the menu is then directly determined from the active page of the
  @class{gtk:stack} widget.

  An anti-example would be binding the \"active-id\" property on a
  @class{gtk:combo-box} widget. This is because the state of the combobox itself
  is probably uninteresting and is actually being used to control something
  else.

  Another anti-example would be to bind to the \"visible-child-name\" property
  of a @class{gtk:stack} widget if this value is actually stored in GSettings.
  In that case, the real source of the value is GSettings. If you want a
  @class{g:action} object to control a setting stored in GSettings, see the
  @code{g_settings_create_action()} function instead, and possibly combine its
  use with the @code{g_settings_bind()} function.
  @see-constructor{g:property-action-new}
  @see-slot{g:property-action-enabled}
  @see-slot{g:property-action-invert-boolean}
  @see-slot{g:property-action-name}
  @see-slot{g:property-action-object}
  @see-slot{g:property-action-parameter-type}
  @see-slot{g:property-action-property-name}
  @see-slot{g:property-action-state}
  @see-slot{g:property-action-state-type}
  @see-class{g:action}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- property-action-enabled ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enabled" 'property-action) t)
 "The @code{enabled} property of type @code{:boolean} (Read) @br{}
  If the action is currently enabled. If the action is disabled then calls to
  the @fun{g:action-activate} and @fun{g:action-change-state} functions have no
  effect. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'property-action-enabled)
      "Accessor"
      (documentation 'property-action-enabled 'function)
 "@version{2022-12-26}
  @syntax[]{(g:property-action-enabled object) => enabled}
  @argument[object]{a @class{g:property-action} object}
  @argument[enabled]{a boolean whether the action is enabled}
  @begin{short}
    Accessor of the @slot[g:property-action]{enabled} slot of the
    @class{g:property-action} class.
  @end{short}
  If the action is currently enabled. If the action is disabled then calls to
  the @fun{g:action-activate} and @fun{g:action-change-state} functions have
  no effect. @br{}
  @see-class{g:property-action}
  @see-function{g:action-activate}
  @see-function{g:action-change-state}")

;;; --- property-action-invert-boolean -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invert-boolean"
                                               'property-action) t)
 "The @code{invert-boolean} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  If @em{true}, the state of the action will be the negation of the property
  value, provided the property is boolean. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'property-action-invert-boolean)
      "Accessor"
      (documentation 'property-action-invert-boolean 'function)
 "@version{2022-12-26}
  @syntax[]{(g:property-action-invert-boolean object) => invert}
  @argument[object]{a @class{g:property-action} object}
  @argument[invert]{a boolean whether the state of the action will be the
    negation}
  @begin{short}
    If @em{true}, the state of the action will be the negation of the property
    value, provided the property is boolean.
  @end{short}
  @see-class{g:property-action}")

;;;--- property-action-name ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'property-action) t)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The name of the action. This is mostly meaningful for identifying the action
  once it has been added to a @class{g:action-map} object. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'property-action-name)
      "Accessor"
      (documentation 'property-action-name 'function)
 "@version{2022-12-26}
  @syntax[]{(g:property-action-name object) => name}
  @argument[object]{a @class{g:property-action} object}
  @argument[name]{a string with the name of the action}
  @begin{short}
    The name of the action. This is mostly meaningful for identifying the
    action once it has been added to a @class{g:action-map} object.
  @end{short}
  @see-class{g:property-action}
  @see-class{g:action-map}")

;;; --- property-action-object -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "object" 'property-action) t)
 "The @code{object} property of type @class{object} (Write / Construct Only)
  @br{}
  The object to wrap a property on. The object must be a GObject with
  properties. @br{}
  @em{Note:} In the Lisp binding this property is not readable and not
  writeable.")

#+liber-documentation
(setf (liber:alias-for-function 'property-action-object)
      "Accessor"
      (documentation 'property-action-object 'function)
 "@version{2022-12-26}
  @begin{short}
    The @slot[g:property-action]{object} slot of the @class{g:property-action}
    class is not readable and not writable.
  @end{short}
  @see-class{g:property-action}")

;;; --- property-action-parameter-type -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "parameter-type"
                                               'property-action) t)
 "The @code{parameter-type} property of type @type{g:variant-type} (Read) @br{}
  The type of the parameter that must be given when activating the action.")

#+liber-documentation
(setf (liber:alias-for-function 'property-action-parameter-type)
      "Accessor"
      (documentation 'property-action-parameter-type 'function)
 "@version{2022-12-26}
  @syntax[]{(g:property-action-parameter-type object) => type}
  @argument[object]{a @class{g:property-action} object}
  @argument[type]{a @class{g:variant-type} of the parameter}
  @begin{short}
    The type of the parameter that must be given when activating the action.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(defvar label (make-instance 'gtk:label))
=> LABEL
(defvar action (g:property-action-new \"action\" label \"xalign\"))
=> ACTION
(g:property-action-parameter-type action)
=> #<GLIB:VARIANT-TYPE {10023AE493@}>
    @end{pre}
  @end{dictionary}
  @see-class{g:property-action}
  @see-class{g:variant-type}")

;;; --- property-action-property-name ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "property-name"
                                               'property-action) t)
 "The @code{property-name} property of type @code{:string}
  (Write / Construct only) @br{}
  The name of the property to wrap on the object. The property must exist on
  the passed-in object and it must be readable and writable and not
  construct-only. @br{}
  @em{Note:} In the Lisp binding this property is not readable und not
  writeable.")

#+liber-documentation
(setf (liber:alias-for-function 'property-action-property-name)
      "Accessor"
      (documentation 'property-action-property-name 'function)
 "@version{2022-12-26}
  @begin{short}
    The @slot[property-action]{property-name} slot of the
    @class{g:property-action} class is not readable and not writable.
  @end{short}
  @see-class{g:property-action}")

;;; --- property-action-state --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "state" 'property-action) t)
 "The @code{state} property of type @type{g:variant} (Read) @br{}
  The state of the action, or @code{nil} if the action is stateless. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'property-action-state)
      "Accessor"
      (documentation 'property-action-state 'function)
 "@version{2022-12-26}
  @syntax[]{(property-action-state object) => state}
  @argument[object]{a @class{g:property-action} object}
  @argument[state]{a @type{g:variant} state of the action}
  @begin{short}
    The state of the action, or @code{nil} if the action is stateless.
  @end{short}
  @see-class{g:property-action}
  @see-type{g:variant}")

;;; --- property-action-state-type ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "state-type" 'property-action) t)
 "The @code{state-type} property of type @class{g:variant-type} (Read) @br{}
  The variant type of the state that the action has, or @code{nil} if the
  action is stateless.")

#+liber-documentation
(setf (liber:alias-for-function 'property-action-state-type)
      "Accessor"
      (documentation 'property-action-state-type 'function)
 "@version{2022-12-26}
  @syntax[]{(g:property-action-state-type object) => type}
  @argument[object]{a @class{g:property-action} object}
  @argument[type]{a @class{g:variant-type} state type of the action}
  @begin{short}
    The variant type of the state that the action has, or @code{nil} if the
    action is stateless.
  @end{short}
  @see-class{g:property-action}
  @see-class{g:variant-type}")

;;; ----------------------------------------------------------------------------
;;; g_property_action_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_property_action_new" property-action-new)
    (gobject:object property-action)
 #+liber-documentation
 "@version{2022-12-26}
  @argument[name]{a string with the name of the action to create}
  @argument[object]{a @class{g:oject} object that has the property to wrap}
  @argument[property]{a string with the name of the property}
  @return{a new @class{g:property-action} object}
  @begin{short}
    Creates an action corresponding to the value of property @arg{property}
    on the object.
  @end{short}
  The property must be existent and readable and writable and not
  construct-only.
  @see-class{g:property-action}"
  (name :string)
  (object gobject:object)
  (property :string))

(export 'property-action-new)

;;; --- End of file gio.property-action.lisp -----------------------------------
