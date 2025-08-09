;;; ----------------------------------------------------------------------------
;;; gio.menu-model.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; GMenuModel
;;;
;;;     An abstract class representing the contents of a menu
;;;
;;; Types and Values
;;;
;;;     GMenuModel
;;;     GMenuAttributeIter
;;;     GMenuLinkIter
;;;
;;; Function
;;;
;;;     g_menu_model_is_mutable
;;;     g_menu_model_get_n_items
;;;     g_menu_model_get_item_attribute_value
;;;     g_menu_model_get_item_attribute                     not implemented
;;;     g_menu_model_get_item_link
;;;     g_menu_model_iterate_item_attributes
;;;     g_menu_model_iterate_item_links
;;;     g_menu_model_items_changed
;;;
;;;     g_menu_attribute_iter_get_next                      not implemented
;;;     g_menu_attribute_iter_next
;;;     g_menu_attribute_iter_get_name
;;;     g_menu_attribute_iter_get_value
;;;
;;;     g_menu_link_iter_get_next                           not implemented
;;;     g_menu_link_iter_next
;;;     g_menu_link_iter_get_name
;;;     g_menu_link_iter_get_value
;;;
;;; Signals
;;;
;;;     items-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GMenuAttributeIter
;;;     ├── GMenuLinkIter
;;;     ╰── GMenuModel
;;;         ├── GDBusMenuModel
;;;         ╰── GMenu
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GMenuModel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GMenuModel" menu-model
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_menu_model_get_type")
  nil)

#+liber-documentation
(setf (documentation 'menu-model 'type)
 "@version{2025-06-29}
  @begin{short}
    The @class{g:menu-model} class represents the contents of a menu -
    an ordered list of menu items.
  @end{short}
  The items are associated with actions, which can be activated through them.
  Items can be grouped in sections, and may have submenus associated with them.
  Both items and sections usually have some representation data, such as labels
  or icons. The type of the associated action, that is, whether it is stateful,
  and what kind of state it has, can influence the representation of the item.

  The conceptual model of menus in a @class{g:menu-model} object is
  hierarchical: sections and submenus are again represented by
  @class{g:menu-model} objects. Menus themselves do not define their own roles.
  Rather, the role of a particular @class{g:menu-model} object is defined by
  the item that references it, or, in the case of the 'root' menu, is defined
  by the context in which it is used.

  As an example, consider the visible portions of this menu.

  @image[menu-example]{Figure: Menu}

  There are 8 \"menus\" visible in the screenshot: one menubar, two submenus
  and 5 sections:
  @begin{itemize}
    @item{the toplevel menubar, containing 4 items}
    @item{the View submenu, containing 3 sections}
    @item{the first section of the View submenu, containing 2 items}
    @item{the second section of the View submenu, containing 1 item}
    @item{the final section of the View submenu, containing 1 item}
    @item{the Highlight Mode submenu, containing 2 sections}
    @item{the Sources section, containing 2 items}
    @item{the Markup section, containing 2 items}
  @end{itemize}
  The example illustrates the conceptual connection between these 8 menus. Each
  large block in the figure represents a menu and the smaller blocks within the
  large block represent items in that menu. Some items contain references to
  other menus.

  @b{A menu example}

  @image[menu-model]{Figure: Menu model}

  Notice that the separators visible in the example appear nowhere in the menu
  model. This is because separators are not explicitly represented in the menu
  model. Instead, a separator is inserted between any two non-empty sections of
  a menu. Section items can have labels just like any other item. In that case,
  a display system may show a section header instead of a separator.

  The motivation for this abstract model of application controls is that modern
  user interfaces tend to make these controls available outside the application.
  Examples include global menus, jumplists, dash boards, and so on. To support
  such uses, it is necessary to 'export' information about actions and their
  representation in menus, which is exactly what the @class{g:action-group}
  exporter and the @class{g:menu-model} exporter do for @class{g:action-group}
  and @class{g:menu-model} objects. The client-side counterparts to make use of
  the exported information are @code{GDBusActionGroup} and
  @code{GDBusMenuModel}.

  The API of the @class{g:menu-model} class is very generic, with iterators for
  the attributes and links of an item, see the
  @fun{g:menu-model-iterate-item-attributes} and
  @fun{g:menu-model-iterate-item-links} functions. The 'standard' attributes
  and link types have names: @code{\"label\"}, @code{\"action\"},
  @code{\"target\"}, @code{\"section\"}, and @code{\"submenu\"}.

  Items in a @class{g:menu-model} object represent active controls if they refer
  to an action that can get activated when the user interacts with the menu
  item. The reference to the action is encoded by the string ID in the
  @code{\"action\"} attribute. An action ID uniquely identifies an action in an
  action group. Which action group(s) provide actions depends on the context in
  which the menu model is used. For example, when the model is exported as the
  application menu of a @class{gtk:application} instance, actions can be
  application-wide or window specific, and thus come from two different action
  groups. By convention, the application-wide actions have names that start with
  @code{\"app.\"}, while the names of window specific actions start with
  @code{\"win.\"}.

  While a wide variety of stateful actions is possible, the following is the
  minimum that is expected to be supported by all users of exported menu
  information:
  @begin{itemize}
    @item{an action with no parameter type and no state}
    @item{an action with no parameter type and boolean state}
    @item{an action with string parameter type and string state}
  @end{itemize}
  @subheading{Stateless.}
  A stateless action typically corresponds to an ordinary menu item. Selecting
  such a menu item will activate the action, with no parameter.

  @subheading{Boolean State.}
  An action with a boolean state will most typically be used with a \"toggle\"
  or \"switch\" menu item. The state can be set directly, but activating the
  action, with no parameter, results in the state being toggled. Selecting a
  toggle menu item will activate the action. The menu item should be rendered
  as \"checked\" when the state is true.

  @subheading{String Parameter and State.}
  Actions with string parameters and state will most typically be used to
  represent an enumerated choice over the items available for a group of radio
  menu items. Activating the action with a string parameter is equivalent to
  setting that parameter as the state. Radio menu items, in addition to being
  associated with the action, will have a target value. Selecting that menu
  item will result in activation of the action with the target value as the
  parameter. The menu item should be rendered as \"selected\" when the state of
  the action is equal to the target value of the menu item.
  @begin[Signal Details]{dictionary}
    @begin[menu-model::items-changed]{signal}
      @begin{pre}
lambda (model position removed added)   :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[model]{The @class{g:menu-model} object that is changing.}
        @entry[position]{The integer with the position of the change.}
        @entry[removed]{The integer with the number of items removed.}
        @entry[added]{The integer with the number of items added.}
      @end{simple-table}
      Emitted when a change has occured to the menu. The only changes that can
      occur to a menu is that items are removed or added. Items may not change,
      except by being removed and added back in the same location. The signal
      is capable of describing both of those changes at the same time.

      The signal means that starting at the index position, removed items were
      removed and added items were added in their place. If removed is zero
      then only items were added. If added is zero then only items were removed.

      As an example, if the menu contains items a, b, c, d, in that order, and
      the signal (2, 1, 3) occurs then the new composition of the menu will be
      a, b, _, _, _, d, with each _ representing some new item.

      Signal handlers may query the model, particularly the added items, and
      expect to see the results of the modification that is being reported. The
      signal is emitted after the modification.
    @end{signal}
  @end{dictionary}
  @see-class{gtk:application}
  @see-class{g:action-group}")

;;; ----------------------------------------------------------------------------
;;; GMenuAttributeIter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GMenuAttributeIter" menu-attribute-iter
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_menu_attribute_iter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'menu-attribute-iter 'type)
 "@version{2024-12-30}
  @begin{short}
    The @class{g:menu-attribute-iter} class is an opaque structure type.
  @end{short}
  @see-function{g:menu-model-iterate-item-attributes}")

;;; ----------------------------------------------------------------------------
;;; GMenuLinkIter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GMenuLinkIter" menu-link-iter
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_menu_link_iter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'menu-link-iter 'type)
 "@version{2024-12-30}
  @begin{short}
    The @class{g:menu-link-iter} class is an opaque structure type.
  @end{short}
  @see-function{g:menu-model-iterate-item-links}")

;;; ----------------------------------------------------------------------------
;;; g_menu_model_is_mutable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_model_is_mutable" menu-model-is-mutable) :boolean
 #+liber-documentation
 "@version{#2024-12-29}
  @argument[model]{a @class{g:menu-model} object}
  @return{@em{True} if @arg{model} is mutable, that is, the
    @code{\"items-changed\"} signal may be emitted.}
  @begin{short}
    Queries if the menu model is mutable.
  @end{short}
  An immutable menu model will never emit the @code{\"items-changed\"} signal.
  Consumers of the model may make optimisations accordingly.
  @see-class{g:menu-model}"
  (model (gobject:object menu-model)))

(export 'menu-model-is-mutable)

;;; ----------------------------------------------------------------------------
;;; g_menu_model_get_n_items
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_model_get_n_items" menu-model-n-items) :int
 #+liber-documentation
 "@version{#2024-12-29}
  @argument[model]{a @class{g:menu-model} object}
  @return{The integer with the number of items in @arg{model}.}
  @begin{short}
    Query the number of items in the menu model.
  @end{short}
  @see-class{g:menu-model}"
  (model (gobject:object menu-model)))

(export 'menu-model-n-items)

;;; ----------------------------------------------------------------------------
;;; g_menu_model_get_item_attribute_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_model_get_item_attribute_value"
               menu-model-item-attribute-value)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[model]{a @class{g:menu-model} object}
  @argument[index]{an integer with the index of the item}
  @argument[attribute]{a string with the attribute to query}
  @argument[vtype]{a @class{g:variant-type} parameter type with the expected
    type of the attribute, of @code{nil}}
  @return{The @symbol{g:variant} parameter with the value of the attribute.}
  @begin{short}
    Queries the item at position @arg{index} in @arg{model} for the attribute
    specified by @arg{attribute}.
  @end{short}
  If @arg{vtype} is not @code{nil} then it specifies the expected type of the
  attribute. If it is @code{nil} then any type will be accepted.

  If the attribute exists and matches @arg{vtype}, or if the expected type is
  unspecified, then the value is returned. If the attribute does not exist, or
  does not match the expected type then @code{nil} is returned.
  @see-class{g:menu-model}
  @see-class{g:variant-type}
  @see-symbol{g:variant}"
  (model (gobject:object menu-model))
  (index :int)
  (attribute :string)
  (vtype (glib:boxed glib:variant-type)))

(export 'menu-model-item-attribute-value)

;;; ----------------------------------------------------------------------------
;;; g_menu_model_get_item_attribute
;;;
;;; Queries item at position item_index in model for the attribute specified by
;;; attribute.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_model_get_item_link
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_model_get_item_link" menu-model-item-link)
    (gobject:object menu-model :return)
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[model]{a @class{g:menu-model} object}
  @argument[index]{an integer with the index of the item}
  @argument[link]{an string with the link to query}
  @return{The linked @class{g:menu-model} object, or @code{nil}.}
  @begin{short}
    Queries the item at position @arg{index} in @arg{model} for the link
    specified by @arg{link}.
  @end{short}
  If the link exists, the linked @class{g:menu-model} object is returned. If
  the link does not exist, @code{nil} is returned.
  @see-class{g:menu-model}"
  (model (gobject:object menu-model))
  (index :int)
  (link :string))

(export 'menu-model-item-link)

;;; ----------------------------------------------------------------------------
;;; g_menu_model_iterate_item_attributes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_model_iterate_item_attributes"
               menu-model-iterate-item-attributes)
    (gobject:object menu-attribute-iter :return)
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[model]{a @class{g:menu-model} object}
  @argument[index]{an integer with the index of the item}
  @return{The new @class{g:menu-attribute-iter} object.}
  @begin{short}
    Creates a @class{g:menu-attribute-iter} object to iterate over the
    attributes of the item at position @arg{item} in @arg{model}.
  @end{short}
  @see-class{g:menu-model}
  @see-class{g:menu-attribute-iter}"
  (model (gobject:object menu-model))
  (index :int))

(export 'menu-model-iterate-item-attributes)

;;; ----------------------------------------------------------------------------
;;; g_menu_model_iterate_item_links
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_model_iterate_item_links" menu-model-iterate-item-links)
    (gobject:object menu-link-iter :return)
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[model]{a @class{g:menu-model} object}
  @argument[index]{an integer with the index of the item}
  @return{The new @class{g:menu-link-iter} object.}
  @begin{short}
    Creates a @class{g:menu-link-iter} object to iterate over the links of the
    item at position @arg{index} in @arg{model}.
  @end{short}
  @see-class{g:menu-model}
  @see-class{g:menu-link-iter}"
  (model (gobject:object menu-model))
  (index :int))

(export 'menu-model-iterate-item-links)

;;; ----------------------------------------------------------------------------
;;; g_menu_model_items_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_model_items_changed" menu-model-items-changed) :void
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[model]{a @class{g:menu-model} object}
  @argument[pos]{an integer with the position of the change}
  @argument[removed]{an integer with the number of items removed}
  @argument[added]{an integer with the number of items added}
  @begin{short}
    Requests emission of the @code{\"items-changed\"} signal on @arg{model}.
  @end{short}

  This function should never be called except by @class{g:menu-model}
  subclasses. Any other calls to this function will very likely lead to a
  violation of the interface of the menu model.

  The implementation should update its internal representation of the menu
  before emitting the signal. The implementation should further expect to
  receive queries about the new state of the menu, and particularly added menu
  items, while signal handlers are running.

  The implementation must dispatch this call directly from a main loop entry
  and not in response to calls, particularly those from the @class{g:menu-model}
  API. Said another way: the menu must not change while user code is running
  without returning to the main loop.
  @see-class{g:menu-model}"
  (model (gobject:object menu-model))
  (pos :int)
  (removed :int)
  (added :int))

(export 'menu-model-items-changed)

;;; ----------------------------------------------------------------------------
;;; g_menu_attribute_iter_get_next
;;;
;;; This function combines g_menu_attribute_iter_next() with
;;; g_menu_attribute_iter_get_name() and g_menu_attribute_iter_get_value().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_attribute_iter_next
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_attribute_iter_next" menu-attribute-iter-next) :boolean
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[iter]{a @class{g:menu-attribute-iter} object}
  @return{@em{True} on success, or @em{false} if there are no more attributes.}
  @begin{short}
    Attempts to advance the iterator to the next, possibly first, attribute.
  @end{short}
  @em{True} is returned on success, or @em{false} if there are no more
  attributes.

  You must call this function when you first acquire the iterator to advance it
  to the first attribute, and determine if the first attribute exists at all.
  @see-class{g:menu-attribute-iter}"
  (iter (gobject:object menu-attribute-iter)))

(export 'menu-attribute-iter-next)

;;; ----------------------------------------------------------------------------
;;; g_menu_attribute_iter_get_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_attribute_iter_get_name" menu-attribute-iter-name)
    :string
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[iter]{a @class{g:menu-attribute-iter} object}
  @return{The string with the name of the attribute.}
  @begin{short}
    Gets the name of the attribute at the current iterator position, as a
    string.
  @end{short}
  The iterator is not advanced.
  @see-class{g:menu-attribute-iter}"
  (iter (gobject:object menu-attribute-iter)))

(export 'menu-attribute-iter-name)

;;; ----------------------------------------------------------------------------
;;; g_menu_attribute_iter_get_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_attribute_iter_get_value" menu-attribute-iter-value)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[iter]{a @class{g:menu-attribute-iter} object}
  @return{The @symbol{g:variant} parameter with the value of the current
    attribute.}
  @begin{short}
    Gets the value of the attribute at the current iterator position, as a
    string.
  @end{short}
  The iterator is not advanced.
  @see-class{g:menu-attribute-iter}
  @see-symbol{g:variant}"
  (iter (gobject:object menu-attribute-iter)))

(export 'menu-attribute-iter-value)

;;; ----------------------------------------------------------------------------
;;; g_menu_link_iter_get_next
;;;
;;; This function combines g_menu_link_iter_next() with
;;; g_menu_link_iter_get_name() and g_menu_link_iter_get_value().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_link_iter_next
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_link_iter_next" menu-link-iter-next) :boolean
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[iter]{a @class{g:menu-link-iter} object}
  @return{@em{True} on success, or @em{false} if there are no more links.}
  @begin{short}
    Attempts to advance the iterator to the next, possibly first, link.
  @end{short}
  @em{True} is returned on success, or @em{false} if there are no more links.

  You must call this function when you first acquire the iterator to advance it
  to the first link, and determine if the first link exists at all.
  @see-class{g:menu-link-iter}"
  (iter (gobject:object menu-link-iter)))

(export 'menu-link-iter-next)

;;; ----------------------------------------------------------------------------
;;; g_menu_link_iter_get_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_link_iter_get_name" menu-link-iter-name) :string
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[iter]{a @class{g:menu-link-iter} object}
  @return{The string with the name of the link.}
  @begin{short}
    Gets the name of the link at the current iterator position, as a string.
  @end{short}
  The iterator is not advanced.
  @see-class{g:menu-link-iter}"
  (iter (gobject:object menu-link-iter)))

(export 'menu-link-iter-name)

;;; ----------------------------------------------------------------------------
;;; g_menu_link_iter_get_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_link_iter_get_value" menu-link-iter-value)
    (gobject:object menu-model :return)
 #+liber-documentation
 "@version{#2024-12-30}
  @argument[iter]{a @class{g:menu-link-iter} object}
  @return{The @class{g:menu-model} object that is linked to.}
  @begin{short}
    Gets the linked @class{g:menu-model} object at the current iterator
    position.
  @end{short}
  The iterator is not advanced.
  @see-class{g:menu-link-iter}
  @see-class{g:menu-model}"
  (iter (gobject:object menu-link-iter)))

(export 'menu-link-iter-value)

;;; --- End of file gio.menu-model-lisp ----------------------------------------
