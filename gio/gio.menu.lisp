;;; ----------------------------------------------------------------------------
;;; gio.menu.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2024 Dieter Kaiser
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
;;; GMenu
;;;
;;;     An implementation of GMenuModel
;;;
;;; Types and Values
;;;
;;;     GMenu
;;;     GMenuItem
;;;
;;; Functions
;;;
;;;     g_menu_new
;;;     g_menu_freeze
;;;     g_menu_insert
;;;     g_menu_prepend
;;;     g_menu_append
;;;     g_menu_insert_item
;;;     g_menu_append_item
;;;     g_menu_prepend_item
;;;     g_menu_insert_section
;;;     g_menu_prepend_section
;;;     g_menu_append_section
;;;     g_menu_append_submenu
;;;     g_menu_insert_submenu
;;;     g_menu_prepend_submenu
;;;     g_menu_remove
;;;     g_menu_remove_all
;;;
;;;     g_menu_item_new
;;;     g_menu_item_new_section
;;;     g_menu_item_new_submenu
;;;     g_menu_item_new_from_model
;;;     g_menu_item_set_label
;;;     g_menu_item_set_icon
;;;     g_menu_item_set_action_and_target_value
;;;     g_menu_item_set_action_and_target                   not implemented
;;;     g_menu_item_set_detailed_action
;;;     g_menu_item_set_section
;;;     g_menu_item_set_submenu
;;;
;;;     g_menu_item_get_attribute_value
;;;     g_menu_item_get_attribute
;;;     g_menu_item_get_link
;;;     g_menu_item_set_attribute_value
;;;     g_menu_item_set_attribute                           not implemented
;;;     g_menu_item_set_link
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GMenuItem
;;;     ╰── GMenuModel
;;;         ╰── GMenu
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GMenu
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GMenu" menu
  (:superclass menu-model
   :export t
   :interfaces nil
   :type-initializer "g_menu_get_type")
  nil)

#+liber-documentation
(setf (documentation 'menu 'type)
 "@version{2024-12-30}
  @begin{short}
    The @class{g:menu} class is an implementation of the abstract
    @class{g:menu-model} class.
  @end{short}
  You populate a @class{g:menu} object by adding @class{g:menu-item} objects to
  it.

  There are some convenience functions to allow you to directly add items,
  avoiding a @class{g:menu-item} object, for the common cases. To add a regular
  item, use the@fun{g:menu-insert} function. To add a section, use the
  @fun{g:menu-insert-section} function. To add a submenu, use the
  @fun{g:menu-insert-submenu} function.
  @see-constructor{g:menu-new}
  @see-class{g:menu-model}
  @see-class{g:menu-item}")

;;; ----------------------------------------------------------------------------
;;; g_menu_new
;;; ----------------------------------------------------------------------------

(declaim (inline menu-new))

(defun menu-new ()
 #+liber-documentation
 "@version{2024-12-30}
  @return{The new @class{g:menu} object.}
  @short{Creates a new @class{g:menu} object.}
  The new menu has no items.
  @see-class{g:menu}"
  (make-instance 'menu))

(export 'menu-new)

;;; ----------------------------------------------------------------------------
;;; g_menu_freeze
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_freeze" menu-freeze) :void
 #+liber-documentation
 "@version{2024-12-30}
  @argument[menu]{a @class{g:menu} object}
  @begin{short}
    Marks the menu as frozen.
  @end{short}
  After the menu is frozen, it is an error to attempt to make any changes to it.
  In effect this means that the @class{g:menu} API must no longer be used.

  This function causes the @fun{g:menu-model-is-mutable} function to begin
  returning @em{false}, which has some positive performance implications.
  @see-class{g:menu}
  @see-function{g:menu-model-is-mutable}"
  (menu (gobject:object menu)))

(export 'menu-freeze)

;;; ----------------------------------------------------------------------------
;;; g_menu_insert
;;; ----------------------------------------------------------------------------

;; TODO: Consider to combine the g:menu-insert, g:menu-append and g:menu-prepend
;; functions in one g:menu-add function.

(cffi:defcfun ("g_menu_insert" %menu-insert) :void
  (menu (gobject:object menu))
  (pos :int)
  (label :string)
  (action :string))

(defun menu-insert (menu pos label action)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[pos]{an integer for the position at which to insert the menu item}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[action]{a detailed action string, or @code{nil}}
  @begin{short}
    Convenience function for inserting a normal menu item into the menu.
  @end{short}
  Combine the @fun{g:menu-item-new} and @fun{g:menu-insert-item} functions for
  a more flexible alternative.
  @see-class{g:menu}
  @see-function{g:menu-item-new}
  @see-function{g:menu-insert-item}"
  (%menu-insert menu
                pos
                (or label (cffi:null-pointer))
                (or action (cffi:null-pointer))))

(export 'menu-insert)

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_prepend" %menu-prepend) :void
  (menu (gobject:object menu))
  (label :string)
  (action :string))

(defun menu-prepend (menu label action)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[action]{a detailed action string, or @code{nil}}
  @begin{short}
    Convenience function for prepending a normal menu item to the start of the
    menu.
  @end{short}
  Combine the @fun{g:menu-item-new} and @fun{g:menu-insert-item} functions for
  a more flexible alternative.
  @see-class{g:menu}
  @see-function{g:menu-item-new}
  @see-function{g:menu-insert-item}"
  (%menu-prepend menu
                 (or label (cffi:null-pointer))
                 (or action (cffi:null-pointer))))

(export 'menu-prepend)

;;; ----------------------------------------------------------------------------
;;; g_menu_append
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_append" %menu-append) :void
  (menu (gobject:object menu))
  (label :string)
  (action :string))

(defun menu-append (menu label action)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[action]{a detailed action string, or @code{nil}}
  @begin{short}
    Convenience function for appending a normal menu item to the end of the
    menu.
  @end{short}
  Combine the @fun{g:menu-item-new} and @fun{g:menu-insert-item} functions for
  a more flexible alternative.
  @see-class{g:menu}
  @see-function{g:menu-item-new}
  @see-function{g:menu-insert-item}"
  (%menu-append menu
                (or label (cffi:null-pointer))
                (or action (cffi:null-pointer))))

(export 'menu-append)

;;; ----------------------------------------------------------------------------
;;; g_menu_insert_item
;;; ----------------------------------------------------------------------------

;; TODO: Consider to combine the g:menu-insert-item, g:menu-append-item and
;; g:menu-prepend-item functions in one g:menu-add-item function.

(cffi:defcfun ("g_menu_insert_item" menu-insert-item) :void
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[pos]{an integer for the position at which to insert @arg{item}}
  @argument[item]{a @class{g:menu-item} object to insert}
  @begin{short}
    Inserts a menu item into the menu.
  @end{short}
  The \"insertion\" is actually done by copying all of the attribute and link
  values of @arg{item} and using them to form a new menu item within the menu.
  As such, @arg{item} itself is not really inserted, but rather, a menu item
  that is exactly the same as the one presently described by @arg{item}.

  This means that @arg{item} is essentially useless after the insertion occurs.
  Any changes you make to it are ignored unless it is inserted again, at which
  point its updated values will be copied.

  There are many convenience functions to take care of common cases. See the
  @fun{g:menu-insert}, @fun{g:menu-insert-section} and
  @fun{g:menu-insert-submenu} functions as well as \"prepend\" and \"append\"
  variants of each of these functions.
  @see-class{g:menu}
  @see-class{g:menu-item}
  @see-function{g:menu-insert}
  @see-function{g:menu-insert-section}
  @see-function{g:menu-insert-submenu}"
  (menu (gobject:object menu))
  (pos :int)
  (item (gobject:object menu-item)))

(export 'menu-insert-item)

;;; ----------------------------------------------------------------------------
;;; g_menu_append_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_append_item" menu-append-item) :void
 #+liber-documentation
 "@version{2024-12-30}
  @argument[menu]{a @class{g:menu} object}
  @argument[item]{a @class{g:menu-item} object to append}
  @begin{short}
    Appends the menu item to the end of the menu.
  @end{short}
  See the @fun{g:menu-insert-item} function for more information.
  @see-class{g:menu}
  @see-class{g:menu-item}
  @see-function{g:menu-insert-item}"
  (menu (gobject:object menu))
  (item (gobject:object menu-item)))

(export 'menu-append-item)

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_prepend_item" menu-prepend-item) :void
 #+liber-documentation
 "@version{2024-12-30}
  @argument[menu]{a @class{g:menu} object}
  @argument[item]{a @class{g:menu-item} object to prepend}
  @begin{short}
    Prepends the menu item to the start of the menu.
  @end{short}
  See the @fun{g:menu-insert-item} function for more information.
  @see-class{g:menu}
  @see-class{g:menu-item}
  @see-function{g:menu-insert-item}"
  (menu (gobject:object menu))
  (item (gobject:object menu-item)))

(export 'menu-prepend-item)

;;; ----------------------------------------------------------------------------
;;; g_menu_insert_section
;;; ----------------------------------------------------------------------------

;; TODO: Consider to combine the g:menu-insert-section, g:menu-append-section
;; and g:menu-prepend-section functions in one g:menu-add-section function.

(cffi:defcfun ("g_menu_insert_section" %menu-insert-section) :void
  (menu (gobject:object menu))
  (pos :int)
  (label :string)
  (section (gobject:object menu-model)))

(defun menu-insert-section (menu pos label section)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[pos]{an integer for the position at which to insert @arg{section}}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[section]{a @class{g:menu-model} object with the items of the
    section}
  @begin{short}
    Convenience function for inserting a section menu item into menu.
  @end{short}
  Combine the @fun{g:menu-item-new-section} and @fun{g:menu-insert-item}
  functions for a more flexible alternative.
  @see-class{g:menu}
  @see-class{g:menu-model}
  @see-function{g:menu-item-new-section}
  @see-function{g:menu-insert-item}"
  (%menu-insert-section menu
                        pos
                        (or label (cffi:null-pointer))
                        section))

(export 'menu-insert-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend_section
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_prepend_section" %menu-prepend-section) :void
  (menu (gobject:object menu))
  (label :string)
  (section (gobject:object menu-model)))

(defun menu-prepend-section (menu label section)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[section]{a @class{g:menu-model} object with the items of the
    section}
  @begin{short}
    Convenience function for prepending a section menu to the start of the menu.
  @end{short}
  Combine the @fun{g:menu-item-new-section} and @fun{g:menu-insert-item}
  functions for a more flexible alternative.
  @see-class{g:menu}
  @see-class{g:menu-model}
  @see-function{g:menu-item-new-section}
  @see-function{g:menu-insert-item}"
  (%menu-prepend-section menu
                         (or label (cffi:null-pointer))
                         section))

(export 'menu-prepend-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_append_section
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_append_section" %menu-append-section) :void
  (menu (gobject:object menu))
  (label :string)
  (section (gobject:object menu-model)))

(defun menu-append-section (menu label section)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[section]{a @class{g:menu-model} object with the items of the
    section}
  @begin{short}
    Convenience function for appending a section menu to the emd of the menu.
  @end{short}
  Combine the @fun{g:menu-item-new-section} and @fun{g:menu-insert-item}
  functions for a more flexible alternative.
  @see-class{g:menu}
  @see-class{g:menu-model}
  @see-function{g:menu-item-new-section}
  @see-function{g:menu-insert-item}"
  (%menu-append-section menu
                        (or label (cffi:null-pointer))
                        section))

(export 'menu-append-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_insert_submenu
;;; ----------------------------------------------------------------------------

;; TODO: Consider to combine the g:menu-insert-submenu, g:menu-append-submenu
;; and g:menu-prepend-submenu functions in one g:menu-add-submenu function.

(cffi:defcfun ("g_menu_insert_submenu" %menu-insert-submenu) :void
  (menu (gobject:object menu))
  (pos :int)
  (label :string)
  (submenu (gobject:object menu-model)))

(defun menu-insert-submenu (menu pos label submenu)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[pos]{an integer for the position at which to insert @arg{submenu}}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[submenu]{a @class{g:menu-model} object with the items of the
    submenu}
  @begin{short}
    Convenience function for inserting a submenu into the menu.
  @end{short}
  Combine the @fun{g:menu-item-new-submenu} and @fun{g:menu-insert-item}
  functions for a more flexible alternative.
  @see-class{g:menu}
  @see-class{g:menu-model}
  @see-function{g:menu-item-new-submenu}
  @see-function{g:menu-insert-item}"
  (%menu-insert-submenu menu
                        pos
                        (or label (cffi:null-pointer))
                        submenu))

(export 'menu-insert-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_append_submenu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_append_submenu" %menu-append-submenu) :void
  (menu (gobject:object menu))
  (label :string)
  (submenu (gobject:object menu-model)))

(defun menu-append-submenu (menu label submenu)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[submenu]{a @class{g:menu-model} object with the items of the
    submenu}
  @begin{short}
    Convenience function for appending a submenu to the end of the menu.
  @end{short}
  Combine the @fun{g:menu-item-new-submenu} and @fun{g:menu-insert-item}
  functions for a more flexible alternative.
  @see-class{g:menu}
  @see-class{g:menu-model}
  @see-function{g:menu-item-new-submenu}
  @see-function{g:menu-insert-item}"
  (%menu-append-submenu menu
                        (or label (cffi:null-pointer))
                        submenu))

(export 'menu-append-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_prepend_submenu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_prepend_submenu" %menu-prepend-submenu) :void
  (menu (gobject:object menu))
  (label :string)
  (submenu (gobject:object menu-model)))

(defun menu-prepend-submenu (menu label submenu)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[submenu]{a @class{g:menu-model} object with the items of the
    submenu}
  @begin{short}
    Convenience function for prepending a submenu to the start of the menu.
  @end{short}
  Combine the @fun{g:menu-item-new-submenu} and @fun{g:menu-insert-item}
  functions for a more flexible alternative.
  @see-class{g:menu}
  @see-class{g:menu-model}
  @see-function{g:menu-item-new-submenu}
  @see-function{g:menu-insert-item}"
  (%menu-prepend-submenu menu
                         (or label (cffi:null-pointer))
                         submenu))

(export 'menu-prepend-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_remove" menu-remove) :void
 #+liber-documentation
 "@version{2025-09-27}
  @argument[menu]{a @class{g:menu} object}
  @argument[pos]{an integer for the position of the menu item to remove}
  @begin{short}
    Removes an item from the menu.
  @end{short}
  The @arg{pos} argument gives the index of the menu item to remove. It is an
  error if @arg{pos} is not in the range from 0 to one less than the number of
  menu items in the menu.

  It is not possible to remove items by identity since items are added to the
  menu simply by copying their links and attributes, that is, identity of the
  menu item itself is not preserved.
  @see-class{g:menu}"
  (menu (gobject:object menu))
  (pos :int))

(export 'menu-remove)

;;; ----------------------------------------------------------------------------
;;; g_menu_remove_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_remove_all" menu-remove-all) :void
 #+liber-documentation
 "@version{2024-12-30}
  @begin{short}
    Removes all items in the menu.
  @end{short}
  @see-class{g:menu}"
  (menu (gobject:object menu)))

(export 'menu-remove-all)

;;; ----------------------------------------------------------------------------
;;; GMenuItem
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GMenuItem" menu-item
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_menu_item_get_type")
  nil)

#+liber-documentation
(setf (documentation 'menu-item 'type)
 "@version{2024-12-30}
  @begin{short}
    The @class{g:menu-item} object is an opaque structure type.
  @end{short}
  You must access it using the API functions.
  @see-constructor{g:menu-item-new}
  @see-constructor{g:menu-item-new-section}
  @see-constructor{g:menu-item-new-submenu}
  @see-constructor{g:menu-item-new-for-model}
  @see-class{g:menu}")

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_new" %menu-item-new)
    (gobject:object menu-item :return)
  (label :string)
  (action :string))

(defun menu-item-new (label action)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[action]{a detailed action string, or @code{nil}}
  @return{The new @class{g:menu-item} object.}
  @begin{short}
    Creates a new @class{g:menu-item} object.
  @end{short}
  If @arg{label} is not @code{nil} it is used to set the @code{\"label\"}
  attribute of the new menu item.

  If @arg{action} is not @code{nil} it is used to set the @code{\"action\"} and
  possibly the @code{\"target\"} attribute of the new item. See the
  @fun{g:menu-item-set-detailed-action} function for more information.
  @see-class{g:menu-item}
  @see-function{g:menu-item-set-detailed-action}"
  (%menu-item-new (or label (cffi:null-pointer))
                  (or action (cffi:null-pointer))))

(export 'menu-item-new)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new_section
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_new_section" %menu-item-new-section)
    (gobject:object menu-item :return)
  (label :string)
  (section (gobject:object menu-model)))

(defun menu-item-new-section (label section)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[section]{a @class{g:menu-model} object with the menu items of the
    section}
  @return{The new @class{g:menu-item} object.}
  @begin{short}
    Creates a new @class{g:menu-item} object representing a section.
  @end{short}
  This is a convenience API around the @fun{g:menu-item-new} and
  @fun{g:menu-item-set-section} functions.

  The effect of having one menu appear as a section of another is exactly as
  it sounds: the items from @arg{section} become a direct part of the menu that
  the items are added to.

  Visual separation is typically displayed between two non-empty sections. If
  @arg{label} is not @code{nil} then it will be encorporated into this visual
  indication. This allows for labeled subsections of a menu.
  @begin[Examples]{dictionary}
    As a simple example, consider a typical \"Edit\" menu from a simple program.
    It probably contains an \"Undo\" and \"Redo\" item, followed by a separator,
    followed by \"Cut\", \"Copy\" and \"Paste\".

    This would be accomplished by creating three @class{g:menu} objects. The
    first would be populated with the \"Undo\" and \"Redo\" items, and the
    second with the \"Cut\", \"Copy\" and \"Paste\" items. The first and second
    menus would then be added as submenus of the third. In XML format, this
    would look something like the following:
    @begin{pre}
<menu id='edit-menu'>
  <section>
    <item label='Undo'/>
    <item label='Redo'/>
  </section>
  <section>
    <item label='Cut'/>
    <item label='Copy'/>
    <item label='Paste'/>
  </section>
</menu>
    @end{pre}
    The following example is exactly equivalent. It is more illustrative of the
    exact relationship between the menus and items, keeping in mind that the
    @code{link} element defines a new menu that is linked to the containing one.
    The style of the second example is more verbose and difficult to read, and
    therefore not recommended except for the purpose of understanding what is
    really going on.
    @begin{pre}
<menu id='edit-menu'>
  <item>
    <link name='section'>
      <item label='Undo'/>
      <item label='Redo'/>
    </link>
  </item>
  <item>
    <link name='section'>
      <item label='Cut'/>
      <item label='Copy'/>
      <item label='Paste'/>
    </link>
  </item>
</menu>
    @end{pre}
  @end{dictionary}
  @see-class{g:menu-item}
  @see-function{g:menu-item-new}
  @see-function{g:menu-item-set-section}"
  (%menu-item-new-section (or label (cffi:null-pointer)) section))

(export 'menu-item-new-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new_submenu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_new_submenu" %menu-item-new-submenu)
    (gobject:object menu-item :return)
  (label :string)
  (submenu (gobject:object menu-model)))

(defun menu-item-new-submenu (label submenu)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[label]{a string for the section label, or @code{nil}}
  @argument[submenu]{a @class{g:menu-model} object with the menu items of the
    submenu}
  @return{The new @class{g:menu-item} object.}
  @begin{short}
    Creates a new @class{g:menu-item} object representing a submenu.
  @end{short}
  This is a convenience API around the @fun{g:menu-item-new} and
  @fun{g:menu-item-set-submenu} functions.
  @see-class{g:menu-item}
  @see-class{g:menu-model}
  @see-function{g:menu-item-new}
  @see-function{g:menu-item-set-submenu}"
  (%menu-item-new-submenu (or label (cffi:null-pointer)) submenu))

(export 'menu-item-new-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_new_from_model
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_new_from_model" menu-item-new-from-model)
    (gobject:object menu-item :return)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[model]{a @class{g:menu-model} object}
  @argument[index]{an integer for the index of a menu item in @arg{model}}
  @return{The new @class{g:menu-item} object.}
  @begin{short}
    Creates a @class{g:menu-item} object as an exact copy of an existing menu
    item in a @class{g:menu-model} object.
  @end{short}
  The @arg{index} argument must be valid, that is, be sure to call the
  @fun{g:menu-model-n-items} function first.
  @see-class{g:menu-item}
  @see-class{g:menu-model}
  @see-function{g:menu-model-n-items}"
  (model (gobject:object menu-model))
  (index :int))

(export 'menu-item-new-from-model)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_label
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_set_label" %menu-item-set-label) :void
  (item (gobject:object menu-item))
  (label :string))

(defun menu-item-set-label (item label)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[item]{a @class{g:menu-item} object}
  @argument[label]{a string for the @code{\"label\"} attribute to set, or
    @code{nil}}
  @begin{short}
    Sets or unsets the @code{\"label\"} attribute of the menu item.
  @end{short}
  If @arg{label} is not @code{nil} it is used as the @code{\"label\"} attribute
  for the menu item. If it is @code{nil} then the @code{\"label\"} attribute is
  unset.
  @see-class{g:menu-item}"
  (%menu-item-set-label item (or label (cffi:null-pointer))))

(export 'menu-item-set-label)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_set_icon" menu-item-set-icon) :void
 #+liber-documentation
 "@version{2024-12-30}
  @argument[item]{a @class{g:menu-item} object}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Sets or unsets the @code{\"icon\"} attribute on the menu item.
  @end{short}
  This call is the same as calling the @fun{g:icon-serialize} function and using
  the result as the value to the @fun{g:menu-item-attribute-value} function
  for the @code{\"icon\"} attribute.

  This API is only intended for use with \"noun\" menu items. Things like
  bookmarks or applications in an \"Open With\" menu. Do not use it on menu
  items that correspond to verbs, such as the stock icons for 'Save' or 'Quit'.

  If the @arg{icon} argument is @code{nil} then the @code{\"icon\"} attribute
  is unset.
  @see-class{g:menu-item}
  @see-function{g:icon-serialize}
  @see-function{g:menu-item-attribute-value}"
  (item (gobject:object menu-item))
  (icon (gobject:object icon)))

(export 'menu-item-set-icon)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_action_and_target_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_set_action_and_target_value"
               %menu-item-set-action-and-target-value) :void
  (item (gobject:object menu-item))
  (action :string)
  (value (:pointer (:struct glib:variant))))

(defun menu-item-set-action-and-target-value (item action value)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[item]{a @class{g:menu-item} object}
  @argument[action]{a string for the name of the @code{\"action\"} attribute
    for this menu item}
  @argument[value]{a @symbol{g:variant} parameter to use as the action target}
  @begin{short}
    Sets or unsets the @code{\"action\"} and @code{\"target\"} attributes of
    the menu item.
  @end{short}
  If @arg{action} is @code{nil} then both the @code{\"action\"} and
  @code{\"target\"} attributes are unset, and @arg{value} is ignored.

  If @arg{action} is not @code{nil} then the @code{\"action\"} attribute is set.
  The @code{\"target\"} attribute is then set to the value of @arg{value} if it
  is not @code{nil} or unset otherwise.

  Normal menu items, that is not submenu, section or other custom item types,
  are expected to have the @code{\"action\"} attribute set to identify the
  action that they are associated with. The state type of the action help to
  determine the disposition of the menu item. See the @class{g:action}
  documentation for an overview of actions.

  In general, clicking on the menu item will result in activation of the named
  action with the @code{\"target\"} attribute given as the parameter to the
  action invocation. If the @code{\"target\"} attribute is not set then the
  action is invoked with no parameter.

  If the action has no state then the menu item is usually drawn as a plain
  menu item, that is with no additional decoration.

  If the action has a boolean state then the menu item is usually drawn as a
  toggle menu item, that is with a checkmark or equivalent indication. The item
  should be marked as 'toggled' or 'checked' when the boolean state is
  @em{true}.

  If the action has a string state then the menu item is usually drawn as a
  radio menu item, that is with a radio bullet or equivalent indication. The
  item should be marked as 'selected' when the string state is equal to the
  value of the target property.

  See the  @fun{g:menu-item-set-detailed-action} function for a equivalent call
  that is probably more convenient for most uses.
  @see-class{g:menu-item}
  @see-class{g:action}
  @see-symbol{g:variant}
  @see-function{g:menu-item-set-detailed-action}"
  (%menu-item-set-action-and-target-value item
                                          (or action (cffi:null-pointer))
                                          (or value (cffi:null-pointer))))

(export 'menu-item-set-action-and-target-value)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_action_and_target
;;;
;;; Sets or unsets the "action" and "target" attributes of menu_item.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_detailed_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_set_detailed_action" menu-item-set-detailed-action)
    :void
 #+liber-documentation
 "@version{2024-12-30}
  @argument[item]{a @class{g:menu-item} object}
  @argument[action]{a detailed action string}
  @begin{short}
    Sets the @code{\"action\"} attribute and possibly the @code{\"target\"}
    attribute of the menu item.
  @end{short}
  The format of @arg{action} is the same format parsed by the
  @fun{g:action-parse-detailed-name} function.

  See the @fun{g:menu-item-set-action-and-target-value} function for a more
  flexible, but slightly less convenient, alternative. See also the
  @fun{g:menu-item-set-action-and-target-value} function  for a description of
  the semantics of the @code{\"action\"} and @code{\"target\"} attributes.
  @see-class{g:menu-item}
  @see-function{g:action-parse-detailed-name}
  @see-function{g:menu-item-set-action-and-target}
  @see-function{g:menu-item-set-action-and-target-value}"
  (item (gobject:object menu-item))
  (action :string))

(export 'menu-item-set-detailed-action)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_section
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_set_section" %menu-item-set-section) :void
  (item (gobject:object menu-item))
  (section (gobject:object menu-model)))

(defun menu-item-set-section (item section)
 #+liber-documentation
 "@version{2024-12-30}
  @argument[item]{a @class{g:menu-item} object}
  @argument[section]{a @class{g:menu-model}, or @code{nil}}
  @begin{short}
    Sets or unsets the @code{\"section\"} link of the menu item to
    @arg{section}.
  @end{short}
  The effect of having one menu appear as a section of another is exactly as
  it sounds: the items from @arg{section} become a direct part of the menu that
  @arg{item} is added to. See the @fun{g:menu-item-new-section} function for
  more information about what it means for a menu item to be a section.
  @see-class{g:menu-item}
  @see-class{g:menu-model}
  @see-function{g:menu-item-new-section}"
  (%menu-item-set-section item (or section (cffi:null-pointer))))

(export 'menu-item-set-section)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_submenu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_menu_item_set_submenu" %menu-item-set-submenu) :void
  (item (gobject:object menu-item))
  (submenu (gobject:object menu-model)))

(defun menu-item-set-submenu (item submenu)
 #+liber-documentation
 "@version{2024-12-30}
  @argument[item]{a @class{g:menu-item} object}
  @argument[submenu]{a @class{g:menu-model} object, or @code{nil}}
  @begin{short}
    Sets or unsets the @code{\"submenu\"} link of the menu item to
    @arg{submenu}.
  @end{short}
  If @arg{submenu} is not @code{nil}, it is linked to. If it is @code{nil} then
  the link is unset.

  The effect of having one menu appear as a submenu of another is exactly as
  it sounds.
  @see-class{g:menu-item}
  @see-class{g:menu-model}"
  (%menu-item-set-submenu item (or submenu (cffi:null-pointer))))

(export 'menu-item-set-submenu)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_get_attribute_value
;;; g_menu_item_set_attribute_value
;;; ----------------------------------------------------------------------------

(defun (setf menu-item-attribute-value) (value item attribute)
  (cffi:foreign-funcall "g_menu_item_set_attribute_value"
                        (gobject:object menu-item) item
                        :string attribute
                        (:pointer (:struct glib:variant)) value
                        :void)
  value)

(cffi:defcfun ("g_menu_item_get_attribute_value" %menu-item-attribute-value)
    (:pointer (:struct glib:variant))
  (item (gobject:object menu-item))
  (attribute :string)
  (vtype (glib:boxed glib:variant-type)))

(defun menu-item-attribute-value (item attribute &optional (vtype nil))
 #+liber-documentation
 "@version{2025-09-27}
  @syntax{(g:menu-item-attribute-value item attribute) => value}
  @syntax{(g:menu-item-attribute-value item attribute vtype) => value}
  @syntax{(setf (g:menu-item-attribute-value item attribute) value)}
  @argument[item]{a @class{g:menu-item} object}
  @argument[attribute]{a string for the attribute name}
  @argument[vtype]{an optional expected @class{g:variant-type} parameter type
    or a type string for the the attribute}
  @argument[value]{a @symbol{g:variant} parameter to use as the value, or
    @code{nil}}
  @begin{short}
    The @fun{g:menu-item-attribute-value} function queries the named attribute
    on the menu item.
  @end{short}
  The @setf{g:menu-item-attribute-value} function sets or unsets an attribute.

  If the @arg{vtype} argument is specified and the attribute does not have this
  type, @code{nil} is returned. @code{nil} is also returned if the attribute
  simply does not exist.

  The attribute to set or unset is specified by @arg{attribute}. This can be
  one of the standard attribute names @code{\"label\"}, @code{\"action\"},
  @code{\"target\"}, or a custom attribute name. Attribute names are restricted
  to lowercase characters, numbers and '-'. Furthermore, the names must begin
  with a lowercase character, must not end with a '-', and must not contain
  consecutive dashes.

  If @arg{value} is not @code{nil} then it is used as the new value for the
  attribute. If @arg{value} is @code{nil} then the attribute is unset. If the
  @symbol{g:variant} parameter is floating, it is consumed.
  @see-class{g:menu-item}
  @see-symbol{g:variant}
  @see-class{g:variant-type}"
  (let ((vtype1 (if (stringp vtype)
                    (glib:variant-type-new vtype)
                    vtype)))
    (%menu-item-attribute-value item attribute vtype1)))

(export 'menu-item-attribute-value)

;;; ----------------------------------------------------------------------------
;;; g_menu_item_get_attribute
;;;
;;; Queries the named attribute on menu_item.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_set_attribute
;;;
;;; Sets or unsets an attribute on menu_item.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_menu_item_get_link
;;; g_menu_item_set_link
;;; ----------------------------------------------------------------------------

(defun (setf menu-item-link) (value item link)
  (cffi:foreign-funcall "g_menu_item_set_link"
                        (gobject:object menu-item) item
                        :string link
                        (gobject:object menu-model) value
                        :void)
  value)

(cffi:defcfun ("g_menu_item_get_link" menu-item-link)
    (gobject:object menu-model :return)
 #+liber-documentation
 "@version{2025-09-27}
  @syntax{(g:menu-item-link item link) => model}
  @syntax{(setf (g:menu-item-link item link) model)}
  @argument[item]{a @class{g:menu-item} object}
  @argument[link]{a string for the type of link to establish or unset}
  @argument[model]{a @class{g:menu-model} object to link to, or @code{nil} to
    unset}
  @begin{short}
    The @sym{g:menu-item-link} function queries the named link on @arg{item}.
  @end{short}
  The @setf{g:menu-item-link} function creates a link from @arg{item} to
  @arg{model} if not @code{nil}, or unsets it.

  Links are used to establish a relationship between a particular menu item
  and another menu. For example, the @code{\"submenu\"} attribute is used to
  associate a submenu with a particular menu item, and the @code{\"section\"}
  attribute is used to create a section. Other types of link can be used, but
  there is no guarantee that clients will be able to make sense of them. Link
  types are restricted to lowercase characters, numbers and '-'. Furthermore,
  the names must begin with a lowercase character, must not end with a '-', and
  must not contain consecutive dashes.
  @see-class{g:menu-item}
  @see-class{g:menu-model}"
  (item (gobject:object menu-item))
  (link :string))

(export 'menu-item-link)

;;; --- End of file gio.menu.lisp ----------------------------------------------
