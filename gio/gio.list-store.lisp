;;; ----------------------------------------------------------------------------
;;; gio.list-store.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2021 - 2023 Dieter Kaiser
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
;;; GListStore
;;;
;;;     A simple implementation of GListModel
;;;
;;; Types and Values
;;;
;;;     GListStore
;;;
;;; Functions
;;;
;;;     g_list_store_new
;;;     g_list_store_insert
;;;     g_list_store_insert_sorted
;;;     g_list_store_append
;;;     g_list_store_remove
;;;     g_list_store_remove_all
;;;     g_list_store_splice
;;;     g_list_store_sort
;;;     g_list_store_find
;;;     g_list_store_find_with_equal_func
;;;     g_list_store_find_with_equal_func_full             Since 2.74
;;;
;;; Properties
;;;
;;;     item-type
;;;     n-items                                            Since 2.74
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GListStore
;;;
;;; Implemented Interfaces
;;;
;;;     GListStore implements GListModel.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GListStore
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GListStore" list-store
  (:superclass gobject:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "g_list_store_get_type")
  ((item-type
    %list-store-item-type ; internal only, not used
    "item-type" "GType" t t)
   #+glib-2-74
   (n-items
    list-store-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'list-store 'type)
 "@version{2023-8-15}
  @begin{short}
    The @class{g:list-store} object is an implementation of the
    @class{g:list-model} interface that stores all items in memory.
  @end{short}
  It provides insertions, deletions, and lookups in logarithmic time with a
  fast path for the common case of iterating the list linearly.
  @see-constructor{g:list-store-new}
  @see-slot{g:list-store-item-type}
  @see-slot{g:list-store-n-items}
  @see-class{g:list-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- list-store-item-type ---------------------------------------------------

;; Note: We define the accessor with the interface function. This we get the
;; expected GType for ITEM-TYPE. The accessor %LIST-STORE-ITEM-TYPE returns
;; a pointer.

#+liber-documentation
(setf (documentation (liber:slot-documentation "item-type" 'list-store) t)
 "The @code{item-type} property of type @class{g:type-t}
  (Read / Write / Construct Only) @br{}
  The type of items contained in the list store. Items must be subclasses of
  the @class{g:object} class.")

(declaim (inline list-store-item-type))

(defun list-store-item-type (object)
  (list-model-item-type object))

#+liber-documentation
(setf (liber:alias-for-function 'list-store-item-type)
      "Accessor"
      (documentation 'list-store-item-type 'function)
 "@version{2023-8-15}
  @syntax[]{(g:list-store-item-type object) => gtype}
  @argument[object]{a @class{g:list-store} object}
  @argument[gtype]{a @class{g:type-t} type}
  @begin{short}
    Accessor of the @slot[g:list-store]{item-type} slot of the
    @class{g:list-store} class.
  @end{short}
  The type of items contained in the list store. Items must be subclasses of
  the @class{g:object} class.
  @begin[Note]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{g:list-store}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- list-store-n-items -----------------------------------------------------

#+(and glib-2-74 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'list-store) t)
 "The @code{n-items} property of type @code{:uint} (Read) @br{}
  The number of items contained in the list store. Since 2.74 @br{}
  Default value: 0")

#+(and glib-2-74 liber-documentation)
(setf (liber:alias-for-function 'list-store-n-items)
      "Accessor"
      (documentation 'list-store-n-items 'function)
 "@version{2023-8-15}
  @syntax[]{(g:list-store-n-items object) => n-items}
  @argument[object]{a @class{g:list-store} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the list store}
  @begin{short}
    Accessor of the @slot[g:list-store]{n-items} slot of the
    @class{g:list-store} class.
  @end{short}
  @see-class{g:list-store}")

;;; ----------------------------------------------------------------------------
;;; g_list_store_new ()
;;; ----------------------------------------------------------------------------

;; Use the C implementation and not MAKE-INSTANCE because we have to pass
;; a pointer of a GType for the ITEM-TYPE property.

(cffi:defcfun ("g_list_store_new" list-store-new) (gobject:object list-store)
 #+liber-documentation
 "@version{2023-8-15}
  @argument[gtype]{a @class{g:type-t} type for the items in the list}
  @return{A new @class{g:list-store} object.}
  @begin{short}
    Creates a new list store with items of @arg{gtype} type.
  @end{short}
  The @arg{gtype} type must be a subclass of the @class{g:object} class.
  @see-class{g:list-store}
  @see-class{g:type-t}
  @see-class{g:object}"
  (gtype gobject:type-t))

(export 'list-store-new)

;;; ----------------------------------------------------------------------------
;;; g_list_store_insert ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_insert" list-store-insert) :void
 #+liber-documentation
 "@version{#2023-8-15}
  @argument[store]{a @class{g:list-store} object}
  @argument[position]{an unsigned integer with the position at which to insert
    the new item}
  @argument[item]{a @class{g:object} object with the new item}
  @begin{short}
    Inserts the item into the list store at @arg{position}.
  @end{short}
  The item must be of type @slot[g:list-store]{item-type} type or derived from
  it. The @arg{position} argument must be smaller than the length of the list
  store, or equal to it to append.

  Use the @fun{g:list-store-splice} function to insert multiple items at the
  same time efficiently.
  @see-class{g:list-store}
  @see-class{g:object}
  @see-function{g:list-store-item-type}
  @see-function{g:list-store-splice}"
  (store (gobject:object list-store))
  (position :uint)
  (item gobject:object))

(export 'list-store-insert)

;;; ----------------------------------------------------------------------------
;;; g_list_store_insert_sorted ()
;;;
;;; guint
;;; g_list_store_insert_sorted (GListStore *store,
;;;                             gpointer item,
;;;                             GCompareDataFunc compare_func,
;;;                             gpointer user_data);
;;;
;;; Inserts item into store at a position to be determined by the compare_func .
;;;
;;; The list must already be sorted before calling this function or the result
;;; is undefined. Usually you would approach this by only ever inserting items
;;; by way of this function.
;;;
;;; This function takes a ref on item .
;;;
;;; store:
;;;     a GListStore
;;;
;;; item:
;;;     the new item.
;;;
;;; compare_func:
;;;     pairwise comparison function for sorting.
;;;
;;; user_data
;;;     user data for compare_func .
;;;
;;; Returns
;;;     the position at which item was inserted
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_append ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_append" list-store-append) :void
 #+liber-documentation
 "@version{2023-8-15}
  @argument[list]{a @class{g:list-store} object}
  @argument[item]{a @class{g:object} object with the new item}
  @begin{short}
    Appends the item to the list store.
  @end{short}
  The item must be of type @slot[g:list-store]{item-type} type. Use the
  @fun{g:list-store-splice} function to append multiple items at the same time
  efficiently.
  @see-class{g:list-store}
  @see-class{g:object}
  @see-function{g:list-store-item-type}
  @see-function{g:list-store-splice}"
  (store (gobject:object list-store))
  (item gobject:object))

(export 'list-store-append)

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_remove" list-store-remove) :void
 #+liber-documentation
 "@version{#2023-8-15}
  @argument[list]{a @class{g:list-store} object}
  @argument[position]{an unsigned integer with the position of the item that
    is to be removed}
  @begin{short}
    Removes the item from the list store that is at @arg{position}.
  @end{short}
  The @arg{position} argument must be smaller than the current length of the
  list store.

  Use the @fun{g:list-store-splice} function to remove multiple items at the
  same time efficiently.
  @see-class{g:list-store}
  @see-function{g:list-store-splice}"
  (list (gobject:object list-store))
  (position :uint))

(export 'list-store-remove)

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove_all ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_remove_all" list-store-remove-all) :void
 #+liber-documentation
 "@version{#2023-8-15}
  @argument[list]{a @class{g:list-store} object}
  @short{Removes all items from the list store.}
  @see-class{g:list-store}"
  (list (gobject:object list-store)))

(export 'list-store-remove-all)

;;; ----------------------------------------------------------------------------
;;; g_list_store_splice ()
;;;
;;; void
;;; g_list_store_splice (GListStore *store,
;;;                      guint position,
;;;                      guint n_removals,
;;;                      gpointer *additions,
;;;                      guint n_additions);
;;;
;;; Changes store by removing n_removals items and adding n_additions items to
;;; it. additions must contain n_additions items of type "item-type". NULL is
;;; not permitted.
;;;
;;; This function is more efficient than g_list_store_insert() and
;;; g_list_store_remove(), because it only emits "items-changed" once for the
;;; change.
;;;
;;; This function takes a ref on each item in additions .
;;;
;;; The parameters position and n_removals must be correct (ie: position +
;;; n_removals must be less than or equal to the length of the list at the time
;;; this function is called).
;;;
;;; store:
;;;     a GListStore
;;;
;;; position:
;;;     the position at which to make the change
;;;
;;; n_removals:
;;;     the number of items to remove
;;;
;;; additions:
;;;     the items to add.
;;;
;;; n_additions:
;;;     the number of items to add
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_sort ()
;;;
;;; void
;;; g_list_store_sort (GListStore *store,
;;;                    GCompareDataFunc compare_func,
;;;                    gpointer user_data);
;;;
;;; Sort the items in store according to compare_func .
;;;
;;; store:
;;;     a GListStore
;;;
;;; compare_func:
;;;     pairwise comparison function for sorting.
;;;
;;; user_data:
;;;     user data for compare_func .
;;;
;;; Since: 2.46
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_find ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_find" %list-store-find) :boolean
  (list (gobject:object list-store))
  (item gobject:object)
  (position (:pointer :uint)))

(defun list-store-find (list item)
 #+liber-documentation
 "@version{2023-8-15}
  @argument[list]{a @class{g:list-store} object}
  @argument[item]{a @class{g:object} item}
  @return{An unsigned integer with the first position of the item, if it was
    found, otherwise @code{nil}.}
  @begin{short}
    Looks up the given item in the list store by looping over the items until
    the first occurrence of @arg{item}.
  @end{short}
  If the @arg{item} argument was not found, then this method will return
  @code{nil}.

  If you need to compare the two items with a custom comparison function, use
  the @fun{g:list-store-find-with-equal-func} function with a custom
  @code{GEqualFunc} instead.
  @see-class{g:list-store}
  @see-class{g:object}
  @see-function{g:list-store-find-with-equal-func}"
  (cffi:with-foreign-object (position :uint)
    (when (%list-store-find list item position)
      (cffi:mem-ref position :uint))))

(export 'list-store-find)

;;; ----------------------------------------------------------------------------
;;; g_list_store_find_with_equal_func ()
;;;
;;; gboolean
;;; g_list_store_find_with_equal_func (GListStore *store,
;;;                                    gpointer item,
;;;                                    GEqualFunc equal_func,
;;;                                    guint *position);
;;;
;;; Looks up the given item in the list store by looping over the items and
;;; comparing them with compare_func until the first occurrence of item which
;;; matches. If item was not found, then position will not be set, and this
;;; method will return FALSE.
;;;
;;; store:
;;;     a GListStore
;;;
;;; item:
;;;     an item.
;;;
;;; equal_func:
;;;     A custom equality check function.
;;;
;;; position:
;;;     the first position of item , if it was found.
;;;
;;; Returns:
;;;     Whether store contains item . If it was found, position will be set to
;;;     the position where item occurred for the first time.
;;;
;;; Since: 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_find_with_equal_func_full
;;;
;;; gboolean
;;; g_list_store_find_with_equal_func_full (GListStore* store,
;;;                                         GObject* item,
;;;                                         GEqualFuncFull equal_func,
;;;                                         gpointer user_data,
;;;                                         guint* position)
;;;
;;; Like g_list_store_find_with_equal_func() but with an additional user_data
;;; that is passed to equal_func.
;;;
;;; item is always passed as second parameter to equal_func.
;;;
;;; Since GLib 2.76 it is possible to pass NULL for item.
;;;
;;; item :
;;;     An item. The argument can be NULL.
;;;
;;; equal_func :
;;;     A custom equality check function.
;;;
;;; user_data :
;;;     User data for equal_func. The argument can be NULL.
;;;
;;; position :
;;;     The first position of item, if it was found. The argument will be set
;;;     by the function. The argument can be NULL.
;;;
;;; Return :
;;;     Whether store contains item. If it was found, position will be set to
;;;     the position where item occurred for the first time.
;;;
;;; Since 2.74
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.list-store.lisp ----------------------------------------
