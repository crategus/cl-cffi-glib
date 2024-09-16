;;; ----------------------------------------------------------------------------
;;; gio.list-model.lisp
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
;;; GListModel
;;;
;;;     An interface describing a dynamic list of objects
;;;
;;; Types and Values
;;;
;;;     GListModel
;;;     GListModelInterface
;;;
;;; Functions
;;;
;;;     g_list_model_get_item_type
;;;     g_list_model_get_n_items
;;;     g_list_model_get_item
;;;     g_list_model_get_object
;;;     g_list_model_items_changed
;;;
;;; Signals
;;;
;;;     items-changed
;;;
;;; Object Hierarchy
;;;
;;;    GInterface
;;;     ╰── GListModel
;;;
;;; Prerequisites
;;;
;;;     GListModel requires GObject.
;;;
;;; Known Implementations
;;;
;;;     GListModel is implemented by GListStore.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GListModel
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GListModel" list-model
  (:export t
   :type-initializer "g_list_model_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'list-model)
      "Interface"
      (documentation 'list-model 'type)
 "@version{2024-3-31}
  @begin{short}
    The @class{g:list-model} interface is an interface that represents a mutable
    list of @class{g:object} instances.
  @end{short}
  Its main intention is as a model for various widgets in user interfaces, such
  as list views, but it can also be used as a convenient method of returning
  lists of data, with support for updates.

  Each object in the list may also report changes in itself via some mechanism,
  normally the @code{\"notify\"} signal. Taken together with the
  @code{\"items-changed\"} signal, this provides for a list that can change its
  membership, and in which the members can change their individual properties.

  A good example would be the list of visible wireless network access points,
  where each access point can report dynamic properties such as signal strength.

  It is important to note that the @class{g:list-model} implementation itself
  does not report changes to the individual items. It only reports changes to
  the list membership. If you want to observe changes to the objects themselves
  then you need to connect signals to the objects that you are interested in.

  All items in a @class{g:list-model} instance are of, or derived from, the same
  type. The @fun{g:list-model-item-type} function returns that type. The type
  may be an interface, in which case all objects in the list must implement it.

  The semantics are close to that of an array. The @fun{g:list-model-n-items}
  function returns the number of items in the list and the
  @fun{g:list-model-item} function returns an item at a (0-based) position. In
  order to allow implementations to calculate the list length lazily, you can
  also iterate over items. Starting from 0, repeatedly call the
  @fun{g:list-model-item} function until it returns @code{null-pointer}.

  An implementation may create objects lazily, but must take care to return the
  same object for a given position until all references to it are gone.

  On the other side, a consumer is expected only to hold references on objects
  that are currently \"user visible\", in order to facilitate the maximum level
  of laziness in the implementation of the list and to reduce the required
  number of signal connections at a given time.

  This interface is intended only to be used from a single thread. The thread
  in which it is appropriate to use it depends on the particular implementation,
  but typically it will be from the thread that owns the thread-default main
  context in effect at the time that the model was created.
  @begin[Signal Details]{dictionary}
    @subheading{The \"items-changed\" signal}
      @begin{pre}
lambda (model pos removed added)    :run-last
      @end{pre}
      This signal is emitted whenever items were added to or removed from
      @arg{model}. At @arg{pos}, removed items were removed and added items
      were added in their place. Note: If @arg{removed} is not equal
      @arg{added}, the positions of all later items in the model change.
      @begin[code]{table}
        @entry[model]{The @class{g:list-model} instance that changed.}
        @entry[pos]{The unsigned integer with the position at which @arg{model}
          changed.}
        @entry[removed]{The unsigned integer with the number of items removed.}
        @entry[added]{The unsigned integer with the number of items addes.}
      @end{table}
  @end{dictionary}
  @see-class{g:list-store}")

;;; ----------------------------------------------------------------------------
;;; GListModelInterface
;;;
;;; struct GioListModelInterface {
;;;   GTypeInterface g_iface;
;;;   GType (* get_item_type) (
;;;     GListModel* list
;;;   );
;;;
;;;   guint (* get_n_items) (
;;;     GListModel* list
;;;   );
;;;   GObject* (* get_item) (
;;;     GListModel* list,
;;;     guint position
;;;   );
;;; }
;;;
;;; The virtual function table for GListModel.
;;;
;;; Interface members
;;;
;;; g_iface
;;; GTypeInterface
;;;
;;; Parent GTypeInterface.
;;;
;;; get_item_type
;;; GType (* get_item_type) (GListModel* list)
;;;
;;; No description available.
;;;
;;; get_n_items
;;; guint (* get_n_items) (GListModel* list)
;;;
;;; No description available.
;;;
;;; get_item
;;;
;;; GObject* (* get_item) (GListModel* list, guint position)
;;;
;;; No description available.
;;;
;;; Virtual methods
;;;
;;; Gio.ListModel.get_item
;;;
;;; Get the item at position. If position is greater than the number of items
;;; in list, NULL is returned.
;;;
;;; Gio.ListModel.get_item_type
;
;;; Gets the type of the items in list.
;;;
;;; Gio.ListModel.get_n_items
;;;
;;; Gets the number of items in list.
;;; ----------------------------------------------------------------------------

(gobject:define-vtable ("GListModel" list-model)
  (:skip parent-instance (:struct gobject:type-interface))
  ;; Methods of the GListModel interface
  (get-item-type (gobject:type-t (model (gobject:object list-model))))
  (get-n-items (:uint (model (gobject:object list-model))))
  (get-item (gobject:object (model (gobject:object list-model))
                            (position :uint))))

(export 'list-model-get-item-type-impl)
(export 'list-model-get-n-items-impl)
(export 'list-model-get-item-impl)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_item_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_model_get_item_type" list-model-item-type) gobject:type-t
 #+liber-documentation
 "@version{2024-3-31}
  @argument[model]{a @class{g:list-model} object}
  @return{The @class{g:type-t} type ID of the items contained in @arg{model}.}
  @begin{short}
    Gets the type of the items in the list model.
  @end{short}
  All items returned from the @fun{g:list-model-item-type} function are of that
  type or a subtype, or are an implementation of that interface. The item type
  of a @class{g:list-model} object can not change during the life of the model.
  @see-class{g:list-model}
  @see-class{g:type-t}"
  (model (gobject:object list-model)))

(export 'list-model-item-type)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_n_items ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_model_get_n_items" list-model-n-items) :uint
 #+liber-documentation
 "@version{2024-3-31}
  @argument[model]{a @class{g:list-model} object}
  @return{The integer with the number of items in @arg{model}.}
  @begin{short}
    Gets the number of items in the list model.
  @end{short}
  Depending on the list model implementation, calling this function may be less
  efficient than iterating the list model with increasing values for @arg{pos}
  until the @fun{g:list-model-item} functions returns the @code{null-pointer}
  value.
  @see-class{g:list-model}
  @see-function{g:list-model-item}"
  (model (gobject:object list-model)))

(export 'list-model-n-items)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_item ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_model_get_item" list-model-item) :pointer
 #+liber-documentation
 "@version{2024-3-31}
  @argument[model]{a @class{g:list-model} object}
  @argument[pos]{an unsigned integer with the position of the item to fetch}
  @return{The pointer for the item at @arg{pos}.}
  @begin{short}
    Get the pointer for the item at @arg{pos} in the list model.
  @end{short}
  If the @arg{pos} argument is greater than the number of items in the list
  model, the @code{null-pointer} value is returned. The @code{null-pointer}
  value is never returned for an index that is smaller than the length of the
  list model. See the @fun{g:list-model-n-items} function.
  @begin[Note]{dictionary}
    This function returns a pointer which can be translated to the
    corresponding object with the @code{cffi:convert-from-foreign} function:
    @begin{pre}
(cffi:convert-from-foreign (g:list-model-item model pos) 'g:object)
    @end{pre}
    In the Lisp implementation, the @fun{g:list-model-object} function is more
    useful, which directly returns the object at @arg{pos}.
  @end{dictionary}
  @see-class{g:list-model}
  @see-function{g:list-model-n-items}
  @see-function{g:list-model-object}"
  (model (gobject:object list-model))
  (pos :uint))

(export 'list-model-item)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_object ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_model_get_object" list-model-object) gobject:object
 #+liber-documentation
 "@version{2024-3-31}
  @argument[model]{a @class{g:list-model} object}
  @argument[pos]{an unsigned integer with the position of the item to fetch}
  @return{The @class{g:object} instance at @arg{pos}.}
  @begin{short}
    Get the item at @arg{pos}.
  @end{short}
  If the @arg{pos} argument is greater than the number of items in the list
  model, @code{nil} is returned. The @code{nil} value is never returned for an
  index that is smaller than the length of the list model. See the
  @fun{g:list-model-n-items} function.
  @see-class{g:list-model}
  @see-class{g:object}
  @see-function{g:list-model-n-items}"
  (model (gobject:object list-model))
  (position :uint))

(export 'list-model-object)

;;; ----------------------------------------------------------------------------
;;; g_list_model_items_changed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_model_items_changed" list-model-items-changed) :void
 #+liber-documentation
 "@version{#2024-3-31}
  @argument[model]{a @class{g:list-model} object}
  @argument[pos]{an unsigned integer with the position at which @arg{model}
    changed}
  @argument[removed]{an unsigned integer with the number of items removed}
  @argument[added]{an unsigned integer with the number of items added}
  @begin{short}
    Emits the @code{\"items-changed\"} signal on @arg{model}.
  @end{short}
  This function should only be called by classes implementing the
  @class{g:list-model} interface. It has to be called after the internal
  representation of the list model has been updated, because handlers connected
  to this signal might query the new state of the list model.

  Implementations must only make changes to the model, as visible to its
  consumer, in places that will not cause problems for that consumer. For models
  that are driven directly by a write API, such as the @class{g:list-store}
  object, changes can be reported in response to uses of that API. For models
  that represent remote data, changes should only be made from a fresh mainloop
  dispatch. It is particularly not permitted to make changes in response to a
  call to the @class{g:list-model} consumer API.

  Stated another way: in general, it is assumed that code making a series of
  accesses to the model via the API, without returning to the main loop, and
  without calling other code, will continue to view the same contents of the
  model.
  @see-class{g:list-model}
  @see-class{g:list-store}"
  (model (gobject:object list-model))
  (pos :uint)
  (removed :uint)
  (added :uint))

(export 'list-model-items-changed)

;;; --- End of file gio.list-model.lisp ----------------------------------------
