;;; ----------------------------------------------------------------------------
;;; gio.list-model.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding for the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2021 - 2025 Dieter Kaiser
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

(gobject:define-ginterface "GListModel" list-model
  (:export t
   :type-initializer "g_list_model_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'list-model)
      "Interface"
      (documentation 'list-model 'type)
 "@version{2025-3-24}
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
  @fun{g:list-model-item} function until it returns @code{nil}.

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
      @begin[code]{table}
        @entry[model]{The @class{g:list-model} instance that changed.}
        @entry[pos]{The unsigned integer with the position at which @arg{model}
          changed.}
        @entry[removed]{The unsigned integer with the number of items removed.}
        @entry[added]{The unsigned integer with the number of items added.}
      @end{table}
      This signal is emitted whenever items were added to or removed from
      @arg{model}. At @arg{pos}, removed items were removed and added items
      were added in their place. Note: If @arg{removed} is not equal
      @arg{added}, the positions of all later items in the model change.
  @end{dictionary}
  @see-class{g:list-store}")

;;; ----------------------------------------------------------------------------
;;; GListModelInterface
;;; ----------------------------------------------------------------------------

(gobject:define-vtable ("GListModel" list-model)
  (:skip parent-instance (:struct gobject:type-interface))
  ;; Methods of the GListModel interface
  (get-item-type (gobject:type-t (model (gobject:object list-model))))
  (get-n-items (:uint (model (gobject:object list-model))))
  (get-item (gobject:object (model (gobject:object list-model))
                            (pos :uint))))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-model-vtable)
      "VTable"
      (liber:symbol-documentation 'list-model-vtable)
 "@version{2025-3-24}
  @begin{declaration}
(gobject:define-vtable (\"GListModel\" list-model)
  (:skip parent-instance (:struct gobject:type-interface))
  ;; Methods of the GListModel interface
  (get-item-type                               ; virtual function
     (gobject:type-t                           ; return type
       (model (gobject:object list-model))))   ; argument
  (get-n-items (:uint
                (model (gobject:object list-model))))
  (get-item (gobject:object
             (model (gobject:object list-model))
             (pos :uint))))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[get-item-type]{Virtual function called by the
        @fun{g:list-model-item-type} function. You must implement the
        @fun{g:list-model-get-item-type-impl} method, when subclassing from
        the @class{g:list-model} interface.}
      @entry[get-n-items]{Virtual function called by the
        @fun{g:list-model-n-items} function. You must implement the
        @fun{g:list-model-get-n-items-impl} method, when subclassing from the
        @class{g:list-model} interface.}
      @entry[get-item]{Virtual function called by the @fun{g:list-model-item}
        function. You must implement the @fun{g:list-model-get-item-impl}
        method, when subclassing from the @class{g:list-model} interface.}
    @end{table}
  @end{values}
  The virtual function table for the @class{g:list-model} interface.
  @begin[Examples]{dictionary}
    Simple example of subclassing the @class{g:list-model} interface. The
    model is internally represented as a Lisp list.
    @begin{pre}
;; Simple implementation which uses a Lisp list
(gobject:define-gobject-subclass \"CLListStore\" cl-list-store
  (:superclass g:object
   :export t
   :interfaces (\"GListModel\"))
  ((:cl list
        :initform '()
        :accessor cl-list-store-list)))

(defmethod gio:list-model-get-item-type-impl ((store cl-list-store))
  (g:gtype \"GAction\"))

(defmethod gio:list-model-get-n-items-impl ((store cl-list-store))
  (length (cl-list-store-list store)))

(defmethod gio:list-model-get-item-impl ((store cl-list-store) pos)
  (let ((item (nth pos (cl-list-store-list store))))
    (when item
      ;; We must add a reference to the returned item
      (g:object-ref item))))
    @end{pre}
  @end{dictionary}
  @see-class{g:list-model}
  @see-function{g:list-model-get-item-type-impl}
  @see-function{g:list-model-get-n-items-impl}
  @see-function{g:list-model-get-item-impl}")

(export 'list-model-vtable)

;;; --- g:list-model-get-item-type-impl ----------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'list-model-get-item-type-impl)
      "Generic"
      (documentation 'list-model-get-item-type-impl 'function)
 "@version{2025-3-24}
  @argument[model]{a @class{g:object} instance}
  @return{The @class{g:type-t} type ID for the items contained in @arg{model}.}
  @begin{short}
    Method called from the @fun{g:list-model-item-type} function for a
    subclass of the @class{g:list-model} interface.
  @end{short}
  You must implement the method, when subclassing the interface.
  @see-class{g:list-model}
  @see-symbol{g:list-model-vtable}")

(export 'list-model-get-item-type-impl)

;;; --- g:list-model-get-n-items-impl ------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'list-model-get-n-items-impl)
      "Generic"
      (documentation 'list-model-get-n-items-impl 'function)
 "@version{2025-3-24}
  @argument[model]{a @class{g:object} instance}
  @return{The unsigned integer with the number of items in @arg{model}.}
  @begin{short}
    Method called from the @fun{g:list-model-n-items} function for a
    subclass of the @class{g:list-model} interface.
  @end{short}
  You must implement the method, when subclassing the interface.
  @see-class{g:list-model}
  @see-symbol{g:list-model-vtable}")

(export 'list-model-get-n-items-impl)

;;; --- g:list-model-get-item-impl ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'list-model-get-item-impl)
      "Generic"
      (documentation 'list-model-get-item-impl 'function)
 "@version{2025-3-24}
  @argument[model]{a @class{g:object} instance}
  @argument[pos]{an unsigned integer for the postion of the item to fetch}
  @return{The @class{g:object} instance at @arg{pos}.}
  @begin{short}
    Method called from the @fun{g:list-model-item} function for a
    subclass of the @class{g:list-model} interface.
  @end{short}
  You must implement the method, when subclassing the interface.
  @begin[Notes]{dictionary}
   You must add a reference with the @fun{g:object-ref} function to the
   returned item.
  @end{dictionary}
  @see-class{g:list-model}
  @see-symbol{g:list-model-vtable}
  @see-function{g:object-ref}")

(export 'list-model-get-item-impl)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_item_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_model_get_item_type" list-model-item-type) gobject:type-t
 #+liber-documentation
 "@version{2025-3-24}
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
;;; g_list_model_get_n_items
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_model_get_n_items" list-model-n-items) :uint
 #+liber-documentation
 "@version{2025-3-24}
  @argument[model]{a @class{g:list-model} object}
  @return{The integer with the number of items in @arg{model}.}
  @begin{short}
    Gets the number of items in the list model.
  @end{short}
  Depending on the list model implementation, calling this function may be less
  efficient than iterating the list model with increasing values for @arg{pos}
  until the @fun{g:list-model-item} functions returns the @code{nil} value.
  @see-class{g:list-model}
  @see-function{g:list-model-item}"
  (model (gobject:object list-model)))

(export 'list-model-n-items)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_item
;;; g_list_model_get_object
;;; ----------------------------------------------------------------------------

;; The C function is exported as g:list-model-item

(cffi:defcfun ("g_list_model_get_object" list-model-item)
    (gobject:object :return)
 #+liber-documentation
 "@version{2025-3-24}
  @argument[model]{a @class{g:list-model} object}
  @argument[pos]{an unsigned integer for the position of the item to fetch}
  @return{The @class{g:object} instance at @arg{pos}.}
  @begin{short}
    Gets the item at @arg{pos}.
  @end{short}
  If the @arg{pos} argument is greater than the number of items in the list
  model, @code{nil} is returned. The @code{nil} value is never returned for an
  index that is smaller than the length of the list model. See the
  @fun{g:list-model-n-items} function.
  @see-class{g:list-model}
  @see-class{g:object}
  @see-function{g:list-model-n-items}"
  (model (gobject:object list-model))
  (pos :uint))

(export 'list-model-item)

;;; ----------------------------------------------------------------------------
;;; g_list_model_items_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_model_items_changed" list-model-items-changed) :void
 #+liber-documentation
 "@version{2025-3-24}
  @argument[model]{a @class{g:list-model} object}
  @argument[pos]{an unsigned integer for the position at which @arg{model}
    changed}
  @argument[removed]{an unsigned integer for the number of items removed}
  @argument[added]{an unsigned integer for the number of items added}
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
