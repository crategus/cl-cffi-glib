;;; ----------------------------------------------------------------------------
;;; gio.list-store.lisp
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
;;;     GCompareDataFunc
;;;     GEqualFuncFull
;;;
;;;     g_list_store_new
;;;     g_list_store_append
;;;     g_list_store_insert
;;;     g_list_store_insert_sorted
;;;     g_list_store_remove
;;;     g_list_store_remove_all
;;;     g_list_store_splice
;;;     g_list_store_sort
;;;     g_list_store_find
;;;     g_list_store_find_with_equal_func
;;;     g_list_store_find_with_equal_func_full              Since 2.74
;;;
;;; Properties
;;;
;;;     item-type
;;;     n-items                                             Since 2.74
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

(gobject:define-gobject "GListStore" list-store
  (:superclass gobject:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "g_list_store_get_type")
  ((item-type
    list-store-item-type
    "item-type" "GType" t t)
   #+glib-2-74
   (n-items
    list-store-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'list-store 'type)
 "@version{2025-3-24}
  @begin{short}
    The @class{g:list-store} object is an implementation of the
    @class{g:list-model} interface that stores all items in memory.
  @end{short}
  It provides insertions, deletions, and lookups in logarithmic time with a
  fast path for the common case of iterating the list linearly. Items must be
  subclasses of the @class{g:object} class.
  @see-constructor{g:list-store-new}
  @see-slot{g:list-store-item-type}
  @see-slot{g:list-store-n-items}
  @see-class{g:list-model}
  @see-class{g:object}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:list-store-item-type -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item-type" 'list-store) t)
 "The @code{item-type} property of type @class{g:type-t}
  (Read / Write / Construct Only) @br{}
  The type of items contained in the list store. Items must be subclasses of
  the @class{g:object} class.")

#+liber-documentation
(setf (liber:alias-for-function 'list-store-item-type)
      "Accessor"
      (documentation 'list-store-item-type 'function)
 "@version{2025-3-24}
  @syntax{(g:list-store-item-type object) => gtype}
  @argument[object]{a @class{g:list-store} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[g:list-store]{item-type} slot of the
    @class{g:list-store} class.
  @end{short}
  The type of items contained in the list store. Items must be subclasses of
  the @class{g:object} class.
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(g:list-store-new \"GAction\")
=> #<GIO:LIST-STORE {1003FCA4C3@}>
(g:list-store-item-type *)
=> #<GTYPE :name \"GAction\" :id 106084831684528>
    @end{pre}
  @end{dictionary}
  @see-class{g:list-store}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- g:list-store-n-items ---------------------------------------------------

#+(and glib-2-74 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'list-store) t)
 "The @code{n-items} property of type @code{:uint} (Read) @br{}
  The number of items contained in the list store. Since 2.74 @br{}
  Default value: 0")

#+(and glib-2-74 liber-documentation)
(setf (liber:alias-for-function 'list-store-n-items)
      "Accessor"
      (documentation 'list-store-n-items 'function)
 "@version{2025-4-26}
  @syntax{(g:list-store-n-items object) => n-items}
  @argument[object]{a @class{g:list-store} object}
  @argument[n-items]{an unsigned integer for the number of items contained in
    the list store}
  @begin{short}
    Accessor of the @slot[g:list-store]{n-items} slot of the
    @class{g:list-store} class.
  @end{short}
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-n-items} function.
  @end{dictionary}

  Since 2.74
  @see-class{g:list-store}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; g_list_store_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_new" list-store-new)
    (gobject:object list-store :return)
 #+liber-documentation
 "@version{2025-4-11}
  @argument[gtype]{a @class{g:type-t} type ID for the items in the list}
  @return{The new @class{g:list-store} object.}
  @begin{short}
    Creates a new list store with items of @arg{gtype} type.
  @end{short}
  The @arg{gtype} type must be a subclass of the @class{g:object} class.
  @begin[Examples]{dictionary}
    Create a list store for @class{g:app-info} objects containing all
    applications registered on the system.
    @begin{pre}
(defun create-application-list ()
  (let ((store (g:list-store-new \"GAppInfo\"))
        (apps (g:app-info-all)))
    (dolist (app apps)
      (g:list-store-append store app))
  store))
    @end{pre}
  @end{dictionary}
  @see-class{g:list-store}
  @see-class{g:type-t}
  @see-class{g:object}"
  (gtype gobject:type-t))

(export 'list-store-new)

;;; ----------------------------------------------------------------------------
;;; g_list_store_append
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_append" list-store-append) :void
 #+liber-documentation
 "@version{2025-4-26}
  @argument[store]{a @class{g:list-store} object}
  @argument[item]{a @class{g:object} object for the new item}
  @begin{short}
    Appends the item to the list store.
  @end{short}
  The item must be of @slot[g:list-store]{item-type} type. Use the
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
;;; g_list_store_insert
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_insert" list-store-insert) :void
 #+liber-documentation
 "@version{2025-4-26}
  @argument[store]{a @class{g:list-store} object}
  @argument[pos]{an unsigned integer for the position at which to insert the
    new item}
  @argument[item]{a @class{g:object} instance for the new item}
  @begin{short}
    Inserts the item into the list store at @arg{pos}.
  @end{short}
  The item must be of @slot[g:list-store]{item-type} type or derived from it.
  The @arg{pos} argument must be smaller than the length of the list store, or
  equal to it to append.

  Use the @fun{g:list-store-splice} function to insert multiple items at the
  same time efficiently.
  @see-class{g:list-store}
  @see-class{g:object}
  @see-function{g:list-store-item-type}
  @see-function{g:list-store-splice}"
  (store (gobject:object list-store))
  (pos :uint)
  (item gobject:object))

(export 'list-store-insert)

;;; ----------------------------------------------------------------------------
;;; GCompareDataFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback compare-data-func :int
    ((a gobject:object)
     (b gobject:object)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func a b)
      (return< () :report "Return -1" -1)
      (return= () :report "Return  0" 0)
      (return> () :report "Return  1" 1))))

#+liber-documentation
(setf (liber:alias-for-symbol 'compare-data-func)
      "Callback"
      (liber:symbol-documentation 'compare-data-func)
 "@version{2025-3-24}
  @syntax{lambda (a b) => result}
  @argument[a]{a @class{g:object} instance}
  @argument[b]{a @class{g:object} instance to compare with}
  @argument[result]{negative integer if @code{a < b}, zero if @code{a = b},
    positive integer if @code{a > b}}
  @begin{short}
    Specifies the type of a comparison function used to compare two values.
  @end{short}
  The function should return a negative integer if the first value comes before
  the second, 0 if they are equal, or a positive integer if the first value
  comes after the second.
  @see-function{g:list-store-insert-sorted}")

(export 'compare-data-func)

;;; ----------------------------------------------------------------------------
;;; g_list_store_insert_sorted
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_insert_sorted" %list-store-insert-sorted) :uint
  (store (gobject:object list-store))
  (item gobject:object)
  (func :pointer)
  (data :pointer))

(defun list-store-insert-sorted (store item func)
 #+liber-documentation
 "@version{2025-3-24}
  @argument[store]{a @class{g:list-store} object}
  @argument[item]{a @class{g:object} object}
  @argument[func]{a @symbol{g:compare-data-func} callback function}
  @begin{return}
    The unsigned integer with the position at which @arg{item} was inserted.
  @end{return}
  @begin{short}
    Inserts the item into the list store at a position to be determined by the
    @arg{func} callback function.
  @end{short}
  The list store must already be sorted before calling this function or the
  result is undefined. Usually you would approach this by only ever inserting
  items by way of this function.
  @see-class{g:list-store}
  @see-class{g:object}
  @see-symbol{g:compare-data-func}"
  (glib:with-stable-pointer (ptr func)
    (%list-store-insert-sorted store
                               item
                               (cffi:callback compare-data-func)
                               ptr)))

(export 'list-store-insert-sorted)

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_remove" list-store-remove) :void
 #+liber-documentation
 "@version{2025-4-26}
  @argument[store]{a @class{g:list-store} object}
  @argument[pos]{an unsigned integer for the position of the item that is to
    be removed}
  @begin{short}
    Removes the item from the list store that is at @arg{pos}.
  @end{short}
  The @arg{pos} argument must be smaller than the current length of the list
  store. Use the @fun{g:list-store-splice} function to remove multiple items at
  the same time efficiently.
  @see-class{g:list-store}
  @see-function{g:list-store-splice}"
  (store (gobject:object list-store))
  (pos :uint))

(export 'list-store-remove)

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_remove_all" list-store-remove-all) :void
 #+liber-documentation
 "@version{2025-3-24}
  @argument[store]{a @class{g:list-store} object}
  @short{Removes all items from the list store.}
  @see-class{g:list-store}"
  (store (gobject:object list-store)))

(export 'list-store-remove-all)

;;; ----------------------------------------------------------------------------
;;; g_list_store_splice
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_splice" %list-store-splice) :void
  (store (gobject:object list-store))
  (pos :uint)
  (n :uint)
  (additions :pointer)
  (n-additions :uint))

(defun list-store-splice (store pos n &rest additions)
 #+liber-documentation
 "@version{2025-4-26}
  @argument[store]{a @class{g:list-store} object}
  @argument[pos]{an unsigned integer for the position at which to make the
    change}
  @argument[n]{an unsigned integer for the number of items to remove}
  @argument[additions]{@class{g:object} instances to add}
  @begin{short}
    Changes the list store by removing @arg{n} items and adding @arg{additions}
    to it.
  @end{short}
  The @arg{additions} values must contain items of the
  @slot[g:list-store]{item-type} type. This function is more efficient than the
  @fun{g:list-store-insert} and @fun{g:list-store-remove} functions, because it
  only emits the @code{\"items-changed\"} signal once for the change.

  The @arg{pos} and @arg{n} arguments must be correct, that is, @arg{pos} +
  @arg{n} must be less than or equal to the length of the list store at the
  time this function is called.
  @see-class{g:list-store}
  @see-class{g:object}
  @see-function{g:list-store-insert}
  @see-function{g:list-store-remove}
  @see-function{g:list-store-item-type}"
  (let ((n-additions (length additions)))
    (cffi:with-foreign-object (additions-ar :pointer n-additions)
      (iter (for i from 0 below n-additions)
            (for addition in additions)
            (setf (cffi:mem-aref additions-ar :pointer i)
                  (if addition
                      (gobject:object-pointer addition)
                      (cffi:null-pointer))))
      (%list-store-splice store pos n additions-ar n-additions))))

(export 'list-store-splice)

;;; ----------------------------------------------------------------------------
;;; g_list_store_sort
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_sort" %list-store-sort) :void
  (store (gobject:object list-store))
  (func :pointer)
  (data :pointer))

(defun list-store-sort (store func)
 #+liber-documentation
 "@version{2025-3-24}
  @argument[store]{a @class{g:list-store} object}
  @argument[func]{a @symbol{g:compare-data-func} callback function for sorting}
  @begin{short}
    Sort the items in the list store according to @arg{func}.
  @end{short}
  @see-class{g:list-store}
  @see-symbol{g:compare-data-func}"
  (glib:with-stable-pointer (ptr func)
    (%list-store-sort store
                      (cffi:callback compare-data-func)
                      ptr)))

(export 'list-store-sort)

;;; ----------------------------------------------------------------------------
;;; g_list_store_find
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_list_store_find" %list-store-find) :boolean
  (store (gobject:object list-store))
  (item gobject:object)
  (pos (:pointer :uint)))

(defun list-store-find (store item)
 #+liber-documentation
 "@version{2025-4-26}
  @argument[store]{a @class{g:list-store} object}
  @argument[item]{a @class{g:object} instance for the item}
  @begin{return}
    The unsigned integer with the first position of the item, if it was found,
    otherwise @code{nil}.
  @end{return}
  @begin{short}
    Looks up the given item in the list store by looping over the items until
    the first occurrence of @arg{item}.
  @end{short}
  If the @arg{item} argument was not found, then this method will return
  @code{nil}.

  If you need to compare the two items with a custom comparison function, use
  the @fun{g:list-store-find-with-equal-func} function with a custom
  @symbol{g:equal-func-full} callback function instead.
  @see-class{g:list-store}
  @see-class{g:object}
  @see-symbol{g:equal-func-full}
  @see-function{g:list-store-find-with-equal-func}"
  (cffi:with-foreign-object (pos :uint)
    (when (%list-store-find store item pos)
      (cffi:mem-ref pos :uint))))

(export 'list-store-find)

;;; ----------------------------------------------------------------------------
;;; GEqualFuncFull
;;; ----------------------------------------------------------------------------

#+glib-2-74
(cffi:defcallback equal-func-full :boolean
    ((a gobject:object)
     (b gobject:object)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func a b)
      (return-true () :report "Return t" t)
      (return-false () :report "Return NIL" nil))))

#+glib-2-74
#+liber-documentation
(setf (liber:alias-for-symbol 'equal-func-full)
      "Callback"
      (liber:symbol-documentation 'equal-func-full)
 "@version{2025-3-24}
  @syntax{lambda (a b) => result}
  @argument[a]{a @class{g:object} instance}
  @argument[b]{a @class{g:object} instance to compare with}
  @argument[result]{@em{true} if @code{a = b}, @em{false} otherwise}
  @begin{short}
    Specifies the type of a callback function used to test two values for
    equality.
  @end{short}
  The function should return @em{true} if both values are equal and @em{false}
  otherwise.

  Since 2.74
  @see-class{g:object}
  @see-function{g:list-store-find-with-equal-func}")

#+glib-2-74
(export 'equal-func-full)

;;; ----------------------------------------------------------------------------
;;; g_list_store_find_with_equal_func
;;; g_list_store_find_with_equal_func_full
;;; ----------------------------------------------------------------------------

#+glib-2-74
(cffi:defcfun ("g_list_store_find_with_equal_func_full"
               %list-store-find-with-equal-func-full) :boolean
  (store (gobject:object list-store))
  (item gobject:object)
  (func :pointer)
  (data :pointer)
  (pos (:pointer :uint)))

#+glib-2-74
(defun list-store-find-with-equal-func (store item func)
 #+liber-documentation
 "@version{2025-4-26}
  @argument[store]{a @class{g:list-store} object}
  @argument[item]{a @class{g:object} instance}
  @argument[func]{a @symbol{g:equal-func-full} callback function}
  @begin{return}
    The unsigned integer with the first position of the item, if it was found,
    otherwise @code{nil}.
  @end{return}
  @begin{short}
    Looks up the given item in the list store by looping over the items and
    comparing them with the @arg{func} callback function until the first
    occurrence of item which matches.
  @end{short}
  If the item was not found, this method will return @em{false}.

  Since 2.74
  @see-class{g:list-store}
  @see-class{g:object}
  @see-symbol{g:equal-func-full}"
  (glib:with-stable-pointer (ptr func)
    (cffi:with-foreign-object (pos :uint)
      (when (%list-store-find-with-equal-func-full
                    store
                    item
                    (cffi:callback equal-func-full)
                    ptr
                    pos)
        (cffi:mem-ref pos :uint)))))

#+glib-2-74
(export 'list-store-find-with-equal-func)

;;; --- End of file gio.list-store.lisp ----------------------------------------
