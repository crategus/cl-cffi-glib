(in-package :glib-test)

(def-suite gio-list-store :in gio-suite)
(in-suite gio-list-store)

;;; --- Types and Values -------------------------------------------------------

;;;     GListStore

(test g-list-store-class
  ;; Type check
  (is (g:type-is-object "GListStore"))
  ;; Check the registered symbol
  (is (eq 'g:list-store
          (gobject:symbol-for-gtype "GListStore")))
  ;; Check the type initializer
  (is (eq (g:gtype "GListStore")
          (g:gtype (cffi:foreign-funcall "g_list_store_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GListStore")))
  ;; Check the children
  (is (equal '()
             (list-children "GListStore")))
  ;; Check the interfaces
  (is (equal '("GListModel")
             (list-interfaces "GListStore")))
  ;; Check the class properties
  (is (equal '("item-type")
             (list-properties "GListStore")))
  (is (equal '()
             (list-signals "GListStore")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GListStore" G-LIST-STORE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GListModel"))
                       ((ITEM-TYPE G-LIST-STORE-ITEM-TYPE "item-type" "GType" T
                         NIL)))
             (get-g-type-definition "GListStore"))))

;;; --- Properties -------------------------------------------------------------

(test list-store-properties
  (let ((store (g:list-store-new "GSimpleAction")))
    ;; The accessor returns a pointer to a GType
    (is (cffi:pointerp (g:list-store-item-type store)))
    ;; The inherited accessor returns the GType
    (is (eq (g:gtype "GSimpleAction") (g:list-model-item-type store)))))

;;; --- Functions --------------------------------------------------------------

;;;     g_list_store_new

(test list-store-new
  (is (typep (g:list-store-new "GSimpleAction") 'g:list-store)))

;;;     g_list_store_insert
;;;     g_list_store_insert_sorted
;;;     g_list_store_append
;;;     g_list_store_remove
;;;     g_list_store_remove_all
;;;     g_list_store_splice
;;;     g_list_store_sort

;;;     g_list_store_find

(test list-store-find
  (let ((store (g:list-store-new "GObject"))
        (action (make-instance 'g:simple-action)))
    ;; Fill the list store with objects
    (is-false (g:list-store-append store (make-instance 'g:menu-item)))
    (is-false (g:list-store-append store (make-instance 'g:menu-item)))
    (is-false (g:list-store-append store action))
    (is-false (g:list-store-append store (make-instance 'g:menu-item)))
    ;; Find the action in the list store
    (is (= 2 (g:list-store-find store action)))))

;;;     g_list_store_find_with_equal_func

;;; 2022-11-20
