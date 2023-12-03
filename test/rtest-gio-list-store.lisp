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
          (glib:symbol-for-gtype "GListStore")))
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
  (is (equal '("item-type" "n-items")
             (list-properties "GListStore")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GListStore")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GListStore" G-LIST-STORE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GListModel"))
                       ((ITEM-TYPE G-LIST-STORE-ITEM-TYPE "item-type" "GType" T
                         NIL)
                        (N-ITEMS G-LIST-STORE-N-ITEMS "n-items" "guint" T NIL)))
             (gobject:get-g-type-definition "GListStore"))))

;;; --- Properties -------------------------------------------------------------

(test g-list-store-properties
  (let ((store (g:list-store-new "GSimpleAction")))
    ;; The accessor returns the GType
    (is (eq (g:gtype "GSimpleAction") (g:list-store-item-type store)))
    ;; The inherited accessor returns the GType
    (is (eq (g:gtype "GSimpleAction") (g:list-model-item-type store)))
    ;; Check default value for N-ITEMS
    (is (= 0 (g:list-store-n-items store)))
    ;; Again for the inheritied accessor
    (is (= 0 (g:list-model-n-items store)))))

;;; --- Functions --------------------------------------------------------------

;;;     g_list_store_new

(test g-list-store-new
  (is (typep (g:list-store-new "GSimpleAction") 'g:list-store)))

;;;     g_list_store_insert

(test g-list-store-insert
  (let ((store (g:list-store-new "GSimpleAction")))
    (is-false (g:list-store-insert store 0 (make-instance 'g:simple-action)))
    (is-false (g:list-store-insert store 0 (make-instance 'g:simple-action)))
    (is-false (g:list-store-insert store 1 (make-instance 'g:simple-action)))
    (is (= 3 (g:list-store-n-items store)))
    (is-false (g:list-store-remove-all store))
    (is (= 0 (g:list-store-n-items store)))))

;;;     g_list_store_insert_sorted

(test g-list-store-insert-sorted.1
  (let ((store (g:list-store-new "GSimpleAction")))
    (dotimes (i 10)
      (is (= 0
             (g:list-store-insert-sorted
                 store
                 (make-instance 'g:simple-action
                                :name
                                (format nil "Action ~a" i))
                 (lambda (a b)
                   (if (string< (g:simple-action-name a)
                                (g:simple-action-name b))
                       1
                       -1))))))
    (is (equal '("Action 9" "Action 8" "Action 7" "Action 6" "Action 5"
                 "Action 4" "Action 3" "Action 2" "Action 1" "Action 0")
               (iter (for i from 0 below 10)
                     (for obj = (g:list-model-object store i))
                     (collect (format nil "~a"
                                          (g:simple-action-name obj))))))))

(test g-list-store-insert-sorted.2
  (let ((store (g:list-store-new "GSimpleAction")))
    (dotimes (i 10)
      (is (= i
             (g:list-store-insert-sorted
                 store
                 (make-instance 'g:simple-action
                                :name
                                (format nil "Action ~a" i))
                 (lambda (a b)
                   (if (string> (g:simple-action-name a)
                                (g:simple-action-name b))
                       1
                       -1))))))
    (is (equal '("Action 0" "Action 1" "Action 2" "Action 3" "Action 4"
                 "Action 5" "Action 6" "Action 7" "Action 8" "Action 9")
               (iter (for i from 0 below 10)
                     (for obj = (g:list-model-object store i))
                     (collect (format nil "~a"
                                          (g:simple-action-name obj))))))))

;;;     g_list_store_append
;;;     g_list_store_remove
;;;     g_list_store_remove_all

(test g-list-store-append/remove
  (let ((store (g:list-store-new "GAction")))
    (is (= 0 (g:list-store-n-items store)))
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    (is (= 1 (g:list-store-n-items store)))
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    (is (= 2 (g:list-store-n-items store)))
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    (is (= 3 (g:list-store-n-items store)))
    (is-false (g:list-store-remove store 1))
    (is (= 2 (g:list-store-n-items store)))
    (is-false (g:list-store-remove-all store))
    (is (= 0 (g:list-store-n-items store)))))

;;;     g_list_store_splice

(test g-list-store-splice.1
  (let ((store (g:list-store-new "GSimpleAction")))
    (dotimes (i 10)
      (is (= i
             (g:list-store-insert-sorted
                 store
                 (make-instance 'g:simple-action
                                :name
                                (format nil "Action ~a" i))
                 (lambda (a b)
                   (if (string> (g:simple-action-name a)
                                (g:simple-action-name b))
                       1
                       -1))))))
    (g:list-store-splice store 5 2 nil)
    (is (equal '("Action 0" "Action 1" "Action 2" "Action 3" "Action 4"
                 "Action 7" "Action 8" "Action 9")
               (iter (for i from 0 below (g:list-store-n-items store))
                     (for obj = (g:list-model-object store i))
                     (collect (g:simple-action-name obj)))))))

(test g-list-store-splice.2
  (let ((store (g:list-store-new "GSimpleAction")))
    (dotimes (i 10)
      (is (= i
             (g:list-store-insert-sorted
                 store
                 (make-instance 'g:simple-action
                                :name
                                (format nil "Action ~a" i))
                 (lambda (a b)
                   (if (string> (g:simple-action-name a)
                                (g:simple-action-name b))
                       1
                       -1))))))
    (g:list-store-splice store 5 2
                         (list (make-instance 'g:simple-action
                                              :name "action")
                               (make-instance 'g:simple-action
                                              :name "action")
                               (make-instance 'g:simple-action
                                              :name "action")))
    (is (equal '("Action 0" "Action 1" "Action 2" "Action 3" "Action 4"
                 "action" "action" "action" "Action 7" "Action 8" "Action 9")
               (iter (for i from 0 below (g:list-store-n-items store))
                     (for obj = (g:list-model-object store i))
                     (collect (g:simple-action-name obj)))))))

;;;     g_list_store_sort

(test g-list-store-sort
  (let ((store (g:list-store-new "GSimpleAction")))
    (dotimes (i 10)
      (is (= 0
             (g:list-store-insert-sorted
                 store
                 (make-instance 'g:simple-action
                                :name
                                (format nil "Action ~a" i))
                 (lambda (a b)
                   (if (string< (g:simple-action-name a)
                                (g:simple-action-name b))
                       1
                       -1))))))
    (is (equal '("Action 9" "Action 8" "Action 7" "Action 6" "Action 5"
                 "Action 4" "Action 3" "Action 2" "Action 1" "Action 0")
               (iter (for i from 0 below 10)
                     (for obj = (g:list-model-object store i))
                     (collect (format nil "~a"
                                          (g:simple-action-name obj))))))
    (g:list-store-sort store
                       (lambda (a b)
                         (if (string> (g:simple-action-name a)
                                      (g:simple-action-name b))
                             1
                             -1)))
    (is (equal '("Action 0" "Action 1" "Action 2" "Action 3" "Action 4"
                 "Action 5" "Action 6" "Action 7" "Action 8" "Action 9")
               (iter (for i from 0 below 10)
                     (for obj = (g:list-model-object store i))
                     (collect (format nil "~a"
                                          (g:simple-action-name obj))))))))

;;;     g_list_store_find

(test g-list-store-find
  (let ((store (g:list-store-new "GObject"))
        (action (make-instance 'g:simple-action)))
    ;; Fill the list store with objects
    (is-false (g:list-store-append store (make-instance 'g:menu-item)))
    (is-false (g:list-store-append store (make-instance 'g:menu-item)))
    (is-false (g:list-store-append store action))
    (is-false (g:list-store-append store (make-instance 'g:menu-item)))
    (is (= 4 (g:list-store-n-items store)))
    ;; Find the action in the list store
    (is (= 2 (g:list-store-find store action)))))

;;;     g_list_store_find_with_equal_func

(test g-list-store-find-with-equal-func
  (let ((store (g:list-store-new "GSimpleAction")))
    (dotimes (i 10)
      (is (= i
             (g:list-store-insert-sorted
                     store
                     (make-instance 'g:simple-action
                                    :name
                                    (format nil "Action ~a" i))
                     (lambda (a b)
                       (if (string> (g:simple-action-name a)
                                    (g:simple-action-name b))
                           1
                           -1))))))
    (is (equal '("Action 0" "Action 1" "Action 2" "Action 3" "Action 4"
                 "Action 5" "Action 6" "Action 7" "Action 8" "Action 9")
               (iter (for i from 0 below 10)
                     (for obj = (g:list-model-object store i))
                     (collect (format nil "~a"
                                          (g:simple-action-name obj))))))
    (is (= 5
           (g:list-store-find-with-equal-func
                   store
                   (make-instance 'g:simple-action
                                  :name "Action 5")
                   (lambda (a b)
                     (string= (g:simple-action-name a)
                              (g:simple-action-name b))))))
    (is (= 7
           (g:list-store-find-with-equal-func
                   store
                   (make-instance 'g:simple-action
                                  :name "Action 7")
                   (lambda (a b)
                     (string= (g:simple-action-name a)
                              (g:simple-action-name b))))))))

;;; --- 2023-9-4 ---------------------------------------------------------------

