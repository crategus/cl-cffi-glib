(in-package :glib-test)

(def-suite gio-list-model :in gio-suite)
(in-suite gio-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GListModel

(test g-list-model-interface
  ;; Check type
  (is (g:type-is-interface "GListModel"))
  ;; Check registered symbol
  (is (eq 'g:list-model
          (glib:symbol-for-gtype "GListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GListModel")
          (g:gtype (cffi:foreign-funcall "g_list_model_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GListModel")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GListModel")))
  ;; Check signals
  (is (equal '("items-changed")
             (glib-test:list-signals "GListModel")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GListModel" GIO:LIST-MODEL
                      (:EXPORT T
                       :TYPE-INITIALIZER "g_list_model_get_type"))
             (gobject:get-gtype-definition "GListModel"))))

;;; --- Signals ----------------------------------------------------------------

;;;     items-changed

(test g-list-model-items-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "items-changed" "GListModel"))))
    (is (string= "items-changed" (g:signal-query-signal-name query)))
    (is (string= "GListModel" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("guint" "guint" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     g_list_model_get_item_type
;;;     g_list_model_get_n_items
;;;     g_list_model_get_item

(test g-list-model-get.1
  (glib-test:with-check-memory (store)
    (setf store (g:list-store-new "GObject"))
    ;; Append some objects
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    (is-false (g:list-store-append store (make-instance 'g:menu-item)))
    ;; Use the interface functions
    (is (eq (g:gtype "GObject") (g:list-model-item-type store)))
    (is (= 2 (g:list-model-n-items store)))
    ;; Access first item
    (is (g:is-object (g:list-model-item store 0)))
    (is (typep (g:list-model-item store 0) 'g:simple-action))
    ;; Access second item
    (is (g:is-object (g:list-model-item store 1)))
    (is (typep (g:list-model-item store 1) 'g:menu-item))
    ;; Access an invalid position
    (is-false  (g:list-model-item store 2))
    ;; Remove references
    (is-false (g:list-store-remove-all store))))

(test g-list-model-get.2
  (glib-test:with-check-memory (store)
    (setf store (g:list-store-new "GAction"))
    ;; Append some objects
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    ;; Use the interace functions
    (is (eq (g:gtype "GAction") (g:list-model-item-type store)))
    (is (= 2 (g:list-model-n-items store)))
    ;; Access an item
    (is (g:is-object (g:list-model-item store 0)))
    (is (typep (g:list-model-item store 0) 'g:simple-action))
    ;; Remove references
    (is-false (g:list-store-remove-all store))))

;;;     g_list_model_get_item

(test g-list-model-item
  (glib-test:with-check-memory (store item)
    (setf store (g:list-store-new "GObject"))
    ;; Append some objects
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    (is-false (g:list-store-append store (make-instance 'g:menu-item)))
    ;; Get an item from the list store
    (is (typep (setf item
                     (g:list-model-item store 0)) 'g:simple-action))
    (is (= 2 (g:object-ref-count item)))
    ;; Get the item a second time from the list store
    (is (typep (setf item
                     (g:list-model-item store 0)) 'g:simple-action))
    (is (= 2 (g:object-ref-count item)))
    ;; No item at position 2
    (is-false (g:list-model-item store 2))
    ;; Remove references
    (is-false (g:list-store-remove-all store))))

;;;     g_list_model_items_changed

;;; --- Subclassing G:LIST-MODEL -----------------------------------------------

;; Simple implementation which uses a Lisp list
(gobject:define-gobject-subclass "CLListStore" cl-list-store
  (:superclass g:object
   :export t
   :interfaces ("GListModel"))
  ((:cl list
        :initform '()
        :accessor cl-list-store-list)))

(defmethod gio:list-model-get-item-type-impl ((store cl-list-store))
  (g:gtype "GAction"))

(defmethod gio:list-model-get-n-items-impl ((store cl-list-store))
  (length (cl-list-store-list store)))

(defmethod gio:list-model-get-item-impl ((store cl-list-store) pos)
  (let ((item (nth pos (cl-list-store-list store))))
    (when item
      ;; We must add a reference to the returned object
      (g:object-ref item))))

(test g-list-model-subclassing
  (glib-test:with-check-memory (model)
    ;; Create model and add two items
    (setf model (make-instance 'cl-list-store))
    (push (make-instance 'g:simple-action) (cl-list-store-list model))
    (push (make-instance 'g:simple-action) (cl-list-store-list model))
    ;; Check functions for the interface
    (is (g:gtype "GAction") (g:list-model-item-type model))
    (is (= 2 (g:list-model-n-items model)))
    (is (typep (g:list-model-item model 0) 'g:simple-action))
    (is (typep (g:list-model-item model 1) 'g:simple-action))
    (is (typep (g:list-model-item model 0) 'g:simple-action))
    (is (typep (g:list-model-item model 1) 'g:simple-action))
    (is-false (g:list-model-item model 2))))

;;; 2025-3-24
