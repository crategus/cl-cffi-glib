(in-package :glib-test)

(def-suite gio-list-model :in gio-suite)
(in-suite gio-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GListModel

(test list-model-interface
  ;; Type check
  (is (g:type-is-interface "GListModel"))
  ;; Check the registered symbol
  (is (eq 'g:list-model
          (gobject:symbol-for-gtype "GListModel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GListModel")
          (g:gtype (cffi:foreign-funcall "g_list_model_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'g:param-spec-name
                     (g:object-interface-list-properties "GListModel"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GListModel"
                                  G-LIST-MODEL
                                  (:EXPORT T))
             (get-g-type-definition "GListModel"))))

;;; --- Signals ----------------------------------------------------------------

;;;     void    items-changed

;;; --- Functions --------------------------------------------------------------

;;;     g_list_model_get_item_type
;;;     g_list_model_get_n_items
;;;     g_list_model_get_item
;;;     g_list_model_get_object

(test list-model-get.1
  (let ((store (g:list-store-new "GObject")))
    ;; Append some objects
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    (is-false (g:list-store-append store (make-instance 'g:menu-item)))
    ;; Use the interace functions
    (is (eq (g:gtype "GObject") (g:list-model-item-type store)))
    (is (= 2 (g:list-model-n-items store)))
    (is (cffi:pointerp (g:list-model-item store 0)))
    (is (typep (g:list-model-object store 0) 'g:simple-action))
    (is (typep (g:list-model-object store 1) 'g:menu-item))))

(test list-model-get.2
  (let ((store (g:list-store-new "GAction")))
    ;; Append some objects
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    (is-false (g:list-store-append store (make-instance 'g:simple-action)))
    ;; Use the interace functions
    (is (eq (g:gtype "GAction") (g:list-model-item-type store)))
    (is (= 2 (g:list-model-n-items store)))
    (is (cffi:pointerp (g:list-model-item store 0)))
    (is (typep (g:list-model-object store 0) 'g:simple-action))
    (is (typep (g:list-model-object store 1) 'g:simple-action))))

;;;     g_list_model_items_changed

;;; --- 2023-1-2 ---------------------------------------------------------------
