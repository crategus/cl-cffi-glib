(in-package :glib-test)

(def-suite gio-menu-model :in gio-suite)
(in-suite gio-menu-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GMenuModel

(test g-menu-model-class
  ;; Check type
  (is (g:type-is-object "GMenuModel"))
  ;; Check registered symbol
  (is (eq 'gio:menu-model
          (glib:symbol-for-gtype "GMenuModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GMenuModel")
          (g:gtype (cffi:foreign-funcall "g_menu_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GMenuModel")))
  ;; Check children
  (is (equal '("GMenu")
             (glib-test:list-children "GMenuModel")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GMenuModel")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GMenuModel")))
  ;; Check signals
  (is (equal '("items-changed")
             (glib-test:list-signals "GMenuModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GMenuModel" GIO:MENU-MODEL
                      (:SUPERCLASS GOBJECT:OBJECT :EXPORT T :INTERFACES NIL
                       :TYPE-INITIALIZER "g_menu_model_get_type")
                      NIL)
             (gobject:get-gtype-definition "GMenuModel"))))

;;;     GMenuAttributeIter

(test g-menu-attribute-iter-class
  ;; Check type
  (is (g:type-is-object "GMenuAttributeIter"))
  ;; Check registered symbol
  (is (eq 'gio:menu-attribute-iter
          (glib:symbol-for-gtype "GMenuAttributeIter")))
  ;; Check type initializer
  (is (eq (g:gtype "GMenuAttributeIter")
          (g:gtype (cffi:foreign-funcall "g_menu_attribute_iter_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GMenuAttributeIter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GMenuAttributeIter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GMenuAttributeIter")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GMenuAttributeIter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GMenuAttributeIter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GMenuAttributeIter" GIO:MENU-ATTRIBUTE-ITER
                      (:SUPERCLASS GOBJECT:OBJECT :EXPORT T :INTERFACES NIL
                       :TYPE-INITIALIZER "g_menu_attribute_iter_get_type")
                      NIL)
             (gobject:get-gtype-definition "GMenuAttributeIter"))))

;;;     GMenuLinkIter

(test g-menu-link-iter-class
  ;; Check type
  (is (g:type-is-object "GMenuLinkIter"))
  ;; Check registered symbol
  (is (eq 'gio:menu-link-iter
          (glib:symbol-for-gtype "GMenuLinkIter")))
  ;; Check type initializer
  (is (eq (g:gtype "GMenuLinkIter")
          (g:gtype (cffi:foreign-funcall "g_menu_link_iter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GMenuLinkIter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GMenuLinkIter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GMenuLinkIter")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GMenuLinkIter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GMenuLinkIter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GMenuLinkIter" GIO:MENU-LINK-ITER
                      (:SUPERCLASS GOBJECT:OBJECT :EXPORT T :INTERFACES NIL
                       :TYPE-INITIALIZER "g_menu_link_iter_get_type")
                      NIL)
             (gobject:get-gtype-definition "GMenuLinkIter"))))

;;; --- Signals ----------------------------------------------------------------

;;;     items-changed

(test g-menu-model-items-changed-signal
  (let* ((name "items-changed")
         (gtype (g:gtype "GMenuModel"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint" "gint" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Function ---------------------------------------------------------------

;;;     g_menu_model_is_mutable
;;;     g_menu_model_get_n_items
;;;     g_menu_model_get_item_attribute_value
;;;     g_menu_model_get_item_attribute
;;;     g_menu_model_get_item_link
;;;     g_menu_model_iterate_item_attributes
;;;     g_menu_model_iterate_item_links
;;;     g_menu_model_items_changed

;;;     g_menu_attribute_iter_get_next
;;;     g_menu_attribute_iter_get_name
;;;     g_menu_attribute_iter_get_value
;;;     g_menu_attribute_iter_next

;;;     g_menu_link_iter_get_name
;;;     g_menu_link_iter_get_next
;;;     g_menu_link_iter_get_value
;;;     g_menu_link_iter_next

;;; 2024-12-29
