(in-package :glib-test)

(def-suite gio-menu :in gio-suite)
(in-suite gio-menu)

;;; --- Types and Values -------------------------------------------------------

;;;     GMenu

(test g-menu-class
  ;; Check type
  (is (g:type-is-object "GMenu"))
  ;; Check registered symbol
  (is (eq 'g:menu
          (glib:symbol-for-gtype "GMenu")))
  ;; Check type initializer
  (is (eq (g:gtype "GMenu")
          (g:gtype (cffi:foreign-funcall "g_menu_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GMenuModel") (g:type-parent "GMenu")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GMenu")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GMenu")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GMenu")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GMenu")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GMenu" GIO:MENU
                      (:SUPERCLASS GIO:MENU-MODEL
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "g_menu_get_type")
                      NIL)
             (gobject:get-gtype-definition "GMenu"))))

;;;     GMenuItem

(test g-menu-item-class
  ;; Check type
  (is (g:type-is-object "GMenuItem"))
  ;; Check registered symbol
  (is (eq 'g:menu-item
          (glib:symbol-for-gtype "GMenuItem")))
  ;; Check type initializer
  (is (eq (g:gtype "GMenuItem")
          (g:gtype (cffi:foreign-funcall "g_menu_item_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GMenuItem")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GMenuItem")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GMenuItem")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GMenuItem")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GMenuItem" GIO:MENU-ITEM
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "g_menu_item_get_type")
                      NIL)
             (gobject:get-gtype-definition "GMenuItem"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_menu_new

(test g-menu-new
  (glib-test:with-check-memory ()
    (is (typep (g:menu-new) 'g:menu))))

;;;     g_menu_freeze

(test g-menu-freeze
  (glib-test:with-check-memory (menu)
    (setf menu (g:menu-new))
    (is-true (g:menu-model-is-mutable menu))
    (is-false (g:menu-freeze menu))
    ;; A frozen menu is not mutable
    (is-false (g:menu-model-is-mutable menu))))

;;;     g_menu_insert
;;;     g_menu_prepend
;;;     g_menu_append

(test g-menu-insert
  (glib-test:with-check-memory (menu)
    (setf menu (g:menu-new))
    (is-false (g:menu-insert menu 0 "insert" "action"))
    (is (= 1 (g:menu-model-n-items menu)))
    (is-false (g:menu-prepend menu "prepend" "action"))
    (is (= 2 (g:menu-model-n-items menu)))
    (is-false (g:menu-append menu "append" "action"))
    (is (= 3 (g:menu-model-n-items menu)))))

;;;     g_menu_insert_item
;;;     g_menu_append_item
;;;     g_menu_prepend_item

(test g-menu-insert-item
  (glib-test:with-check-memory (menu item1 item2 item3)
    (setf menu (g:menu-new))
    (is-false (g:menu-insert-item menu 0
                                       (setf item1
                                             (g:menu-item-new "insert" nil))))
    (is (= 1 (g:menu-model-n-items menu)))
    (is-false (g:menu-prepend-item menu
                                   (setf item2
                                         (g:menu-item-new "prepend" nil))))
    (is (= 2 (g:menu-model-n-items menu)))
    (is-false (g:menu-append-item menu
                                  (setf item3
                                        (g:menu-item-new "append" nil))))
    (is (= 3 (g:menu-model-n-items menu)))
    ;; Remove references
    (is-false (g:menu-remove-all menu))
    (is (= 0 (g:menu-model-n-items menu)))))

;;;     g_menu_insert_section
;;;     g_menu_prepend_section
;;;     g_menu_append_section

(test g-menu-insert-section
  (glib-test:with-check-memory (menu section)
    (setf menu (g:menu-new))
    (setf section (g:menu-new))
    ;; Add threee items to the section
    (is-false (g:menu-append section "item1" nil))
    (is-false (g:menu-append section "item2" nil))
    (is-false (g:menu-append section "item3" nil))
    (is (= 3 (g:menu-model-n-items section)))
    ;; Insert section into the menu
    (is-false (g:menu-insert-section menu 0 "section1" section))
    (is (= 1 (g:menu-model-n-items menu)))
    ;; Prepend section to the menu
    (is-false (g:menu-prepend-section menu "section2" section))
    (is (= 2 (g:menu-model-n-items menu)))
    ;; Append section to the menu
    (is-false (g:menu-append-section menu "section3" section))
    (is (= 3 (g:menu-model-n-items menu)))
    ;; Remove references
    (is-false (g:menu-remove-all menu))))

;;;     g_menu_append_submenu
;;;     g_menu_insert_submenu
;;;     g_menu_prepend_submenu

(test g-menu-insert-submenu
  (glib-test:with-check-memory (menu submenu)
    (setf menu (g:menu-new))
    (setf submenu (g:menu-new))
    ;; Add threee items to the submenu
    (is-false (g:menu-append submenu "item1" nil))
    (is-false (g:menu-append submenu "item2" nil))
    (is-false (g:menu-append submenu "item3" nil))
    (is (= 3 (g:menu-model-n-items submenu)))
    ;; Insert submenu into the menu
    (is-false (g:menu-insert-submenu menu 0 "submenu1" submenu))
    (is (= 1 (g:menu-model-n-items menu)))
    ;; Prepend submenu to the menu
    (is-false (g:menu-prepend-submenu menu "submenu2" submenu))
    (is (= 2 (g:menu-model-n-items menu)))
    ;; Append submenu to the menu
    (is-false (g:menu-append-submenu menu "submenu3" submenu))
    (is (= 3 (g:menu-model-n-items menu)))
    ;; Remove references
    (is-false (g:menu-remove-all submenu))
    (is-false (g:menu-remove-all menu))))

;;;     g_menu_remove
;;;     g_menu_remove_all

(test g-menu-remove
  (glib-test:with-check-memory (menu)
    (setf menu (g:menu-new))
    (is-false (g:menu-append menu "append1" nil))
    (is-false (g:menu-append menu "append2" nil))
    (is-false (g:menu-append menu "append3" nil))
    (is (= 3 (g:menu-model-n-items menu)))
    (is-false (g:menu-remove menu 1))
    (is (= 2 (g:menu-model-n-items menu)))
    (is-false (g:menu-remove-all menu))
    (is (= 0 (g:menu-model-n-items menu)))))

;;;     g_menu_item_new

(test g-menu-item-new
  (glib-test:with-check-memory ()
    (is (typep (g:menu-item-new nil nil) 'g:menu-item))))

;;;     g_menu_item_new_section

(test g-menu-item-new-section
  (glib-test:with-check-memory (model section)
    (is (typep (setf model (g:menu-new)) 'g:menu-model))
    (is (typep (setf section
                     (g:menu-item-new-section "section" model))
        'g:menu-item))
    (is-false (g:menu-item-set-section section nil))))

;;;     g_menu_item_new_submenu

(test g-menu-item-new-submenu
  (glib-test:with-check-memory (model submenu)
    (is (typep (setf model (g:menu-new)) 'g:menu-model))
    (is (typep (setf submenu
                     (g:menu-item-new-submenu "submenu" model))
               'g:menu-item))
    (is-false (g:menu-item-set-submenu submenu nil))))

;;;     g_menu_item_new_from_model

(test g-menu-item-new-from-model
  (glib-test:with-check-memory (item model)
    (is (typep (setf model (g:menu-new)) 'g:menu-model))
    (is-false (g:menu-append-item model
                                  (setf item
                                        (g:menu-item-new "append" nil))))
    (is (typep (setf item
                     (g:menu-item-new-from-model model 0)) 'g:menu-item))))

;;;     g_menu_item_set_label

(test g-menu-item-set-label
  (glib-test:with-check-memory (item)
    (is (typep (setf item (g:menu-item-new nil nil)) 'g:menu-item))
    (is-false (g:menu-item-set-label item "label"))
    (is-false (g:menu-item-set-label item nil))))

;;;     g_menu_item_set_icon

(test g-menu-item-set-icon
  (glib-test:with-check-memory (item icon)
    (is (typep (setf icon (g:themed-icon-new "gnome-dev-cdrom")) 'g:icon))
    (is (typep (setf item (g:menu-item-new nil nil)) 'g:menu-item))
    (is-false (g:menu-item-set-icon item icon))
    (is-false (g:menu-item-set-icon item nil))))

;;;     g_menu_item_set_action_and_target_value

(test g-menu-item-set-action-and-target-value
  (glib-test:with-check-memory (item)
    (let ((value (g:variant-new-int32 123)))
      (is (typep (setf item (g:menu-item-new nil nil)) 'g:menu-item))
      (is-false (g:menu-item-set-action-and-target-value item "action" value))
      (is-false (g:menu-item-set-action-and-target-value item "action" nil))
      (is-false (g:menu-item-set-action-and-target-value item nil nil)))))

;;;     g_menu_item_set_detailed_action

(test g-menu-item-set-detailed-action
  (glib-test:with-check-memory (item)
    (is (typep (setf item (g:menu-item-new nil nil)) 'g:menu-item))
    (is-false (g:menu-item-set-detailed-action item "action"))
    (is-false (g:menu-item-set-detailed-action item "app.action"))
    (is-false (g:menu-item-set-detailed-action item "app.action(123)"))
    (is-false (g:menu-item-set-detailed-action item "app.action::123"))))

;;;     g_menu_item_set_section

(test g-menu-item-set-section
  (glib-test:with-check-memory (item model)
    (is (typep (setf model (g:menu-new)) 'g:menu))
    (is (typep (setf item (g:menu-item-new nil nil)) 'g:menu-item))
    (is-false (g:menu-item-set-section item model))
    (is-false (g:menu-item-set-section item nil))))

;;;     g_menu_item_set_submenu

(test g-menu-item-set-submenu
  (glib-test:with-check-memory (item model)
    (is (typep (setf model (g:menu-new)) 'g:menu))
    (is (typep (setf item (g:menu-item-new nil nil)) 'g:menu-item))
    (is-false (g:menu-item-set-submenu item model))
    (is-false (g:menu-item-set-submenu item nil))))

;;;     g_menu_item_get_attribute_value
;;;     g_menu_item_set_attribute_value

(test g-menu-item-attribute-value.1
  (glib-test:with-check-memory (item)
    (setf item (g:menu-item-new "Label" "Action"))
    (is (string= "Label"
                 (g:variant-string
                     (g:menu-item-attribute-value item
                                                  "label"
                                                  (g:variant-type-new "s")))))
    (is (string= "Action"
                 (g:variant-string
                     (g:menu-item-attribute-value item
                                                  "action"
                                                  (g:variant-type-new "s")))))))

(test g-menu-item-attribute-value.2
  (glib-test:with-check-memory (item)
    (setf item (g:menu-item-new "Label" "Action"))
    (is (string= "Label"
                 (g:variant-string
                     (g:menu-item-attribute-value item
                                                  "label"
                                                  (g:variant-type-new "s")))))

    (is (string= "newlabel"
                 (g:variant-string
                     (setf (g:menu-item-attribute-value item "label")
                           (g:variant-new-string "newlabel")))))
    (is (string= "newlabel"
                 (g:variant-string
                     (g:menu-item-attribute-value item
                                                  "label"
                                                  (g:variant-type-new "s")))))

    (is (string= "Action"
                 (g:variant-string
                     (g:menu-item-attribute-value item
                                                  "action"
                                                  (g:variant-type-new "s")))))
    (is (string= "newaction"
                 (g:variant-string
                     (setf (g:menu-item-attribute-value item "action")
                           (g:variant-new-string "newaction")))))
    (is (string= "newaction"
                 (g:variant-string
                     (g:menu-item-attribute-value item
                                                  "action"
                                                  (g:variant-type-new "s")))))))

(test g-menu-item-attribute-value.3
  (glib-test:with-check-memory (item)
    (setf item (g:menu-item-new "Label" "Action"))
    (is (string= "Label"
                 (g:variant-string
                     (g:menu-item-attribute-value item "label"))))
    (is (string= "newlabel"
                 (g:variant-string
                     (setf (g:menu-item-attribute-value item "label")
                           (g:variant-new-string "newlabel")))))
    (is (string= "newlabel"
                 (g:variant-string
                     (g:menu-item-attribute-value item "label"))))

    (is (string= "Action"
                 (g:variant-string
                     (g:menu-item-attribute-value item "action"))))
    (is (string= "newaction"
                 (g:variant-string
                     (setf (g:menu-item-attribute-value item "action")
                           (g:variant-new-string "newaction")))))
    (is (string= "newaction"
                 (g:variant-string
                     (g:menu-item-attribute-value item "action"))))))

(test g-menu-item-attribute-value.4
  (glib-test:with-check-memory (item)
    (setf item (g:menu-item-new "Label" "Action"))
    (is (string= "Label"
                 (g:variant-string
                     (g:menu-item-attribute-value item "label" "s"))))
    (is (string= "newlabel"
                 (g:variant-string
                     (setf (g:menu-item-attribute-value item "label")
                           (g:variant-new-string "newlabel")))))
    (is (string= "newlabel"
                 (g:variant-string
                     (g:menu-item-attribute-value item "label" "s"))))

    (is (string= "Action"
                 (g:variant-string
                     (g:menu-item-attribute-value item "action" "s"))))
    (is (string= "newaction"
                 (g:variant-string
                     (setf (g:menu-item-attribute-value item "action")
                           (g:variant-new-string "newaction")))))
    (is (string= "newaction"
                 (g:variant-string
                     (g:menu-item-attribute-value item "action" "s"))))))

;;;     g_menu_item_get_link
;;;     g_menu_item_set_link

(test g-menu-item-link
  (glib-test:with-check-memory (item submenu)
    (setf item (g:menu-item-new "Label" "Action"))
    (setf submenu (g:menu-new))
    (is (typep (setf (g:menu-item-link item "submenu") submenu) 'g:menu-model))
    (is (typep (g:menu-item-link item "submenu") 'g:menu-model))
    ;; Remove references
    (is-false (setf (g:menu-item-link item "submenu") nil))
    (is-false (g:menu-item-link item "submenu"))
    (is-false (g:menu-remove-all submenu))))

;;; 2024-12-30
