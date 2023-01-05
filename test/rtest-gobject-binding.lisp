(in-package :glib-test)

(def-suite gobject-binding :in gobject-suite)
(in-suite gobject-binding)

;;; --- Types and Values -------------------------------------------------------

;;;     GBindingFlags

(test binding-flags
  ;; Check the type
  (is (g:type-is-flags "GBindingFlags"))
  ;; Check the registered symbol
  (is (eq 'g:binding-flags
          (gobject:symbol-for-gtype "GBindingFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GBindingFlags")
          (g:gtype (cffi:foreign-funcall "g_binding_flags_get_type" :size))))
  ;; Check the names
  (is (equal '("G_BINDING_DEFAULT" "G_BINDING_BIDIRECTIONAL"
               "G_BINDING_SYNC_CREATE" "G_BINDING_INVERT_BOOLEAN")
             (list-flags-item-name "GBindingFlags")))
  ;; Check the values
  (is (equal '(0 1 2 4)
             (list-flags-item-value "GBindingFlags")))
  ;; Check the nick names
  (is (equal '("default" "bidirectional" "sync-create" "invert-boolean")
             (list-flags-item-nick "GBindingFlags")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GBindingFlags"
                              G-BINDING-FLAGS
                              (:EXPORT T)
                              (:DEFAULT 0)
                              (:BIDIRECTIONAL 1)
                              (:SYNC-CREATE 2)
                              (:INVERT-BOOLEAN 4))
             (gobject:get-g-type-definition "GBindingFlags"))))

;;;     GBinding

(test binding-class
  ;; Type check
  (is (g:type-is-object "GBinding"))
  ;; Check the registered symbol
  (is (eq 'g:binding
          (gobject:symbol-for-gtype "GBinding")))
  ;; Check the type initializer
  (is (eq (g:gtype "GBinding")
          (g:gtype (cffi:foreign-funcall "g_binding_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject") (g:type-parent "GBinding")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g:type-name (g:type-children "GBinding"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g:type-name (g:type-interfaces "GBinding"))))
  ;; Check the class properties
  (is (equal '("flags" "source" "source-property" "target" "target-property")
             (stable-sort (mapcar #'g:param-spec-name
                                  (g:object-class-list-properties "GBinding"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GBinding" G-BINDING
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL)
                       ((FLAGS G-BINDING-FLAGS "flags" "GBindingFlags" T NIL)
                        (SOURCE G-BINDING-SOURCE "source" "GObject" T NIL)
                        (SOURCE-PROPERTY G-BINDING-SOURCE-PROPERTY
                         "source-property" "gchararray" T NIL)
                        (TARGET G-BINDING-TARGET "target" "GObject" T NIL)
                        (TARGET-PROPERTY G-BINDING-TARGET-PROPERTY
                         "target-property" "gchararray" T NIL)))
             (gobject:get-g-type-definition "GBinding"))))

;;; --- Properties -------------------------------------------------------------

#+nil
(test binding-properties
  (let* ((toggle (make-instance 'gtk:toggle-button))
         (revealer (make-instance 'gtk:revealer))
         (binding (g:object-bind-property toggle "active"
                                          revealer "reveal-child"
                                          :default)))
    (is (equal '() (g:binding-flags binding)))
    (is (eq (g:gtype "GtkToggleButton")
            (g:object-type (g:binding-source binding))))
    (is (string= "active" (g:binding-source-property binding)))
    (is (eq (g:gtype "GtkRevealer") (g:object-type (g:binding-target binding))))
    (is (string= "reveal-child" (g:binding-target-property binding)))))

;;; --- Functions --------------------------------------------------------------

;;;     g-binding-unbind
;;;     g-object-bind-property

#+nil
(test object-bind-property
  (let* ((toggle (make-instance 'gtk:toggle-button))
         (revealer (make-instance 'gtk:revealer))
         (binding (g:object-bind-property toggle "active"
                                          revealer "reveal-child"
                                          :sync-create)))
    (is (eq (g:gtype "GBinding") (g:object-type binding)))
    (is (equal '(:sync-create) (g:binding-flags binding)))
    ;; Default values are false
    (is-false (gtk:toggle-button-active toggle))
    (is-false (gtk:revealer-reveal-child revealer))
    ;; Set toogle active true
    (is-true (setf (gtk:toggle-button-active toggle) t))
    ;; reveal-child is true
    (is-true (gtk:revealer-reveal-child revealer))
    ;; Unbind the binding
    (is-false (g:binding-unbind binding))))

;;;     GBindingTransformFunc
;;;     g_object_bind_property_full
;;;     g_object_bind_property_with_closures

;;; --- 2023-1-2 ---------------------------------------------------------------
