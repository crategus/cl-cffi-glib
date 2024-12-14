(in-package :glib-test)

(def-suite gobject-binding :in gobject-suite)
(in-suite gobject-binding)

(defvar *verbose-gobject-binding* nil)

(defparameter gobject-binding
              '(g:binding-flags
                g:binding-source
                g:binding-source-property
                g:binding-target
                g:binding-target-property
                g:binding-dup-source
                g:binding-dup-target
                g:binding-unbind
                g:object-bind-property
                g:object-bind-property-full))

(export 'gobject-binding)

;;; --- Types and Values -------------------------------------------------------

;;;     GBindingFlags

(test g-binding-flags
  ;; Check type
  (is (g:type-is-flags "GBindingFlags"))
  ;; Check registered symbol
  (is (eq 'g:binding-flags
          (glib:symbol-for-gtype "GBindingFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GBindingFlags")
          (g:gtype (cffi:foreign-funcall "g_binding_flags_get_type" :size))))
  ;; Check names
  (is (equal '("G_BINDING_DEFAULT" "G_BINDING_BIDIRECTIONAL"
               "G_BINDING_SYNC_CREATE" "G_BINDING_INVERT_BOOLEAN")
             (glib-test:list-flags-item-names "GBindingFlags")))
  ;; Check values
  (is (equal '(0 1 2 4)
             (glib-test:list-flags-item-values "GBindingFlags")))
  ;; Check nick names
  (is (equal '("default" "bidirectional" "sync-create" "invert-boolean")
             (glib-test:list-flags-item-nicks "GBindingFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GBindingFlags" GOBJECT:BINDING-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "g_binding_flags_get_type")
                                     (:DEFAULT 0)
                                     (:BIDIRECTIONAL 1)
                                     (:SYNC-CREATE 2)
                                     (:INVERT-BOOLEAN 4))
             (gobject:get-gtype-definition "GBindingFlags"))))

;;;     GBinding

(test g-binding-class
  ;; Check type
  (is (g:type-is-object "GBinding"))
  ;; Check registered symbol
  (is (eq 'g:binding
          (glib:symbol-for-gtype "GBinding")))
  ;; Check type initializer
  (is (eq (g:gtype "GBinding")
          (g:gtype (cffi:foreign-funcall "g_binding_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GBinding")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GBinding")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GBinding")))
  ;; Check class properties
  (is (equal '("flags" "source" "source-property" "target" "target-property")
             (glib-test:list-properties "GBinding")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GBinding")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GBinding" GOBJECT:BINDING
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "g_binding_get_type")
                       ((FLAGS BINDING-FLAGS "flags" "GBindingFlags" T NIL)
                        (SOURCE BINDING-SOURCE "source" "GObject" T NIL)
                        (SOURCE-PROPERTY BINDING-SOURCE-PROPERTY
                         "source-property" "gchararray" T NIL)
                        (TARGET BINDING-TARGET "target" "GObject" T NIL)
                        (TARGET-PROPERTY BINDING-TARGET-PROPERTY
                         "target-property" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GBinding"))))

;;; --- Properties -------------------------------------------------------------

(test g-binding-properties.1
  (let* ((action1 (make-instance 'g:simple-action))
         (action2 (make-instance 'g:simple-action))
         (binding (g:object-bind-property action1 "enabled"
                                          action2 "enabled"
                                          :bidirectional)))
    (is-false (setf (g:simple-action-enabled action1) nil))
    (is (eq (g:simple-action-enabled action1)
            (g:simple-action-enabled action2)))
    (is-true (setf (g:simple-action-enabled action2) t))
    (is (eq (g:simple-action-enabled action1)
            (g:simple-action-enabled action2)))
    (is (equal '(:bidirectional) (g:binding-flags binding)))
    (is (eq (g:gtype "GSimpleAction")
            (g:type-from-instance (g:binding-source binding))))
    (is (string= "enabled" (g:binding-source-property binding)))
    (is (eq (g:gtype "GSimpleAction")
            (g:type-from-instance (g:binding-target binding))))
    (is (string= "enabled" (g:binding-target-property binding)))
    (is-false (g:binding-unbind binding))
    ;; Check memory management
    (is (= 1 (g:object-ref-count action1)))
    (is (= 1 (g:object-ref-count action2)))
    (is (= 1 (g:object-ref-count binding)))))

(test g-binding-properties.2
  (let* ((action1 (make-instance 'g:simple-action))
         (action2 (make-instance 'g:simple-action))
         (binding (g:object-bind-property action1 "enabled"
                                          action2 "enabled"
                                          :invert-boolean)))
    (is-false (setf (g:simple-action-enabled action1) nil))
    (is (eq (g:simple-action-enabled action1)
            (not (g:simple-action-enabled action2))))
    (is-true (setf (g:simple-action-enabled action2) t))
    (is (eq (g:simple-action-enabled action1)
            (not (g:simple-action-enabled action2))))
    (is (equal '(:invert-boolean) (g:binding-flags binding)))
    (is (eq (g:gtype "GSimpleAction")
            (g:type-from-instance (g:binding-source binding))))
    (is (string= "enabled" (g:binding-source-property binding)))
    (is (eq (g:gtype "GSimpleAction")
            (g:type-from-instance (g:binding-target binding))))
    (is (string= "enabled" (g:binding-target-property binding)))
    (is-false (g:binding-unbind binding))
    ;; Check memory management
    (is (= 1 (g:object-ref-count action1)))
    (is (= 1 (g:object-ref-count action2)))
    (is (= 1 (g:object-ref-count binding)))))

;;; --- Functions --------------------------------------------------------------

;;;     g_binding_dup_source
;;;     g_binding_dup_target

(test g-binding-dup-source/target
  (let* ((action1 (make-instance 'g:simple-action))
         (action2 (make-instance 'g:simple-action))
         (binding (g:object-bind-property action1 "enabled"
                                          action2 "enabled"
                                          :default)))
    (is (eq action1 (g:binding-dup-source binding)))
    (is (eq action2 (g:binding-dup-target binding)))
    (is-false (g:binding-unbind binding))
    ;; Check memory management
    (is (= 1 (g:object-ref-count action1)))
    (is (= 1 (g:object-ref-count action2)))
    (is (= 1 (g:object-ref-count binding)))))

;;;     g_binding_unbind
;;;     g_object_bind_property

(test g-object-bind-property
  (let* ((action1 (make-instance 'g:simple-action))
         (action2 (make-instance 'g:simple-action))
         (binding (g:object-bind-property action1 "enabled"
                                          action2 "enabled"
                                          :sync-create)))
    (is (eq (g:gtype "GBinding") (g:type-from-instance binding)))
    (is (equal '(:sync-create) (g:binding-flags binding)))
    ;; Default values are true
    (is-true (g:simple-action-enabled action1))
    (is-true (g:simple-action-enabled action2))
    ;; Set action1 to false
    (is-false (setf (g:simple-action-enabled action1) nil))
    ;; enabled is false for action2
    (is-false (g:simple-action-enabled action2))
    ;; Unbind the binding
    (is-false (g:binding-unbind binding))
    ;; Check memory management
    (is (= 1 (g:object-ref-count action1)))
    (is (= 1 (g:object-ref-count action2)))
    (is (= 1 (g:object-ref-count binding)))))

;;;     GBindingTransformFunc
;;;     g_object_bind_property_full

(test g-object-bind-property-full.1
  (let* ((action1 (make-instance 'g:simple-action))
         (action2 (make-instance 'g:simple-action))
         binding)
    (is (typep (setf binding
                     (g:object-bind-property-full
                             action1 "enabled"
                             action2 "enabled"
                             :default
                             ;; TRANSFORM-TO function
                             (lambda (binding from to)
                               (g:value-set to (g:value-get from) "gboolean")
                               (when *verbose-gobject-binding*
                                 (format t "~%")
                                 (format t " binding: ~a~%" binding)
                                 (format t "    from: ~a~%" (g:value-get from))
                                 (format t "      to: ~a~%" (g:value-get to)))
                               t)
                             ;; TRANSFORM-FROM function
                             nil))
               'g:binding))
    ;; Default values
    (is-true (g:simple-action-enabled action1))
    (is-true (g:simple-action-enabled action2))
    ;; Set action1 NIL
    (is-false (setf (g:simple-action-enabled action1) nil))
    (is-false (g:simple-action-enabled action1))
    (is-false (g:simple-action-enabled action2))
    ;; Set action1 T
    (is-true (setf (g:simple-action-enabled action1) t))
    (is-true (g:simple-action-enabled action1))
    (is-true (g:simple-action-enabled action2))
    (is-false (g:binding-unbind binding))
    ;; Check memory management
    (is (= 1 (g:object-ref-count action1)))
    (is (= 1 (g:object-ref-count action2)))
    (is (= 1 (g:object-ref-count binding)))))

(test g-object-bind-property-full.2
  (let* ((action1 (make-instance 'g:simple-action))
         (action2 (make-instance 'g:simple-action))
         binding)
    (is (typep (setf binding
                     (g:object-bind-property-full action1 "enabled"
                                                  action2 "enabled"
                                                  :default
                                                   nil
                                                   nil))
               'g:binding))
    ;; Default values
    (is-true (g:simple-action-enabled action1))
    (is-true (g:simple-action-enabled action2))
    ;; Set action1 NIL
    (is-false (setf (g:simple-action-enabled action1) nil))
    (is-false (g:simple-action-enabled action1))
    (is-false (g:simple-action-enabled action2))
    ;; Set action1 T
    (is-true (setf (g:simple-action-enabled action1) t))
    (is-true (g:simple-action-enabled action1))
    (is-true (g:simple-action-enabled action2))
    (is-false (g:binding-unbind binding))
    ;; Check memory management
    (is (= 1 (g:object-ref-count action1)))
    (is (= 1 (g:object-ref-count action2)))
    (is (= 1 (g:object-ref-count binding)))))

(test g-object-bind-property-full.3
  (let* ((action1 (make-instance 'g:simple-action))
         (action2 (make-instance 'g:simple-action))
         binding)
    (is (typep (setf binding
                     (g:object-bind-property-full
                             action1 "enabled"
                             action2 "enabled"
                             :bidirectional
                             ;; TRANSFORM-TO function
                             (lambda (binding from to)
                               (g:value-set to (g:value-get from) "gboolean")
                               (when *verbose-gobject-binding*
                                 (format t "~&in TRANSFORM-TO~%")
                                 (format t " binding: ~a~%" binding)
                                 (format t "    from: ~a~%" (g:value-get from))
                                 (format t "      to: ~a~%" (g:value-get to)))
                               t)
                             ;; TRANSFORM-FROM function
                             (lambda (binding from to)
                               (g:value-set to (g:value-get from) "gboolean")
                               (when *verbose-gobject-binding*
                                 (format t "~&in TRANSFORM-FROM~%")
                                 (format t " binding: ~a~%" binding)
                                 (format t "    from: ~a~%" (g:value-get from))
                                 (format t "      to: ~a~%" (g:value-get to)))
                               t)))
               'g:binding))
    ;; Default values
    (is-true (g:simple-action-enabled action1))
    (is-true (g:simple-action-enabled action2))
    ;; Set action2 NIL
    (is-false (setf (g:simple-action-enabled action2) nil))
    (is-false (g:simple-action-enabled action1))
    (is-false (g:simple-action-enabled action2))
    ;; Set action2 T
    (is-true (setf (g:simple-action-enabled action2) t))
    (is-true (g:simple-action-enabled action1))
    (is-true (g:simple-action-enabled action2))
    (is-false (g:binding-unbind binding))
    ;; Check memory management
    (is (= 1 (g:object-ref-count action1)))
    (is (= 1 (g:object-ref-count action2)))
    (is (= 1 (g:object-ref-count binding)))))

;;; 2024-12-12
