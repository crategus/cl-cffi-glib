(in-package :glib-test)

(def-suite gio-property-action :in gio-suite)
(in-suite gio-property-action)

;;; --- Types and Values -------------------------------------------------------

;;;     GPropertyAction

(test g-property-action-class
  ;; Check type
  (is (g:type-is-object "GPropertyAction"))
  ;; Check registered symbol
  (is (eq 'g:property-action
          (glib:symbol-for-gtype "GPropertyAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GPropertyAction")
          (g:gtype (cffi:foreign-funcall "g_property_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GPropertyAction")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GPropertyAction")))
  ;; Check interfaces
  (is (equal '("GAction")
             (glib-test:list-interfaces "GPropertyAction")))
  ;; Check class properties
  (is (equal '("enabled" "invert-boolean" "name" "object" "parameter-type"
               "property-name" "state" "state-type")
             (glib-test:list-properties "GPropertyAction")))
  (is (equal '()
             (glib-test:list-signals "GPropertyAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GPropertyAction" GIO:PROPERTY-ACTION
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GAction")
                        :TYPE-INITIALIZER "g_property_action_get_type")
                       ((ENABLED PROPERTY-ACTION-ENABLED
                         "enabled" "gboolean" T NIL)
                        (INVERT-BOOLEAN PROPERTY-ACTION-INVERT-BOOLEAN
                         "invert-boolean" "gboolean" T NIL)
                        (NAME PROPERTY-ACTION-NAME "name" "gchararray" T NIL)
                        (OBJECT PROPERTY-ACTION-OBJECT
                         "object" "GObject" NIL NIL)
                        (PARAMETER-TYPE PROPERTY-ACTION-PARAMETER-TYPE
                         "parameter-type" "GVariantType" T NIL)
                        (PROPERTY-NAME PROPERTY-ACTION-PROPERTY-NAME
                         "property-name" "gchararray" NIL NIL)
                        (STATE PROPERTY-ACTION-STATE "state" "GVariant" T NIL)
                        (STATE-TYPE PROPERTY-ACTION-STATE-TYPE
                         "state-type" "GVariantType" T NIL)))
             (gobject:get-gtype-definition "GPropertyAction"))))

;;; --- Properties -------------------------------------------------------------

;; FIXME: The examples does not work with GThemedIcon. The properties must be
;; readable and writable.

#+nil
(test property-action-properties.1
  (let* ((icon (make-instance 'g:themed-icon))
         (action (g:property-action-new "action" icon "use-default-fallbacks")))
    ;; enabled
    (is-true (g:property-action-enabled action))
;    (signals (error) (setf (g:property-action-enabled action) nil))
;    ;; invert-boolean
;    (is-false (g:property-action-invert-boolean action))
;    ;; name
;    (is (string= "action" (g:property-action-name action)))
;    ;;object is not readable
;    (signals (error) (g:property-action-object action))
;    ;; parameter-type is double float
;    (is (typep (g:property-action-parameter-type action) 'g:variant-type))
;    (is (string= "i"
;                 (g:variant-type-dup-string
;                     (g:property-action-parameter-type action))))
;    ;; property-name is not readable
;    (signals (error) (g:property-action-property-name action))
;    ;; state
;    (is (not (cffi:null-pointer-p (g:property-action-state action))))
;    ;; FIXME: We cannot get the value of the variant!?
;;    (is-false (g:variant-type (g:property-action-state action)))
;;    (is-false (g:variant-int16 (g:property-action-state action)))
;    ;; state-type is a double float
;    (is (typep (g:property-action-state-type action) 'g:variant-type))
;    (is (string= "i"
;                 (g:variant-type-dup-string
;                     (g:property-action-state-type action))))
                     ))

#+nil
(test property-action-properties.2
  (let* ((icon (make-instance 'g:themed-icon
                               :name "text"))
         (action (g:property-action-new "action" icon "name")))
    ;; enabled
;    (is-true (g:property-action-enabled action))
;    (signals (error) (setf (g:property-action-enabled action) nil))
;    ;; invert-boolean
;    (is-false (g:property-action-invert-boolean action))
;    ;; name
;    (is (string= "action" (g:property-action-name action)))
;    ;;object is not readable
;    (signals (error) (g:property-action-object action))
;    ;; parameter-type is double float
;    (is (typep (g:property-action-parameter-type action) 'g:variant-type))
;    (is (string= "s"
;                 (g:variant-type-dup-string
;                     (g:property-action-parameter-type action))))
;    ;; property-name is not readable
;    (signals (error) (g:property-action-property-name action))
;    ;; state
;    (is (not (cffi:null-pointer-p (g:property-action-state action))))
;    ;; FIXME: We cannot get the value of the variant!?
;;    (is-false (g:variant-string (g:property-action-state action)))
;    ;; state-type is a double float
;    (is (typep (g:property-action-state-type action) 'g:variant-type))
;    (is (string= "s"
;                 (g:variant-type-dup-string
;                     (g:property-action-state-type action))))
                     ))

;;; --- Functions --------------------------------------------------------------

;;;     g_property_action_new

#++nil
(test property-action-new
  (let ((icon (make-instance 'g:themed-icon)))
    (is (typep (g:property-action-new "action" icon "name")
                'g:property-action))))

;;; 2024-9-17
