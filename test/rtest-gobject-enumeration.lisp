(in-package :glib-test)

(def-suite gobject-enumeration :in gobject-suite)
(in-suite gobject-enumeration)

;;; --- Types and Values -------------------------------------------------------

(test define-genum-macro
  (is (equal '(PROGN
 (CFFI:DEFCENUM (GTK-RESPONSE-TYPE :INT :ALLOW-UNDECLARED-VALUES NIL)
   (:NONE -1)
   (:REJECT -2)
   (:ACCEPT -3)
   (:DELETE-EVENT -4)
   (:OK -5)
   (:CANCEL -6)
   (:CLOSE -7)
   (:YES -8)
   (:NO -9)
   (:APPLY -10)
   (:HELP -11))
 (EXPORT 'GTK-RESPONSE-TYPE (FIND-PACKAGE "GLIB-TEST"))
 (GLIB-INIT:AT-INIT NIL
   (IF (CFFI:FOREIGN-SYMBOL-POINTER "gtk_response_type_get_type")
       (CFFI:FOREIGN-FUNCALL-POINTER
        (CFFI:FOREIGN-SYMBOL-POINTER "gtk_response_type_get_type") NIL :SIZE)
       (WARN "Type initializer '~A' is not available"
             "gtk_response_type_get_type")))
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
   (SETF (GLIB:SYMBOL-FOR-GTYPE "GtkResponseType") 'GTK-RESPONSE-TYPE)))
              (macroexpand '(gobject:define-genum "GtkResponseType"
                                                  gtk-response-type
                             (:export t
                              :type-initializer "gtk_response_type_get_type")
                             (:none -1)
                             (:reject -2)
                             (:accept -3)
                             (:delete-event -4)
                             (:ok -5)
                             (:cancel -6)
                             (:close -7)
                             (:yes -8)
                             (:no -9)
                             (:apply -10)
                             (:help -11))))))

(test define-gflags-macro
  (is (equal '(PROGN
 (CFFI:DEFBITFIELD GDK-DRAG-ACTION
   :INT
   (:DEFAULT 1)
   (:COPY 2)
   (:MOVE 4)
   (:LINK 8)
   (:PRIVATE 16)
   (:ASK 32))
 (EXPORT 'GDK-DRAG-ACTION (FIND-PACKAGE "GLIB-TEST"))
 (GLIB-INIT:AT-INIT NIL
   (IF (CFFI:FOREIGN-SYMBOL-POINTER "gdk_drag_action_get_type")
       (CFFI:FOREIGN-FUNCALL-POINTER
        (CFFI:FOREIGN-SYMBOL-POINTER "gdk_drag_action_get_type") NIL :SIZE)
       (WARN "Type initializer '~A' is not available"
             "gdk_drag_action_get_type")))
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
   (SETF (GLIB:SYMBOL-FOR-GTYPE "GdkDragAction") 'GDK-DRAG-ACTION)))
             (macroexpand '(gobject:define-gflags "GdkDragAction"
                                                  gdk-drag-action
                            (:export t
                             :type-initializer "gdk_drag_action_get_type")
                            (:default 1)
                            (:copy 2)
                            (:move 4)
                            (:link 8)
                            (:private 16)
                            (:ask 32))))))

;;;   g-enum-class

(test g-enum-class
  (is (= 32 (cffi:foreign-type-size '(:struct g:enum-class))))
  (is (equal '(:maximum :minimum :n-values :type-class :values)
             (sort (cffi:foreign-slot-names '(:struct g:enum-class))
                   #'string-lessp))))

;;;   g-enum-value

(test g-enum-value
  (is (= 24 (cffi:foreign-type-size '(:struct g:enum-value))))
  (is (equal '(:name :nick :value)
             (sort (cffi:foreign-slot-names '(:struct g:enum-value))
                   #'string-lessp))))

;;;   g-flags-class

(test g-flags-class
  (is (= 24 (cffi:foreign-type-size '(:struct g:flags-class))))
  (is (equal '(:mask :n-values :type-class :values)
             (sort (cffi:foreign-slot-names '(:struct g:flags-class))
                   #'string-lessp))))

;;;   g-flags-value

(test g-flags-value
  (is (= 24 (cffi:foreign-type-size '(:struct g:flags-value))))
  (is (equal '(:name :nick :value)
             (sort (cffi:foreign-slot-names '(:struct g:flags-value))
                   #'string-lessp))))

;;; --- Functions --------------------------------------------------------------

;;;     G_ENUM_CLASS_TYPE
;;;     G_ENUM_CLASS_TYPE_NAME

;;;     g-type-is-enum

(test g-type-is-enum
  (is-false (g:type-is-enum "GApplicationFlags"))
  (is-true  (g:type-is-enum "GEmblemOrigin")))

;;; ----------------------------------------------------------------------------

(test get-gvalue-enum.1
  (gobject:with-value (gvalue "GEmblemOrigin" 0)
    (is (eq :unknown (gobject::get-gvalue-enum gvalue)))
    (is (= 1 (gobject::set-gvalue-enum gvalue :device)))
    (is (eq :device (gobject::get-gvalue-enum gvalue)))))

(test get-gvalue-enum.2
  (gobject:with-value (gvalue "GEmblemOrigin" 0)
    (is (eq :unknown (g:value-get gvalue)))
    (is (= 1 (gobject::set-gvalue gvalue :device "GEmblemOrigin")))
    (is (eq :device (g:value-get gvalue)))))

(test get-gvalue-enum.3
  (gobject:with-value (gvalue "GEmblemOrigin" 0)
    (is (eq :unknown (g:value-get gvalue)))
    (is (= 1 (g:value-set gvalue :device "GEmblemOrigin")))
    (is (eq :device (g:value-get gvalue)))))

(test get-gvalue-enum.4
  (gobject:with-value (gvalue "GEmblemOrigin" 0)
    (is (eq :unknown (g:value-get gvalue)))
    (is (= 1 (g:value-set gvalue 1 "GEmblemOrigin")))
    (is (eq :device (g:value-get gvalue)))
    (is (= 2 (g:value-set gvalue 2 "GEmblemOrigin")))
    (is (eq :LIVEMETADATA (g:value-get gvalue)))))

(test get-gvalue-enum.5
  (gobject:with-value (gvalue "GEmblemOrigin" 0)
    (is (= 0 (gobject::%value-enum gvalue)))
    (is (= 1 (setf (gobject::%value-enum gvalue) 1)))
    (is (= 1 (gobject::%value-enum gvalue)))))

(test get-gvalue-enum.6
  (gobject:with-value (gvalue "GEmblemOrigin" 0)
    (is (eq :unknown (g:value-enum gvalue)))
    (is (= 1 (setf (g:value-enum gvalue) 1)))
    (is (eq :device (g:value-enum gvalue)))))

;;;     G_ENUM_CLASS
;;;     G_IS_ENUM_CLASS

;;;     G_TYPE_IS_FLAGS

(test g-type-is-flags
  (is-true  (g:type-is-flags "GApplicationFlags"))
  (is-false (g:type-is-flags "GEmblemOrigin")))

;;; ----------------------------------------------------------------------------

(test get-gvalue-flags.1
  (gobject:with-value (gvalue "GApplicationFlags" 0)
    (is (equal '() (gobject::get-gvalue-flags gvalue)))
    (is (= 1 (gobject::set-gvalue-flags gvalue :is-service)))
    (is (equal '(:is-service) (gobject::get-gvalue-flags gvalue)))))

(test get-gvalue-flags.2
  (gobject:with-value (gvalue "GApplicationFlags" 0)
    (is (equal '() (g:value-get gvalue)))
    (is (= 1 (gobject::set-gvalue gvalue :is-service "GApplicationFlags")))
    (is (equal '(:is-service) (g:value-get gvalue)))))

(test get-gvalue-flags.3
  (gobject:with-value (gvalue "GApplicationFlags" 0)
    (is (equal '() (g:value-get gvalue)))
    (is (= 1 (g:value-set gvalue :is-service "GApplicationFlags")))
    (is (equal '(:is-service) (g:value-get gvalue)))))

(test get-gvalue-flags.4
  (gobject:with-value (gvalue "GApplicationFlags" 0)
    (is (equal '() (g:value-get gvalue)))
    (is (= 5 (g:value-set gvalue
                          '(:is-service :handles-open) "GApplicationFlags")))
    (is (equal '(:is-service :handles-open) (g:value-get gvalue)))))

(test get-gvalue-flags.5
  (gobject:with-value (gvalue "GApplicationFlags" 0)
    (is (equal '() (g:value-get gvalue)))
    (is (= 5 (g:value-set gvalue 5 "GApplicationFlags")))
    (is (equal '(:is-service :handles-open) (g:value-get gvalue)))))

;; TODO: G:VALUE-FLAGS duplicatates the implementation, but does no conversion
;; beetween Lisp keywords and C integers. Consider to change the implementation
(test get-gvalue-flags.6
  (gobject:with-value (gvalue "GApplicationFlags" 0)
    (is (= 0 (gobject::%value-flags gvalue)))
    (is (= 5 (setf (gobject::%value-flags gvalue) 5)))
    (is (= 5 (gobject::%value-flags gvalue)))))

(test get-gvalue-flags.7
  (gobject:with-value (gvalue "GApplicationFlags" 0)
    (is (equal '() (g:value-flags gvalue)))
;    (is (= 5 (setf (g:value-flags gvalue) 5)))
;    (is (= 5 (g:value-flags gvalue)))
    ))

;;;     G_FLAGS_CLASS
;;;     G_IS_FLAGS_CLASS
;;;     G_FLAGS_CLASS_TYPE

;;;     g_enum_get_value
;;;     g_enum_get_value_by_name
;;;     g_enum_get_value_by_nick
;;;     g_enum_to_string
;;;     g_flags_get_first_value
;;;     g_flags_get_value_by_name
;;;     g_flags_get_value_by_nick
;;;     g_flags_to_string
;;;     g_enum_register_static
;;;     g_flags_register_static
;;;     g_enum_complete_type_info
;;;     g_flags_complete_type_info

;;; 2024-11-26
