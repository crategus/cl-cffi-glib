(in-package :glib-test)

(def-suite gobject-enumeration :in gobject-suite)
(in-suite gobject-enumeration)

;;; --- Types and Values -------------------------------------------------------

(test define-g-enum-macro
  (is (equal '(PROGN
 (DEFCENUM (GTK-RESPONSE-TYPE :INT :ALLOW-UNDECLARED-VALUES NIL)
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
 (SETF (GLIB:SYMBOL-FOR-GTYPE "GtkResponseType") 'GTK-RESPONSE-TYPE)
 (EXPORT 'GTK-RESPONSE-TYPE (FIND-PACKAGE "GLIB-TEST"))
 (GLIB-INIT:AT-INIT NIL
   (IF (CFFI:FOREIGN-SYMBOL-POINTER "gtk_response_type_get_type")
       (CFFI:FOREIGN-FUNCALL-POINTER
        (CFFI:FOREIGN-SYMBOL-POINTER "gtk_response_type_get_type") NIL :SIZE)
       (WARN "Type initializer '~A' is not available"
             "gtk_response_type_get_type"))))
              (macroexpand '(define-g-enum "GtkResponseType" gtk-response-type
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

(test define-g-flags-macro
  (is (equal '(PROGN
 (DEFBITFIELD GDK-DRAG-ACTION :INT
   (:DEFAULT 1)
   (:COPY 2)
   (:MOVE 4)
   (:LINK 8)
   (:PRIVATE 16)
   (:ASK 32))
 (SETF (GLIB:SYMBOL-FOR-GTYPE "GdkDragAction") 'GDK-DRAG-ACTION)
 (EXPORT 'GDK-DRAG-ACTION (FIND-PACKAGE "GLIB-TEST"))
 (GLIB-INIT:AT-INIT NIL
   (IF (CFFI:FOREIGN-SYMBOL-POINTER "gdk_drag_action_get_type")
       (CFFI:FOREIGN-FUNCALL-POINTER
        (CFFI:FOREIGN-SYMBOL-POINTER "gdk_drag_action_get_type") NIL :SIZE)
       (WARN "Type initializer '~A' is not available"
             "gdk_drag_action_get_type"))))
             (macroexpand '(define-g-flags "GdkDragAction" gdk-drag-action
                            (:export t
                             :type-initializer "gdk_drag_action_get_type")
                            (:default 1)
                            (:copy 2)
                            (:move 4)
                            (:link 8)
                            (:private 16)
                            (:ask 32))))))

;;;   g-enum-class

(test enum-class
  (is (= 32 (cffi:foreign-type-size '(:struct g:enum-class))))
  (is (equal '(:maximum :minimum :n-values :type-class :values)
             (sort (cffi:foreign-slot-names '(:struct g:enum-class))
                   #'string-lessp))))

;;;   g-enum-value

(test enum-value
  (is (= 24 (cffi:foreign-type-size '(:struct g:enum-value))))
  (is (equal '(:name :nick :value)
             (sort (cffi:foreign-slot-names '(:struct g:enum-value))
                   #'string-lessp))))

;;;   g-flags-class

(test flags-class
  (is (= 24 (cffi:foreign-type-size '(:struct g:flags-class))))
  (is (equal '(:mask :n-values :type-class :values)
             (sort (cffi:foreign-slot-names '(:struct g:flags-class))
                   #'string-lessp))))

;;;   g-flags-value

(test flags-value
  (is (= 24 (cffi:foreign-type-size '(:struct g:flags-value))))
  (is (equal '(:name :nick :value)
             (sort (cffi:foreign-slot-names '(:struct g:flags-value))
                   #'string-lessp))))

;;; --- Functions --------------------------------------------------------------

;;;     G_ENUM_CLASS_TYPE
;;;     G_ENUM_CLASS_TYPE_NAME

;;;     g-type-is-enum

(test type-is-enum
  (is-false (g:type-is-enum "GApplicationFlags"))
  (is-true  (g:type-is-enum "GEmblemOrigin")))

;;;     G_ENUM_CLASS
;;;     G_IS_ENUM_CLASS

;;;     G_TYPE_IS_FLAGS

(test type-is-flags
  (is-true  (g:type-is-flags "GApplicationFlags"))
  (is-false (g:type-is-flags "GEmblemOrigin")))

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

;;; --- 2023-1-2 ---------------------------------------------------------------
