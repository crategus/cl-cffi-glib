(in-package :glib-test)

(def-suite gobject-generating :in gobject-suite)
(in-suite gobject-generating)

;;; --- gobject::parse-property ------------------------------------------------

(test parse-property.1
  (let ((property (first (mapcar #'gobject::parse-property
                                 '((use-header-bar
                                    dialog-use-header-bar
                                    "use-header-bar" "gint" t t))))))
    (is (eq 'use-header-bar (gobject::gobject-property-name property)))
    (is (eq 'dialog-use-header-bar
            (gobject::gobject-property-accessor property)))
    (is-true (gobject::gobject-property-writable property))
    (is-true (gobject::gobject-property-readable property))
    (is (string= "use-header-bar" (gobject::gobject-property-gname property)))
    (is (string= "gint" (gobject::gobject-property-gtype property)))))

(test parse-property.2
  (let ((property (first (mapcar #'gobject::parse-property
                                 '((:cffi filename
                                    file-chooser-filename
                                    (g-string :free-from-foreign t
                                              :free-to-foreign t)
                                    "gtk_file_chooser_get_filename"
                                    "gtk_file_chooser_set_filename"))))))
    (is (eq 'filename (gobject::property-name property)))
    (is (eq 'file-chooser-filename
            (gobject::property-accessor property)))
    (is-true (gobject::property-writable property))
    (is-true (gobject::property-readable property))
    (is (string= "gtk_file_chooser_get_filename"
                 (gobject::cffi-property-reader property)))
    (is (string= "gtk_file_chooser_set_filename"
                 (gobject::cffi-property-writer property)))))

;;; --- meta-property->slot ----------------------------------------------------

(test meta-property->slot.1
  (let ((property (first (mapcar #'gobject::parse-property
                                 '((use-header-bar
                                    dialog-use-header-bar
                                    "use-header-bar" "gint" t t))))))
    (is (equal '(USE-HEADER-BAR
                 :ALLOCATION :GOBJECT-PROPERTY
                 :G-PROPERTY-TYPE "gint"
                 :ACCESSOR DIALOG-USE-HEADER-BAR
                 :INITARG :USE-HEADER-BAR
                 :G-PROPERTY-NAME "use-header-bar")
               (gobject::meta-property->slot "" property)))))

(test meta-property->slot.2
  (let ((property (first (mapcar #'gobject::parse-property
                                 '((:cffi filename
                                    file-chooser-filename
                                    (g-string :free-from-foreign t
                                              :free-to-foreign t)
                                    "gtk_file_chooser_get_filename"
                                    "gtk_file_chooser_set_filename"))))))
    (is (equal '(FILENAME
                 :ALLOCATION :GOBJECT-FN
                 :G-PROPERTY-TYPE (G-STRING :FREE-FROM-FOREIGN T
                                            :FREE-TO-FOREIGN T)
                 :ACCESSOR FILE-CHOOSER-FILENAME
                 :INITARG :FILENAME
                 :G-GETTER "gtk_file_chooser_get_filename"
                 :G-SETTER "gtk_file_chooser_set_filename")
               (gobject::meta-property->slot "" property)))))

;;; --- define-g-object-class --------------------------------------------------

(test define-g-object-class-macro.1
  (is (equal '(PROGN
 (DEFCLASS SIMPLE-ACTION (GIO:ACTION)
           ((ENABLED :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gboolean"
             :ACCESSOR SIMPLE-ACTION-ENABLED :INITARG :ENABLED :G-PROPERTY-NAME
             "enabled")
            (NAME :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gchararray"
             :ACCESSOR SIMPLE-ACTION-NAME :INITARG :NAME :G-PROPERTY-NAME
             "name")
            (PARAMETER-TYPE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "GVariantType" :ACCESSOR SIMPLE-ACTION-PARAMETER-TYPE :INITARG
             :PARAMETER-TYPE :G-PROPERTY-NAME "parameter-type")
            (STATE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "GVariant"
             :ACCESSOR SIMPLE-ACTION-STATE :INITARG :STATE :G-PROPERTY-NAME
             "state")
            (STATE-TYPE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "GVariantType" :ACCESSOR SIMPLE-ACTION-STATE-TYPE :INITARG
             :STATE-TYPE :G-PROPERTY-NAME "state-type"))
           (:GNAME . "GSimpleAction")
           (:INITIALIZER . "g_simple_action_get_type")
           (:METACLASS GOBJECT:GOBJECT-CLASS))
 (EXPORT 'SIMPLE-ACTION (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-ENABLED (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-NAME (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-PARAMETER-TYPE (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-STATE (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-STATE-TYPE (FIND-PACKAGE "GLIB-TEST")))
             (macroexpand
               '(gobject:define-g-object-class "GSimpleAction" simple-action
                                       (:superclass gobject:object
                                        :export t
                                        :interfaces ("GAction")
                                        :type-initializer
                                        "g_simple_action_get_type")
                                       ((enabled
                                         simple-action-enabled
                                         "enabled" "gboolean" t t)
                                        (name
                                         simple-action-name
                                         "name" "gchararray" t nil)
                                        (parameter-type
                                         simple-action-parameter-type
                                         "parameter-type" "GVariantType" t nil)
                                        (state
                                         simple-action-state
                                         "state" "GVariant" t t)
                                        (state-type
                                         simple-action-state-type
                                         "state-type" "GVariantType" t nil)))))))

;; Add a slot of type :cffi
(test define-g-object-class-macro.3
  (is (equal '(PROGN
 (DEFCLASS SIMPLE-ACTION (GIO:ACTION)
           ((ENABLED :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gboolean"
             :ACCESSOR SIMPLE-ACTION-ENABLED :INITARG :ENABLED :G-PROPERTY-NAME
             "enabled")
            (NAME :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gchararray"
             :ACCESSOR SIMPLE-ACTION-NAME :INITARG :NAME :G-PROPERTY-NAME
             "name")
            (PARAMETER-TYPE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "GVariantType" :ACCESSOR SIMPLE-ACTION-PARAMETER-TYPE :INITARG
             :PARAMETER-TYPE :G-PROPERTY-NAME "parameter-type")
            (STATE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "GVariant"
             :ACCESSOR SIMPLE-ACTION-STATE :INITARG :STATE :G-PROPERTY-NAME
             "state")
            (STATE-TYPE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "GVariantType" :ACCESSOR SIMPLE-ACTION-STATE-TYPE :INITARG
             :STATE-TYPE :G-PROPERTY-NAME "state-type")
            (STATE-HINT :ALLOCATION :GOBJECT-FN :G-PROPERTY-TYPE GLIB:VARIANT
             :ACCESSOR SIMPLE-ACTION-STATE-HINT :INITARG :STATE-HINT :G-GETTER
             NIL :G-SETTER "g_simple_action_set_state_hint"))
           (:GNAME . "GSimpleAction")
           (:INITIALIZER . "g_simple_action_get_type")
           (:METACLASS GOBJECT:GOBJECT-CLASS))
 (EXPORT 'SIMPLE-ACTION (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-ENABLED (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-NAME (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-PARAMETER-TYPE (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-STATE (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-STATE-TYPE (FIND-PACKAGE "GLIB-TEST"))
 (EXPORT 'SIMPLE-ACTION-STATE-HINT (FIND-PACKAGE "GLIB-TEST")))
             (macroexpand
               '(gobject:define-g-object-class "GSimpleAction" simple-action
                                       (:superclass gobject:object
                                        :export t
                                        :interfaces ("GAction")
                                        :type-initializer
                                        "g_simple_action_get_type")
                                       ((enabled
                                         simple-action-enabled
                                         "enabled" "gboolean" t t)
                                        (name
                                         simple-action-name
                                         "name" "gchararray" t nil)
                                        (parameter-type
                                         simple-action-parameter-type
                                         "parameter-type" "GVariantType" t nil)
                                        (state
                                         simple-action-state
                                         "state" "GVariant" t t)
                                        (state-type
                                         simple-action-state-type
                                         "state-type" "GVariantType" t nil)
                                        (:cffi state-hint
                                         simple-action-state-hint
                                         glib:variant
                                         nil
                                         "g_simple_action_set_state_hint")))))))

(test define-g-interface-macro
  (is (equal '(PROGN
                (DEFCLASS ACTION NIL
                  ((ENABLED :ALLOCATION :GOBJECT-PROPERTY
                            :G-PROPERTY-TYPE "gboolean"
                            :ACCESSOR ACTION-ENABLED
                            :INITARG :ENABLED
                            :G-PROPERTY-NAME "enabled")
                   (NAME :ALLOCATION :GOBJECT-PROPERTY
                         :G-PROPERTY-TYPE "gchararray"
                         :ACCESSOR ACTION-NAME
                         :INITARG :NAME
                         :G-PROPERTY-NAME "name")
                   (PARAMETER-TYPE :ALLOCATION :GOBJECT-PROPERTY
                                   :G-PROPERTY-TYPE "GVariantType"
                                   :ACCESSOR ACTION-PARAMETER-TYPE
                                   :INITARG :PARAMETER-TYPE
                                   :G-PROPERTY-NAME "parameter-type")
                   (STATE :ALLOCATION :GOBJECT-PROPERTY
                          :G-PROPERTY-TYPE "GVariant"
                          :ACCESSOR ACTION-STATE
                          :INITARG :STATE
                          :G-PROPERTY-NAME "state")
                   (STATE-TYPE :ALLOCATION :GOBJECT-PROPERTY
                               :G-PROPERTY-TYPE "GVariantType"
                               :ACCESSOR ACTION-STATE-TYPE
                               :INITARG :STATE-TYPE
                               :G-PROPERTY-NAME "state-type"))
                  (:GNAME . "GAction")
                  (:INITIALIZER . "g_action_get_type")
                  (:INTERFACE-P . T)
                  (:METACLASS GOBJECT:GOBJECT-CLASS))
                (EXPORT 'ACTION (FIND-PACKAGE "GLIB-TEST"))
                (EXPORT 'ACTION-ENABLED (FIND-PACKAGE "GLIB-TEST"))
                (EXPORT 'ACTION-NAME (FIND-PACKAGE "GLIB-TEST"))
                (EXPORT 'ACTION-PARAMETER-TYPE (FIND-PACKAGE "GLIB-TEST"))
                (EXPORT 'ACTION-STATE (FIND-PACKAGE "GLIB-TEST"))
                (EXPORT 'ACTION-STATE-TYPE (FIND-PACKAGE "GLIB-TEST"))
                (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
                  (SETF (GETHASH "GAction" GOBJECT::*KNOWN-INTERFACES*) 'ACTION)))
             (macroexpand '(gobject:define-g-interface "GAction" action
                                               (:export t
                                                :type-initializer
                                                "g_action_get_type")
                                               ((enabled
                                                 action-enabled
                                                 "enabled" "gboolean" t nil)
                                                (name
                                                 action-name
                                                 "name" "gchararray" t nil)
                                                (parameter-type
                                                 action-parameter-type
                                                 "parameter-type" "GVariantType"
                                                 t nil)
                                                (state
                                                 action-state
                                                 "state" "GVariant" t nil)
                                                (state-type
                                                 action-state-type
                                                 "state-type" "GVariantType"
                                                 t nil)))))))

;;; --- 2023-7-9 ---------------------------------------------------------------
