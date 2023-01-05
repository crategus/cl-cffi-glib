(def-suite gobject-class :in gobject-suite)
(in-suite gobject-class)

;; (define-g-object-class "GtkBox" gtk-box
;;   (:superclass gtk-container
;;    :export t
;;    :interfaces ("AtkImplementorIface"
;;                 "GtkBuildable"
;;                 "GtkOrientable")
;;    :type-initializer "gtk_box_get_type")
;;   ((baseline-position
;;     gtk-box-base-line-position
;;     "baseline-position" "GtkBaselinePosition" t t)
;;    (homogeneous
;;     gtk-box-homogeneous
;;     "homogeneous" "gboolean" t t)
;;    (spacing
;;     gtk-box-spacing
;;     "spacing" "gint" t t)))

(test gobject-class-properties.1
  (let ((class (find-class 'gtk-box)))
    (is (string= "GtkBox" (gobject-class-g-type-name class)))
    (is (string= "GtkBox" (gobject-class-direct-g-type-name class)))
    (is (string= "gtk_box_get_type" (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class))))

(test compute-new-initargs-for-metaclass.1

  (is (equal (append (list :direct-superclasses
                               (list (find-class 'gtk-container)
                                     (find-class 'atk-implementor-iface)
                                     (find-class 'gtk-buildable)
                                     (find-class 'gtk-orientable)))
                 '(:DIRECT-SLOTS 
                   ((:NAME GTK-TESTSUITE::BASELINE-POSITION 
                     :READERS (GTK:GTK-BOX-BASELINE-POSITION) 
                     :WRITERS ((SETF GTK:GTK-BOX-BASELINE-POSITION)) 
                     :INITARGS (:BASELINE-POSITION)
                     :G-PROPERTY-NAME "baseline-position" 
                     :G-PROPERTY-TYPE "GtkBaselinePosition" 
                     :ALLOCATION :GOBJECT-PROPERTY) 
                    (:NAME GTK-TESTSUITE::HOMOGENEOUS 
                     :READERS (GTK:GTK-BOX-HOMOGENEOUS) 
                     :WRITERS ((SETF GTK:GTK-BOX-HOMOGENEOUS)) 
                     :INITARGS (:HOMOGENEOUS) 
                     :G-PROPERTY-NAME "homogeneous" 
                     :G-PROPERTY-TYPE "gboolean" 
                     :ALLOCATION :GOBJECT-PROPERTY) 
                    (:NAME GTK-TESTSUITE::SPACING 
                     :READERS (GTK:GTK-BOX-SPACING) 
                     :WRITERS ((SETF GTK:GTK-BOX-SPACING)) 
                     :INITARGS (:SPACING) 
                     :G-PROPERTY-NAME "spacing" 
                     :G-PROPERTY-TYPE "gint" 
                     :ALLOCATION :GOBJECT-PROPERTY)) 
                   :G-TYPE-NAME "GtkBox" 
                   :G-TYPE-INITIALIZER "gtk_box_get_type" 
                   :DIRECT-DEFAULT-INITARGS NIL))
             (gobject::compute-new-initargs-for-metaclass
                 (append (list :direct-superclasses
                               (list (find-class 'gtk-container)
                                     (find-class 'atk-implementor-iface)
                                     (find-class 'gtk-buildable)
                                     (find-class 'gtk-orientable)))
                 '(:DIRECT-SLOTS 
                   ((:NAME GTK-TESTSUITE::BASELINE-POSITION 
                     :READERS (GTK:GTK-BOX-BASELINE-POSITION) 
                     :WRITERS ((SETF GTK:GTK-BOX-BASELINE-POSITION)) 
                     :INITARGS (:BASELINE-POSITION)
                     :G-PROPERTY-NAME "baseline-position" 
                     :G-PROPERTY-TYPE "GtkBaselinePosition" 
                     :ALLOCATION :GOBJECT-PROPERTY) 
                    (:NAME GTK-TESTSUITE::HOMOGENEOUS 
                     :READERS (GTK:GTK-BOX-HOMOGENEOUS) 
                     :WRITERS ((SETF GTK:GTK-BOX-HOMOGENEOUS)) 
                     :INITARGS (:HOMOGENEOUS) 
                     :G-PROPERTY-NAME "homogeneous" 
                     :G-PROPERTY-TYPE "gboolean" 
                     :ALLOCATION :GOBJECT-PROPERTY) 
                    (:NAME GTK-TESTSUITE::SPACING 
                     :READERS (GTK:GTK-BOX-SPACING) 
                     :WRITERS ((SETF GTK:GTK-BOX-SPACING)) 
                     :INITARGS (:SPACING) 
                     :G-PROPERTY-NAME "spacing" 
                     :G-PROPERTY-TYPE "gint" 
                     :ALLOCATION :GOBJECT-PROPERTY)) 
                   :G-TYPE-NAME "GtkBox" 
                   :G-TYPE-INITIALIZER "gtk_box_get_type" 
                   :DIRECT-DEFAULT-INITARGS NIL))
                 'G-OBJECT)

             )))
          








;; (define-g-interface "GtkOrientable" gtk-orientable
;;   (:export t
;;    :type-initializer "gtk_orientable_get_type")
;;   (orientation
;;    gtk-orientable-orientation
;;    "orientation" "GtkOrientation" t t))

(test gobject-class-properties.2
  (let ((class (find-class 'gtk-orientable)))
    (is (string= "GtkOrientable" (gobject-class-g-type-name class)))
    (is (string= "GtkOrientable" (gobject-class-direct-g-type-name class)))
    (is (string= "gtk_orientable_get_type" (gobject-class-g-type-initializer class)))
    (is-true (gobject-class-interface-p class))))

(test compute-new-initargs-for-metaclass.2
  (is (equal

      (append '(:DIRECT-SLOTS
                                ((:NAME ORIENTATION :READERS
                                  (GTK-ORIENTABLE-ORIENTATION) :WRITERS
                                  ((SETF GTK-ORIENTABLE-ORIENTATION)) :INITARGS
                                  (:ORIENTATION) :G-PROPERTY-NAME "orientation"
                                  :G-PROPERTY-TYPE "GtkOrientation" :ALLOCATION
                                  :GOBJECT-PROPERTY))
                                :G-TYPE-NAME "GtkOrientable" :G-INTERFACE-P T
                                :G-TYPE-INITIALIZER "gtk_orientable_get_type"
                                :DIRECT-DEFAULT-INITARGS NIL
                                )
                (list :direct-superclasses (list (find-class 'g-object))))


            (gobject::compute-new-initargs-for-metaclass
                '(:DIRECT-SUPERCLASSES NIL 
                  :DIRECT-SLOTS ((:NAME GTK-TESTSUITE::ORIENTATION
                                  :READERS (GTK:GTK-ORIENTABLE-ORIENTATION) 
                                  :WRITERS ((SETF GTK:GTK-ORIENTABLE-ORIENTATION)) 
                                  :INITARGS (:ORIENTATION) 
                                  :G-PROPERTY-NAME "orientation" 
                                  :G-PROPERTY-TYPE "GtkOrientation" 
                                  :ALLOCATION :GOBJECT-PROPERTY))
                  :G-TYPE-NAME "GtkOrientable" 
                  :G-INTERFACE-P T 
                  :G-TYPE-INITIALIZER "gtk_orientable_get_type" 
                  :DIRECT-DEFAULT-INITARGS NIL)
                 'G-OBJECT))))

