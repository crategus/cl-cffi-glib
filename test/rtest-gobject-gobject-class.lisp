(in-package :glib-test)

(def-suite gobject-class :in gobject-suite)
(in-suite gobject-class)

(test gobject-class-properties.1
  (let ((class (find-class 'gio:simple-action)))
    (is (string= "GSimpleAction" (gobject::gobject-class-gname class)))
    (is (string= "GSimpleAction"
                 (gobject::gobject-class-direct-gname class)))
    (is (string= "g_simple_action_get_type"
                 (gobject::gobject-class-initializer class)))
    (is-false (gobject::gobject-class-interface-p class))))

#+nil
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

(test gobject-class-properties.2
  (let ((class (find-class 'gio:action)))
    (is (string= "GAction" (gobject::gobject-class-gname class)))
    (is (string= "GAction" (gobject::gobject-class-direct-gname class)))
    (is (string= "g_action_get_type"
                 (gobject::gobject-class-initializer class)))
    (is-true (gobject::gobject-class-interface-p class))))

#+nil
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

;;; --- 2023-6-20 --------------------------------------------------------------
