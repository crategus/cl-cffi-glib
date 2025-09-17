(in-package :glib-test)

(def-suite gobject-subclassing :in gobject-suite)
(in-suite gobject-subclassing)

(defvar *verbose-gobject-subclassing* nil)

;;; --- filter-properties-to-register ------------------------------------------

(test filter-properties-to-register
  (let ((properties '((use-header-bar
                       dialog-use-header-bar
                       "use-header-bar" "gint" t t)
                      (title
                       dialog-title
                       "title" "gchararray" t nil)
                      (:cffi filename
                       file-chooser-filename
                       (:string :free-from-foreign t
                                :free-to-foreign t)
                       "gtk_file_chooser_get_filename"
                       "gtk_file_chooser_set_filename")
                      (:cl position
                       :initform :bottom
                       :initarg position))))
    (is (equal '(("use-header-bar" "gint" DIALOG-USE-HEADER-BAR T T)
                 ("title" "gchararray" DIALOG-TITLE T NIL))
               (gobject::filter-properties-to-register properties)))))

;;; ----------------------------------------------------------------------------

;;; GIcon Inferface

;;; struct GioIconIface {
;;;   GTypeInterface g_iface;
;;;   guint (* hash) (
;;;     GIcon* icon
;;;   );
;;;   gboolean (* equal) (
;;;     GIcon* icon1,
;;;     GIcon* icon2
;;;   );
;;;   gboolean (* to_tokens) (
;;;     GIcon* icon,
;;;     GPtrArray* tokens,
;;;     gint* out_version
;;;   );
;;;   GIcon* (* from_tokens) (
;;;     gchar** tokens,
;;;     gint num_tokens,
;;;     gint version,
;;;     GError** error
;;;   );
;;;   GVariant* (* serialize) (
;;;     GIcon* icon
;;;   );
;;; }

;;; We must define a vtable to use the GIcon interface in a subclass
;;; We do not override any of the virtual functions, but define the correct
;;; size of the vtable.

(gobject:define-vtable ("GIcon" icon)
  (:skip parent-instance (:struct g:type-interface))
  ;; Methods of the GIcon interface
  (:skip hash :pointer)
  (:skip equal :pointer)
  (:skip to-tokens :pointer)
  (:skip from-tokens :pointer)
  (:skip serialize :pointer))

;;; ----------------------------------------------------------------------------

;;     Define a Clock3

(gobject:define-gobject-subclass "Clock3" clock3
  (:superclass g:object
   :export nil
   :interfaces ("GIcon"))
  ((:cl timezone
        :initform local-time:+utc-zone+
        :accessor clock3-timezone)
   (location
    clock3-location
    "location" "gchararray" t t)))

;; Initialize a slot
(defmethod initialize-instance :after
           ((obj clock3) &key &allow-other-keys)
  (setf (clock3-location obj) "UTC"))

(test clock3-class
  ;; Check type
  (is (g:type-is-object "Clock3"))
  ;; Check registered symbol
  (is (eq 'clock3
          (glib:symbol-for-gtype "Clock3")))
  ;; Check type initializer
  ;; no type initializer
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "Clock3")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "Clock3")))
  ;; Check interfaces
  (is (equal '("GIcon")
             (glib-test:list-interfaces "Clock3")))
  ;; Check class properties
  (is (equal '("location")
             (glib-test:list-properties "Clock3")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "Clock3")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "Clock3" CLOCK3
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GIcon"))
                      ((LOCATION CLOCK3-LOCATION "location" "gchararray" T T)))
             (gobject:get-gtype-definition "Clock3"))))

(test check-implementation-clock3
  (let ((info (gobject::get-subclass-info "Clock3"))
        (clock nil))
    ;; Check subclass-info
    (is (string= "Clock3" (gobject::subclass-info-gname info)))
    (is (eq 'clock3 (gobject::subclass-info-class info)))
    (is (string= "GObject" (gobject::subclass-info-parent info)))
    (is (equal '("GIcon") (gobject::subclass-info-interfaces info)))
    (is (equal '(("location" "gchararray" CLOCK3-LOCATION T T))
               (gobject::subclass-info-properties info)))
    ;; Create an instance and access the properties
    (is (typep (setf clock (make-instance 'clock3)) 'clock3))
    (is (typep (clock3-timezone clock) 'local-time::timezone))
    (is (string= "UTC" (clock3-location clock)))
    ;; Use GObject functions for the registered property
    ;; Slot TIMEZONE is not registered to GTK
    (is-false (g:object-class-find-property "Clock3" "timezone"))
    ;; Get GParamSpec for registered LOCATION property
    (is (cffi:pointerp (g:object-class-find-property "Clock3" "location")))
    ;; TIMEZONE is not registered as property
    (signals (error) (g:object-property clock "timezone"))
    (is (typep (clock3-timezone clock) 'local-time::timezone))
    ;; LOCATION is registered, we can access the property
    (is (string= "UTC" (g:object-property clock "location")))
    (is (string= "UTC" (clock3-location clock)))))

;;; ----------------------------------------------------------------------------

;;;     Define clock4

(gobject:define-gobject-subclass "Clock4" clock4
  (:superclass g:object
   :export t
   :interfaces ())
  ((:cl timezone
        :initform local-time:+utc-zone+
        :accessor clock4-timezone)
   (:cl location
        :initform "UTC"
        :accessor clock4-location)))

(defmethod g:object-class-init :after
           ((subclass (eql (find-class 'clock4))) class data)
  (declare (ignore class data))
  (when *verbose-gobject-subclassing*
    (format t "~&in CLASS-INIT for ~a~%" subclass)))

(test clock4-class
  ;; Check type
  (is (g:type-is-object "Clock4"))
  ;; Check registered symbol
  (is (eq 'clock4
          (glib:symbol-for-gtype "Clock4")))
  ;; Check type initializer
  ;; no type initializer
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "Clock4")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "Clock4")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "Clock4")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "Clock4")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "Clock4")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "Clock4" CLOCK4
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL)
                       NIL)
             (gobject:get-gtype-definition "Clock4"))))

(test filter-properties-to-register.clock4
  (let ((properties '((:cl timezone
                           :initform local-time:+utc-zone+
                           :accessor clock4-timezone)
                      (:cl location
                           :initform "UTC"
                           :accessor clock4-location))))
  (is (equal '()
             (gobject::filter-properties-to-register properties)))))

(test property->slot.clock4
  (let ((properties '((:cl timezone
                           :initform local-time:+utc-zone+
                           :accessor clock4-timezone)
                      (:cl location
                           :initform "UTC"
                           :accessor clock4-location))))
    (is (every (lambda (property)
                 (typep property 'gobject::property))
               (setf properties
                     (mapcar #'gobject::parse-property properties))))
    (is (equal '((TIMEZONE :INITFORM LOCAL-TIME:+UTC-ZONE+
                           :ACCESSOR CLOCK4-TIMEZONE)
                 (LOCATION :INITFORM "UTC"
                           :ACCESSOR CLOCK4-LOCATION))
               (mapcar (lambda (property)
                         (gobject::property->slot "Clock4" property))
                       properties)))))

(test check-implementation-clock4
  (let ((info (gobject::get-subclass-info "Clock4"))
        (clock nil))
    ;; Check subclass-info
    (is (string= "Clock4" (gobject::subclass-info-gname info)))
    (is (eq 'clock4 (gobject::subclass-info-class info)))
    (is (string= "GObject" (gobject::subclass-info-parent info)))
    (is (equal '() (gobject::subclass-info-interfaces info)))
    (is (equal '() (gobject::subclass-info-properties info)))

    ;; Create an instance and access the properties
    (is (typep (setf clock (make-instance 'clock4)) 'clock4))
    (is (typep (clock4-timezone clock) 'local-time::timezone))
    (is (string= "UTC" (clock4-location clock)))))

;;; ----------------------------------------------------------------------------

;;;     Define Clock5

(gobject:define-gobject-subclass "Clock5" clock5
  (:superclass g:object
   :export nil
   :interfaces ())
  ((:cl timezone
        :initform local-time:+utc-zone+
        :accessor clock5-timezone)
   (location
    clock5-location
    "location" "gchararray" t t)))

(defmethod g:object-class-init :after
           ((subclass (eql (find-class 'clock5))) class data)
  (declare (ignore class data))
  (when *verbose-gobject-subclassing*
    (format t "~&in G:OBJECT-CLASS-INIT for ~a~%" subclass)))

(defmethod g:object-instance-init :after
           ((subclass (eql (find-class 'clock5))) instance data)
  (when *verbose-gobject-subclassing*
    (format t "~&in G:OBJECT-INSTANCE-INIT~%")
    (format t "   subclass : ~a~%" subclass)
    (format t "   instance : ~a~%" instance)
    (format t "       data : ~a~%" data)))

(test clock5-class
  ;; Check type
  (is (g:type-is-object "Clock5"))
  ;; Check registered symbol
  (is (eq 'clock5
          (glib:symbol-for-gtype "Clock5")))
  ;; Check type initializer
  ;; no type initializer
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "Clock5")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "Clock5")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "Clock5")))
  ;; Check class properties
  (is (equal '("location")
             (glib-test:list-properties "Clock5")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "Clock5")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "Clock5" CLOCK5
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL)
                       ((LOCATION CLOCK5-LOCATION "location" "gchararray" T T)))
             (gobject:get-gtype-definition "Clock5"))))

(test filter-properties-to-register.clock5
  (let ((properties '((:cl timezone
                           :initform local-time:+utc-zone+
                           :accessor clock5-timezone)
                      (location
                       clock5-location
                       "location" "gchararray" t t))))
  (is (equal '(("location" "gchararray" CLOCK5-LOCATION T T))
             (gobject::filter-properties-to-register properties)))))

(test property->slot.clock5
  (let ((properties '((:cl timezone
                           :initform local-time:+utc-zone+
                           :accessor clock5-timezone)
                      (location
                       clock5-location
                       "location" "gchararray" t t))))
    (is (every (lambda (property)
                 (typep property 'gobject::property))
               (setf properties
                     (mapcar #'gobject::parse-property properties))))
    (is (equal '((TIMEZONE :INITFORM LOCAL-TIME:+UTC-ZONE+
                           :ACCESSOR CLOCK5-TIMEZONE)
                 (LOCATION :ALLOCATION :GOBJECT-PROPERTY
                           :PROP-TYPE "gchararray"
                           :ACCESSOR CLOCK5-LOCATION
                           :INITARG :LOCATION
                           :PROP-NAME "location"))
               (mapcar (lambda (property)
                         (gobject::property->slot "Clock5" property))
                       properties)))))

(test check-implementation-clock5
  (let ((info (gobject::get-subclass-info "Clock5"))
        (clock nil))
    ;; Check subclass-info
    (is (string= "Clock5" (gobject::subclass-info-gname info)))
    (is (eq 'clock5 (gobject::subclass-info-class info)))
    (is (string= "GObject" (gobject::subclass-info-parent info)))
    (is (equal '() (gobject::subclass-info-interfaces info)))
    (is (equal '(("location" "gchararray" CLOCK5-LOCATION T T))
               (gobject::subclass-info-properties info)))

    ;; Create an instance and access the properties
    (is (typep (setf clock (make-instance 'clock5)) 'clock5))
    (is (typep (clock5-timezone clock) 'local-time::timezone))
    (is (string= "" (clock5-location clock)))
    ;; Use GObject functions for the registered property
    ;; Slot TIMEZONE is not registered to GTK
    (is-false (g:object-class-find-property "Clock5" "timezone"))
    ;; Get GParamSpec for registered LOCATION property
    (is (cffi:pointerp (g:object-class-find-property "Clock5" "location")))
    ;; TIMEZONE is not registered as property
    (signals (error) (g:object-property clock "timezone"))
    (is (typep (clock5-timezone clock) 'local-time::timezone))
    ;; LOCATION is registered, we can access the property
    (is (string= "UTC" (setf (g:object-property clock "location") "UTC")))
    (is (string= "UTC" (g:object-property clock "location")))
    (is (string= "abc" (setf (clock5-location clock) "abc")))))

;;; 2025-09-17
