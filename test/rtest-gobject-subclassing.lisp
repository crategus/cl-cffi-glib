(in-package :glib-test)

(def-suite gobject-subclassing :in gobject-suite)
(in-suite gobject-subclassing)

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

;;; --- register-object-type-implementation ------------------------------------

;; Define a GClock1

(defclass clock1 (gio:icon)
  (;; Initialize with the UTC timezone
   (timezone :initform local-time:+utc-zone+
             :accessor clock-timezone)
   ;; Initialize with the name of the UTC timezone
   (location :initform "UTC"
             :accessor clock-location))
  (:gname . "GClock1")
  (:metaclass gobject:gobject-class))

(gobject:register-object-type-implementation "GClock1"         ; name
                                             clock1            ; class
                                             "GObject"         ; parent
                                             ()                ; interfaces
                                             nil)              ; properties

(test clock1-class
  ;; Type check
  (is (g:type-is-object "GClock1"))
  ;; Check the registered symbol
  (is (eq 'clock1
          (glib:symbol-for-gtype "GClock1")))
  ;; Check the type initializer
  ;; no type initializer
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GClock1")))
  ;; Check the children
  (is (equal '()
             (list-children "GClock1")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GClock1")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GClock1")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GClock1")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GClock1" G-CLOCK1
                               (:SUPERCLASS G-OBJECT
                                :EXPORT T
                                :INTERFACES NIL)
                               NIL)
             (gobject:get-g-type-definition "GClock1"))))

(test register-object-type-implementation.1
  (let ((info (gobject::get-subclass-info "GClock1"))
        (clock nil))
    ;; Check subclass-info
    (is (string= "GClock1" (gobject::subclass-info-gname info)))
    (is (eq 'clock1 (gobject::subclass-info-class info)))
    (is (string= "GObject" (gobject::subclass-info-parent info)))
    (is (equal '() (gobject::subclass-info-interfaces info)))
    (is (equal '() (gobject::subclass-info-properties info)))
    ;; Create an instance and access the properties
    (is (typep (setf clock (make-instance 'clock1)) 'clock1))
    (is (typep (clock-timezone clock) 'local-time::timezone))
    (is (string= "UTC" (clock-location clock)))
))

;;; ----------------------------------------------------------------------------

;;     Define a GClock2

(defclass clock2 (gio:icon)
  (;; Initialize with the UTC timezone
   (timezone :initform local-time:+utc-zone+
             :accessor clock2-timezone)
   ;; Initialize with the name of the UTC timezone
   (location :initform "UTC"
             :accessor clock2-location))
  (:gname . "GClock2")
  (:metaclass gobject:gobject-class))

(gobject:register-object-type-implementation "GClock2"         ; name
                                             clock2            ; class
                                             "GObject"         ; parent
                                             ()                ; interfaces
                                             (("location"      ; properties
                                               "gchararray"
                                               clock2-location
                                               t t)))

(test clock2-class
  ;; Type check
  (is (g:type-is-object "GClock2"))
  ;; Check the registered symbol
  (is (eq 'clock2
          (glib:symbol-for-gtype "GClock2")))
  ;; Check the type initializer
  ;; no type initializer
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GClock2")))
  ;; Check the children
  (is (equal '()
             (list-children "GClock2")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GClock2")))
  ;; Check the class properties
  (is (equal '("location")
             (list-properties "GClock2")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GClock2")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GClock2" G-CLOCK2
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL)
                               ((LOCATION G-CLOCK2-LOCATION "location"
                                 "gchararray" T T)))
             (gobject:get-g-type-definition "GClock2"))))

(test register-object-type-implementation.2
  (let ((info (gobject::get-subclass-info "GClock2"))
        (clock nil) (pspec nil))
    ;; Check subclass-info
    (is (string= "GClock2" (gobject::subclass-info-gname info)))
    (is (eq 'clock2 (gobject::subclass-info-class info)))
    (is (string= "GObject" (gobject::subclass-info-parent info)))
    (is (equal '() (gobject::subclass-info-interfaces info)))
    (is (equal '(("location" "gchararray" CLOCK2-LOCATION T T))
               (gobject::subclass-info-properties info)))
    ;; Create an instance and access the properties
    (is (typep (setf clock (make-instance 'clock2)) 'clock2))
    (is (typep (clock2-timezone clock) 'local-time::timezone))
    (is (string= "UTC" (clock2-location clock)))
    ;; Use GObject functions for the registered property
    ;; Slot TIMEZONE is not registered to GTK
    (is-false (g:object-class-find-property "GClock2" "timezone"))
    ;; Get GParamSpec for registered LOCATION property
    (is (cffi:pointerp (setf pspec
                             (g:object-class-find-property "GClock2"
                                                           "location"))))
    ;; TIMEZONE is not registered as property
    (signals (error) (g:object-property clock "timezone"))
    (is (typep (clock2-timezone clock) 'local-time::timezone))
    ;; LOCATION is registered, we can access the property
    (is (string= "UTC" (g:object-property clock "location")))
    (is (string= "UTC" (clock2-location clock)))
))

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

;;     Define a GClock3

(defclass clock3 (gio:icon)
  (;; Initialize with the UTC timezone
   (timezone :initform local-time:+utc-zone+
             :accessor clock3-timezone)
   ;; Initialize with the name of the UTC timezone
   (location :initform "UTC"
             :accessor clock3-location))
  (:gname . "GClock3")
  (:metaclass gobject:gobject-class))

(gobject:register-object-type-implementation "GClock3"         ; name
                                             clock3            ; class
                                             "GObject"         ; parent
                                             ("GIcon")         ; interfaces
                                             (("location"      ; properties
                                               "gchararray"
                                               clock3-location
                                               t t)))

(test clock3-class
  ;; Type check
  (is (g:type-is-object "GClock3"))
  ;; Check the registered symbol
  (is (eq 'clock3
          (glib:symbol-for-gtype "GClock3")))
  ;; Check the type initializer
  ;; no type initializer
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GClock3")))
  ;; Check the children
  (is (equal '()
             (list-children "GClock3")))
  ;; Check the interfaces
  (is (equal '("GIcon")
             (list-interfaces "GClock3")))
  ;; Check the class properties
  (is (equal '("location")
             (list-properties "GClock3")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GClock3")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GClock3" G-CLOCK3
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GIcon"))
                               ((LOCATION G-CLOCK3-LOCATION "location"
                                 "gchararray" T T)))
             (gobject:get-g-type-definition "GClock3"))))

(test register-object-type-implementation.3
  (let ((info (gobject::get-subclass-info "GClock3"))
        (clock nil) (pspec nil))
    ;; Check subclass-info
    (is (string= "GClock3" (gobject::subclass-info-gname info)))
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
    (is-false (g:object-class-find-property "GClock3" "timezone"))
    ;; Get GParamSpec for registered LOCATION property
    (is (cffi:pointerp (setf pspec
                             (g:object-class-find-property "GClock3"
                                                           "location"))))
    ;; TIMEZONE is not registered as property
    (signals (error) (g:object-property clock "timezone"))
    (is (typep (clock3-timezone clock) 'local-time::timezone))
    ;; LOCATION is registered, we can access the property
    (is (string= "UTC" (g:object-property clock "location")))
    (is (string= "UTC" (clock3-location clock)))
))

;;; ----------------------------------------------------------------------------

;;;     Define clock4

(gobject:define-g-object-subclass "GClock4" clock4
  (:superclass g:object
   :export t
   :interfaces ())
  ((:cl timezone
        :initform local-time:+utc-zone+
        :accessor clock4-timezone)
   (:cl location
        :initform "UTC"
        :accessor clock4-location)))

(test clock4-class
  ;; Type check
  (is (g:type-is-object "GClock4"))
  ;; Check the registered symbol
  (is (eq 'clock4
          (glib:symbol-for-gtype "GClock4")))
  ;; Check the type initializer
  ;; no type initializer
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GClock4")))
  ;; Check the children
  (is (equal '()
             (list-children "GClock4")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GClock4")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GClock4")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GClock4")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GClock4" G-CLOCK4
                               (:SUPERCLASS G-OBJECT
                                :EXPORT T
                                :INTERFACES NIL)
                               NIL)
             (gobject:get-g-type-definition "GClock4"))))

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
  (let ((info (gobject::get-subclass-info "GClock4"))
        (clock nil))
    ;; Check subclass-info
    (is (string= "GClock4" (gobject::subclass-info-gname info)))
    (is (eq 'clock4 (gobject::subclass-info-class info)))
    (is (string= "GObject" (gobject::subclass-info-parent info)))
    (is (equal '() (gobject::subclass-info-interfaces info)))
    (is (equal '() (gobject::subclass-info-properties info)))

    ;; Create an instance and access the properties
    (is (typep (setf clock (make-instance 'clock4)) 'clock4))
    (is (typep (clock4-timezone clock) 'local-time::timezone))
    (is (string= "UTC" (clock4-location clock)))))

;;; ----------------------------------------------------------------------------

;;;     Define clock5

(gobject:define-g-object-subclass "GClock5" clock5
  (:superclass g:object
   :export nil
   :interfaces ())
  ((:cl timezone
        :initform local-time:+utc-zone+
        :accessor clock5-timezone)
   (location
    clock5-location
    "location" "gchararray" t t)))

(test clock5-class
  ;; Type check
  (is (g:type-is-object "GClock5"))
  ;; Check the registered symbol
  (is (eq 'clock5
          (glib:symbol-for-gtype "GClock5")))
  ;; Check the type initializer
  ;; no type initializer
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GClock5")))
  ;; Check the children
  (is (equal '()
             (list-children "GClock5")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GClock5")))
  ;; Check the class properties
  (is (equal '("location")
             (list-properties "GClock5")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GClock5")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GClock5" G-CLOCK5
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL)
                               ((LOCATION G-CLOCK5-LOCATION "location"
                                 "gchararray" T T)))
             (gobject:get-g-type-definition "GClock5"))))

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
                           :G-PROPERTY-TYPE "gchararray"
                           :ACCESSOR CLOCK5-LOCATION 
                           :INITARG :LOCATION 
                           :G-PROPERTY-NAME "location"))
               (mapcar (lambda (property)
                         (gobject::property->slot "Clock5" property))
                       properties)))))

(test check-implementation-clock5
  (let ((info (gobject::get-subclass-info "GClock5"))
        (clock nil) (pspec nil))
    ;; Check subclass-info
    (is (string= "GClock5" (gobject::subclass-info-gname info)))
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
    (is-false (g:object-class-find-property "GClock5" "timezone"))
    ;; Get GParamSpec for registered LOCATION property
    (is (cffi:pointerp (setf pspec
                             (g:object-class-find-property "GClock5"
                                                           "location"))))
    ;; TIMEZONE is not registered as property
    (signals (error) (g:object-property clock "timezone"))
    (is (typep (clock5-timezone clock) 'local-time::timezone))
    ;; LOCATION is registered, we can access the property
    (is (string= "UTC" (setf (g:object-property clock "location") "UTC")))
    (is (string= "UTC" (g:object-property clock "location")))
    (is (string= "abc" (setf (clock5-location clock) "abc")))))
  
;;; --- 2023-11-13 -------------------------------------------------------------
