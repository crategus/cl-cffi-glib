(in-package :glib-test)

(def-suite gobject-base :in gobject-suite)
(in-suite gobject-base)

;;; --- Lisp functions ---------------------------------------------------------

;;;     class-property-pspec

(test class-property-pspec
  (let ((pspec nil))
    ;; Get GParamSpec for the gname and check the values
    (is (typep (setf pspec
                     (gobject::class-property-pspec "GApplication" "flags"))
               'gobject::%param-spec))
    (is (string= "flags" (gobject::%param-spec-name pspec)))
    (is (eq (g:gtype "GApplicationFlags") (gobject::%param-spec-type pspec)))
    (is-true (gobject::%param-spec-readable pspec))
    (is-true (gobject::%param-spec-writable pspec))
    (is (eq (g:gtype "GApplication") (gobject::%param-spec-owner-type pspec)))
    ;; Get GParamSpec for the GType
    (is (typep (gobject::class-property-pspec (g:gtype "GApplication") "flags")
               'gobject::%param-spec))
    ;; Results for not registered properties
    (is-false (gobject::class-property-pspec "GApplication" "unknown"))
    (is-false (gobject::class-property-pspec "unknown" "unknown"))))

;;;     class-property-type

(test class-property-type
  (is (eq (g:gtype "GApplicationFlags")
          (gobject::class-property-type "GApplication" "flags")))
  (is (eq (g:gtype "GApplicationFlags")
          (gobject::class-property-type "GApplication"
                                        "flags"
                                        :assert-readable t
                                        :assert-writable t)))
  (is (eq (g:gtype "gboolean")
          (gobject::class-property-type "GApplication" "is-busy")))
  (is (eq (g:gtype "gboolean")
          (gobject::class-property-type "GApplication"
                                        "is-busy"
                                        :assert-readable t)))
  ;; Property is not writable
  (signals (error) (gobject::class-property-type "GApplication"
                                                 "is-busy"
                                                 :assert-writable t))
  ;; Invalid property
  (signals (error) (gobject::class-property-type "GApplication" "unknown"))
  (signals (error) (gobject::class-property-type "unknown" "unknown")))

;;; --- Types and Values -------------------------------------------------------

;;;     GParameter

(test g-parameter-cstruct
  (is (= 32 (cffi:foreign-type-size '(:struct gobject::%parameter))))
  (is (equal '(GOBJECT::NAME GOBJECT:VALUE)
             (cffi:foreign-slot-names '(:struct gobject::%parameter)))))

;;;     GObject

(test g-object-class
  ;; Type check
  (is (g:type-is-object "GObject"))
  ;; Check the registered symbol
  (is (eq 'g:object
          (glib:symbol-for-gtype "GObject")))
  ;; Check the type initializer
  (is (eq (g:gtype "GObject")
          (g:gtype (cffi:foreign-funcall "g_object_get_type" :size)))))

;;; --- Properties -------------------------------------------------------------

(test g-object-properties
  (let ((object (make-instance 'g:simple-action)))
    (is (cffi:pointerp (g:object-pointer object)))
    (is (cffi:pointerp (g:object-pointer object)))
    (is-true (g:object-has-reference object))
    (is (typep (g:object-signal-handlers object) 'array))))

(test g-object-pointer.1
  (let* ((object (make-instance 'g:simple-action))
         (ptr (g:object-pointer object)))
    (is (cffi:pointerp ptr))
    (is (cffi:null-pointer-p (setf (g:object-pointer object) (cffi:null-pointer))))
    (is (cffi:null-pointer-p (g:object-pointer object)))
    (is (cffi:pointerp (setf (g:object-pointer object) ptr)))
    (is (cffi:pointerp (g:object-pointer object)))))

;; Use the abbreviation POINTER for G:OBJECT-POINTER
(test g-object-pointer.2
  (let* ((object (make-instance 'g:simple-action))
         (ptr (glib:pointer object)))
    (is (cffi:pointerp ptr))
    (is (cffi:null-pointer-p (setf (glib:pointer object) (cffi:null-pointer))))
    (is (cffi:null-pointer-p (glib:pointer object)))
    (is (cffi:pointerp (setf (glib:pointer object) ptr)))
    (is (cffi:pointerp (glib:pointer object)))))

;;; --- Signals ----------------------------------------------------------------

(test g-object-signals
  ;; Check the list of signals
  (is (equal '("notify")
             (mapcar #'g:signal-name
                     (g:signal-list-ids "GObject"))))

  ;; Query info for "notify" signal
  (let ((query (g:signal-query (g:signal-lookup "notify" "GObject"))))
    (is (string= "notify" (g:signal-query-signal-name query)))
    (is (string= "GObject" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST :NO-RECURSE :DETAILED :ACTION :NO-HOOKS)
               (g:signal-query-signal-flags query)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GParam")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     g_type_is_object

(test g-type-is-object
  ;; Check for the fundamental types
  (is-false (g:type-is-object "gboolean"))
  (is-false (g:type-is-object "GResource"))
  (is-false (g:type-is-object "gchar"))
  (is-false (g:type-is-object "GChecksum"))
  (is-false (g:type-is-object "gdouble"))
  (is-false (g:type-is-object "GEmblemOrigin"))
  (is-false (g:type-is-object "GApplicationFlags"))
  (is-false (g:type-is-object "gfloat"))
  (is-false (g:type-is-object "GType"))
  (is-false (g:type-is-object "gint"))
  (is-false (g:type-is-object "gint64"))
  (is-false (g:type-is-object "GAction"))
  (is-false (g:type-is-object "unknown"))
  (is-false (g:type-is-object "glong"))
  (is-false (g:type-is-object "void"))
  (is-true  (g:type-is-object "GApplication"))
  (is-false (g:type-is-object "GParamBoolean"))
  (is-false (g:type-is-object "gpointer"))
  (is-false (g:type-is-object "gchararray"))
  (is-false (g:type-is-object "guchar"))
  (is-false (g:type-is-object "guint"))
  (is-false (g:type-is-object "guint64"))
  (is-false (g:type-is-object "gulong"))
  (is-false (g:type-is-object "GVariant"))
  ;; Some more types
  (is-true  (g:type-is-object "GObject"))
  ;; Check for NIL
  (is-false (g:type-is-object nil)))

;;;     g_is_object

(test g-is-object
  (is-true  (g:is-object (make-instance 'g:simple-action)))
  (is-true  (g:is-object (g:object-pointer (make-instance 'g:simple-action))))
  (is-false (g:is-object (g:param-spec-boolean "new" "nick" "blurb" nil nil))))

;;;     g-is-object-class
;;;     g-object-class

;;;     g_object_type

(test g-object-type
  (is-false (g:object-type nil))
  (is (eq (g:gtype "GSimpleAction")
          (g:object-type (make-instance 'g:simple-action))))
  (is (eq (g:gtype "GSimpleAction")
          (g:object-type (g:object-pointer (make-instance 'g:simple-action))))))

;;;     g_object_type_name

(test g-object-type-name
  (is-false (g:object-type-name nil))
  (is (string= "GSimpleAction"
               (g:object-type-name (make-instance 'g:simple-action))))
  (is (string= "GSimpleAction"
               (g:object-type-name
                   (g:object-pointer (make-instance 'g:simple-action))))))

;;;     g-object-class-type
;;;     g-object-class-name
;;;     g_object_class_install_property
;;;     g_object_class_install_properties

;;;     g-object-class-find-property

(test g-object-class-find-property
  (is (g:is-param-spec (g:object-class-find-property "GSimpleAction" "name")))
  (is (g:is-param-spec
          (g:object-class-find-property (g:gtype "GSimpleAction") "name")))
  ;; Returns nil Unknown if the class does not have the property
  (is-false (g:object-class-find-property "GSimpleAction" "xxx"))
  ;; Error when gtype is not GObject type
  (signals (error) (g:object-class-find-property "unknown" "xxx")))

;;;     g_object_class_list_properties

(test g-object-class-list-properties
  (is (equal '("name" "parameter-type" "enabled" "state-type" "state")
             (mapcar #'g:param-spec-name
                     (g:object-class-list-properties "GSimpleAction"))))
  ;; Error when gtype is not GObject type
  (signals (error) (g:object-class-list-properties "unknown")))

;;;     g_object_class_override_property
;;;     g_object_interface_install_property

;;;     g-object-interface-find-property

(test g-object-interface-find-property
  (is (g:is-param-spec (g:object-interface-find-property "GAction" "enabled")))
  (is (g:is-param-spec
          (g:object-interface-find-property (g:gtype "GAction") "enabled")))
  (is-false (g:object-interface-find-property "GAction" "xxx"))
  (signals (error) (g:object-interface-find-property "unknwon" "xxx")))

;;;     g_object_interface_list_properties

(test g-object-interface-list-properties
  (is (equal '("enabled" "name" "parameter-type" "state" "state-type")
             (mapcar #'g:param-spec-name
                     (g:object-interface-list-properties "GAction"))))
  (is (equal '()
             (mapcar #'g:param-spec-name
                     (g:object-interface-list-properties "GActionMap"))))
  (signals (error) (g:object-interface-list-properties "unknown")))

;;;     g-object-new

#+nil
(test g-object-new
  (is (eq (gtype "GtkButton")
          (g:object-type (g:object-new "GtkButton"))))
  (is (eq (gtype "GtkButton")
          (g:object-type (g:object-new "GtkButton" :label "text" :margin 6)))))

;;;     g_object_new_with_properties
;;;     g_object_newv

;;;     g_object_ref
;;;     g_object_unref
;;;     g_object_ref_sink
;;;     g_clear_object

;;;     GInitiallyUnowned
;;;     GInitiallyUnownedClass

;;;     G_TYPE_INITIALLY_UNOWNED

;;;     g_object_is_floating
;;;     g_object_force_floating
;;;     g_object_weak_ref
;;;     g_object_weak_unref
;;;     g_object_add_weak_pointer
;;;     g_object_remove_weak_pointer
;;;     g_object_add_toggle_ref
;;;     g_object_remove_toggle_ref

;;;     g_object_connect
;;;     g_object_disconnect
;;;     g_object_set
;;;     g_object_get

;;;     g-object-notify

#+nil
(test g-object-notify
  (let* ((message nil)
         (button (make-instance 'gtk:button))
         (handler-id (g:signal-connect button "notify::can-default"
                       (lambda (widget pspec)
                         (setf message
                               "Signal notify::can-default in g-object-notify")
                         (is (gtype "GtkButton") (g-object-type widget))
                         (is (g-is-param-spec pspec))))))
    (is (integerp handler-id))
    ;; Notify button
    (is-false (g-object-notify button "can-default"))
    (is (string= "Signal notify::can-default in g-object-notify" message))))

;;;     g_object_notify_by_pspec

;;;     g-object-freeze-notify
;;;     g-object-thaw-notify

#+nil
(test g-object-freeze-notify.1
  (let* ((message nil)
         (button (make-instance 'gtk:button))
         (handler-id (g:signal-connect button "notify::can-default"
                       (lambda (widget pspec)
                         (setf message
                               (append message (list "notify::can-default")))
                         (is (eq (gtype "GtkButton") (g-object-type widget)))
                         (is (g-is-param-spec pspec))))))
    (is (integerp handler-id))
    ;; Freeze the notify signal
    (is-false (g-object-freeze-notify button))
    (setf message (append message (list "freeze")))
    ;; Notify button
    (is-false (g-object-notify button "can-default"))
    ;; Thaw the notify signal
    (setf message (append message (list "thaw")))
    (is-false (g-object-thaw-notify button))
    ;; Check the order of the execution
    (is (equal '("freeze" "thaw" "notify::can-default") message))))

;; Counter sample without g-object-freeze-notify

#+nil
(test g-object-freeze-notify.2
  (let* ((message nil)
         (button (make-instance 'gtk:button))
         (handler-id (g:signal-connect button "notify::can-default"
                         (lambda (widget pspec)
                           (setf message
                                 (append message (list "notify::can-default")))
                           (is (eq (gtype "GtkButton") (g-object-type widget)))
                           (is (g-is-param-spec pspec))))))
    (is (integerp handler-id))
    ;; Freeze the notify signal
;    (g-object-freeze-notify button)
    (setf message (append message (list "freeze")))
    ;; Notify button
    (g-object-notify button "can-default")
    ;; Thaw the notify signal
    (setf message (append message (list "thaw")))
;    (g-object-thaw-notify button)
    ;; Check the order of the execution
    (is (equal '("freeze" "notify::can-default" "thaw") message))))

;;;     g-object-data

(test g-object-data
  (let ((item (make-instance 'g:menu-item)))
    ;; no property
    (is-false (g:object-data item "prop"))
    ;; set integer property
    (is (= 999 (setf (g:object-data item "prop") 999)))
    (is (= 999 (g:object-data item "prop")))
    ;; set Lisp list
    (is (equal '(a b c) (setf (g:object-data item "prop") '(a b c))))
    (is (equal '(a b c) (g:object-data item "prop")))
    ;; set g:object
    (is (eq item (setf (g:object-data item "prop") item)))
    (is (eq item (g:object-data item "prop")))
    ;; remove the association
    (is-false (setf (g:object-data item "prop") nil))
    (is-false (g:object-data item "prop"))))

;;;     g-object-set-data-full

;; FIXME: Reimplement g:object-set-data-full

#+nil
(defvar *data-full-status* nil)

;; Callback function for the destroy notify handler
#+nil
(cffi:defcallback destroy-notify-cb :void ((data :pointer))
  (declare (ignore data))
  (format t "~&in DESTROY-NOTIFY-CB~%")
  (is (string= "destroy-notify-cb"
               (setf *data-full-status* "destroy-notify-cb"))))

#+nil
(test g-object-set-data-full.1
  (let ((action (make-instance 'g:simple-action)))
    ;; Set data on the object with a destroy callback
    (is (= 100 (g:object-set-data-full action
                                       "property"
                                       100
                                       (cffi:callback destroy-notify-cb))))
    ;; Clear the status
    (is-false (setf *data-full-status* nil))
    ;; Get the data
    (is (= 100 (g:object-data action "property")))
    ;; Destroy the data, the callback will be executed
    (is-false (setf (g:object-data action "property") nil))
    ;; Check status
    (is (string= "destroy-notify-cb" *data-full-status*))))

#+nil
(test g-object-set-data-full.2
  (let ((item (make-instance 'g:menu-item)))
    ;; no property
    (is-false (g:object-data item "prop"))
    ;; set integer property
    (is (= 999 (g:object-set-data-full item
                                       "prop"
                                       999
                                       (cffi:callback destroy-notify-cb))))
    (is (= 999 (g:object-data item "prop")))
    ;; set Lisp list
    (is (equal '(a b c)
               (g:object-set-data-full item
                                       "prop"
                                       '(a b c)
                                        (cffi:callback destroy-notify-cb))))
    (is (equal '(a b c) (g:object-data item "prop")))
    ;; set g:object
    (is (eq item (g:object-set-data-full item
                                         "prop"
                                         item
                                         (cffi:callback destroy-notify-cb))))
    (is (eq item (g:object-data item "prop")))
    ;; remove the association
    (is-false (setf (g:object-data item "prop") nil))
    (is-false (g:object-data item "prop"))))


;;;     g-object-steal-data

#+nil
(test g-object-steal-data
  (let ((button (make-instance 'gtk:button)))
    ;; Set the data
    (is (cffi:pointerp (setf (g-object-data button "property") (make-pointer 100))))
    (is (= 100 (cffi:pointer-address (g-object-data button "property"))))
    ;; Steal the data
    (is (= 100 (cffi:pointer-address (g-object-steal-data button "property"))))
    (is (=   0 (cffi:pointer-address (g-object-data button "property"))))))

;;;     g_object_dup_data
;;;     g_object_replace_data
;;;     g_object_get_qdata
;;;     g_object_set_qdata
;;;     g_object_set_qdata_full
;;;     g_object_steal_qdata
;;;     g_object_dup_qdata
;;;     g_object_replace-qdata

;;;     g-object-property

#+nil
(test g-object-property.1
  (let ((obj (make-instance 'gtk:label)))
    (is (= 1.0d0
           (setf (g-object-property (g-object-pointer obj) "angle") 1.0d0)))
    (is (= 1.0d0 (g-object-property (g-object-pointer obj) "angle")))
    (is (eq :start
            (setf (g-object-property (g-object-pointer obj) "ellipsize")
                  :start)))
    (is (eq :start (g-object-property (g-object-pointer obj) "ellipsize")))
    (is (eq :fill
            (setf (g-object-property (g-object-pointer obj) "justify") :fill)))
    (is (eq :fill (g-object-property (g-object-pointer obj) "justify")))
    (is (string= "label"
                 (setf (g-object-property (g-object-pointer obj) "label")
                       "label")))
    (is (string= "label" (g-object-property (g-object-pointer obj) "label")))
    (is (= 10
           (setf (g-object-property (g-object-pointer obj) "max-width-chars")
                 10)))
    (is (= 10 (g-object-property (g-object-pointer obj) "max-width-chars")))))

#+nil
(test g-object-property.2
  (let ((obj (make-instance 'gtk:label)))
    (is (= 1.0d0 (setf (g-object-property obj "angle") 1.0d0)))
    (is (= 1.0d0 (g-object-property obj "angle")))
    (is (eq :start (setf (g-object-property obj "ellipsize") :start)))
    (is (eq :start (g-object-property obj "ellipsize")))
    (is (eq :fill (setf (g-object-property obj "justify") :fill)))
    (is (eq :fill (g-object-property obj "justify")))
    (is (string= "label" (setf (g-object-property obj "label") "label")))
    (is (string= "label" (g-object-property obj "label")))
    (is (= 10 (setf (g-object-property obj "max-width-chars") 10)))
    (is (= 10 (g-object-property obj "max-width-chars")))))

#+nil
(test g-object-property.3
  (let ((obj (make-instance 'gtk:label :label "label")))
    (is (= 0.0d0 (g-object-property obj "angle" "gdouble")))
    (is (= 2.0d0 (setf (g-object-property obj "angle" "gdouble") 2.0d0)))
    (is (= 2.0d0 (g-object-property obj "angle")))))

#+nil
(test g-object-property.4
  (let ((obj (make-instance 'gtk:label :label "label")))
    ;; FIXME: Is  NIL the expected return value?
    (is-false (g-object-property obj "attributes"))))

#+nil
(test g-object-property.5
  (let ((obj (make-instance 'gtk:label :label "label")))
    (is (= 0 (g-object-property obj "cursor-position" "gint")))
    ;; cursor-position is not writable
;    (is (= 2 (setf (g-object-property obj "cursor-position" "gint") 2)))
;    (is (= 2 (g-object-property obj "cursor-position")))
  ))

#+nil
(test g-object-property.6
  (let ((obj (make-instance 'gtk:label :label "label")))
    (is (eq :none (g-object-property obj "ellipsize" "PangoEllipsizeMode")))
    (is (eq :start
            (setf (g-object-property obj "ellipsize" "PangoEllipsizeMode")
                  :start)))
    (is (eq :start (g-object-property obj "ellipsize")))))

#+nil
(test g-object-property.7
  (let ((obj (make-instance 'gtk:label :label "label")))
    (is (eq :left (g-object-property obj "justify")))
    (is (eq :center
            (setf (g-object-property obj "justify" "GtkJustification")
                  :center)))
    (is (eq :center (g-object-property obj "justify")))))

#+nil
(test g-object-property.8
  (let ((obj (make-instance 'gtk:label :label "label")))
    (is (string= "label" (g-object-property obj "label")))
    (is (string= "text"
                 (setf (g-object-property obj "label" "gchararray") "text")))
    (is (string= "text" (g-object-property obj "label")))))

#+nil
(test g-object-property.9
  (let ((obj (make-instance 'gtk:label :label "label")))
    (is (= -1 (g-object-property obj "max-width-chars" "gint")))
    (is (= 10 (setf (g-object-property obj "max-width-chars" "gint") 10)))
    (is (= 10 (g-object-property obj "max-width-chars")))))

#+nil
(test g-object-property.10
  (let ((obj (make-instance 'gtk:label :label "label")))
    (is (= 16777215 (g-object-property obj "mnemonic-keyval" "guint")))
    ;; mnemonic-keyval is not writable
;    (is (= 10000000
;            (setf (g-object-property obj "mnemonic-keyval" "guint") 10000000)))
;    (is (= 10000000 (g-object-property obj "mnmonic-keyval")))
  ))

;;;     g_object_new_valist
;;;     g_object_set_valist
;;;     g_object_get_valist
;;;     g_object_watch_closure
;;;     g_object_run_dispose

;;;     G_OBJECT_WARN_INVALID_PROPERTY_ID

;;;     GWeakRef
;;;     g_weak_ref_init
;;;     g_weak_ref_clear
;;;     g_weak_ref_get
;;;     g_weak_ref_set
;;;     g_assert_finalize_object

;;; --- 2023-11-4 --------------------------------------------------------------
