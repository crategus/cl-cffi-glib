(in-package :glib-test)

(def-suite gobject-base :in gobject-suite)
(in-suite gobject-base)

(defparameter gobject-base
              '(g:object-pointer
                g:object-has-reference

                gobject::get-gobject-for-pointer
                gobject::get-gobject-for-pointer-strong
                gobject::get-gobject-for-pointer-weak
                gobject::rem-gobject-for-pointer-strong
                gobject::rem-gobject-for-pointer-weak

                gobject::release
                gobject::dispose-carefully
                gobject::activate-gc-hooks
                gobject::register-gobject-for-gc
                gobject::should-ref-sink-at-creation
                gobject::register-gobject
                gobject::get-or-create-gobject-for-pointer
                gobject::create-gobject-from-pointer
                gobject::class-property-pspec
                gobject::class-property-type
                gobject::create-gobject-from-class
                gobject::call-gobject-constructor
                gobject::object-class-get-property
                gobject::object-class-set-property

                g:type-is-object
                g:is-object
                g:object-class-find-property
                g:object-class-list-properties
                g:object-interface-find-property
                g:object-interface-list-properties
                g:object-new
                g:object-ref
                g:object-ref-count
                g:object-unref
                g:object-notify
                g:object-freeze-notify
                g:object-thaw-notify
                g:object-data
                g:object-set-data-full
                g:object-property
              ))

(export 'gobject-base)

;;; --- Lisp functions ---------------------------------------------------------

;;;     class-property-pspec

(test class-property-pspec
  (let ((pspec nil))
    ;; Get GParamSpec for the gname and check values
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
  ;; Check type
  (is (g:type-is-object "GObject"))
  ;; Check registered symbol
  (is (eq 'g:object
          (glib:symbol-for-gtype "GObject")))
  ;; Check type initializer
  (is (eq (g:gtype "GObject")
          (g:gtype (cffi:foreign-funcall "g_object_get_type" :size))))
  ;; Check signals
  (is (equal '("notify")
             (glib-test:list-signals "GObject"))))

;;; --- Properties -------------------------------------------------------------

(test g-object-properties
  (let ((object (make-instance 'g:simple-action)))
    (is (cffi:pointerp (g:object-pointer object)))
    (is (cffi:pointerp (glib:pointer object)))
    (is-true (gobject::object-has-reference object))))

;;; --- Signals ----------------------------------------------------------------

(test g-object-notify-signal
  (let* ((name "notify")
         (gtype (g:gtype "GObject"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :DETAILED :NO-HOOKS :NO-RECURSE :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GParam")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

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

;;;     g_object_class_install_property
;;;     g_object_class_install_properties

;;;     g_object_class_find_property

(test g-object-class-find-property
  (is (g:is-param-spec (g:object-class-find-property "GSimpleAction" "name")))
  (is (g:is-param-spec
          (g:object-class-find-property (g:gtype "GSimpleAction") "name")))
  ;; Returns nil if the class does not have the property
  (is-false (g:object-class-find-property "GSimpleAction" "xxx"))
  ;; Error when gtype is not GObject type
  (signals (error) (g:object-class-find-property "GAction" "name"))
  (signals (error) (g:object-class-find-property "unknown" "xxx")))

;;;     g_object_class_list_properties

(test g-object-class-list-properties
  (is (equal '("name" "parameter-type" "enabled" "state-type" "state")
             (mapcar #'g:param-spec-name
                     (g:object-class-list-properties "GSimpleAction"))))
  ;; Error when gtype is not GObject type
  (signals (error) (g:object-class-list-properties "GAction"))
;  (signals (error) (g:object-class-list-properties "unknown"))
)

;;;     g_object_class_override_property
;;;     g_object_interface_install_property

;;;     g_object_interface_find_property

(test g-object-interface-find-property
  (is (g:is-param-spec (g:object-interface-find-property "GAction" "enabled")))
  (is (g:is-param-spec
          (g:object-interface-find-property (g:gtype "GAction") "enabled")))
  (is-false (g:object-interface-find-property "GAction" "xxx"))
  ;; Error if gtype is not a GInterface type
  (signals (error) (g:object-interface-find-property "GSimpleAction" "name"))
;  (signals (error) (g:object-interface-find-property "unknwon" "xxx"))
)

;;;     g_object_interface_list_properties

(test g-object-interface-list-properties
  (is (equal '("enabled" "name" "parameter-type" "state" "state-type")
             (mapcar #'g:param-spec-name
                     (g:object-interface-list-properties "GAction"))))
  (is (equal '()
             (mapcar #'g:param-spec-name
                     (g:object-interface-list-properties "GActionMap"))))
;  (signals (error) (g:object-interface-list-properties "unknown"))
)

;;;     g_object_new

(test g-object-new
  (let (action)
    (is (eq (g:gtype "GSimpleAction")
            (g:type-from-instance (g:object-new "GSimpleAction"))))
    (is (eq (g:gtype "GSimpleAction")
            (g:type-from-instance (setf action
                                        (g:object-new "GSimpleAction"
                                                      :name "action"
                                                      :enabled nil)))))
    (is (string= "action" (g:simple-action-name action)))
    (is-false (g:simple-action-enabled action))))

;;;     g_object_new_with_properties

;;;     g_object_ref
;;;     g:object-ref-count
;;;     g_object_unref

(test g-object-ref/unref.1
  (let ((action (make-instance 'g:simple-action)))
    (is (= 1 (g:object-ref-count action)))
    (is (eq action (g:object-ref action)))
    (is (= 2 (g:object-ref-count action)))
    (is-false (g:object-unref action))
    (is (= 1 (g:object-ref-count action)))))

(test g-object-ref/unref.2
  (let* ((action (make-instance 'g:simple-action))
         (pointer (g:object-pointer action)))
    (is (= 1 (g:object-ref-count pointer)))
    (is (eq action (g:object-ref pointer)))
    (is (= 2 (g:object-ref-count pointer)))
    (is-false (g:object-unref pointer))
    (is (= 1 (g:object-ref-count pointer)))))

;;;     g_object_ref_sink

;;;     GInitiallyUnowned

;;;     g_object_add_toggle_ref
;;;     g_object_remove_toggle_ref

;;;     g_object_notify

(test g-object-notify
  (let* ((message nil)
         (action (make-instance 'g:simple-action))
         (handler-id (g:signal-connect action "notify::name"
                         (lambda (object pspec)
                           (setf message
                                 "Signal notify::name emitted")
                           (is (eq (g:gtype "GSimpleAction")
                                   (g:type-from-instance object)))
                           (is (g:is-param-spec pspec))
                           (is (string= "name" (g:param-spec-name pspec)))))))
    (is (integerp handler-id))
    ;; Notify action
    (is-false (g:object-notify action "name"))
    (is (string= "Signal notify::name emitted" message))))

;;;     g_object_notify_by_pspec

;;;     g_object_freeze_notify
;;;     g_object_thaw_notify

(test g-object-freeze-notify.1
  (let* ((message nil)
         (action (make-instance 'g:simple-action))
         (handler-id (g:signal-connect action "notify::name"
                         (lambda (object pspec)
                           (setf message
                                 (append message (list "notify::name")))
                           (is (eq (g:gtype "GSimpleAction")
                                   (g:type-from-instance object)))
                           (is (g:is-param-spec pspec))))))
    (is (integerp handler-id))
    ;; Freeze the notify signal
    (is-false (g:object-freeze-notify action))
    (setf message (append message (list "freeze")))
    ;; Notify button
    (is-false (g:object-notify action "name"))
    ;; Thaw the notify signal
    (setf message (append message (list "thaw")))
    (is-false (g:object-thaw-notify action))
    ;; Check order of the execution
    (is (equal '("freeze" "thaw" "notify::name") message))))

;; Counter sample without g-object-freeze-notify

(test g-object-freeze-notify.2
  (let* ((message nil)
         (action (make-instance 'g:simple-action))
         (handler-id (g:signal-connect action "notify::name"
                         (lambda (object pspec)
                           (setf message
                                 (append message (list "notify::name")))
                           (is (eq (g:gtype "GSimpleAction")
                                   (g:type-from-instance object)))
                           (is (g:is-param-spec pspec))))))
    (is (integerp handler-id))
    ;; Freeze the notify signal
;    (g-object-freeze-notify button)
    (setf message (append message (list "freeze")))
    ;; Notify button
    (g:object-notify action "name")
    ;; Thaw the notify signal
    (setf message (append message (list "thaw")))
;    (g-object-thaw-notify button)
    ;; Check order of the execution
    (is (equal '("freeze" "notify::name" "thaw") message))))

;;;     g_object_data

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

;;;     g_object_set_data_full

(test g-object-set-data-full
  (let ((action (make-instance 'g:simple-action))
        (msg nil))
    ;; Set a destroy notify callback on the object
    (is-false (g:object-set-data-full action
                                      "property"
                                      (lambda ()
                                        (setf msg "destroy-notify-cb"))))
    ;; Destroy the data, the callback will be executed
    (is-false (setf (g:object-data action "property") nil))
    ;; Check status
    (is (string= "destroy-notify-cb" msg))))

;;;     g_object_property

(test g-object-property
  (let ((obj (g:simple-action-new "action" "s")))
    ;; Get the property NAME of GSimpleAction
    (is (string= "action" (g:object-property obj "name")))
    ;; We do not pass the object itself, but the pointer
    (is (string= "action" (g:object-property (g:object-pointer obj) "name")))
    ;; Set and get the property ENABLED of GSimpleAction
    (is-false (setf (g:object-property obj "enabled") nil))
    (is-false (g:object-property obj "enabled"))
    ;; We do not pass the object itself, but the pointer
    (is-true (setf (g:object-property (g:object-pointer obj) "enabled") t))
    (is-true (g:object-property (g:object-pointer obj) "enabled"))))

;;; 2024-12-14
