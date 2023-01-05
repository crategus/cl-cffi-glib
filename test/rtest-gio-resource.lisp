(in-package :glib-test)

(def-suite gio-resource :in gio-suite)
(in-suite gio-resource)

;;; --- Types and Values -------------------------------------------------------

;;;     GResource

(test resource-boxed
  ;; Type check
  (is (g:type-is-a (g:gtype "GResource") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GResource")
          (g:gtype (cffi:foreign-funcall "g_resource_get_type" :size)))))

;;;     GResourceFlags

(test resource-flags
  ;; Check the type
  (is (g:type-is-flags "GResourceFlags"))
  ;; Check the registered symbol
  (is (eq 'g:resource-flags
          (gobject:symbol-for-gtype "GResourceFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GResourceFlags")
          (g:gtype (cffi:foreign-funcall "g_resource_flags_get_type" :size))))
  ;; Check the names
  (is (equal '("G_RESOURCE_FLAGS_NONE" "G_RESOURCE_FLAGS_COMPRESSED")
             (list-flags-item-name "GResourceFlags")))
  ;; Check the values
  (is (equal '(0 1)
             (list-flags-item-value "GResourceFlags")))
  ;; Check the nick names
  (is (equal '("none" "compressed")
             (list-flags-item-nick "GResourceFlags")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GResourceFlags"
                              G-RESOURCE-FLAGS
                              (:EXPORT T)
                              (:NONE 0)
                              (:COMPRESSED 1))
             (get-g-type-definition "GResourceFlags"))))

;;;     GResourceLookupFlags

(test resource-lookup-flags
  ;; Check the type
  (is (g:type-is-flags "GResourceLookupFlags"))
  ;; Check the registered symbol
  (is (eq 'g:resource-lookup-flags
          (gobject:symbol-for-gtype "GResourceLookupFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GResourceLookupFlags")
          (g:gtype (cffi:foreign-funcall "g_resource_lookup_flags_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("G_RESOURCE_LOOKUP_FLAGS_NONE")
             (list-flags-item-name "GResourceLookupFlags")))
  ;; Check the values
  (is (equal '(0)
             (list-flags-item-value "GResourceLookupFlags")))
  ;; Check the nick names
  (is (equal '("none")
             (list-flags-item-nick "GResourceLookupFlags")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GResourceLookupFlags"
                              G-RESOURCE-LOOKUP-FLAGS
                              (:EXPORT T)
                              (:NONE 0))
             (get-g-type-definition "GResourceLookupFlags"))))

;;;     GStaticResource
;;;     G_RESOURCE_ERROR
;;;     GResourceError

;;; --- Functions --------------------------------------------------------------

;;;     g_resource_load

(test resource-load.1
  (let ((resource (g:resource-load
                      (sys-path "test/rtest-gio-resource.gresource"))))
    (is (typep resource 'g:resource))
    (is-false (g:resource-unref resource))))

(test resource-load.2
  (signals (error) (g:resource-load "unknown")))

;;;     g_resource_new_from_data
;;;     g_resource_ref
;;;     g_resource_unref

;;;     g_resource_lookup_data

(test resource-lookup-data
  (let ((resource (g:resource-load
                      (sys-path "test/rtest-gio-resource.gresource"))))
    (is-false (g:resources-register resource))
    (is (cffi:pointerp (g:resource-lookup-data resource
                                          "/com/crategus/test/ducky.png"
                                          :none)))
    (is (cffi:pointerp (g:resource-lookup-data resource
                                          "/com/crategus/test/rtest-dialog.ui"
                                          :none)))
    (is-false (g:resources-unregister resource))))

;;;     g_resource_open_stream

;;;     g_resource_enumerate_children

(test resource-enumerate-children
  (with-g-resource (resource (sys-path "test/rtest-gio-resource.gresource"))
    (is (equal '("ducky.png" "floppybuddy.gif" "gtk-logo-24.png"
                 "rtest-application.ui" "rtest-dialog.ui" "rtest-dialog2.ui")
               (sort (g:resource-enumerate-children resource
                                                    "/com/crategus/test"
                                                    :none)
                     #'string<)))))

;;;     g_resource_get_info

(test resource-info
  (with-g-resource (resource (sys-path "test/rtest-gio-resource.gresource"))
    (is (equal '(248546 0)
               (multiple-value-list
                   (g:resource-info resource
                                    "/com/crategus/test/ducky.png"
                                    :none))))
    (is (equal '(5216 0)
               (multiple-value-list
                   (g:resource-info resource
                                    "/com/crategus/test/floppybuddy.gif"
                                    :none))))
    (is (equal '(1703 0)
               (multiple-value-list
                   (g:resource-info resource
                                    "/com/crategus/test/rtest-application.ui"
                                    :none))))))

;;;     g_static_resource_init
;;;     g_static_resource_fini
;;;     g_static_resource_get_resource

;;;     g_resources_register
;;;     g_resources_unregister

#+gtk4
(test resources-register
  (let ((image nil)
        (resource (g:resource-load
                      (sys-path "test/rtest-gio-resource.gresource"))))
    (is (typep resource 'g:resource))
    ;; Register the resource
    (is-false (g:resources-register resource))
    (is (typep (setf image
                     (gtk:image-new-from-resource "/com/crategus/test/ducky.png"))
               'gtk:image))
    ;; Pixbuf is loaded
    (is (typep (gtk:image-paintable image) 'gdk:texture))
    ;; Unregister the resource
    (is-false (g:resources-unregister resource))
    (is (typep (setf image
                     (gtk:image-new-from-resource "/com/crategus/test/ducky.png"))
               'gtk:image))
    ;; Pixbuf is not loaded
    (is-false (typep (gtk:image-paintable image) 'gdk:texture))
    (is-false (g:resource-unref resource))))

#+gtk3
(test resources-register
  (let ((image nil)
        (resource (g:resource-load
                      (sys-path "test/rtest-gio-resource.gresource"))))
    (is (typep resource 'g:resource))
    ;; Register the resource
    (is-false (g:resources-register resource))
    (is (typep (setf image
                     (gtk:image-new-from-resource "/com/crategus/test/ducky.png"))
               'gtk:image))
    ;; Pixbuf is loaded
    (is (typep (gtk:image-pixbuf image) 'gdk:pixbuf))
    ;; Unregister the resource
    (is-false (g:resources-unregister resource))
    (is (typep (setf image
                     (gtk:image-new-from-resource "/com/crategus/test/ducky.png"))
               'gtk:image))
    ;; Pixbuf is not loaded
    (is-false (typep (gtk:image-pixbuf image) 'gdk:pixbuf))
    (is-false (g:resource-unref resource))))

;;;     g_resources_lookup_data

(test resources-lookup-data
  (with-g-resource (resource (sys-path "test/rtest-gio-resource.gresource"))
    (is (cffi:pointerp (g:resources-lookup-data "/com/crategus/test/ducky.png"
                                           :none)))
    (is (cffi:pointerp (g:resources-lookup-data "/com/crategus/test/rtest-dialog.ui"
                                           :none)))))

;;;     g_resources_open_stream

;;;     g_resources_enumerate_children

(test resources-enumerate-children
  (with-g-resource (resource (sys-path "test/rtest-gio-resource.gresource"))
    (is (equal '("ducky.png" "floppybuddy.gif" "gtk-logo-24.png"
                 "rtest-application.ui" "rtest-dialog.ui" "rtest-dialog2.ui")
               (sort (g:resources-enumerate-children "/com/crategus/test"
                                                     :none)
                     #'string<)))))

;;;     g_resources_get_info

(test resources-info
  (with-g-resource (resource (sys-path "test/rtest-gio-resource.gresource"))
    (is (equal '(248546 0)
               (multiple-value-list
                   (g:resources-info "/com/crategus/test/ducky.png"
                                     :none))))
    (is (equal '(5216 0)
               (multiple-value-list
                   (g:resources-info "/com/crategus/test/floppybuddy.gif"
                                     :none))))
    (is (equal '(1703 0)
               (multiple-value-list
                   (g:resources-info "/com/crategus/test/rtest-application.ui"
                                     :none))))))

;;; --- 2023-1-2 ---------------------------------------------------------------
