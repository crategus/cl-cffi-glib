(in-package :glib-test)

(def-suite gio-resource :in gio-suite)
(in-suite gio-resource)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (glib-sys:check-and-create-resources "test/resource/rtest-gio-resource.xml"
                                       :package "cl-cffi-glib"
                                       :sourcedir "test/resource/"
                                       :verbose t))

;;; --- Types and Values -------------------------------------------------------

;;;     GResourceFlags

(test g-resource-flags
  ;; Check type
  (is (g:type-is-flags "GResourceFlags"))
  ;; Check registered symbol
  (is (eq 'g:resource-flags
          (glib:symbol-for-gtype "GResourceFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GResourceFlags")
          (g:gtype (cffi:foreign-funcall "g_resource_flags_get_type" :size))))
  ;; Check names
  (is (equal '("G_RESOURCE_FLAGS_NONE" "G_RESOURCE_FLAGS_COMPRESSED")
             (glib-test:list-flags-item-names "GResourceFlags")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-flags-item-values "GResourceFlags")))
  ;; Check nick names
  (is (equal '("none" "compressed")
             (glib-test:list-flags-item-nicks "GResourceFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GResourceFlags" GIO:RESOURCE-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "g_resource_flags_get_type")
                                     (:NONE 0)
                                     (:COMPRESSED 1))
             (gobject:get-gtype-definition "GResourceFlags"))))

;;;     GResourceLookupFlags

(test g-resource-lookup-flags
  ;; Check type
  (is (g:type-is-flags "GResourceLookupFlags"))
  ;; Check registered symbol
  (is (eq 'g:resource-lookup-flags
          (glib:symbol-for-gtype "GResourceLookupFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GResourceLookupFlags")
          (g:gtype (cffi:foreign-funcall "g_resource_lookup_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("G_RESOURCE_LOOKUP_FLAGS_NONE")
             (glib-test:list-flags-item-names "GResourceLookupFlags")))
  ;; Check values
  (is (equal '(0)
             (glib-test:list-flags-item-values "GResourceLookupFlags")))
  ;; Check nick names
  (is (equal '("none")
             (glib-test:list-flags-item-nicks "GResourceLookupFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GResourceLookupFlags"
                                     GIO:RESOURCE-LOOKUP-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "g_resource_lookup_flags_get_type")
                                     (:NONE 0))
             (gobject:get-gtype-definition "GResourceLookupFlags"))))

;;;     GResource

(test g-resource-boxed
  ;; Check type
  (is (g:type-is-boxed "GResource"))
  ;; Check type initializer
  (is (eq (g:gtype "GResource")
          (g:gtype (cffi:foreign-funcall "g_resource_get_type" :size))))
  ;; Check registered name
  (is (eq 'g:resource
          (glib:symbol-for-gtype "GResource"))))

;;;     GStaticResource
;;;     G_RESOURCE_ERROR
;;;     GResourceError

;;; --- Functions --------------------------------------------------------------

;;;     g_resource_load

(test g-resource-load.1
  (let* ((path (glib-sys:sys-path "test/resource/rtest-gio-resource.gresource"))
         (resource (g:resource-load path)))
    (is (typep resource 'g:resource))))

(test g-resource-load.2
  (signals (error) (g:resource-load "unknown")))

;;;     g_resource_new_from_data
;;;     g_resource_ref
;;;     g_resource_unref

;;;     g_resource_lookup_data

(test g-resource-lookup-data
  (let ((path (glib-sys:sys-path "test/resource/rtest-gio-resource.gresource")))
    (g:with-resource (resource path)
      (is (cffi:pointerp
              (g:resource-lookup-data resource
                                      "/com/crategus/test/ducky.png"
                                      :none)))
      (is (cffi:pointerp
          (g:resource-lookup-data resource
                                  "/com/crategus/test/dialog.ui"
                                  :none))))))

;;;     g_resource_open_stream

;;;     g_resource_enumerate_children

(test g-resource-enumerate-children
  (let ((path (glib-sys:sys-path "test/resource/rtest-gio-resource.gresource")))
    (g:with-resource (resource path)
      (is (equal '("application.ui" "dialog.ui" "dialog2.ui" "ducky.png"
                   "floppybuddy.gif" "gtk-logo-24.png")
                 (sort (g:resource-enumerate-children resource
                                                      "/com/crategus/test"
                                                      :none)
                       #'string<))))))

;;;     g_resource_get_info

(test g-resource-info
  (let ((path (glib-sys:sys-path "test/resource/rtest-gio-resource.gresource")))
    (g:with-resource (resource path)
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
      (is (equal '(1528 0)
                 (multiple-value-list
                     (g:resource-info resource
                                      "/com/crategus/test/application.ui"
                                      :none)))))))

;;;     g_static_resource_init
;;;     g_static_resource_fini
;;;     g_static_resource_get_resource

;;;     g_resources_register
;;;     g_resources_unregister

;;;     g_resources_lookup_data

(test g-resources-lookup-data
  (let ((path (glib-sys:sys-path "test/resource/rtest-gio-resource.gresource")))
    (g:with-resource (resource path)
      (is (cffi:pointerp
              (g:resources-lookup-data "/com/crategus/test/ducky.png"
                                       :none)))
      (is (cffi:pointerp
              (g:resources-lookup-data "/com/crategus/test/dialog.ui"
                                       :none))))))

;;;     g_resources_open_stream

;;;     g_resources_enumerate_children

(test g-resources-enumerate-children
  (let ((path (glib-sys:sys-path "test/resource/rtest-gio-resource.gresource")))
    (g:with-resource (resource path)
      (is (equal '("application.ui" "dialog.ui" "dialog2.ui" "ducky.png"
                   "floppybuddy.gif" "gtk-logo-24.png")
                 (sort (g:resources-enumerate-children "/com/crategus/test"
                                                       :none)
                       #'string<))))))

;;;     g_resources_get_info

(test g-resources-info
  (let ((path (glib-sys:sys-path "test/resource/rtest-gio-resource.gresource")))
    (g:with-resource (resource path)
      (is (equal '(248546 0)
                 (multiple-value-list
                     (g:resources-info "/com/crategus/test/ducky.png"
                                       :none))))
      (is (equal '(5216 0)
                 (multiple-value-list
                     (g:resources-info "/com/crategus/test/floppybuddy.gif"
                                       :none))))
      (is (equal '(1528 0)
                 (multiple-value-list
                     (g:resources-info "/com/crategus/test/application.ui"
                                       :none)))))))

;;; 2024-9-17
