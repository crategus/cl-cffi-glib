(in-package :glib-test)

(def-suite gio-file-info :in gio-suite)
(in-suite gio-file-info)

;;; --- Types and Values -------------------------------------------------------

;;;     GFileInfo

(test g-file-info-class
  ;; Check type
  (is (g:type-is-object "GFileInfo"))
  ;; Check registered symbol
  (is (eq 'gio:file-info
          (glib:symbol-for-gtype "GFileInfo")))
  ;; Check type initializer
  (is (eq (g:gtype "GFileInfo")
          (g:gtype (cffi:foreign-funcall "g_file_info_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GFileInfo")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GFileInfo")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GFileInfo")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GFileInfo")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GFileInfo")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GFileInfo" GIO:FILE-INFO
                       (:SUPERCLASS GOBJECT:OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "g_file_info_get_type")
                        NIL)
             (gobject:get-gtype-definition "GFileInfo"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_file_info_new

(test g-file-info-new
  (let (info)
    (is (typep (setf info (g:file-info-new)) 'g:file-info))
    (is (= 1 (g:object-ref-count info)))))

;;;     g_file_info_clear_status
;;;     g_file_info_copy_into
;;;     g_file_info_dup
;;;     g_file_info_get_access_date_time                    Since 2.70
;;;     g_file_info_get_attribute_as_string
;;;     g_file_info_get_attribute_boolean
;;;     g_file_info_get_attribute_byte_string
;;;     g_file_info_get_attribute_data
;;;     g_file_info_get_attribute_file_path                 Since 2.78
;;;     g_file_info_get_attribute_int32
;;;     g_file_info_get_attribute_int64
;;;     g_file_info_get_attribute_object
;;;     g_file_info_get_attribute_status
;;;     g_file_info_get_attribute_string
;;;     g_file_info_get_attribute_stringv
;;;     g_file_info_get_attribute_type
;;;     g_file_info_get_attribute_uint32
;;;     g_file_info_get_attribute_uint64
;;;     g_file_info_get_content_type
;;;     g_file_info_get_creation_date_time                  Since 2.70
;;;     g_file_info_get_deletion_date
;;;     g_file_info_get_display_name
;;;     g_file_info_get_edit_name
;;;     g_file_info_get_etag
;;;     g_file_info_get_file_type
;;;     g_file_info_get_icon
;;;     g_file_info_get_is_backup
;;;     g_file_info_get_is_hidden
;;;     g_file_info_get_is_symlink
;;;     g_file_info_get_modification_date_time              Since 2.62
;;;     g_file_info_get_modification_time                   Deprecated 2.62
;;;     g_file_info_get_name
;;;     g_file_info_get_size
;;;     g_file_info_get_sort_order
;;;     g_file_info_get_symbolic_icon
;;;     g_file_info_get_symlink_target
;;;     g_file_info_has_attribute
;;;     g_file_info_has_namespace
;;;     g_file_info_list_attributes
;;;     g_file_info_remove_attribute
;;;     g_file_info_set_access_date_time                    Since 2.70
;;;     g_file_info_set_attribute
;;;     g_file_info_set_attribute_boolean
;;;     g_file_info_set_attribute_byte_string
;;;     g_file_info_set_attribute_file_path                 Since 2.78
;;;     g_file_info_set_attribute_int32
;;;     g_file_info_set_attribute_int64
;;;     g_file_info_set_attribute_mask
;;;     g_file_info_set_attribute_object
;;;     g_file_info_set_attribute_status
;;;     g_file_info_set_attribute_string
;;;     g_file_info_set_attribute_stringv
;;;     g_file_info_set_attribute_uint32
;;;     g_file_info_set_attribute_uint64
;;;     g_file_info_set_content_type
;;;     g_file_info_set_creation_date_time                  Since 2.70
;;;     g_file_info_set_display_name
;;;     g_file_info_set_edit_name
;;;     g_file_info_set_file_type
;;;     g_file_info_set_icon
;;;     g_file_info_set_is_hidden
;;;     g_file_info_set_is_symlink
;;;     g_file_info_set_modification_date_time              Since 2.62
;;;     g_file_info_set_modification_time                   Deprecated 2.62
;;;     g_file_info_set_name
;;;     g_file_info_set_size
;;;     g_file_info_set_sort_order
;;;     g_file_info_set_symbolic_icon
;;;     g_file_info_set_symlink_target
;;;     g_file_info_unset_attribute_mask

;;; 2024-9-17
