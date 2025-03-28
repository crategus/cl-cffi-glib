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
  (glib-test:with-check-memory (info)
    (is (typep (setf info (g:file-info-new)) 'g:file-info))))

;;;     g_file_info_clear_status
;;;     g_file_info_copy_into
;;;     g_file_info_dup

;;;     g_file_info_get_access_date_time                    Since 2.70
;;;     g_file_info_set_access_date_time                    Since 2.70

(test g-file-info-access-date-time
  (glib-test:with-check-memory (info)
    (is (typep (setf info (g:file-info-new)) 'g:file-info))
    (is (= 0 (setf (g:file-info-access-date-time info) 0)))
    (is (= 0 (g:file-info-access-date-time info)))))

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

;;;     g_file_info_get_modification_time                   Deprecated 2.62

;;;     g_file_info_get_modification_date_time              Since 2.62
;;;     g_file_info_get_name
;;;     g_file_info_get_size

(test g-file-info-name
  (glib-test:with-check-memory (info file)
    (let ((path (glib-sys:sys-path "test/rtest-gio-file.lisp")))
      (setf file (g:file-new-for-path path))
      (setf info (g:file-query-info file "*" :none))

      (is (<= 3944380000 (g:file-info-modification-date-time info)))
      (is (string= "rtest-gio-file.lisp" (g:file-info-name info)))
      (is (= 8335 (g:file-info-size info))))))

;;;     g_file_info_get_sort_order
;;;     g_file_info_get_symbolic_icon
;;;     g_file_info_get_symlink_target

;;;     g_file_info_has_attribute
;;;     g_file_info_has_namespace

(test g-file-info-has-attribute/namespace
  (glib-test:with-check-memory (info file)
    (let ((path (glib-sys:sys-path "test/rtest-gio-file.lisp")))
      (setf file (g:file-new-for-path path))
      (setf info (g:file-query-info file "*" :none))
      ;; Check namespaces
      (is-true (g:file-info-has-namespace info "standard"))
      (is-true (g:file-info-has-namespace info "etag"))
      (is-true (g:file-info-has-namespace info "id"))
      (is-true (g:file-info-has-namespace info "access"))
      (is-true (g:file-info-has-namespace info "time"))
      (is-true (g:file-info-has-namespace info "unix"))
      (is-true (g:file-info-has-namespace info "owner"))
      ;; Check attributes
      (is-true (g:file-info-has-attribute info "standard::type"))
      (is-true (g:file-info-has-attribute info "etag::value"))
      (is-true (g:file-info-has-attribute info "id::file"))
      (is-true (g:file-info-has-attribute info "access::can-read"))
      (is-true (g:file-info-has-attribute info "time::modified"))
      (is-true (g:file-info-has-attribute info "unix::device"))
      (is-true (g:file-info-has-attribute info "owner::user")))))

;;;     g_file_info_list_attributes

(test g-file-info-list-attributes.1
  (glib-test:with-check-memory (info file)
    (let ((path (glib-sys:sys-path "test/rtest-gio-file.lisp")))
      (setf file (g:file-new-for-path path))
      (setf info (g:file-query-info file "standard::*" :none))
      (is (equal '("standard::type"
                   "standard::is-hidden"
                   "standard::is-backup"
                   "standard::is-symlink"
                   "standard::name"
                   "standard::display-name"
                   "standard::edit-name"
                   "standard::copy-name"
                   "standard::icon"
                   "standard::content-type"
                   "standard::fast-content-type"
                   "standard::size"
                   "standard::allocated-size"
                   "standard::symbolic-icon")
       (g:file-info-list-attributes info))))))

#-windows
(test g-file-info-list-attributes.2
  (glib-test:with-check-memory (info file)
    (let ((path (glib-sys:sys-path "test/rtest-gio-file.lisp")))
      (setf file (g:file-new-for-path path))
      (setf info (g:file-query-info file "*" :none))
      (is (equal '("standard::type"
                   "standard::is-hidden"
                   "standard::is-backup"
                   "standard::is-symlink"
                   "standard::name"
                   "standard::display-name"
                   "standard::edit-name"
                   "standard::copy-name"
                   "standard::icon"
                   "standard::content-type"
                   "standard::fast-content-type"
                   "standard::size"
                   "standard::allocated-size"
                   "standard::symbolic-icon"
                   "etag::value"
                   "id::file"
                   "id::filesystem"
                   "access::can-read"
                   "access::can-write"
                   "access::can-execute"
                   "access::can-delete"
                   "access::can-trash"
                   "access::can-rename"
                   "time::modified"
                   "time::modified-usec"
                   "time::access"
                   "time::access-usec"
                   "time::changed"
                   "time::changed-usec"
                   "time::created"
                   "time::created-usec"
                   "time::modified-nsec"
                   "time::access-nsec"
                   "time::created-nsec"
                   "time::changed-nsec"
                   "unix::device"
                   "unix::inode"
                   "unix::mode"
                   "unix::nlink"
                   "unix::uid"
                   "unix::gid"
                   "unix::rdev"
                   "unix::block-size"
                   "unix::blocks"
                   "unix::is-mountpoint"
                   "owner::user"
                   "owner::user-real"
                   "owner::group")
       (g:file-info-list-attributes info))))))

#+windows
(test g-file-info-list-attributes.2
  (glib-test:with-check-memory (info file)
    (let ((path (glib-sys:sys-path "test/rtest-gio-file.lisp")))
      (setf file (g:file-new-for-path path))
      (setf info (g:file-query-info file "*" :none))
      (is (equal '("standard::type" "standard::is-hidden" "standard::is-backup"
                   "standard::is-symlink" "standard::name"
                   "standard::display-name" "standard::edit-name"
                   "standard::copy-name" "standard::icon"
                   "standard::content-type" "standard::fast-content-type"
                   "standard::size" "standard::allocated-size"
                   "standard::symbolic-icon" "etag::value" "id::file"
                   "id::filesystem" "access::can-read" "access::can-write"
                   "access::can-execute" "access::can-delete"
                   "access::can-trash" "access::can-rename" "time::modified"
                   "time::modified-usec" "time::access" "time::access-usec"
                   "time::created" "time::created-usec" "time::modified-nsec"
                   "time::access-nsec" "time::created-nsec" "unix::device"
                   "unix::mode" "unix::nlink" "unix::is-mountpoint"
                   "dos::is-archive" "dos::is-system" "dos::is-mountpoint"
                   "owner::user" "owner::group")
       (g:file-info-list-attributes info))))))

;;;     g_file_info_remove_attribute

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

;;; 2025-3-27
