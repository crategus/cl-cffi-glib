;;; ----------------------------------------------------------------------------
;;; gio.file-info.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.88 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2026 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Types and Values
;;;
;;;     GFileInfo
;;;     GFileAttributeType
;;;     GFileAttributeStatus                                not implemented
;;;
;;; Functions
;;;
;;;     g_file_info_new
;;;     g_file_info_dup
;;;     g_file_info_copy_into
;;;
;;;     g_file_info_clear_status                            not implemented
;;;     g_file_info_get_attribute_status                    not implemented
;;;     g_file_info_set_attribute_status                    not implemented
;;;
;;;     g_file_info_has_namespace
;;;     g_file_info_has_attribute
;;;     g_file_info_remove_attribute
;;;     g_file_info_list_attributes
;;;
;;;     g_file_info_get_attribute_type
;;;     g_file_info_set_attribute
;;;
;;;     g_file_info_get_attribute_string                    not exported
;;;     g_file_info_set_attribute_string                    not exported
;;;     g_file_info_get_attribute_byte_string               not exported
;;;     g_file_info_set_attribute_byte_string               not exported
;;;     g_file_info_get_attribute_boolean                   not exported
;;;     g_file_info_set_attribute_boolean                   not exported
;;;     g_file_info_get_attribute_uint32                    not exported
;;;     g_file_info_set_attribute_uint32                    not exported
;;;     g_file_info_get_attribute_int32                     not exported
;;;     g_file_info_set_attribute_int32                     not exported
;;;     g_file_info_get_attribute_uint64                    not exported
;;;     g_file_info_set_attribute_uint64                    not exported
;;;     g_file_info_get_attribute_int64                     not exported
;;;     g_file_info_set_attribute_int64                     not exported
;;;     g_file_info_get_attribute_object                    not exported
;;;     g_file_info_set_attribute_object                    not exported
;;;     g_file_info_get_attribute_stringv                   not exported
;;;     g_file_info_set_attribute_stringv                   not exported
;;;
;;;     g_file_info_get_attribute_as_string
;;;     g_file_info_get_attribute_data                      not implemented
;;;
;;;     g_file_info_get_is_hidden
;;;     g_file_info_set_is_hidden
;;;     g_file_info_get_is_symlink
;;;     g_file_info_set_is_symlink
;;;     g_file_info_get_is_backup
;;;
;;;     g_file_info_get_attribute_file_path                 Since 2.78
;;;     g_file_info_set_attribute_file_path                 Since 2.78


;;;     g_file_info_get_creation_date_time                  Since 2.70
;;;     g_file_info_set_creation_date_time                  Since 2.70

;;;     g_file_info_get_modification_date_time
;;;     g_file_info_set_modification_date_time
;;;     g_file_info_set_modification_time                   Deprecated 2.62
;;;     g_file_info_get_modification_time                   Deprecated 2.62

;;;     g_file_info_get_access_date_time                    Since 2.70
;;;     g_file_info_set_access_date_time                    Since 2.70

;;;     g_file_info_get_deletion_date


;;;     g_file_info_get_content_type
;;;     g_file_info_set_content_type

;;;     g_file_info_get_display_name
;;;     g_file_info_get_edit_name
;;;     g_file_info_get_etag
;;;     g_file_info_get_file_type

;;;     g_file_info_get_icon
;;;     g_file_info_set_icon


;;;     g_file_info_get_name
;;;     g_file_info_set_name
;;;     g_file_info_get_size
;;;     g_file_info_set_size
;;;     g_file_info_get_sort_order
;;;     g_file_info_set_sort_order
;;;     g_file_info_get_symbolic_icon
;;;     g_file_info_set_symbolic_icon
;;;     g_file_info_get_symlink_target
;;;     g_file_info_set_symlink_target

;;;     g_file_info_set_display_name
;;;     g_file_info_set_edit_name
;;;     g_file_info_set_file_type

;;;
;;;     g_file_info_set_attribute_mask                      not implemented
;;;     g_file_info_unset_attribute_mask                    not implemented
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GFileAttributeType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GFileAttributeType" file-attribute-type
  (:export t
   :type-initializer "g_file_attribute_type_get_type")
  (:invalid 0)
  (:string 1)
  (:byte-string 2)
  (:boolean 3)
  (:uint32 4)
  (:int32 5)
  (:uint64 6)
  (:int64 7)
  (:object 8)
  (:stringv 9))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-attribute-type)
      "GEnum"
      (liber:symbol-documentation 'file-attribute-type)
 "@version{2026-03-22}
  @begin{declaration}
(gobject:define-genum \"GFileAttributeType\" file-attribute-type
  (:export t
   :type-initializer \"g_file_attribute_type_get_type\")
  (:invalid 0)
  (:string 1)
  (:byte-string 2)
  (:boolean 3)
  (:uint32 4)
  (:int32 5)
  (:uint64 6)
  (:int64 7)
  (:object 8)
  (:stringv 9))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:invalid]{Indicates an invalid or uninitialized type.}
      @entry[:string]{A UTF8 string.}
      @entry[:byte-string]{A string of non-zero bytes.}
      @entry[:boolean]{A boolean value.}
      @entry[:uint32]{An unsigned 4-byte/32-bit integer.}
      @entry[:int32]{A signed 4-byte/32-bit integer.}
      @entry[:uint64]{An unsigned 8-byte/64-bit integer.}
      @entry[:int64]{A signed 8-byte/64-bit integer.}
      @entry[:object]{A @class{g:object} object.}
      @entry[:stringv]{An array of strings.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The data types for file attributes.
  @end{short}
  @see-class{g:file-info}")

(export 'file-attribute-type)

;;; ----------------------------------------------------------------------------
;;; GFileAttributeStatus
;;; ----------------------------------------------------------------------------

#+nil
(gobject:define-genum "GFileAttributeStatus" file-attribute-status
  (:export t
   :type-initializer "g_file_attribute_status_get_type")
  (:unset 0)
  (:set 1)
  (:error-setting 2))

#+nil
(setf (liber:alias-for-symbol 'file-attribute-status)
      "GEnum"
      (liber:symbol-documentation 'file-attribute-status)
 "@version{2026-03-22}
  @begin{declaration}
(gobject:define-genum \"GFileAttributeStatus\" file-attribute-status
  (:export t
   :type-initializer \"g_file_attribute_status_get_type\")
  (:unset 0)
  (:set 1)
  (:error-setting 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:unset]{}
      @entry[:set]{}
      @entry[:error-setting]{}
    @end{simple-table}
  @end{values}
  @begin{short}

  @end{short}
  @see-class{g:file-info}")

#+nil
(export 'file-attribute-type)

;;; ----------------------------------------------------------------------------
;;; GFileInfo
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GFileInfo" file-info
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_file_info_get_type")
  nil)

#+liber-documentation
(setf (documentation 'file-info 'type)
 "@version{2026-03-22}
  @begin{short}
    The @class{g:file-info} class implements methods for getting information
    that all files should contain, and allows for manipulation of extended
    attributes.
  @end{short}
  See the file attributes document for more information on how GIO handles file
  attributes. To obtain a @class{g:file-info} object for a @class{g:file}
  object, use the @fun{g:file-query-info} function.

  To change the actual attributes of a file, you should then set the attribute
  in the @class{g:file-info} object and call the
  @fun{g:file-set-attributes-from-info} function on a @class{g:file} instance.
  However, not all attributes can be changed in the file. For instance, the
  actual size of a file cannot be changed via the @fun{g:file-info-size}
  function.

  It is an error to call these accessors without specifying their required file
  attributes when creating the @class{g:file-info} instance. Use the
  @fun{g:file-info-has-attribute} or @fun{g:file-info-list-attributes} functions
  to check what attributes are specified for a @class{g:file-info} instance.
  @see-constructor{g:file-info-new}
  @see-constructor{g:file-info-dup}
  @see-class{g:file}")

;;; ----------------------------------------------------------------------------
;;; g_file_info_new
;;; ----------------------------------------------------------------------------

(defun file-info-new ()
 #+liber-documentation
 "@version{2026-03-22}
  @return{The newly created @class{g:file-info} instance.}
  @short{Creates a new @class{g:file-info} instance.}
  @see-class{g:file-info}"
  (make-instance 'file-info))

(export 'file-info-new)

;;; ----------------------------------------------------------------------------
;;; g_file_info_dup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_dup" file-info-dup)
    (gobject:object file-info :return)
 "@version{2026-03-22}
  @argument[info]{a @class{g:file-info} instance}
  @return{The new duplicate of @arg{info}.}
  @short{Duplicates a file info.}
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-dup)

;;; ----------------------------------------------------------------------------
;;; g_file_info_copy_into
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_copy_into" file-info-copy-into) :void
 "@version{2026-03-22}
  @argument[src]{a @class{g:file-info} instance}
  @argument[dest]{another @class{g:file-info} instance to copy attributes to}
  @begin{short}
    First clears all of the file attributes of @arg{dest}, and then copies all
    of the file attributes from @arg{src} to @arg{dest}.
  @end{short}
  @see-class{g:file-info}"
  (src (gobject:object file-info))
  (dest (gobject:object file-info)))

(export 'file-info-copy-into)

;;; ----------------------------------------------------------------------------
;;; g_file_info_clear_status                                not implemented
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_file_info_clear_status" file-info-clear-status) :void
 "@version{2026-03-22}
  @argument[info]{a @class{g:file-info} instance}
  @short{Clears the status information from @arg{info}.}
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

#+nil
(export 'file-info-clear-status)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_status                        not implemented
;;; g_file_info_get_attribute_status
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("g_file_info_get_attribute_status" file-info-attribute-status)
    file-attribute-status
  (info (gobject:object file-info))
  (attribute :string))

#+nil
(export 'file-info-attribute-status)

;;; ----------------------------------------------------------------------------
;;; g_file_info_has_namespace
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_has_namespace" file-info-has-namespace) :boolean
 #+liber-documentation
 "@version{2026-03-22}
  @argument[info]{a @class{g:file-info} object}
  @argument[namespace]{a string for the file attribute namespace}
  @begin{return}
    @em{True} if @arg{info} has an attribute in @arg{namespace}, @code{nil}
    otherwise.
  @end{return}
  @begin{short}
    Checks if a file info has an attribute in the specified @arg{namespace}.
  @end{short}
  @see-class{g:file-info}"
  (info (gobject:object file-info))
  (namespace :string))

(export 'file-info-has-namespace)

;;; ----------------------------------------------------------------------------
;;; g_file_info_has_attribute
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_has_attribute" file-info-has-attribute) :boolean
 #+liber-documentation
 "@version{2026-03-22}
  @argument[info]{a @class{g:file-info} object}
  @argument[attribute]{a string for the file attribute key}
  @begin{return}
    @em{True} if @arg{info} has an attribute key named @arg{attribute}, @code{nil}
    otherwise.
  @end{return}
  @begin{short}
    Checks if a file info has an attribute key named @arg{attribute}.
  @end{short}
  @see-class{g:file-info}"
  (info (gobject:object file-info))
  (attribute :string))

(export 'file-info-has-attribute)

;;; ----------------------------------------------------------------------------
;;; g_file_info_remove_attribute
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_remove_attribute" file-info-remove-attribute) :void
 #+liber-documentation
 "@version{2026-03-22}
  @argument[info]{a @class{g:file-info} object}
  @argument[attribute]{a string for the file attribute key}
  @begin{short}
    Removes all cases of @arg{attribute} from @arg{info} if it exists.
  @end{short}
  @see-class{g:file-info}"
  (info (gobject:object file-info))
  (attribute :string))

(export 'file-info-remove-attribute)

;;; ----------------------------------------------------------------------------
;;; g_file_info_list_attributes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_list_attributes" %file-info-list-attributes)
    (glib:strv-t :free-from-foreign t)
  (info (gobject:object file-info))
  (namespace :string))

(defun file-info-list-attributes (info &optional namespace)
 #+liber-documentation
 "@version{2026-03-22}
  @argument[info]{a @class{g:file-info} object}
  @argument[namespace]{a string for the namespace of the file attribute key,
    or @code{nil} to list all attributes}
  @begin{return}
    The list of strings of all of the possible attribute types for the given
    @arg{namespace}, or @code{nil} on error.
  @end{return}
  @begin{short}
    List the attributes of the file info.
  @end{short}
  @begin[Examples]{dictionary}
    The list of attributes for a Lisp file on a Ubuntu system:
    @begin{pre}
(let* ((path (glib-sys:sys-path \"myfile.lisp\"))
       (file (g:file-new-for-path path))
       (info (g:file-query-info file \"standard::*\" :none)))
  (g:file-info-list-attributes info))
=> '(\"standard::type\" \"standard::is-hidden\" \"standard::is-backup\"
     \"standard::is-symlink\" \"standard::name\" \"standard::display-name\"
     \"standard::edit-name\" \"standard::copy-name\" \"standard::icon\"
     \"standard::content-type\" \"standard::fast-content-type\"
     \"standard::size\" \"standard::allocated-size\"
     \"standard::symbolic-icon\")
    @end{pre}
  @end{dictionary}
  @see-class{g:file-info}"
  (%file-info-list-attributes info (or namespace (cffi:null-pointer))))

(export 'file-info-list-attributes)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_attribute_type" file-info-attribute-type)
    file-attribute-type
 #+liber-documentation
 "@version{2026-03-22}
  @argument[info]{a @class{g:file-info} instance}
  @argument[attribute]{a string for a file attribute key}
  @begin{short}
    Gets the attribute type for an attribute key.
  @end{short}
  @see-class{g:file-info}"
  (info (gobject:object file-info))
  (attribute :string))

(export 'file-info-attribute-type)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute) (value info attribute)
  (let ((atype (file-info-attribute-type info attribute)))
    (cond ((eq :invalid atype)
           (warn "G:FILE-INFO-ATTRIBUTE: Invalid or uninitialized type."))
          ((eq :string atype)
           (setf (file-info-attribute-string info attribute) value))
          ((eq :byte-string atype)
           (setf (file-info-attribute-byte-string info attribute) value))
          ((eq :boolean atype)
           (setf (file-info-attribute-boolean info attribute) value))
          ((eq :uint32 atype)
           (setf (file-info-attribute-uint32 info attribute) value))
          ((eq :int32 atype)
           (setf (file-info-attribute-int32 info attribute) value))
          ((eq :uint64 atype)
           (setf (file-info-attribute-uint64 info attribute) value))
          ((eq :int64 atype)
           (setf (file-info-attribute-int64 info attribute) value))
          ((eq :object atype)
           (setf (file-info-attribute-object info attribute) value))
          ((eq :stringv atype)
           (setf (file-info-attribute-stringv info attribute) value))
          (t
           (warn "G:FILE-INFO-ATTRIBUTE: Unkown attribute type.")))))

(defun file-info-attribute (info attribute)
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-attribute info attribute) => value}
  @syntax{(setf (g:file-info-attribute info attribute) value)}
  @argument[info]{a @class{g:file-info} instance}
  @argument[attribute]{a string for the file attribute key}
  @argument[value]{a value for the attribute key}
  @begin{short}
    Gets or sets the value of the attribute key.
  @end{short}
  This function replaces the C functions for the various attribute types. The
  attribute type is determined by calling the @fun{g:file-info-attribute-type}
  function for @arg{attribute}. The corresponding function is then called to
  obtain or set the attribute value.
  @begin[Notes]{dictionary}
    The values for the date and time key attributes are integers representing
    the Unix UTC time and not the Lisp universal time. See the
    @fun{g:file-info-creation-date-time}, @fun{g:file-info-access-date-time} and
    @fun{g:file-info-modification-date-time} functions, which return the Lisp universal
    time.
  @end{dictionary}
  @see-class{g:file-info}
  @see-function{g:file-info-creation-date-time}
  @see-function{g:file-info-access-date-time}
  @see-function{g:file-info-modification-date-time}"
  (let ((atype (file-info-attribute-type info attribute)))
    (cond ((eq :invalid atype)
           (warn "G:FILE-INFO-ATTRIBUTE: Invalid or uninitialized type."))
          ((eq :string atype)
           (file-info-attribute-string info attribute))
          ((eq :byte-string atype)
           (file-info-attribute-byte-string info attribute))
          ((eq :boolean atype)
           (file-info-attribute-boolean info attribute))
          ((eq :uint32 atype)
           (file-info-attribute-uint32 info attribute))
          ((eq :int32 atype)
           (file-info-attribute-int32 info attribute))
          ((eq :uint64 atype)
           (file-info-attribute-uint64 info attribute))
          ((eq :int64 atype)
           (file-info-attribute-int64 info attribute))
          ((eq :object atype)
           (file-info-attribute-object info attribute))
          ((eq :stringv atype)
           (file-info-attribute-stringv info attribute))
          (t
           (warn "G:FILE-INFO-ATTRIBUTE: Unkown attribute type.")))))

(export 'file-info-attribute)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_string                        not exported
;;; g_file_info_get_attribute_string
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute-string) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_string"
                        (gobject:object file-info) info
                        :string attribute
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_attribute_string" file-info-attribute-string)
    (:string :free-from-foreign nil)
  (info (gobject:object file-info))
  (attribute :string))

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_byte_string                   not exported
;;; g_file_info_get_attribute_byte_string
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute-byte-string) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_byte_string"
                        (gobject:object file-info) info
                        :string attribute
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_attribute_byte_string"
               file-info-attribute-byte-string) :string
  (info (gobject:object file-info))
  (attribute :string))

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_boolean                       not exported
;;; g_file_info_get_attribute_boolean
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute-boolean) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_boolean"
                        (gobject:object) info
                        :string attribute
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_attribute_boolean"
               file-info-attribute-boolean) :boolean
  (info (gobject:object file-info))
  (attribute :string))

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_uint32                        not exported
;;; g_file_info_get_attribute_uint32
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute-uint32) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_uint32"
                        (gobject:object file-info) info
                        :string attribute
                        :uint32 value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_attribute_uint32" file-info-attribute-uint32)
    :uint32
  (info (gobject:object file-info))
  (attribute :string))

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_int32                         not exported
;;; g_file_info_get_attribute_int32
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute-int32) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_int32"
                        (gobject:object file-info) info
                        :string attribute
                        :int32 value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_attribute_int32" file-info-attribute-int32)
    :int32
  (info (gobject:object file-info))
  (attribute :string))

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_uint64                        not exported
;;; g_file_info_get_attribute_uint64
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute-uint64) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_uint64"
                        (gobject:object file-info) info
                        :string attribute
                        :uint64 value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_attribute_uint64" file-info-attribute-uint64)
    :uint64
  (info (gobject:object file-info))
  (attribute :string))

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_int64                         not exported
;;; g_file_info_get_attribute_int64
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute-int64) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_int64"
                        (gobject:object file-info) info
                        :string attribute
                        :int64 value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_attribute_int64" file-info-attribute-int64)
    :int64
  (info (gobject:object file-info))
  (attribute :string))

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_object                        not exported
;;; g_file_info_get_attribute_object
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute-object) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_object"
                        (gobject:object file-info) info
                        :string attribute
                        gobject:object value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_attribute_object" file-info-attribute-object)
    gobject:object
  (info (gobject:object file-info))
  (attribute :string))

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_stringv                       not exported
;;; g_file_info_get_attribute_stringv
;;; ----------------------------------------------------------------------------

(defun (setf file-info-attribute-stringv) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_stringv"
                        (gobject:object file-info) info
                        :string attribute
                        glib:strv-t value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_attribute_stringv" file-info-attribute-stringv)
    (glib:strv-t :free-from-foreign nil)
  (info (gobject:object file-info))
  (attribute :string))

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_as_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_attribute_as_string"
               file-info-attribute-as-string) :string
 #+liber-documentation
 "@version{2026-03-22}
  @argument[info]{a @class{g:file-info} instance}
  @argument[attribute]{a string for a file attribute key}
  @begin{short}
    Gets the value of an attribute key, formatted as a human readable string.
  @end{short}
  This escapes things as needed to make the string valid UTF-8 and readable by
  humans. It is not meant to be a machine readable or reversible escaping
  format.
  @see-class{g:file-info}"
  (info (gobject:object file-info))
  (attribute :string))

(export 'file-info-attribute-as-string)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_data                          not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_is_hidden
;;; g_file_info_get_is_hidden
;;; ----------------------------------------------------------------------------

(defun (setf file-info-is-hidden) (value info)
  (cffi:foreign-funcall "g_file_info_set_is_hidden"
                        (gobject:object file-info) info
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_is_hidden" file-info-is-hidden) :boolean
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-is-hidden info) => setting}
  @syntax{(setf (g:file-info-is-hidden info) setting)}
  @argument[info]{a @class{g:file-info} instance}
  @argument[setting]{a boolean for the @code{\"standard::is-hidden\"} attribute key}
  @begin{short}
    Gets or sets whether a file is hidden.
  @end{short}
  It is an error to retrieve @arg{setting} if @arg{info} does not contain
  the @code{\"standard::is-hidden\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-is-hidden)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_is_backup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_is_backup" file-info-is-backup) :boolean
 #+liber-documentation
 "@version{2026-03-22}
  @argument[info]{a @class{g:file-info} instance}
  @begin{short}
    Checks if a file is a backup file.
  @end{short}
  The exact semantics of what constitutes a backup file are backend-specific.
  For local files, a file is considered a backup if its name ends with ~ and it
  is a regular file. This follows the POSIX convention used by text editors such
  as Emacs.

  It is an error to call this if @arg{info} does not contain the
  @code{\"standard::is-backup\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-is-backup)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_is_symlink
;;; g_file_info_get_is_symlink
;;; ----------------------------------------------------------------------------

(defun (setf file-info-is-symlink) (value info)
  (cffi:foreign-funcall "g_file_info_set_is_symlink"
                        (gobject:object file-info) info
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_is_symlink" file-info-is-symlink) :boolean
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-is-symlink info) => setting}
  @syntax{(setf (g:file-info-is-symlink info) setting)}
  @argument[info]{a @class{g:file-info} object}
  @argument[setting]{a boolean for the @code{\"standard::symlink\"} attribute key}
  @begin{short}
    Gets or sets whether a file is a symlink.
  @end{short}
  It is an error to retrieve @arg{setting} if @arg{info} does not contain
  the @code{\"standard::is-symlink\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-is-symlink)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_name
;;; g_file_info_get_name
;;; ----------------------------------------------------------------------------

(defun (setf file-info-name) (value info)
  (cffi:foreign-funcall "g_file_info_set_name"
                        (gobject:object file-info) info
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_name" file-info-name) :string
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-name info) => name}
  @syntax{(setf (g:file-info-name info) name)}
  @argument[info]{a @class{g:file-info} object}
  @argument[name]{a string for the name}
  @begin{short}
    Gets or sets the name for a given @arg{info}.
  @end{short}
  It is an error to retrieve the name if @arg{info} does not contain
  the @code{\"standard::name\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-name)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_display_name
;;; g_file_info_get_display_name
;;; ----------------------------------------------------------------------------

(defun (setf file-info-display-name) (value info)
  (cffi:foreign-funcall "g_file_info_set_display_name"
                        (gobject:object file-info) info
                        :string value
                        :void)
    value)

(cffi:defcfun ("g_file_info_get_display_name" file-info-display-name) :string
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-display-name info) => name}
  @syntax{(setf (g:file-info-display-name info) name)}
  @argument[info]{a @class{g:file-info} object}
  @argument[name]{a string for the display name}
  @begin{short}
    Gets or sets the display name for a given @arg{info}.
  @end{short}
  It is an error to retrieve the name if @arg{info} does not contain
  the @code{\"standard::display-name\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-display-name)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_edit_name
;;; g_file_info_get_edit_name
;;; ----------------------------------------------------------------------------

(defun (setf file-info-edit-name) (value info)
  (cffi:foreign-funcall "g_file_info_set_edit_name"
                        (gobject:object file-info) info
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_edit_name" file-info-edit-name) :string
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-edit-name info) => name}
  @syntax{(setf (g:file-info-edit-name info) name)}
  @argument[info]{a @class{g:file-info} object}
  @argument[name]{a string for the edit name}
  @begin{short}
    Gets or sets the edit name for a given @arg{info}.
  @end{short}
  It is an error to retrieve the name if @arg{info} does not contain
  the @code{\"standard::edit-name\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-edit-name)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_icon
;;; g_file_info_get_icon
;;; ----------------------------------------------------------------------------

(defun (setf file-info-icon) (icon info)
  (cffi:foreign-funcall "g_file_info_set_icon"
                        (gobject:object file-info) info
                        (gobject:object icon) icon
                        :void)
  icon)

(cffi:defcfun ("g_file_info_get_icon" file-info-icon) (gobject:object icon)
 "@version{2026-03-22}
  @syntax{(g:file-info-icon info) => icon}
  @syntax{(setf (g:file-info-icon info) icon)}
  @argument[info]{a @class{g:file-info} object}
  @argument[icon]{a @class{g:icon} instance }
  @begin{short}
    Gets or sets the icon for a given @arg{info}.
  @end{short}
  It is an error to call this if the @class{g:file-info} instance does not
  contain @code{\"standard::icon\"} attribute key.
  @see-class{g:file-info}
  @see-class{g:icon}"
  (info (gobject:object file-info)))

(export 'file-info-icon)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_symbolic_icon
;;; g_file_info_get_symbolic_icon
;;; ----------------------------------------------------------------------------

(defun (setf file-info-symbolic-icon) (value info)
  (cffi:foreign-funcall "g_file_info_set_symbolic_icon"
                        (gobject:object file-info) info
                        (gobject:object icon) value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_symbolic_icon" file-info-symbolic-icon)
    (gobject:object icon)
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-symbolic-icon info) => icon}
  @syntax{(setf (g:file-info-symbolic-icon info) icon)}
  @argument[info]{a @class{g:file-info} object}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Gets or sets the symbolic icon for a given @arg{info}.
  @end{short}
  It is an error to retrieve the symbolic icon if @arg{info} does not contain
  the @code{\"standard::symbolic-icon\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-symbolic-icon)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_content_type
;;; g_file_info_get_content_type
;;; ----------------------------------------------------------------------------

(defun (setf file-info-content-type) (value info)
  (cffi:foreign-funcall "g_file_info_set_content_type"
                        (gobject:object file-info) info
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_content_type" file-info-content-type) :string
 "@version{2026-03-22}
  @syntax{(g:file-info-content-type info) => ctype}
  @syntax{(setf (g:file-info-content-type info) ctype)}
  @argument[info]{a @class{g:file-info} instance}
  @argument[ctype]{a string for the content type}
  @begin{short}
    Gets or sets the content type of the file.
  @end{short}
  It is an error to call this function if @arg{info} does not contain the
  @code{\"standard::content-type\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-content-type)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_size
;;; g_file_info_get_size
;;; ----------------------------------------------------------------------------

(defun (setf file-info-size) (value info)
  (cffi:foreign-funcall "g_file_info_set_size"
                        (gobject:object file-info) info
                        :offset value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_size" file-info-size) :offset
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-size info) => size}
  @syntax{(setf (g:file-info-size info) size)}
  @argument[info]{a @class{g:file-info} object}
  @argument[size]{an integer for the size of the file}
  @begin{short}
    Gets or sets the file size for a given @arg{info}.
  @end{short}
  It is an error to retrieve the size if @arg{info} does not contain
  the @code{\"standard::size\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-size)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_creation_date_time                      Since 2.70
;;; g_file_info_set_creation_date_time
;;; ----------------------------------------------------------------------------

#+glib-2-70
(defun (setf file-info-creation-date-time) (value info)
  (let ((date (cffi:convert-to-foreign value 'glib:date-time)))
    (cffi:foreign-funcall "g_file_info_set_creation_date_time"
                          (gobject:object file-info) info
                          (glib:boxed glib:date-time) date
                          :void)
    value))

#+glib-2-70
(defun file-info-creation-date-time (info)
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-creation-date-time info) => time}
  @syntax{(setf (g:file-info-creation-date-time info) time)}
  @argument[info]{a @class{g:file-info} instance}
  @argument[time]{an unsigned integer for the Lisp universal time}
  @begin{short}
    Gets or sets the creation time of the current @arg{info}.
  @end{short}
  It is an error to call this if the @arg{info} does not contain the
  @code{\"time::created\"} attribute. If the @code{\"time::created-usec\"}
  attribute is provided, the resulting universal time will additionally have
  microsecond precision.

  If nanosecond precision is needed, the @code{\"time::created-nsec\"} attribute
  must be queried separately using the @fun{g:file-info-attribute} function.

  Since 2.70
  @see-class{g:file-info}
  @see-function{g:file-info-attribute}"
  (cffi:convert-from-foreign
      (cffi:foreign-funcall "g_file_info_get_creation_date_time"
                            (gobject:object file-info) info
                            (glib:boxed glib:date-time))
      'glib:date-time))

#+glib-2-70
(export 'file-info-creation-date-time)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_access_date_time                        Since 2.70
;;; g_file_info_set_access_date_time
;;; ----------------------------------------------------------------------------

#+glib-2-70
(defun (setf file-info-access-date-time) (value info)
  (let ((date (cffi:convert-to-foreign value 'glib:date-time)))
    (cffi:foreign-funcall "g_file_info_set_access_date_time"
                          (gobject:object file-info) info
                          (glib:boxed glib:date-time) date
                          :void)
    value))

#+glib-2-70
(defun file-info-access-date-time (info)
 "@version{2026-03-22}
  @syntax{(g:file-info-access-date-time info) => time}
  @syntax{(setf (g:file-info-access-date-time info) time)}
  @argument[info]{a @class{g:file-info} instance}
  @argument[time]{an unsigned integer for the universal time}
  @begin{short}
    Gets or sets the access time of the current @arg{info} and returns it as a
    Lisp universal time.
  @end{short}
  It is an error to call this function if @arg{info} does not contain the
  @code{\"time:access\"} attribute. If the @code{\"time::access-usec\"}
  attribute is provided, the resulting universal time will additionally have
  microsecond precision.

  If nanosecond precision is needed, the @code{\"time::acces-nsec\"} attribute
  must be queried separately using the @fun{g:file-info-attribute} function.

  Since 2.70
  @see-class{g:file-info}
  @see-class{g:file-info-attribute}"
  (cffi:convert-from-foreign
      (cffi:foreign-funcall "g_file_info_get_access_date_time"
                            (gobject:object file-info) info
                            (glib:boxed glib:date-time))
      'glib:date-time))

#+glib-2-70
(export 'file-info-access-date-time)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_modification_time                       Deprecated 2.62
;;; g_file_info_get_modification_time
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_modification_date_time
;;; g_file_info_get_modification_date_time
;;; ----------------------------------------------------------------------------

(defun (setf file-info-modification-date-time) (value info)
  (let ((date (cffi:convert-to-foreign value 'glib:date-time)))
    (cffi:foreign-funcall "g_file_info_set_modification_date_time"
                          (gobject:object file-info) info
                          (glib:boxed glib:date-time) date
                          :void)
    value))

(defun file-info-modification-date-time (info)
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-modification-date-time info) => time}
  @syntax{(setf (g:file-info-modification-date-time info) time)}
  @argument[info]{a @class{g:file-info} object}
  @argument[time]{a Lisp universal time}
  @begin{short}
    Gets or sets Sets the @code{\"time::modifieed\"} and
    @code{\"time::modified-usec\"} attributes in the file info to the given
    @arg{time} value.
  @end{short}
  The @code{\"time::modified-nsec\"} attribute will be cleared.

  It is an error to retrieve the modified time if @arg{info} does not contain
  the @code{\"time::modified\"} attribute. If the
  @code{time::modified-modified-usec} attribute is provided, the resulting time
  will additionally have microsecond precision.

  If nanosecond precision is needed, the @code{\"time::modified-nsec\"}
  attribute must be queried separately using the @fun{g:file-info-attribute}
  function.
  @see-class{g:file-info}
  @see-function{g:file-info-attribute}"
  (cffi:convert-from-foreign
      (cffi:foreign-funcall "g_file_info_get_modification_date_time"
                            (gobject:object file-info) info
                            (glib:boxed glib:date-time))
      'glib:date-time))

(export 'file-info-modification-date-time)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_deletion_date
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_deletion_date" file-info-deletion-date)
    glib:date-time
 #+liber-documentation
 "@version{#2026-03-22}
  @argument[info]{a @class{g:file-info} instance}
  @return{The integer for the Lisp universal time.}
  @begin{short}
    Returns the Lisp universal time representing the deletion date of the file,
    as available in the @code{\"trash::deletion-date} attribute key.
  @end{short}
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-deletion-date)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_etag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_etag" file-info-etag) :string
 #+liber-documentation
 "@version{#2026-03-22}
  @argument[info]{a @class{g:file-info} instance}
  @begin{return}
    The string containing the value of the @code{\"etag::value\"} attribute key.
  @end{return}
  @begin{short}
    Gets the entity tag for a given @arg{info}.
  @end{short}
  It is an error to call this if @arg{info} does not contain the
  @code{\"etag::value\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-etag)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_file_type
;;; g_file_info_set_file_type
;;; ----------------------------------------------------------------------------

(defun (setf file-info-file-type) (value info)
  (cffi:foreign-funcall "g_file_info_set_file-type"
                        (gobject:object file-info) info
                        file-type value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_file_type" file-info-file-type) file-type
 #+liber-documentation
 "@version{#2026-03-22}
  @argument[info]{a @class{g:file-info} instance}
  @return{The @symbol{g:file-type} value for @arg{info}.}
  @begin{short}
    Gets or sets the file type, whether it is a regular file, symlink, and so on.
  @end{short}
  This is different from the content type, see the @fun{g:file-info-content-type}
  function.

  It is an error to call this if @arg{info} does not contain the
  @code{\"standard::type\"} attribute key.
  @see-class{g:file-info}
  @see-function{g:file-info-content-type}"
  (info (gobject:object file-info)))

(export 'file-info-file-type)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_sort_order
;;; g_file_info_get_sort_order
;;; ----------------------------------------------------------------------------

(defun (setf file-info-sort-order) (value info)
  (cffi:foreign-funcall "g_file_info_set_sort_order"
                        (gobject:object file-info) info
                        :int32 value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_sort_order" file-info-sort-order) :int32
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-sort-order info) => order}
  @syntax{(setf (g:file-info-sort-order info) order)}
  @argument[info]{a @class{g:file-info} object}
  @argument[order]{an integer for the sort order}
  @begin{short}
    Gets or sets the sort order for a given @arg{info}.
  @end{short}
  It is an error to retrieve the sort order if @arg{info} does not contain
  the @code{\"standard::sort-order\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-sort-order)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_symlink_target
;;; g_file_info_get_symlink_target
;;; ----------------------------------------------------------------------------

(defun (setf file-info-symlink-target) (value info)
  (cffi:foreign-funcall "g_file_info_set_symlink_target"
                        (gobject:object file-info) info
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_symlink_target" file-info-symlink-target)
    :string
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-symlink-target info) => target}
  @syntax{(setf (g:file-info-symlink-target info) target)}
  @argument[info]{a @class{g:file-info} object}
  @argument[target]{a string containing a path to a symlink target}
  @begin{short}
    Gets or sets the symlink target for a given @arg{info}.
  @end{short}
  It is an error to retrieve the symlink target if @arg{info} does not contain
  the @code{\"standard::symlink-target\"} attribute key.
  @see-class{g:file-info}"
  (info (gobject:object)))

(export 'file-info-symlink-target)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_file_path
;;; g_file_info_get_attribute_file_path
;;; ----------------------------------------------------------------------------

#+glib-2-78
(defun (setf file-info-attribute-file-path) (value info attribute)
  (cffi:foreign-funcall "g_file_info_set_attribute_file_path"
                        (gobject:object file-info) info
                        :string attribute
                        :string value
                        :void)
  value)

#+glib-2-78
(cffi:defcfun ("g_file_info_get_attribute_file_path"
               file-info-attribute-file-path) :string
 #+liber-documentation
 "@version{2026-03-22}
  @syntax{(g:file-info-attribute-file-path info attribute) => path}
  @syntax{(setf (g:file-info-attribute-file-path info attribute) path)}
  @argument[info]{a @class{g:file-info} instance}
  @argument[attribute]{a string for a file attribute key}
  @argument[path]{a string for the file path}
  @begin{short}
    Gets or sets the value of a byte string attribute as a file path.
  @end{short}
  If the attribute does not contain a byte string, @code{nil} will be returned.
  This function is meant to be used by language bindings that have specific
  handling for Unix paths.

  Since 2.78
  @see-class{g:file-info}"
  (info (gobject:object file-info))
  (attribute :string))

#+glib-2-78
(export 'file-info-attribute-file-path)

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_mask
;;;
;;; Sets mask on info to match specific attribute types.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_unset_attribute_mask
;;;
;;; Unsets a mask set by g_file_info_set_attribute_mask(), if one is set.
;;; ----------------------------------------------------------------------------

;;; ----- End of file gio.file-info.lisp ---------------------------------------
