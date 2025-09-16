;;; ----------------------------------------------------------------------------
;;; gio.file-info.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2025 Dieter Kaiser
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
;;;     FileInfo
;;;
;;; Functions
;;;
;;;     g_file_info_new
;;;
;;;     g_file_info_clear_status
;;;     g_file_info_copy_into
;;;     g_file_info_dup
;;;
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

;;;     g_file_info_get_access_date_time                    Since 2.70
;;;     g_file_info_set_access_date_time                    Since 2.70
;;;     g_file_info_get_content_type
;;;     g_file_info_set_content_type
;;;     g_file_info_get_creation_date_time                  Since 2.70
;;;     g_file_info_set_creation_date_time                  Since 2.70

;;;     g_file_info_get_deletion_date
;;;     g_file_info_get_display_name
;;;     g_file_info_get_edit_name
;;;     g_file_info_get_etag
;;;     g_file_info_get_file_type

;;;     g_file_info_get_icon
;;;     g_file_info_set_icon

;;;     g_file_info_get_is_backup
;;;     g_file_info_get_is_hidden
;;;     g_file_info_get_is_symlink
;;;     g_file_info_get_modification_date_time
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

;;;     g_file_info_set_display_name
;;;     g_file_info_set_edit_name
;;;     g_file_info_set_file_type

;;;     g_file_info_set_is_hidden
;;;     g_file_info_set_is_symlink
;;;     g_file_info_set_modification_date_time
;;;     g_file_info_set_modification_time                   Deprecated 2.62
;;;     g_file_info_set_name
;;;     g_file_info_set_size
;;;     g_file_info_set_sort_order
;;;     g_file_info_set_symbolic_icon
;;;     g_file_info_set_symlink_target
;;;     g_file_info_unset_attribute_mask
;;; ----------------------------------------------------------------------------

(in-package :gio)

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
 "@version{2024-12-28}
  @begin{short}
    The @class{g:file-info} class implements methods for getting information
    that all files should contain, and allows for manipulation of extended
    attributes.
  @end{short}
  See the file attributes document for more information on how GIO handles file
  attributes. To obtain a @class{g:file-info} object for a @class{g:file}
  object, use the @fun{g:file-query-info} function or its async variant.

  To change the actual attributes of a file, you should then set the attribute
  in the @class{g:file-info} object and call the
  @fun{g:file-set-attributes-from-info} or @fun{g:file-set-attributes-async}
  functions on a @class{g:file} instance.

  However, not all attributes can be changed in the file. For instance, the
  actual size of a file cannot be changed via the @fun{g:file-info-set-size}
  function. You may call the @fun{g:file-query-settable-attributes} and
  @fun{g:file-query-writable-namespaces} function to discover the settable
  attributes of a particular file at runtime.

  The direct accessors, such as the @fun{g:file-info-name} function, are
  slightly more optimized than the generic attribute accessors, such as
  the @fun{g:file-info-attribute-byte-string} function.This optimization will
  matter only if calling the API in a tight loop.

  It is an error to call these accessors without specifying their required file
  attributes when creating the @class{g:file-info} instance. Use the
  @fun{g:file-info-has-attribute} or @fun{g:file-info-list-attributes} functions
  to check what attributes are specified for a @class{g:file-info} instance.

  The @symbol{g:file-attribute-matcher} instance allows for searching through a
  @class{g:file-info} instance for attributes.
  @see-constructor{g:file-info-new}
  @see-class{g:file}")

;;; ----------------------------------------------------------------------------
;;; g_file_info_new
;;; ----------------------------------------------------------------------------

(defun file-info-new ()
 #+liber-documentation
 "@version{2024-10-23}
  @return{The newly created @class{g:file-info} instance.}
  @short{Creates a new @class{g:file-info} instance.}
  @see-class{g:file-info}"
  (make-instance 'file-info))

(export 'file-info-new)

;;; ----------------------------------------------------------------------------
;;; g_file_info_clear_status
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_clear_status" file-info-clear-status) :void
 "@version{#2025-06-15}
  @argument[info]{a @class{g:file-info} instance}
  @short{Clears the status information from @arg{info}.}
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-clear-status)

;;; ----------------------------------------------------------------------------
;;; g_file_info_copy_into
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_copy_into" file-info-copy-into) :void
 "@version{#2025-06-15}
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
;;; g_file_info_dup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_dup" file-info-dup)
    (gobject:object file-info :return)
 "@version{#2025-06-15}
  @argument[info]{a @class{g:file-info} instance}
  @return{The new duplicate of @arg{info}.}
  @short{Duplicates a file info.}
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-dup)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_as_string
;;;
;;; Gets the value of an attribute, formatted as a string. This escapes things
;;; as needed to make the string valid UTF-8.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_attribute_as_string"
               file-info-attribute-as-string) :string
  (info (gobject:object file-info))
  (attribute :string))

(export 'file-info-attribute-as-string)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_boolean
;;;
;;; Gets the value of a boolean attribute. If the attribute does not contain a
;;; boolean value, FALSE will be returned.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_attribute_boolean"
               file-info-attribute-boolean) :boolean
  (info (gobject:object file-info))
  (attribute :string))

(export 'file-info-attribute-boolean)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_byte_string
;;;
;;; Gets the value of a byte string attribute. If the attribute does not
;;; contain a byte string, NULL will be returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_data
;;;
;;; Gets the attribute type, value and status for an attribute key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_file_path
;;;
;;; Gets the value of a byte string attribute as a file path.
;;;
;;; Since 2.78
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_int32
;;;
;;; Gets a signed 32-bit integer contained within the attribute. If the
;;; attribute does not contain a signed 32-bit integer, or is invalid, 0 will
;;; be returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_int64
;;;
;;; Gets a signed 64-bit integer contained within the attribute. If the
;;; attribute does not contain a signed 64-bit integer, or is invalid, 0 will
;;; be returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_object
;;;
;;; Gets the value of a GObject attribute. If the attribute does not contain a
;;; GObject, NULL will be returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_status
;;;
;;; Gets the attribute status for an attribute key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_string
;;;
;;; Gets the value of a string attribute. If the attribute does not contain a
;;; string, NULL will be returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_stringv
;;;
;;; Gets the value of a stringv attribute. If the attribute does not contain a
;;; stringv, NULL will be returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_type
;;;
;;; Gets the attribute type for an attribute key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_uint32
;;;
;;; Gets an unsigned 32-bit integer contained within the attribute. If the
;;; attribute does not contain an unsigned 32-bit integer, or is invalid, 0
;;; will be returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_attribute_uint64
;;;
;;; Gets a unsigned 64-bit integer contained within the attribute. If the
;;; attribute does not contain an unsigned 64-bit integer, or is invalid, 0
;;; will be returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_access_date_time                        Since 2.70
;;; g_file_info_set_access_date_time
;;; ----------------------------------------------------------------------------

#+glib-2-70
(defun (setf file-info-access-date-time) (value info)
  (cffi:foreign-funcall "g_file_info_set_access_date_time"
                        (gobject:object file-info) info
                        glib:date-time value
                        :void)
  value)

#+glib-2-70
(cffi:defcfun ("g_file_info_get_access_date_time" file-info-access-date-time)
    glib:date-time
 "@version{#2025-06-16}
  @syntax{(g:file-info-access-date-time info) => time}
  @syntax{(setf (g:file-info-access-date-time info) time)}
  @argument[info]{a @class{g:file-info} instance}
  @argument[time]{an unsigned integer for the universal time}
  @begin{short}
    The @fun{g:file-info-access-date-time} function gets the access time of the
    current @arg{info} and returns it as a Lisp universal time.
  @end{short}
  It is an error to call this function if the @class{g:file-info} instance does
  not contain the @code{G_FILE_ATTRIBUTE_TIME_ACCESS} attribute. If the
  @code{G_FILE_ATTRIBUTE_TIME_ACCESS_USEC} attribute is provided, the resulting
  universal time will additionally have microsecond precision.

  If nanosecond precision is needed, the
  @code{G_FILE_ATTRIBUTE_TIME_ACCESS_NSEC} attribute must be queried separately
  using the @fun{g:file-info-attribute-uint32} function.

  The @setf{g:file-info-access-date-time} function sets the
  @code{G_FILE_ATTRIBUTE_TIME_ACCESS} and
  @code{G_FILE_ATTRIBUTE_TIME_ACCESS_USEC} attributes in the file info to the
  given date/time value. The @code{G_FILE_ATTRIBUTE_TIME_ACCESS_NSEC} attribute
  will be cleared.

  Since 2.70
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

#+glib-2-70
(export 'file-info-access-date-time)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_content_type
;;; g_file_info_set_content_type
;;; ----------------------------------------------------------------------------

(defun (setf file-info-content-type) (value info)
  (cffi:foreign-funcall "g_file_info_set_content_type"
                        (gobject:object file-info) info
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_content_type" file-info-content-type) :string
 "@version{#2025-06-15}
  @syntax{(g:file-info-content-type info) => ctype}
  @syntax{(setf (g:file-info-content-type info) ctype)}
  @argument[info]{a @class{g:file-info} instance}
  @argument[ctype]{a string for the content type}
  @begin{short}
    The @fun{g:file-info-content-type} function gets the content type of the
    file.
  @end{short}
  It is an error to call this function if @arg{info} does not contain the
  @code{G_FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE} attribute.

  The @setf{g:file-info-content-type} function sets the content type attribute
  for a given @arg{info}.
  @see-class{g:file-info}"
  (info (gobject:object file-info)))

(export 'file-info-content-type)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_creation_date_time
;;; g_file_info_set_creation_date_time
;;; ----------------------------------------------------------------------------

(defun (setf file-info-creation-date-time) (value info)
  (cffi:foreign-funcall "g_file_info_set_creation_data-time"
                        (gobject:object file-info) info
                        glib:date-time value
                        :void)
  value)

(cffi:defcfun ("g_file_info_get_creation_date_time"
               file-info-creation-date-time) glib:date-time
  (info (gobject:object file-info)))

(export 'file-info-creation-date-time)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_deletion_date
;;;
;;; Returns the GDateTime representing the deletion date of the file, as
;;; available in G_FILE_ATTRIBUTE_TRASH_DELETION_DATE. If the
;;; G_FILE_ATTRIBUTE_TRASH_DELETION_DATE attribute is unset, NULL is returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_display_name
;;;
;;; Gets a display name for a file. This is guaranteed to always be set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_edit_name
;;;
;;; Gets the edit name for a file.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_etag
;;;
;;; Gets the entity tag for a given GFileInfo. See G_FILE_ATTRIBUTE_ETAG_VALUE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_file_type
;;;
;;; Gets a file’s type (whether it is a regular file, symlink, etc). This is
;;; different from the file’s content type, see g_file_info_get_content_type().
;;; ----------------------------------------------------------------------------

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
 "@version{#2025-06-15}
  @syntax{(g:file-info-icon info) => icon}
  @syntax{(setf (g:file-info-icon info) icon)}
  @argument[info]{a @class{g:file-info} object}
  @argument[icon]{a @class{g:icon} instance }
  @begin{short}
    Gets or sets the icon for a given @arg{info}.
  @end{short}
  It is an error to call this if the @class{g:file-info} instance does not
  contain @code{G_FILE_ATTRIBUTE_STANDARD_ICON}.
  @see-class{g:file-info}
  @see-class{g:icon}"
  (info (gobject:object file-info)))

(export 'file-info-icon)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_is_backup
;;;
;;; Checks if a file is a backup file.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_is_hidden
;;;
;;; Checks if a file is hidden.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_is_symlink
;;;
;;; Checks if a file is a symlink.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_modification_date_time
;;;
;;; Gets the modification time of the current info and returns it as a
;;; GDateTime.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_modification_date_time"
               file-info-modification-date-time) glib:date-time
  (info (gobject:object file-info)))

(export 'file-info-modification-date-time)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_modification_time
;;;
;;; Gets the modification time of the current info and sets it in result.
;;;
;;; Deprecated 2.62
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_name
;;;
;;; Gets the name for a file. This is guaranteed to always be set.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_name" file-info-name) :string
  (info (gobject:object file-info)))

(export 'file-info-name)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_size
;;;
;;; Gets the file’s size (in bytes). The size is retrieved through the value of
;;; the G_FILE_ATTRIBUTE_STANDARD_SIZE attribute and is converted from #guint64
;;; to #goffset before returning the result.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_get_size" file-info-size) :offset
  (info (gobject:object file-info)))

(export 'file-info-size)

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_sort_order
;;;
;;; Gets the value of the sort_order attribute from the GFileInfo. See
;;; G_FILE_ATTRIBUTE_STANDARD_SORT_ORDER.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_symbolic_icon
;;;
;;; Gets the symbolic icon for a file.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_get_symlink_target
;;;
;;; Gets the symlink target for a given GFileInfo.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_has_attribute
;;;
;;; Checks if a file info structure has an attribute named attribute.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_has_attribute" file-info-has-attribute) :boolean
  (info (gobject:object file-info))
  (attribute :string))

(export 'file-info-has-attribute)

;;; ----------------------------------------------------------------------------
;;; g_file_info_has_namespace
;;;
;;; Checks if a file info structure has an attribute in the specified
;;; name_space.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_has_namespace" file-info-has-namespace) :boolean
  (info (gobject:object file-info))
  (namespace :string))

(export 'file-info-has-namespace)

;;; ----------------------------------------------------------------------------
;;; g_file_info_list_attributes
;;;
;;; Lists the file info structure’s attributes.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_info_list_attributes" %file-info-list-attributes)
    glib:strv-t
  (info (gobject:object file-info))
  (namespace :string))

(defun file-info-list-attributes (info &optional namespace)
  (%file-info-list-attributes info (or namespace (cffi:null-pointer))))

(export 'file-info-list-attributes)

;;; ----------------------------------------------------------------------------
;;; g_file_info_remove_attribute
;;;
;;; Removes all cases of attribute from info if it exists.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute
;;;
;;; Sets the attribute to contain the given value, if possible. To unset the
;;; attribute, use G_FILE_ATTRIBUTE_TYPE_INVALID for type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_boolean
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_byte_string
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_file_path
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;;
;;; Since 2.78
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_int32
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_int64
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_mask
;;;
;;; Sets mask on info to match specific attribute types.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_object
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_status
;;;
;;; Sets the attribute status for an attribute key. This is only needed by
;;; external code that implement g_file_set_attributes_from_info() or similar
;;; functions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_string
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_stringv
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_uint32
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_attribute_uint64
;;;
;;; Sets the attribute to contain the given attr_value, if possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_display_name
;;;
;;; Sets the display name for the current GFileInfo. See
;;; G_FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_edit_name
;;;
;;; Sets the edit name for the current file. See
;;; G_FILE_ATTRIBUTE_STANDARD_EDIT_NAME.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_file_type
;;;
;;; Sets the file type in a GFileInfo to type. See
;;; G_FILE_ATTRIBUTE_STANDARD_TYPE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_is_hidden
;;;
;;; Sets the “is_hidden” attribute in a GFileInfo according to is_hidden. See
;;; G_FILE_ATTRIBUTE_STANDARD_IS_HIDDEN.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_is_symlink
;;;
;;; Sets the “is_symlink” attribute in a GFileInfo according to is_symlink.
;;; See G_FILE_ATTRIBUTE_STANDARD_IS_SYMLINK.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_modification_date_time
;;;
;;; Sets the G_FILE_ATTRIBUTE_TIME_MODIFIED and
;;; G_FILE_ATTRIBUTE_TIME_MODIFIED_USEC attributes in the file info to the
;;; given date/time value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_modification_time
;;;
;;; Sets the G_FILE_ATTRIBUTE_TIME_MODIFIED and
;;; G_FILE_ATTRIBUTE_TIME_MODIFIED_USEC attributes in the file info to the
;;; given time value.
;;;
;;; Deprecated 2.62
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_name
;;;
;;; Sets the name attribute for the current GFileInfo. See
;;; G_FILE_ATTRIBUTE_STANDARD_NAME.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_size
;;;
;;; Sets the G_FILE_ATTRIBUTE_STANDARD_SIZE attribute in the file info to the
;;; given size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_sort_order
;;;
;;; Sets the sort order attribute in the file info structure. See
;;; G_FILE_ATTRIBUTE_STANDARD_SORT_ORDER.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_symbolic_icon
;;;
;;; Sets the symbolic icon for a given GFileInfo. See
;;; G_FILE_ATTRIBUTE_STANDARD_SYMBOLIC_ICON.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_set_symlink_target
;;;
;;; Sets the G_FILE_ATTRIBUTE_STANDARD_SYMLINK_TARGET attribute in the file
;;; info to the given symlink target.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_file_info_unset_attribute_mask
;;;
;;; Unsets a mask set by g_file_info_set_attribute_mask(), if one is set.
;;; ----------------------------------------------------------------------------

;;; ----- End of file gio.file-info.lisp ---------------------------------------
