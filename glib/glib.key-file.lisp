;;; ----------------------------------------------------------------------------
;;; glib.key-value.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.76 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2023 Dieter Kaiser
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
;;; Key-value file parser
;;;
;;;     parses .ini-like config files
;;;
;;; Types and Values
;;;
;;;     GKeyFile
;;;     GKeyFileFlags
;;;
;;;     G_KEY_FILE_ERROR
;;;
;;;     GKeyFileError
;;;
;;;     G_KEY_FILE_DESKTOP_GROUP
;;;     G_KEY_FILE_DESKTOP_KEY_TYPE
;;;     G_KEY_FILE_DESKTOP_KEY_VERSION
;;;     G_KEY_FILE_DESKTOP_KEY_NAME
;;;     G_KEY_FILE_DESKTOP_KEY_GENERIC_NAME
;;;     G_KEY_FILE_DESKTOP_KEY_NO_DISPLAY
;;;     G_KEY_FILE_DESKTOP_KEY_COMMENT
;;;     G_KEY_FILE_DESKTOP_KEY_ICON
;;;     G_KEY_FILE_DESKTOP_KEY_HIDDEN
;;;     G_KEY_FILE_DESKTOP_KEY_ONLY_SHOW_IN
;;;     G_KEY_FILE_DESKTOP_KEY_NOT_SHOW_IN
;;;     G_KEY_FILE_DESKTOP_KEY_TRY_EXEC
;;;     G_KEY_FILE_DESKTOP_KEY_EXEC
;;;     G_KEY_FILE_DESKTOP_KEY_PATH
;;;     G_KEY_FILE_DESKTOP_KEY_TERMINAL
;;;     G_KEY_FILE_DESKTOP_KEY_MIME_TYPE
;;;     G_KEY_FILE_DESKTOP_KEY_CATEGORIES
;;;     G_KEY_FILE_DESKTOP_KEY_STARTUP_NOTIFY
;;;     G_KEY_FILE_DESKTOP_KEY_STARTUP_WM_CLASS
;;;     G_KEY_FILE_DESKTOP_KEY_URL
;;;     G_KEY_FILE_DESKTOP_KEY_ACTIONS
;;;     G_KEY_FILE_DESKTOP_KEY_DBUS_ACTIVATABLE
;;;     G_KEY_FILE_DESKTOP_TYPE_APPLICATION
;;;     G_KEY_FILE_DESKTOP_TYPE_LINK
;;;     G_KEY_FILE_DESKTOP_TYPE_DIRECTORY
;;;
;;; Functions
;;;
;;;     g_key_file_new
;;;     g_key_file_free
;;;     g_key_file_ref
;;;     g_key_file_unref
;;;     g_key_file_set_list_separator
;;;     g_key_file_load_from_file
;;;     g_key_file_load_from_data
;;;     g_key_file_load_from_bytes
;;;     g_key_file_load_from_data_dirs
;;;     g_key_file_load_from_dirs
;;;     g_key_file_to_data
;;;     g_key_file_save_to_file
;;;     g_key_file_get_start_group
;;;     g_key_file_get_groups
;;;     g_key_file_get_keys
;;;     g_key_file_has_group
;;;     g_key_file_has_key
;;;
;;;     g_key_file_get_value
;;;     g_key_file_get_string
;;;     g_key_file_get_locale_string
;;;     g_key_file_get_locale_for_key
;;;     g_key_file_get_boolean
;;;     g_key_file_get_integer
;;;     g_key_file_get_int64
;;;     g_key_file_get_uint64
;;;     g_key_file_get_double
;;;     g_key_file_get_string_list
;;;     g_key_file_get_locale_string_list
;;;     g_key_file_get_boolean_list
;;;     g_key_file_get_integer_list
;;;     g_key_file_get_double_list
;;;     g_key_file_get_comment
;;;
;;;     g_key_file_set_value
;;;     g_key_file_set_string
;;;     g_key_file_set_locale_string
;;;     g_key_file_set_boolean
;;;     g_key_file_set_integer
;;;     g_key_file_set_int64
;;;     g_key_file_set_uint64
;;;     g_key_file_set_double
;;;     g_key_file_set_string_list
;;;     g_key_file_set_locale_string_list
;;;     g_key_file_set_boolean_list
;;;     g_key_file_set_integer_list
;;;     g_key_file_set_double_list
;;;     g_key_file_set_comment
;;;
;;;     g_key_file_remove_group
;;;     g_key_file_remove_key
;;;     g_key_file_remove_comment
;;; ----------------------------------------------------------------------------

(in-package :glib)

(declaim (inline ensure-pointer))

(defun ensure-pointer (value)
  (if value value (cffi:null-pointer)))

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_ERROR
;;;
;;; #define G_KEY_FILE_ERROR g_key_file_error_quark()
;;;
;;; Error domain for key file parsing. Errors in this domain will be from the
;;; GKeyFileError enumeration.
;;;
;;; See GError for information on error domains.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GKeyFileError
;;;
;;; typedef enum {
;;;   G_KEY_FILE_ERROR_UNKNOWN_ENCODING,
;;;   G_KEY_FILE_ERROR_PARSE,
;;;   G_KEY_FILE_ERROR_NOT_FOUND,
;;;   G_KEY_FILE_ERROR_KEY_NOT_FOUND,
;;;   G_KEY_FILE_ERROR_GROUP_NOT_FOUND,
;;;   G_KEY_FILE_ERROR_INVALID_VALUE
;;; } GKeyFileError;
;;;
;;; Error codes returned by key file parsing.
;;;
;;; G_KEY_FILE_ERROR_UNKNOWN_ENCODING
;;;     the text being parsed was in an unknown encoding
;;;
;;; G_KEY_FILE_ERROR_PARSE
;;;     document was ill-formed
;;;
;;; G_KEY_FILE_ERROR_NOT_FOUND
;;;     the file was not found
;;;
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND
;;;     a requested key was not found
;;;
;;; G_KEY_FILE_ERROR_GROUP_NOT_FOUND
;;;     a requested group was not found
;;;
;;; G_KEY_FILE_ERROR_INVALID_VALUE
;;;     a value could not be parsed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GKeyFileFlags
;;; ----------------------------------------------------------------------------

(defbitfield key-file-flags
  (:none 0)
  (:keep-comments #.(ash 1 0))
  (:keep-translations #.(ash 1 1)))

#+liber-documentation
(setf (liber:alias-for-symbol 'key-file-flags)
      "Bitfield"
      (liber:symbol-documentation 'key-file-flags)
 "@version{2023-1-25}
  @begin{short}
    Flags which influence the parsing of key values.
  @end{short}
  @begin{pre}
(defbitfield key-file-flags
  (:none 0)
  (:keep-comments #.(ash 1 0))
  (:keep-translations #.(ash 1 1)))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No flags, default behaviour.}
    @entry[:keep-coments]{Use this flag if you plan to write the possibly
      modified contents of the key file back to a file. Otherwise all comments
      will be lost when the key file is written back.}
    @entry[:keep-translations]{Use this flag if you plan to write the possibly
      modified contents of the key file back to a file. Otherwise only the
      translations for the current language will be written back.}
  @end{table}
  @see-type{g:key-file}")

(export 'key-file-flags)

;;; ----------------------------------------------------------------------------
;;; GKeyFile
;;; ----------------------------------------------------------------------------

(defcstruct key-file)

#+liber-documentation
(setf (liber:alias-for-type 'key-file)
      "CStruct"
      (documentation 'key-file 'type)
 "@version{2023-1-25}
  @begin{short}
    The @sym{g:key-file} structure lets you parse, edit or create files
    containing groups of key-value pairs, which we call key files for lack of a
    better name.
  @end{short}
  Several freedesktop.org specifications use key files now, e.g. the Desktop
  Entry Specification and the Icon Theme Specification.

  The syntax of key files is described in detail in the Desktop Entry
  Specification, here is a quick summary: Key files consists of groups of
  key-value pairs, interspersed with comments.
  @begin{pre}
# this is just an example
# there can be comments before the first group

[First Group]

Name=Key File Example this value shows escaping

# localized strings are stored in multiple key-value pairs
Welcome=Hello
Welcome[de]=Hallo
Welcome[fr_FR]=Bonjour
Welcome[it]=Ciao
Welcome[be@@latin]=Hello

[Another Group]

Numbers=2;20;-200;0

Booleans=true;false;true;true
  @end{pre}
  Lines beginning with a @code{'#'} and blank lines are considered comments.

  Groups are started by a header line containing the group name enclosed in
  @code{'['} and @code{']'}, and ended implicitly by the start of the next group
  or the end of the file. Each key-value pair must be contained in a group.

  Key-value pairs generally have the form @code{key=value}, with the exception
  of localized strings, which have the form @code{key[locale]=value}, with a
  locale identifier of the form @code{lang_COUNTRYMODIFIER} where @code{COUNTRY}
  and @code{MODIFIER} are optional. Space before and after the @code{'='}
  character are ignored. Newline, tab, carriage return and backslash characters
  in value are escaped as @code{\n}, @code{\t}, @code{\r}, and @code{\\},
  respectively. To preserve leading spaces in values, these can also be escaped
  as @code{\s}.

  Key files can store strings, possibly with localized variants, integers,
  booleans and lists of these. Lists are separated by a separator character,
  typically @code{';'} or @code{','}. To use the list separator character in a
  value in a list, it has to be escaped by prefixing it with a backslash.

  This syntax is obviously inspired by the .ini files commonly met on Windows,
  but there are some important differences:
  @begin{itemize}
    @item{.ini files use the @code{';'} character to begin comments, key files
      use the @code{'#'} character.}
    @item{Key files do not allow for ungrouped keys meaning only comments can
      precede the first group.}
    @item{Key files are always encoded in UTF-8.}
    @item{Key and Group names are case-sensitive. For example, a group called
      @code{[GROUP]} is a different from @code{[group]}.}
    @item{.ini files do not have a strongly typed boolean entry type, they only
      have @code{GetProfileInt()}. In key files, only true and false (in lower
      case) are allowed.}
  @end{itemize}
  Note that in contrast to the Desktop Entry Specification, groups in key
  files may contain the same key multiple times. The last entry wins. Key
  files may also contain multiple groups with the same name. They are merged
  together. Another difference is that keys and group names in key files are
  not restricted to ASCII characters.
  @begin[Examples]{dictionary}
    Here is an example of loading a key file and reading a value:
    @begin{pre}
(with-g-key-file (keyfile)
  ;; Load the key file
  (unless (g:key-file-load-from-file keyfile \"rtest-glib-key-file.ini\" :none)
    (error \"Error loading the key file: RTEST-GLIB-KEY-FILE.INI\"))
  ;; Read a string from the key file
  (let ((value (g:key-file-string keyfile \"First Group\" \"Welcome\")))
    (unless value
      (setf value \"default-value\"))
    ... ))
    @end{pre}
    Here is an example of creating and saving a key file:
    @begin{pre}
(with-g-key-file (keyfile)
  ;; Load existing key file
  (g:key-file-load-from-file keyfile \"rtest-glib-key-file.ini\" :none)
  ;; Add a string to the First Group
  (setf (g:key-file-string keyfile \"First Group\" \"SomeKey\") \"New Value\")
  ;; Save to a file
  (unless (g:key-file-save-to-file keyfile \"rtest-glib-key-file-example.ini\")
    (error \"Error saving key file.\"))
  ;; Or save to data for use elsewhere
  (let ((data (g:key-file-to-data keyfile)))
    (unless data
      (error \"Error saving key file.\"))
    ... ))
    @end{pre}
  @end{dictionary}
  @see-macro{with-g-key-file}")

(export 'key-file)

;;; ----------------------------------------------------------------------------
;;; with-g-key-file
;;; ----------------------------------------------------------------------------

(defmacro with-g-key-file ((keyfile) &body body)
 #+liber-documentation
 "@version{2023-2-1}
  @syntax[]{(with-g-key-file (keyfile) body) => result}
  @argument[keyfile]{a newly allocated @type{g:key-file} instance}
  @begin{short}
    The @sym{with-g-file-key} macro allocates a new @type{g:key-file} instance
    and executes the body that uses the key file.
  @end{short}
  After execution of the body the allocated memory for the key file is released.
  @see-type{g:key-file}"
  `(let ((,keyfile (key-file-new)))
     (unwind-protect
       (progn ,@body)
       (key-file-free ,keyfile))))

(export 'with-g-key-file)

;;; ----------------------------------------------------------------------------
;;; g_key_file_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_new" key-file-new) (:pointer (:struct key-file))
 #+liber-documentation
 "@version{2023-1-25}
  @return{An empty @type{g:key-file} instance.}
  @begin{short}
    Creates a new empty @type{g:key-file} instance.
  @end{short}
  Use the @fun{g:key-file-load-from-file}, or @fun{g:key-file-load-from-data}
  functions to read an existing key file.
  @see-type{g:key-file}
  @see-function{g:key-file-load-from-file}
  @see-function{g:key-file-load-from-data}")

(export 'key-file-new)

;;; ----------------------------------------------------------------------------
;;; g_key_file_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_free" key-file-free) :void
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @begin{short}
    Clears all keys and groups from @arg{keyfile}, and decreases the reference
    count by 1.
  @end{short}
  If the reference count reaches zero, frees the key file and all its allocated
  memory.
  @see-type{g:key-file}"
  (keyfile (:pointer (:struct key-file))))

(export 'key-file-free)

;;; ----------------------------------------------------------------------------
;;; g_key_file_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_ref" key-file-ref) (:pointer (:struct key-file))
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:file-key} instance}
  @return{The same @type{g:key-file} instance.}
  @begin{short}
    Increases the reference count of @arg{keyfile}.
  @end{short}
  @see-type{g:key-file}
  @see-function{g:key-file-unref}"
  (keyfile (:pointer (:struct key-file))))

(export 'key-file-ref)

;;; ----------------------------------------------------------------------------
;;; g_key_file_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_unref" key-file-unref) :void
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @begin{short}
    Decreases the reference count of @arg{keyfile} by 1.
  @end{short}
  If the reference count reaches zero, frees the key file and all its allocated
  memory.
  @see-type{g:key-file}
  @see-function{g:key-file-ref}"
  (keyfile (:pointer (:struct key-file))))

(export 'key-file-unref)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_list_separator ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_set_list_separator" %key-file-set-list-separator) :void
  (keyfile (:pointer (:struct key-file)))
  (separator :char))

(defun key-file-set-list-separator (keyfile separator)
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[separator]{a char with the separator}
  @begin{short}
    Sets the character which is used to separate values in lists.
  @end{short}
  Typically @code{';'} or @code{','} are used as separators. The default list
  separator is @code{';'}.
  @see-type{g:key-file}"
  (%key-file-set-list-separator keyfile (char-code separator)))

(export 'key-file-set-list-separator)

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_load_from_file" %key-file-load-from-file) :boolean
  (keyfile (:pointer (:struct key-file)))
  (filename :string)
  (flags key-file-flags)
  (err :pointer))

(defun key-file-load-from-file (keyfile path flags)
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[path]{a pathname or namestring with the path of a file to load}
  @argument[flags]{a @symbol{g:key-file-flags} value}
  @return{@em{True} if a key file could be loaded, @em{false} otherwise.}
  @begin{short}
    Loads a key file into a @type{g:key-file} instance.
  @end{short}
  If the file could not be loaded then @em{false} is returned.
  @see-type{g:key-file}
  @see-symbol{g:key-file-flags}"
  (with-ignore-g-error (err)
    (%key-file-load-from-file keyfile (namestring path) flags err)))

(export 'key-file-load-from-file)

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_load_from_data" %key-file-load-from-data) :boolean
  (keyfile (:pointer (:struct key-file)))
  (data :string)
  (len :size)
  (flags key-file-flags)
  (err :pointer))

(defun key-file-load-from-data (keyfile data flags)
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[data]{a string with the key file loaded in memory}
  @argument[flags]{a @symbol{g:key-file-flags} value}
  @return{@em{True} if a key file could be loaded, otherwise @em{false}.}
  @begin{short}
    Loads a key file from memory into a @type{g:key-file} instance.
  @end{short}
  If the data cannot be loaded then @em{false} is returned.
  @see-type{g:key-file}
  @see-symbol{g:key-file-flags}"
  (with-ignore-g-error (err)
    (%key-file-load-from-data keyfile data (length data) flags err)))

(export 'key-file-load-from-data)

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_bytes ()
;;;
;;; gboolean
;;; g_key_file_load_from_bytes (GKeyFile *key_file,
;;;                             GBytes *bytes,
;;;                             GKeyFileFlags flags,
;;;                             GError **error);
;;;
;;; Loads a key file from the data in bytes into an empty GKeyFile structure.
;;; If the object cannot be created then error is set to a GKeyFileError.
;;;
;;; key_file :
;;;     an empty GKeyFile struct
;;;
;;; bytes :
;;;     a GBytes
;;;
;;; flags ;
;;;     flags from GKeyFileFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if a key file could be loaded, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_data_dirs ()
;;;
;;; gboolean g_key_file_load_from_data_dirs (GKeyFile *key_file,
;;;                                          const gchar *file,
;;;                                          gchar **full_path,
;;;                                          GKeyFileFlags flags,
;;;                                          GError **error);
;;;
;;; This function looks for a key file named file in the paths returned from
;;; g_get_user_data_dir() and g_get_system_data_dirs(), loads the file into
;;; key_file and returns the file's full path in full_path. If the file could
;;; not be loaded then an error is set to either a GFileError or GKeyFileError.
;;;
;;; key_file :
;;;     an empty GKeyFile struct
;;;
;;; file :
;;;     a relative path to a filename to open and parse. [type filename]
;;;
;;; full_path :
;;;     return location for a string containing the full path of the file, or
;;;     NULL.
;;;
;;; flags :
;;;     flags from GKeyFileFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if a key file could be loaded, FALSE othewise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_dirs ()
;;;
;;; gboolean g_key_file_load_from_dirs (GKeyFile *key_file,
;;;                                     const gchar *file,
;;;                                     const gchar **search_dirs,
;;;                                     gchar **full_path,
;;;                                     GKeyFileFlags flags,
;;;                                     GError **error);
;;;
;;; This function looks for a key file named file in the paths specified in
;;; search_dirs, loads the file into key_file and returns the file's full path
;;; in full_path. If the file could not be loaded then an error is set to either
;;; a GFileError or GKeyFileError.
;;;
;;; key_file :
;;;     an empty GKeyFile struct
;;;
;;; file :
;;;     a relative path to a filename to open and parse
;;;
;;; search_dirs :
;;;     NULL-terminated array of directories to search
;;;
;;; full_path :
;;;     return location for a string containing the full path of the file,
;;;     or NULL
;;;
;;; flags :
;;;     flags from GKeyFileFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if a key file could be loaded, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_to_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_to_data" %key-file-to-data) :string
  (keyfile (:pointer (:struct key-file)))
  (len (:pointer :size))
  (err :pointer))

(defun key-file-to-data (keyfile)
 #+liber-documentation
 "@version{2023-1-25}
  @syntax[]{(g:key-file-to-data keyfile) => data, len}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[data]{a string holding the contents of the key file}
  @argument[len]{an integer with the length of @arg{data}}
  @begin{short}
    Outputs the key file as a string.
  @end{short}
  @see-type{g:key-file}
  @see-function{g:key-file-save-to-file}"
  (with-g-error (err)
    (with-foreign-object (len :size)
      (values (%key-file-to-data keyfile len err)
              (cffi:mem-ref len :size)))))

(export 'key-file-to-data)

;;; ----------------------------------------------------------------------------
;;; g_key_file_save_to_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_save_to_file" %key-file-save-to-file) :boolean
  (keyfile (:pointer (:struct key-file)))
  (filename :string)
  (err :pointer))

(defun key-file-save-to-file (keyfile path)
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[path]{a pathname or namestring with the file to write to}
  @return{@em{True} if successful, else @em{false}.}
  @begin{short}
    Writes the contents of the key file to a file.
  @end{short}
  @see-type{g:key-file}
  @see-function{g:key-file-load-from-file}"
  (with-g-error (err)
    (%key-file-save-to-file keyfile (namestring path) err)))

(export 'key-file-save-to-file)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_start_group () -> key-file-start-group
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_get_start_group" key-file-start-group) :string
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @return{A string with the start group of the key file.}
  @begin{short}
    Returns the name of the start group of the key file.
  @end{short}
  @see-type{g:key-file}"
  (keyfile (:pointer (:struct key-file))))

(export 'key-file-start-group)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_groups () -> key-file-groups
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_get_groups" %key-file-groups)
    (strv-t :free-from-foreign t)
  (keyfile (:pointer (:struct key-file)))
  (len (:pointer :size)))

(defun key-file-groups (keyfile)
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @return{A list of strings.}
  @begin{short}
    Returns all groups in the key file loaded with @arg{keyfile}.
  @end{short}
  @see-type{g:key-file}"
  (%key-file-groups keyfile (cffi:null-pointer)))

(export 'key-file-groups)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_keys () -> key-file-keys
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_get_keys" %key-file-keys) (strv-t :free-from-foreign t)
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (len (:pointer :size))
  (err :pointer))

(defun key-file-keys (keyfile group)
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name}
  @return{A list of strings.}
  @begin{short}
    Returns all keys for the group name.
  @end{short}
  In the event that the group name cannot be found, @code{nil} is returned.
  @see-type{g:key-file}"
  (with-g-error (err)
    (%key-file-keys keyfile group (cffi:null-pointer) err)))

(export 'key-file-keys)

;;; ----------------------------------------------------------------------------
;;; g_key_file_has_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_has_group" key-file-has-group) :boolean
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name}
  @return{@em{True} if @arg{group} is a part of @arg{keyfile}, @em{false}
    otherwise.}
  @begin{short}
    Looks whether the key file has the group @arg{group}.
  @end{short}
  @see-type{g:key-file}"
  (keyfile (:pointer (:struct key-file)))
  (group :string))

(export 'key-file-has-group)

;;; ----------------------------------------------------------------------------
;;; g_key_file_has_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_has_key" %key-file-has-key) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-has-key (keyfile group key)
 #+liber-documentation
 "@version{2023-1-25}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name}
  @argument[key]{a string with the key name}
  @return{@em{True} if @arg{key} is a part of @arg{group}, @em{false}
    otherwise.}
  @begin{short}
    Looks whether the key file has the key @arg{key} in the group
    @arg{group}.
  @end{short}
  @see-type{g:key-file}"
  (with-g-error (err)
    (%key-file-has-key keyfile group key err)))

(export 'key-file-has-key)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_value ()
;;; g_key_file_set_value () -> key-file-value
;;; ----------------------------------------------------------------------------

(defun (setf key-file-value) (value keyfile group key)
  (cffi:foreign-funcall "g_key_file_set_value"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :string value
                        :void)
  value)

(defcfun ("g_key_file_get_value" %key-file-value) :string
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-value (keyfile group key)
 #+liber-documentation
 "@version{2023-1-26}
  @syntax[]{(g:key-file-value keyfile group key) => value}
  @syntax[]{(setf (g:key-file-value keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name}
  @argument[key]{a string with a key}
  @argument[value]{a string with the value}
  @begin{short}
    The @sym{g:file-key-value} function returns the raw value associated with
    @arg{key} under @arg{group}.
  @end{short}
  Use the @fun{g:key-file-string} functon to retrieve an unescaped UTF-8 string.
  In the event the key or group name cannot be found, @code{nil} is returned.

  The @sym{(setf g:key-file-value)} function associates a new value with
  @arg{key} under @arg{group}. If @arg{key} cannot be found then it is created.
  If @arg{group} cannot be found then it is created. To set an UTF-8 string
  which may contain characters that need escaping (such as newlines or spaces),
  use the @fun{g:key-file-string} function.
  @see-type{g:key-file}
  @see-function{g:key-file-string}"
  (with-ignore-g-error (err)
    (%key-file-value keyfile group key err)))

(export 'key-file-value)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_string ()
;;; g_key_file_set_string () -> key-file-string
;;; ----------------------------------------------------------------------------

(defun (setf key-file-string) (value keyfile group key)
  (cffi:foreign-funcall "g_key_file_set_string"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :string value
                        :void)
  value)

(defcfun ("g_key_file_get_string" %key-file-string) :string
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-string (keyfile group key)
 #+liber-documentation
 "@version{2023-1-25}
  @syntax[]{(g:key-file-string keyfile group key) => value}
  @syntax[]{(setf (g:key-file-string keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name}
  @argument[key]{a string with the key name}
  @argument[value]{a string or @code{nil}}
  @begin{short}
    The @sym{g:key-file-string} function returns the string value associated
    with @arg{key} under @arg{group}.
  @end{short}
  In the event the key or the group name cannot be found, @code{nil} is
  returned. The @sym{(setf g:key-file-string)} function associates a new string
  value with @arg{key} under @arg{group}. If @arg{key} or @arg{group} cannot be
  found then they are created. Unlike the @fun{g:key-file-value} function, this
  function handles characters that need escaping, such as newlines.
  @see-type{g:key-file}
  @see-function{g:key-file-value}"
  (with-ignore-g-error (err)
    (%key-file-string keyfile group key err)))

(export 'key-file-string)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_locale_string ()
;;;
;;; gchar * g_key_file_get_locale_string (GKeyFile *key_file,
;;;                                       const gchar *group_name,
;;;                                       const gchar *key,
;;;                                       const gchar *locale,
;;;                                       GError **error);
;;;
;;; Returns the value associated with key under group_name translated in the
;;; given locale if available. If locale is NULL then the current locale is
;;; assumed.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. If the value associated with key cannot be
;;; interpreted or no suitable translation can be found then the untranslated
;;; value is returned.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; locale :
;;;     a locale identifier or NULL. [allow-none]
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a newly allocated string or NULL if the specified key cannot be found.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_locale_for_key ()
;;;
;;; gchar *
;;; g_key_file_get_locale_for_key (GKeyFile *key_file,
;;;                                const gchar *group_name,
;;;                                const gchar *key,
;;;                                const gchar *locale);
;;;
;;; Returns the actual locale which the result of g_key_file_get_locale_string()
;;; or g_key_file_get_locale_string_list() came from.
;;;
;;; If calling g_key_file_get_locale_string() or
;;; g_key_file_get_locale_string_list() with exactly the same key_file ,
;;; group_name , key and locale , the result of those functions will have
;;; originally been tagged with the locale that is the result of this function.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; locale :
;;;     a locale identifier or NULL.
;;;
;;; Returns :
;;;     the locale from the file, or NULL if the key was not found or the entry
;;;     in the file was was untranslated.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_boolean ()
;;;
;;; gboolean g_key_file_get_boolean (GKeyFile *key_file,
;;;                                  const gchar *group_name,
;;;                                  const gchar *key,
;;;                                  GError **error);
;;;
;;; Returns the value associated with key under group_name as a boolean.
;;;
;;; If key cannot be found then FALSE is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the value associated with key
;;; cannot be interpreted as a boolean then FALSE is returned and error is set
;;; to G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as a boolean, or FALSE if the key was
;;;     not found or could not be parsed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_integer ()
;;;
;;; gint g_key_file_get_integer (GKeyFile *key_file,
;;;                              const gchar *group_name,
;;;                              const gchar *key,
;;;                              GError **error);
;;;
;;; Returns the value associated with key under group_name as an integer.
;;;
;;; If key cannot be found then 0 is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the value associated with key
;;; cannot be interpreted as an integer then 0 is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as an integer, or 0 if the key was not
;;;     found or could not be parsed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_int64 ()
;;;
;;; gint64 g_key_file_get_int64 (GKeyFile *key_file,
;;;                              const gchar *group_name,
;;;                              const gchar *key,
;;;                              GError **error);
;;;
;;; Returns the value associated with key under group_name as a signed 64-bit
;;; integer. This is similar to g_key_file_get_integer() but can return 64-bit
;;; results without truncation.
;;;
;;; key_file :
;;;     a non-NULL GKeyFile
;;;
;;; group_name :
;;;     a non-NULL group name
;;;
;;; key :
;;;     a non-NULL key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as a signed 64-bit integer, or 0 if
;;;     the key was not found or could not be parsed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_uint64 ()
;;;
;;; guint64 g_key_file_get_uint64 (GKeyFile *key_file,
;;;                                const gchar *group_name,
;;;                                const gchar *key,
;;;                                GError **error);
;;;
;;; Returns the value associated with key under group_name as an unsigned
;;; 64-bit integer. This is similar to g_key_file_get_integer() but can return
;;; large positive results without truncation.
;;;
;;; key_file :
;;;     a non-NULL GKeyFile
;;;
;;; group_name :
;;;     a non-NULL group name
;;;
;;; key :
;;;     a non-NULL key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as an unsigned 64-bit integer, or 0
;;;     if the key was not found or could not be parsed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_double ()
;;;
;;; gdouble g_key_file_get_double (GKeyFile *key_file,
;;;                                const gchar *group_name,
;;;                                const gchar *key,
;;;                                GError **error);
;;;
;;; Returns the value associated with key under group_name as a double. If
;;; group_name is NULL, the start_group is used.
;;;
;;; If key cannot be found then 0.0 is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the value associated with key
;;; cannot be interpreted as a double then 0.0 is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as a double, or 0.0 if the key was not
;;;     found or could not be parsed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_string_list ()
;;; g_key_file_set_string_list () -> key-file-string-list
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_set_string_list" %key-file-set-string-list) :void
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (value strv-t)
  (len :size))

(defun (setf key-file-string-list) (value keyfile group key)
  (%key-file-set-string-list keyfile group key value (length value))
  value)

(defcfun ("g_key_file_get_string_list" %key-file-get-string-list) strv-t
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (len (:pointer :size))
  (err :pointer))

(defun key-file-string-list (keyfile group key)
 #+liber-documentation
 "@version{2023-1-25}
  @syntax[]{(g:key-file-string-list keyfile group key) => value}
  @syntax[]{(setf (g:key-file-string-list keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name}
  @argument[key]{a string with the key name}
  @argument[value]{a list of strings}
  @begin{short}
    The @sym{g:key-file-string-list} function returns the values associated
    with @arg{key} under @arg{group}.
  @end{short}
  In the event the key or the group name cannot be found, @code{nil} is
  returned. The @sym{(setf g:key-file-string-list)} function associates a list
  of string values for @arg{key} under @arg{group}. If @arg{key} or @arg{group}
  cannot be found then they are created.
  @see-type{g:key-file}"
  (with-ignore-g-error (err)
    (with-foreign-object (len :size)
      (%key-file-get-string-list keyfile group key len err))))

(export 'key-file-string-list)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_locale_string_list ()
;;;
;;; gchar ** g_key_file_get_locale_string_list (GKeyFile *key_file,
;;;                                             const gchar *group_name,
;;;                                             const gchar *key,
;;;                                             const gchar *locale,
;;;                                             gsize *length,
;;;                                             GError **error);
;;;
;;; Returns the values associated with key under group_name translated in the
;;; given locale if available. If locale is NULL then the current locale is
;;; assumed.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. If the values associated with key cannot be
;;; interpreted or no suitable translations can be found then the untranslated
;;; values are returned. The returned array is NULL-terminated, so length may
;;; optionally be NULL.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; locale :
;;;     a locale identifier or NULL
;;;
;;; length :
;;;     return location for the number of returned strings or NULL
;;;
;;; error :
;;;     return location for a GError or NULL
;;;
;;; Returns :
;;;     a newly allocated NULL-terminated string array or NULL if the key isn't
;;;     found. The string array should be freed with g_strfreev().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_boolean_list ()
;;;
;;; gboolean * g_key_file_get_boolean_list (GKeyFile *key_file,
;;;                                         const gchar *group_name,
;;;                                         const gchar *key,
;;;                                         gsize *length,
;;;                                         GError **error);
;;;
;;; Returns the values associated with key under group_name as booleans.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the values associated with key
;;; cannot be interpreted as booleans then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; length :
;;;     the number of booleans returned. [out]
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the values associated with the key as a list of booleans, or NULL if the
;;;     key was not found or could not be parsed. The returned list of booleans
;;;     should be freed with g_free() when no longer needed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_integer_list ()
;;;
;;; gint * g_key_file_get_integer_list (GKeyFile *key_file,
;;;                                     const gchar *group_name,
;;;                                     const gchar *key,
;;;                                     gsize *length,
;;;                                     GError **error);
;;;
;;; Returns the values associated with key under group_name as integers.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the values associated with key
;;; cannot be interpreted as integers then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; length :
;;;     the number of integers returned. [out]
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the values associated with the key as a list of integers, or NULL if the
;;;     key was not found or could not be parsed. The returned list of integers
;;;     should be freed with g_free() when no longer needed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_double_list ()
;;;
;;; gdouble * g_key_file_get_double_list (GKeyFile *key_file,
;;;                                       const gchar *group_name,
;;;                                       const gchar *key,
;;;                                       gsize *length,
;;;                                       GError **error);
;;;
;;; Returns the values associated with key under group_name as doubles.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the values associated with key
;;; cannot be interpreted as doubles then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; length :
;;;     the number of doubles returned. [out]
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the values associated with the key as a list of doubles, or NULL if the
;;;     key was not found or could not be parsed. The returned list of doubles
;;;     should be freed with g_free() when no longer needed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_comment ()
;;;
;;; gchar * g_key_file_get_comment (GKeyFile *key_file,
;;;                                 const gchar *group_name,
;;;                                 const gchar *key,
;;;                                 GError **error);
;;;
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name, or NULL. [allow-none]
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     a comment that should be freed with g_free()
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; g_key_file_set_comment ()
;;;
;;; gboolean g_key_file_set_comment (GKeyFile *key_file,
;;;                                  const gchar *group_name,
;;;                                  const gchar *key,
;;;                                  const gchar *comment,
;;;                                  GError **error);
;;;
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name, or NULL. [allow-none]
;;;
;;; key :
;;;     a key. [allow-none]
;;;
;;; comment :
;;;     a comment
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     TRUE if the comment was written, FALSE otherwise
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_set_comment" %key-file-set-comment) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (comment :string)
  (err :pointer))

(defun (setf key-file-comment) (value keyfile group key)
  (with-ignore-g-error (err)
    (%key-file-set-comment keyfile
                           (ensure-pointer group)
                           (ensure-pointer key)
                           value
                           err)
    value))

(defcfun ("g_key_file_get_comment" %key-file-get-comment) :string
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-comment (keyfile group key)
 #+liber-documentation
 "@version{2023-1-26}
  @syntax[]{(g:key-file-comment keyfile group key) => value}
  @syntax[]{(setf (g:key-file-comment keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name}
  @argument[key]{a string with the key name}
  @argument[value]{a string with the comment}
  @begin{short}
    The @sym{g:key-file-comment} retrieves a comment above @arg{key} from
    @arg{group}.
  @end{short}
  If @arg{key} is @code{nil} then the comment will be read from above
  @arg{group}. If both @arg{key} and @arg{group} are @code{nil}, then the
  comment will be read from above the first group in the file.

  The @sym{(setf g:key-file-comment)} function places a comment above @arg{key}
  from @arg{group}. If @arg{key} is @code{nil} then the comment will be written
  above @arg{group}. If both @arg{key} and @arg{group} are @code{nil}, then the
  comment will be written above the first group in the file.
  @see-type{g:key-file}"
  (with-ignore-g-error (err)
    (%key-file-get-comment keyfile
                           (ensure-pointer group)
                           (ensure-pointer key)
                           err)))

(export 'key-file-comment)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_locale_string ()
;;;
;;; void g_key_file_set_locale_string (GKeyFile *key_file,
;;;                                    const gchar *group_name,
;;;                                    const gchar *key,
;;;                                    const gchar *locale,
;;;                                    const gchar *string);
;;;
;;; Associates a string value for key and locale under group_name. If the
;;; translation for key cannot be found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; locale :
;;;     a locale identifier
;;;
;;; string :
;;;     a string
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_boolean ()
;;;
;;; void g_key_file_set_boolean (GKeyFile *key_file,
;;;                              const gchar *group_name,
;;;                              const gchar *key,
;;;                              gboolean value);
;;;
;;; Associates a new boolean value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     TRUE or FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_integer ()
;;;
;;; void g_key_file_set_integer (GKeyFile *key_file,
;;;                              const gchar *group_name,
;;;                              const gchar *key,
;;;                              gint value);
;;;
;;; Associates a new integer value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     an integer value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_int64 ()
;;;
;;; void g_key_file_set_int64 (GKeyFile *key_file,
;;;                            const gchar *group_name,
;;;                            const gchar *key,
;;;                            gint64 value);
;;;
;;; Associates a new integer value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     an integer value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_uint64 ()
;;;
;;; void g_key_file_set_uint64 (GKeyFile *key_file,
;;;                             const gchar *group_name,
;;;                             const gchar *key,
;;;                             guint64 value);
;;;
;;; Associates a new integer value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     an integer value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_double ()
;;;
;;; void g_key_file_set_double (GKeyFile *key_file,
;;;                             const gchar *group_name,
;;;                             const gchar *key,
;;;                             gdouble value);
;;;
;;; Associates a new double value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     an double value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_locale_string_list ()
;;;
;;; void g_key_file_set_locale_string_list (GKeyFile *key_file,
;;;                                         const gchar *group_name,
;;;                                         const gchar *key,
;;;                                         const gchar *locale,
;;;                                         const gchar * const list[],
;;;                                         gsize length);
;;;
;;; Associates a list of string values for key and locale under group_name. If
;;; the translation for key cannot be found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; locale :
;;;     a locale identifier
;;;
;;; list :
;;;     a NULL-terminated array of locale string values
;;;
;;; length :
;;;     the length of list
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_boolean_list ()
;;;
;;; void g_key_file_set_boolean_list (GKeyFile *key_file,
;;;                                   const gchar *group_name,
;;;                                   const gchar *key,
;;;                                   gboolean list[],
;;;                                   gsize length);
;;;
;;; Associates a list of boolean values with key under group_name. If key cannot
;;; be found then it is created. If group_name is NULL, the start_group is used.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; list :
;;;     an array of boolean values. [array length=length]
;;;
;;; length :
;;;     length of list
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_integer_list ()
;;;
;;; void g_key_file_set_integer_list (GKeyFile *key_file,
;;;                                   const gchar *group_name,
;;;                                   const gchar *key,
;;;                                   gint list[],
;;;                                   gsize length);
;;;
;;; Associates a list of integer values with key under group_name. If key cannot
;;; be found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; list :
;;;     an array of integer values. [array length=length]
;;;
;;; length :
;;;     number of integer values in list
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_double_list ()
;;;
;;; void g_key_file_set_double_list (GKeyFile *key_file,
;;;                                  const gchar *group_name,
;;;                                  const gchar *key,
;;;                                  gdouble list[],
;;;                                  gsize length);
;;;
;;; Associates a list of double values with key under group_name. If key cannot
;;; be found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; list :
;;;     an array of double values. [array length=length]
;;;
;;; length :
;;;     number of double values in list
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_remove_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_remove_group" %key-file-remove-group) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (err :pointer))

(defun key-file-remove-group (keyfile group)
 #+liber-documentation
 "@version{#2023-1-26}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name to remove}
  @return{@em{True} if the group was removed, @em{false} otherwise.}
  @begin{short}
    Removes the specified group from the key file.
  @end{short}
  @see-type{g:key-file}"
  (with-ignore-g-error (err)
    (%key-file-remove-group keyfile group err)))

(export 'key-file-remove-group)

;;; ----------------------------------------------------------------------------
;;; g_key_file_remove_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_remove_key" %key-file-remove-key) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-remove-key (keyfile group key)
 #+liber-documentation
 "@version{#2023-1-26}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name}
  @argument[key]{a string with the key name to remove}
  @return{@em{True} if the key was removed, @em{false} otherwise.}
  @begin{short}
    Removes the specified key from the key file.
  @end{short}
  @see-type{g:key-file}"
  (with-ignore-g-error (err)
    (%key-file-remove-key keyfile group key err)))

(export 'key-file-remove-key)

;;; ----------------------------------------------------------------------------
;;; g_key_file_remove_comment ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_remove_comment" %key-file-remove-comment) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-remove-comment (keyfile group key)
 #+liber-documentation
 "@version{#2023-1-26}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string with the group name}
  @argument[key]{a string with the key name to remove}
  @return{@em{True} if the comment was removed, @em{false} otherwise.}
  @begin{short}
    Removes a comment above @arg{key} from @arg{group}.
  @end{short}
  If @arg{key} is @code{nil} then the comment will be removed above @arg{group}.
  If both @arg{key} and @arg{group} are @code{nil}, then the comment will be
  removed above the first group in the file.
  @see-type{g:key-file}"
  (with-ignore-g-error (err)
    (%key-file-remove-comment keyfile
                              (ensure-pointer group)
                              (ensure-pointer key)
                              err)))

(export 'key-file-remove-comment)

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_GROUP
;;;
;;; #define G_KEY_FILE_DESKTOP_GROUP                "Desktop Entry"
;;;
;;; The name of the main group of a desktop entry file, as defined in the
;;; Desktop Entry Specification. Consult the specification for more details
;;; about the meanings of the keys below.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_TYPE
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_TYPE             "Type"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the
;;; type of the desktop entry. Usually G_KEY_FILE_DESKTOP_TYPE_APPLICATION,
;;; G_KEY_FILE_DESKTOP_TYPE_LINK, or G_KEY_FILE_DESKTOP_TYPE_DIRECTORY.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_VERSION
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_VERSION          "Version"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the
;;; version of the Desktop Entry Specification used for the desktop entry file.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_NAME
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_NAME             "Name"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a localized string
;;; giving the specific name of the desktop entry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_GENERIC_NAME
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_GENERIC_NAME     "GenericName"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a localized string
;;; giving the generic name of the desktop entry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_NO_DISPLAY
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_NO_DISPLAY       "NoDisplay"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a boolean stating
;;; whether the desktop entry should be shown in menus.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_COMMENT
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_COMMENT          "Comment"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a localized string
;;; giving the tooltip for the desktop entry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_ICON
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_ICON             "Icon"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a localized string
;;; giving the name of the icon to be displayed for the desktop entry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_HIDDEN
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_HIDDEN           "Hidden"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a boolean stating
;;; whether the desktop entry has been deleted by the user.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_ONLY_SHOW_IN
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_ONLY_SHOW_IN     "OnlyShowIn"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a list of strings
;;; identifying the environments that should display the desktop entry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_NOT_SHOW_IN
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_NOT_SHOW_IN      "NotShowIn"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a list of strings
;;; identifying the environments that should not display the desktop entry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_TRY_EXEC
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_TRY_EXEC         "TryExec"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the
;;; file name of a binary on disk used to determine if the program is actually
;;; installed. It is only valid for desktop entries with the Application type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_EXEC
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_EXEC             "Exec"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the
;;; command line to execute. It is only valid for desktop entries with the
;;; Application type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_PATH
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_PATH             "Path"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string containing
;;; the working directory to run the program in. It is only valid for desktop
;;; entries with the Application type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_TERMINAL
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_TERMINAL         "Terminal"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a boolean stating
;;; whether the program should be run in a terminal window. It is only valid
;;; for desktop entries with the Application type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_MIME_TYPE
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_MIME_TYPE        "MimeType"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a list of strings
;;; giving the MIME types supported by this desktop entry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_CATEGORIES
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_CATEGORIES       "Categories"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a list of strings
;;; giving the categories in which the desktop entry should be shown in a menu.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_STARTUP_NOTIFY
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_STARTUP_NOTIFY   "StartupNotify"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a boolean stating
;;; whether the application supports the Startup Notification Protocol
;;; Specification.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_STARTUP_WM_CLASS
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_STARTUP_WM_CLASS "StartupWMClass"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is string identifying the
;;; WM class or name hint of a window that the application will create, which
;;; can be used to emulate Startup Notification with older applications.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_URL
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_URL              "URL"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the
;;; URL to access. It is only valid for desktop entries with the Link type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_TYPE_APPLICATION
;;;
;;; #define G_KEY_FILE_DESKTOP_TYPE_APPLICATION     "Application"
;;;
;;; The value of the G_KEY_FILE_DESKTOP_KEY_TYPE, key for desktop entries
;;; representing applications.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_TYPE_LINK
;;;
;;; #define G_KEY_FILE_DESKTOP_TYPE_LINK            "Link"
;;;
;;; The value of the G_KEY_FILE_DESKTOP_KEY_TYPE, key for desktop entries
;;; representing links to documents.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_TYPE_DIRECTORY
;;;
;;; #define G_KEY_FILE_DESKTOP_TYPE_DIRECTORY "Directory"
;;;
;;; The value of the G_KEY_FILE_DESKTOP_KEY_TYPE, key for desktop entries
;;; representing directories.
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.key-value.lisp ----------------------------------------
