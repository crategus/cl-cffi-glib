;;; ----------------------------------------------------------------------------
;;; glib.key-value.lisp
;;;
;;; The documentation in this file is taken from the GLib Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GLib library,
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
;;; Key-value file parser
;;;
;;;     parses .ini-like config files
;;;
;;; Types and Values
;;;
;;;     GKeyFile
;;;     GKeyFileFlags
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

;;; ----------------------------------------------------------------------------
;;; GKeyFileFlags
;;; ----------------------------------------------------------------------------

(cffi:defbitfield key-file-flags
  (:none 0)
  (:keep-comments #.(ash 1 0))
  (:keep-translations #.(ash 1 1)))

#+liber-documentation
(setf (liber:alias-for-symbol 'key-file-flags)
      "Bitfield"
      (liber:symbol-documentation 'key-file-flags)
 "@version{2025-05-23}
  @begin{declaration}
(cffi:defbitfield key-file-flags
  (:none 0)
  (:keep-comments #.(ash 1 0))
  (:keep-translations #.(ash 1 1)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No flags, default behaviour.}
      @entry[:keep-coments]{Use this flag if you plan to write the possibly
        modified contents of the key file back to a file. Otherwise all comments
        will be lost when the key file is written back.}
      @entry[:keep-translations]{Use this flag if you plan to write the possibly
        modified contents of the key file back to a file. Otherwise only the
        translations for the current language will be written back.}
    @end{table}
  @end{values}
  @begin{short}
    Flags which influence the parsing of key values.
  @end{short}
  @see-type{g:key-file}")

(export 'key-file-flags)

;;; ----------------------------------------------------------------------------
;;; GKeyFile
;;; ----------------------------------------------------------------------------

(cffi:defcstruct key-file)

#+liber-documentation
(setf (liber:alias-for-type 'key-file)
      "CStruct"
      (documentation 'key-file 'type)
 "@version{2025-05-23}
  @begin{declaration}
(cffi:defcstruct key-file)
  @end{declaration}
  @begin{short}
    The @type{g:key-file} structure lets you parse, edit or create files
    containing groups of key-value pairs, which we call key files for lack of a
    better name.
  @end{short}
  Several freedesktop.org specifications use key files now, for example, the
  @url[https://specifications.freedesktop.org/desktop-entry-spec/latest/]{Desktop
  Entry Specification} and the
  @url[https://specifications.freedesktop.org/icon-theme-spec/latest/]{ Icon
  Theme Specification}.

  The syntax of key files is described in detail in the
  @url[https://specifications.freedesktop.org/desktop-entry-spec/latest/]{Desktop
  Entry Specification}, here is a quick summary: Key files consists of groups of
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
  Note that in contrast to the
  @url[https://specifications.freedesktop.org/desktop-entry-spec/latest/]{Desktop
  Entry Specification}, groups in key files may contain the same key multiple
  times. The last entry wins. Key files may also contain multiple groups with
  the same name. They are merged together. Another difference is that keys and
  group names in key files are not restricted to ASCII characters.

  This is a list of standard group and key names for key files.
  @begin[code]{table}
    @entry[\"Desktop Entry\"]{The name of the main group of a desktop entry
      file, as defined in the Desktop Entry Specification. Consult the
      specification for more details about the meanings of the keys below.}
    @entry[\"Type\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      string giving the type of the desktop entry. Usually
      @code{\"Application\"}, @code{\"Link\"}, or @code{\"Directory\"}.}
    @entry[\"Version\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      string giving the version of the Desktop Entry Specification used for the
      desktop entry file.}
    @entry[\"Name\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      localized string giving the specific name of the desktop entry.}
    @entry[\"GenericName\"]{A key under @code{\"Desktop Entry\"}, whose value
      is a localized string giving the generic name of the desktop entry.}
    @entry[\"NoDisplay\"]{A key under @code{\"Desktop Entry\"}, whose value is
      a boolean stating whether the desktop entry should be shown in menus.}
    @entry[\"Comment\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      localized string giving the tooltip for the desktop entry.}
    @entry[\"Icon\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      localized string giving the name of the icon to be displayed for the
      desktop entry.}
    @entry[\"Hidden\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      boolean stating whether the desktop entry has been deleted by the user.}
    @entry[\"OnlyShowIn\"]{A key under @code{\"Desktop Entry\"}, whose value is
      a list of strings identifying the environments that should display the
      desktop entry.}
    @entry[\"NotShowIn\"]{A key under @code{\"Desktop Entry\"}, whose value is
      a list of strings identifying the environments that should not display
      the desktop entry.}
    @entry[\"TryExec\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      string giving the file name of a binary on disk used to determine if the
      program is actually installed. It is only valid for desktop entries with
      the Application type.}
    @entry[\"Exec\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      string giving the command line to execute. It is only valid for desktop
      entries with the Application type.}
    @entry[\"Path\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      string containing the working directory to run the program in. It is only
      valid for desktop entries with the Application type.}
    @entry[\"Terminal\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      boolean stating whether the program should be run in a terminal window. It
      is only valid for desktop entries with the Application type.}
    @entry[\"MimeType\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      list of strings giving the MIME types supported by this desktop entry.}
    @entry[\"Categories\"]{A key under @code{\"Desktop Entry\"}, whose value is
      a list of strings giving the categories in which the desktop entry should
      be shown in a menu.}
    @entry[\"StartupNotify\"]{A key under @code{\"Desktop Entry\"}, whose value
      is a boolean stating whether the application supports the Startup
      Notification Protocol Specification.}
    @entry[\"StartupWMClass\"]{A key under @code{\"Desktop Entry\"}, whose value
      is string identifying the WM class or name hint of a window that the
      application will create, which can be used to emulate Startup Notification
      with older applications.}
    @entry[\"URL\"]{A key under @code{\"Desktop Entry\"}, whose value is a
      string giving the URL to access. It is only valid for desktop entries with
      the Link type.}
    @entry[\"Application\"]{The value of the @code{\"Type\"}, key for desktop
    entries representing applications.}
    @entry[\"Link\"]{The value of the @code{\"Type\"}, key for desktop entries
      representing links to documents.}
    @entry[\"Directory\"]{The value of the @code{\"Type\"}, key for desktop
      entries representing directories.}
  @end{table}
  @begin[Examples]{dictionary}
    Here is an example of loading a key file and reading a value:
    @begin{pre}
(g:with-key-file (keyfile)
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
(g:with-key-file (keyfile)
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
  @see-macro{g:with-key-file}")

(export 'key-file)

;;; ----------------------------------------------------------------------------
;;; g:with-key-file
;;; ----------------------------------------------------------------------------

(defmacro with-key-file ((keyfile) &body body)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:with-key-file (keyfile) body) => result}
  @argument[keyfile]{a newly allocated @type{g:key-file} instance}
  @begin{short}
    The @macro{g:with-key-file} macro allocates a new @type{g:key-file} instance
    and executes the body that uses the key file.
  @end{short}
  After execution of the body the allocated memory for the key file is released
  using the @fun{g:key-file-free} function.

  The key file is created using the @fun{g:key-file-new} function. Use the
  @fun{g:key-file-load-from-file} or @fun{g:key-file-load-from-data} functions
  to read an existing key file. Alternatively, use the
  @macro{g:with-key-file-from-file} or @macro{g:with-key-file-from-data} macros
  to create and load the key file in one step.
  @see-type{g:key-file}
  @see-function{g:key-file-free}
  @see-macro{g:with-key-file-from-file}
  @see-macro{g:with-key-file-from-data}
  @see-function{g:key-file-load-from-file}
  @see-function{g:key-file-load-from-data}"
  `(let ((,keyfile (key-file-new)))
     (unwind-protect
       (progn ,@body)
       (key-file-free ,keyfile))))

(export 'with-key-file)

;;; ----------------------------------------------------------------------------
;;; g:with-key-file-from-file
;;; ----------------------------------------------------------------------------

(defmacro with-key-file-from-file
          ((keyfile path &optional (flags :none)) &body body)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:with-key-file-from-file (keyfile path flags) body) => result}
  @argument[keyfile]{a newly allocated @type{g:key-file} instance}
  @argument[path]{a pathname or namestring for the path of a file to load}
  @argument[flags]{an optional @symbol{g:key-file-flags} value, the default
    value is @code{:none}}
  @begin{short}
    The @macro{g:with-key-file-from-file} macro allocates a new
    @type{g:key-file} instance and executes the body that uses the key file.
  @end{short}
  After execution of the body the allocated memory for the key file is released
  using the @fun{g:key-file-free} function.

  The key file is created using the @fun{g:key-file-new} function and loaded
  using the @fun{g:key-file-load-from-file} function. If the key file cannot be
  loaded then an error condition is thrown.
  @see-type{g:key-file}
  @see-symbol{g:key-file-flags}
  @see-function{g:key-file-free}
  @see-macro{g:with-key-file}
  @see-function{g:key-file-load-from-file}"
  `(let ((,keyfile (key-file-new)))
     (unwind-protect
       (if (key-file-load-from-file ,keyfile (namestring ,path) ,flags)
           (progn ,@body)
           (cl:error "G:WITH-KEY-FILE-FROM-FILE: Key file cannot be loaded."))
       (key-file-free ,keyfile))))

(export 'with-key-file-from-file)

;;; ----------------------------------------------------------------------------
;;; g:with-key-file-from-data
;;; ----------------------------------------------------------------------------

(defmacro with-key-file-from-data
          ((keyfile data &optional (flags :none)) &body body)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:with-key-file-from-data (keyfile data flags) body) => result}
  @argument[keyfile]{a newly allocated @type{g:key-file} instance}
  @argument[data]{a string for the key file loaded in memory}
  @argument[flags]{an optional @symbol{g:key-file-flags} value, the default
    value is @code{:none}}
  @begin{short}
    The @macro{g:with-key-file-from-data} macro allocates a new
    @type{g:key-file} instance and executes the body that uses the key file.
  @end{short}
  After execution of the body the allocated memory for the key file is released
  using the @fun{g:key-file-free} function.

  The key file is created using the @fun{g:key-file-new} function and loaded
  using the @fun{g:key-file-load-from-data} function. If the key file cannot be
  loaded then an error condition is thrown.
  @see-type{g:key-file}
  @see-symbol{g:key-file-flags}
  @see-function{g:key-file-free}
  @see-macro{g:with-key-file}
  @see-function{g:key-file-new}
  @see-function{g:key-file-load-from-data}"
  `(let ((,keyfile (key-file-new)))
     (unwind-protect
       (if (key-file-load-from-data ,keyfile (namestring ,data) ,flags)
           (progn ,@body)
           (cl:error "G:WITH-KEY-FILE-FROM-DATA: Key file cannot be loaded."))
       (key-file-free ,keyfile))))

(export 'with-key-file-from-data)

;;; ----------------------------------------------------------------------------
;;; g_key_file_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_new" key-file-new) (:pointer (:struct key-file))
 #+liber-documentation
 "@version{2025-05-23}
  @return{The new empty @type{g:key-file} instance.}
  @begin{short}
    Creates a new empty @type{g:key-file} instance.
  @end{short}
  Use the @fun{g:key-file-load-from-file}, or @fun{g:key-file-load-from-data}
  functions to read an existing key file.
  @see-type{g:key-file}
  @see-macro{g:with-key-file}
  @see-function{g:key-file-load-from-file}
  @see-function{g:key-file-load-from-data}")

(export 'key-file-new)

;;; ----------------------------------------------------------------------------
;;; g_key_file_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_free" key-file-free) :void
 #+liber-documentation
 "@version{2025-05-23}
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
;;; g_key_file_ref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_ref" key-file-ref) (:pointer (:struct key-file))
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @return{The same @type{g:key-file} instance.}
  @begin{short}
    Increases the reference count of @arg{keyfile}.
  @end{short}
  @see-type{g:key-file}
  @see-function{g:key-file-unref}"
  (keyfile (:pointer (:struct key-file))))

(export 'key-file-ref)

;;; ----------------------------------------------------------------------------
;;; g_key_file_unref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_unref" key-file-unref) :void
 #+liber-documentation
 "@version{2025-05-23}
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
;;; g_key_file_set_list_separator
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_set_list_separator" %key-file-set-list-separator)
    :void
  (keyfile (:pointer (:struct key-file)))
  (separator :char))

(defun key-file-set-list-separator (keyfile separator)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[separator]{a char for the separator}
  @begin{short}
    Sets the character which is used to separate values in lists.
  @end{short}
  Typically @code{';'} or @code{','} are used as separators. The default list
  separator is @code{';'}.
  @see-type{g:key-file}"
  (%key-file-set-list-separator keyfile (char-code separator)))

(export 'key-file-set-list-separator)

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_load_from_file" %key-file-load-from-file) :boolean
  (keyfile (:pointer (:struct key-file)))
  (filename :string)
  (flags key-file-flags)
  (err :pointer))

(defun key-file-load-from-file (keyfile path flags)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[path]{a pathname or namestring for the path of a file to load}
  @argument[flags]{a @symbol{g:key-file-flags} value}
  @return{@em{True} if a key file could be loaded, @em{false} otherwise.}
  @begin{short}
    Loads a key file into a @type{g:key-file} instance.
  @end{short}
  If the file could not be loaded then @em{false} is returned.
  @see-type{g:key-file}
  @see-symbol{g:key-file-flags}
  @see-macro{g:with-key-file-from-file}"
  (with-ignore-error (err)
    (%key-file-load-from-file keyfile (namestring path) flags err)))

(export 'key-file-load-from-file)

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_load_from_data" %key-file-load-from-data) :boolean
  (keyfile (:pointer (:struct key-file)))
  (data :string)
  (len :size)
  (flags key-file-flags)
  (err :pointer))

(defun key-file-load-from-data (keyfile data flags)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[data]{a string for the key file loaded in memory}
  @argument[flags]{a @symbol{g:key-file-flags} value}
  @return{@em{True} if a key file could be loaded, otherwise @em{false}.}
  @begin{short}
    Loads a key file from memory into a @type{g:key-file} instance.
  @end{short}
  If the data cannot be loaded then @em{false} is returned.
  @see-type{g:key-file}
  @see-symbol{g:key-file-flags}"
  (with-ignore-error (err)
    (%key-file-load-from-data keyfile data (length data) flags err)))

(export 'key-file-load-from-data)

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_bytes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_load_from_bytes" %key-file-load-from-bytes) :boolean
  (keyfile (:pointer (:struct key-file)))
  (bytes (boxed bytes))
  (flags key-file-flags)
  (err :pointer))

(defun key-file-load-from-bytes (keyfile bytes flags)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[bytes]{a @class{g:bytes} instance}
  @argument[flags]{a @symbol{g:key-file-flags} value}
  @return{@em{True} if a key file could be loaded, otherwise @em{false}.}
  @begin{short}
    Loads a key file from the data in the @class{g:bytes} instance.
  @end{short}
  If the data cannot be loaded then @em{false} is returned.
  @see-type{g:key-file}
  @see-class{g:bytes}
  @see-symbol{g:key-file-flags}"
  (with-error (err)
    (%key-file-load-from-bytes keyfile bytes flags err)))

(export 'key-file-load-from-bytes)

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_data_dirs
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_load_from_data_dirs" %key-file-load-from-data-dirs)
    :boolean
  (keyfile (:pointer (:struct key-file)))
  (file :string)
  (path :pointer)
  (flags key-file-flags)
  (err :pointer))

(defun key-file-load-from-data-dirs (keyfile file flags)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @symbol{g:key-file} instance}
  @argument[file]{a string for the relative path to a filename to open and
    parse}
  @argument[flags]{a @symbol{g:key-file-flags} value}
  @begin{return}
    The string containing the full path of the file, or @code{nil} if the file
    cannot be loaded.
  @end{return}
  @begin{short}
    This function looks for a key file named file in the paths returned from
    the @code{g_get_user_data_dir()} and @code{g_get_system_data_dirs()}
    functions, loads the file into @arg{keyfile} and returns the full path of
    the file.
  @end{short}
  If the file could not be loaded then @code{nil} is returned.
  @see-symbol{g:key-file}
  @see-symbol{g:key-file-flags}"
  (with-ignore-error (err)
    (cffi:with-foreign-object (path :pointer)
      (when (%key-file-load-from-data-dirs keyfile file path flags err)
        (values (cffi:mem-ref path :string))))))

(export 'key-file-load-from-data-dirs)

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_dirs
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_load_from_dirs" %key-file-load-from-dirs) :boolean
  (keyfile (:pointer (:struct key-file)))
  (file :string)
  (dirs :pointer)
  (path :pointer)
  (flags key-file-flags)
  (err :pointer))

(defun key-file-load-from-dirs (keyfile file dirs flags)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @symbol{g:key-file} instance}
  @argument[file]{a string for the relative path to a filename to open and
    parse}
  @argument[dirs]{a list of strings for the directories to search}
  @argument[flags]{a @symbol{g:key-file-flags} value}
  @begin{return}
    The string containing the full path of the file, or @code{nil} if the file
    could not be loaded.
  @end{return}
  @begin{short}
    This function looks for a key file named @arg{file} in the paths specified
    in @arg{dirs}, loads the file into @arg{keyfile} and returns the full path
    of the file.
  @end{short}
  If the file could not be loaded then @code{nil} is returned.
  @see-symbol{g:key-file}
  @see-symbol{g:key-file-flags}"
  (with-ignore-error (err)
    (glib-sys:with-foreign-string-array (ptr dirs)
      (cffi:with-foreign-object (path :pointer)
        (when (%key-file-load-from-dirs keyfile file ptr path flags err)
          (values (cffi:mem-ref path :string)))))))

(export 'key-file-load-from-dirs)

;;; ----------------------------------------------------------------------------
;;; g_key_file_to_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_to_data" %key-file-to-data) :string
  (keyfile (:pointer (:struct key-file)))
  (len (:pointer :size))
  (err :pointer))

(defun key-file-to-data (keyfile)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-to-data keyfile) => data, len}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[data]{a string holding the contents of the key file}
  @argument[len]{an integer for the length of @arg{data}}
  @begin{short}
    Outputs the key file as a string.
  @end{short}
  @see-type{g:key-file}
  @see-function{g:key-file-save-to-file}"
  (with-error (err)
    (cffi:with-foreign-object (len :size)
      (values (%key-file-to-data keyfile len err)
              (cffi:mem-ref len :size)))))

(export 'key-file-to-data)

;;; ----------------------------------------------------------------------------
;;; g_key_file_save_to_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_save_to_file" %key-file-save-to-file) :boolean
  (keyfile (:pointer (:struct key-file)))
  (filename :string)
  (err :pointer))

(defun key-file-save-to-file (keyfile path)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[path]{a pathname or namestring for the file to write to}
  @return{@em{True} if successful, otherwise @em{false}.}
  @begin{short}
    Writes the contents of the key file to a file.
  @end{short}
  @see-type{g:key-file}
  @see-function{g:key-file-load-from-file}"
  (with-error (err)
    (%key-file-save-to-file keyfile (namestring path) err)))

(export 'key-file-save-to-file)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_start_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_get_start_group" key-file-start-group) :string
 #+liber-documentation
 "@version{2025-09-27}
  @argument[keyfile]{a @type{g:key-file} instance}
  @return{The string for the start group of the key file.}
  @begin{short}
    Returns the name of the start group of the key file.
  @end{short}
  @see-type{g:key-file}"
  (keyfile (:pointer (:struct key-file))))

(export 'key-file-start-group)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_groups
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_get_groups" %key-file-groups)
    (strv-t :free-from-foreign t)
  (keyfile (:pointer (:struct key-file)))
  (len (:pointer :size)))

(defun key-file-groups (keyfile)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @return{The list of strings with the groups.}
  @begin{short}
    Returns all groups in the key file loaded with @arg{keyfile}.
  @end{short}
  @see-type{g:key-file}"
  (%key-file-groups keyfile (cffi:null-pointer)))

(export 'key-file-groups)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_keys
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_get_keys" %key-file-keys)
    (strv-t :free-from-foreign t)
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (len (:pointer :size))
  (err :pointer))

(defun key-file-keys (keyfile group)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @return{The list of strings with the keys.}
  @begin{short}
    Returns all keys for the group name.
  @end{short}
  In the event that the group name cannot be found, @code{nil} is returned.
  @see-type{g:key-file}"
  (with-error (err)
    (%key-file-keys keyfile group (cffi:null-pointer) err)))

(export 'key-file-keys)

;;; ----------------------------------------------------------------------------
;;; g_key_file_has_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_has_group" key-file-has-group) :boolean
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @begin{return}
    @em{True} if @arg{group} is a part of @arg{keyfile}, @em{false} otherwise.
  @end{return}
  @begin{short}
    Looks whether the key file has the group @arg{group}.
  @end{short}
  @see-type{g:key-file}"
  (keyfile (:pointer (:struct key-file)))
  (group :string))

(export 'key-file-has-group)

;;; ----------------------------------------------------------------------------
;;; g_key_file_has_key
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_has_key" %key-file-has-key) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-has-key (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name}
  @begin{return}
    @em{True} if @arg{key} is a part of @arg{group}, @em{false} otherwise.
  @end{return}
  @begin{short}
    Looks whether the key file has the key @arg{key} in the group
    @arg{group}.
  @end{short}
  @see-type{g:key-file}"
  (with-error (err)
    (%key-file-has-key keyfile group key err)))

(export 'key-file-has-key)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_value
;;; g_key_file_set_value
;;; ----------------------------------------------------------------------------

(defun (setf key-file-value) (value keyfile group key)
  (cffi:foreign-funcall "g_key_file_set_value"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :string value
                        :void)
  value)

(cffi:defcfun ("g_key_file_get_value" %key-file-value) :string
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-value (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-value keyfile group key) => value}
  @syntax{(setf (g:key-file-value keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for a key}
  @argument[value]{a string for the value}
  @begin{short}
    The @fun{g:key-file-value} function returns the raw value associated with
    @arg{key} under @arg{group}.
  @end{short}
  Use the @fun{g:key-file-string} function to retrieve an unescaped UTF-8
  string. In the event the key or group name cannot be found, @code{nil} is
  returned.

  The @setf{g:key-file-value} function associates a new value with @arg{key}
  under @arg{group}. If @arg{key} cannot be found then it is created. If
  @arg{group} cannot be found then it is created. To set an UTF-8 string which
  may contain characters that need escaping (such as newlines or spaces), use
  the @fun{g:key-file-string} function.
  @see-type{g:key-file}
  @see-function{g:key-file-string}"
  (with-ignore-error (err)
    (%key-file-value keyfile group key err)))

(export 'key-file-value)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_string
;;; g_key_file_set_string
;;; ----------------------------------------------------------------------------

(defun (setf key-file-string) (value keyfile group key)
  (cffi:foreign-funcall "g_key_file_set_string"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :string (or value (cffi:null-pointer))
                        :void)
  value)

(cffi:defcfun ("g_key_file_get_string" %key-file-string) :string
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-string (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-string keyfile group key) => value}
  @syntax{(setf (g:key-file-string keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name}
  @argument[value]{a string for the value for @arg{key}}
  @begin{short}
    The @fun{g:key-file-string} function returns the string value associated
    with @arg{key} under @arg{group}.
  @end{short}
  In the event the key or the group name cannot be found, @code{nil} is
  returned. The @setf{g:key-file-string} function associates a new string value
  with @arg{key} under @arg{group}. If @arg{key} or @arg{group} cannot be found
  then they are created. Unlike the @fun{g:key-file-value} function, this
  function handles characters that need escaping, such as newlines.
  @see-type{g:key-file}
  @see-function{g:key-file-value}"
  (with-ignore-error (err)
    (%key-file-string keyfile group key err)))

(export 'key-file-string)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_locale_string
;;; g_key_file_get_locale_string
;;; ----------------------------------------------------------------------------

(defun (setf key-file-locale-string) (str keyfile group key locale)
  (cffi:foreign-funcall "g_key_file_set_locale_string"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :string locale
                        :string str
                        :void)
  str)

(cffi:defcfun ("g_key_file_get_locale_string" %key-file-locale-string) :string
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (locale :string)
  (err :pointer))

(defun key-file-locale-string (keyfile group key locale)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-locale-string keyfile group key locale) => value}
  @syntax{(setf (g:key-file-locale-string keyfile group key locale) value)}
  @argument[keyfile]{a @symbol{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key}
  @argument[locale]{a string for the identifier}
  @argument[value]{a string for the value for the specified key or @code{nil}
    if the key cannot be found}
  @begin{short}
    The @fun{g:key-file-locale-string} function returns the value associated
    with @arg{key} under @arg{group} translated in the given @arg{locale} if
    available.
  @end{short}
  If @arg{locale} is @code{nil} then the current locale is assumed. If @arg{key}
  cannot be found then @code{nil} is returned. If the value associated with
  @arg{key} cannot be interpreted or no suitable translation can be found then
  the untranslated value is returned.

  The @setf{g:key-file-locale-string} function associates a string value for
  @arg{key} and @arg{locale} under @arg{group}. If the translation for @arg{key}
  cannot be found then it is created.
  @see-symbol{g:key-file}"
  (with-ignore-error (err)
    (%key-file-locale-string keyfile group key locale err)))

(export 'key-file-locale-string)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_locale_for_key
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_get_locale_for_key" %key-file-locale-for-key) :string
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (local :string))

(defun key-file-locale-for-key (keyfile group key &optional locale)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[keyfile]{a @symbol{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key}
  @argument[locale]{a string for the locale identifier}
  @begin{return}
    The string for the locale from the file, or @code{nil} if the key was not
    found or the entry in the file was untranslated.
  @end{return}
  @begin{short}
    Returns the actual locale which the result of the
    @fun{g:key-file-locale-string} or @fun{g:key-file-locale-string-list}
    function came from.
  @end{short}

  If calling the @fun{g:key-file-locale-string} or
  @fun{g:key-file-locale-string-list} function with exactly the same
  @arg{keyfile}, @arg{group}, @arg{key} and @arg{locale}, the result of those
  functions will have originally been tagged with the locale that is the result
  of this function.
  @see-symbol{g:key-file}
  @see-function{g:key-file-locale-string}
  @see-function{g:key-file-locale-string-list}"
  (%key-file-locale-for-key keyfile group key (or locale (cffi:null-pointer))))

(export 'key-file-locale-for-key)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_boolean
;;; g_key_file_get_boolean
;;; ----------------------------------------------------------------------------

(defun (setf key-file-boolean) (value keyfile group key)
  (cffi:foreign-funcall "g_key_file_set_boolean"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("g_key_file_get_boolean" %key-file-boolean) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-boolean (keyfile group key)
 #+liber-documentation
 "@version{2025-09-27}
  @syntax{(g:key-file-boolean keyfile group key) => value}
  @syntax{(setf (g:key-file-boolean keyfile group key) value)}
  @argument[keyfile]{a @symbol{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key}
  @argument[value]{a boolean for the value associated with the key}
  @begin{short}
    Gets or sets the value associated with @arg{key} under @arg{group} as a
    boolean.
  @end{short}
  If @arg{key} cannot be found when it is set, the key is created. If @arg{key}
  cannot be found when it is retrieved, @code{nil} is returned. Likewise, if the
  value associated with @arg{key} cannot be interpreted as a boolean then
  @code{nil} is returned.
  @see-symbol{g:key-file}"
  (with-ignore-error (err)
    (%key-file-boolean keyfile group key err)))

(export 'key-file-boolean)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_integer
;;; g_key_file_get_integer
;;; ----------------------------------------------------------------------------

(defun (setf key-file-integer) (value keyfile group key)
  (cffi:foreign-funcall "g_key_file_set_integer"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :int value
                        :void)
  value)

(cffi:defcfun ("g_key_file_get_integer" %key-file-integer) :int
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-integer (keyfile group key)
 #+liber-documentation
 "@version{2025-09-27}
  @syntax{(g:key-file-integer keyfile group key) => value}
  @syntax{(setf (g:key-file-integer keyfile group key) value)}
  @argument[keyfile]{a @symbol{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key}
  @argument[value]{an integer for the value associated with the key as an
    integer}
  @begin{short}
    Gets or sets the value associated with @arg{key} under @arg{group} as an
    integer.
  @end{short}
  If @arg{key} cannot be found when it is set, the key is created. If @arg{key}
  cannot be found when it is retrieved, 0 is returned. Likewise, if the value
  associated with @arg{key} cannot be interpreted as an integer then 0 is
  returned.
  @see-symbol{g:key-file}"
  (with-ignore-error (err)
    (%key-file-integer keyfile group key err)))

(export 'key-file-integer)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_int64
;;; g_key_file_get_int64
;;; ----------------------------------------------------------------------------

(defun (setf key-file-int64) (value keyfile group key)
  (cffi:foreign-funcall "g_key_file_set_int64"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :int64 value)
  value)

(cffi:defcfun ("g_key_file_get_int64" %key-file-int64) :int64
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-int64 (keyfile group key)
 #+liber-documentation
 "@version{2025-09-27}
  @syntax{(g:key-file-int64 keyfile group key) => value}
  @syntax{(setf (g:key-file-int64 keyfile group key) value)}
  @argument[keyfile]{a @symbol{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key}
  @argument[value]{an integer for the value associated with the key as an
    integer}
  @begin{short}
    Gets or sets the value associated with @arg{key} under @arg{group} as an
    integer.
  @end{short}
  If @arg{key} cannot be found when it is set, the key is created. If @arg{key}
  cannot be found when it is retrieved, 0 is returned. Likewise, if the value
  associated with @arg{key} cannot be interpreted as an integer then 0 is
  returned.

  This is similar to the @fun{g:key-file-integer} function but can return 64-bit
  results without truncation.
  @see-symbol{g:key-file}
  @see-function{g:key-file-integer}"
  (with-ignore-error (err)
    (%key-file-int64 keyfile group key err)))

(export 'key-file-int64)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_uint64
;;; g_key_file_get_uint64
;;; ----------------------------------------------------------------------------

(defun (setf key-file-uint64) (value keyfile group key)
  (cffi:foreign-funcall "g_key_file_set_uint64"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :uint64 value)
  value)

(cffi:defcfun ("g_key_file_get_uint64" %key-file-uint64) :uint64
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-uint64 (keyfile group key)
 #+liber-documentation
 "@version{2025-09-27}
  @syntax{(g:key-file-uint64 keyfile group key) => value}
  @syntax{(setf (g:key-file-uint64 keyfile group key) value)}
  @argument[keyfile]{a @symbol{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key}
  @argument[value]{an integer for the value associated with the key as an
    integer}
  @begin{short}
    Gets or sets the value associated with @arg{key} under @arg{group} as an
    integer.
  @end{short}
  If @arg{key} cannot be found when it is set, the key is created. If @arg{key}
  cannot be found when it is retrieved, 0 is returned. Likewise, if the value
  associated with @arg{key} cannot be interpreted as an integer then 0 is
  returned.

  This is similar to the @fun{g:key-file-integer} function but can return 64-bit
  results without truncation.
  @see-symbol{g:key-file}
  @see-function{g:key-file-integer}"
  (with-ignore-error (err)
    (%key-file-uint64 keyfile group key err)))

(export 'key-file-uint64)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_double
;;; g_key_file_get_double
;;; ----------------------------------------------------------------------------

(defun (setf key-file-double) (value keyfile group key)
  (let ((value (coerce value 'double-float)))
    (cffi:foreign-funcall "g_key_file_set_double"
                          (:pointer (:struct key-file)) keyfile
                          :string group
                          :string key
                          :double value)
    value))

(cffi:defcfun ("g_key_file_get_double" %key-file-double) :double
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-double (keyfile group key)
 #+liber-documentation
 "@version{2025-09-27}
  @syntax{(g:key-file-double keyfile group key) => value}
  @syntax{(setf (g:key-file-double keyfile grop key) value)}
  @argument[keyfile]{a @symbol{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key}
  @argument[value]{a number coerced to a double float for the value associated
    with the key as a double float}
  @begin{short}
    Gets or sets the value associated with @arg{key} under @arg{group} as a
    double float.
  @end{short}
  If @arg{group} is @code{nil}, the start group is used.
  If @arg{key} cannot be found when it is set, the key is created. If @arg{key}
  cannot be found when it is retrieved, 0.0d0 is returned. Likewise, if the
  value associated with @arg{key} cannot be interpreted as a double float then
  0.0d0 is returned.
  @see-symbol{g:key-file}"
  (with-ignore-error (err)
    (%key-file-double keyfile group key err)))

(export 'key-file-double)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_string_list
;;; g_key_file_set_string_list
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_set_string_list" %key-file-set-string-list) :void
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (value strv-t)
  (len :size))

(defun (setf key-file-string-list) (value keyfile group key)
  (%key-file-set-string-list keyfile group key value (length value))
  value)

(cffi:defcfun ("g_key_file_get_string_list" %key-file-get-string-list) strv-t
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (len (:pointer :size))
  (err :pointer))

(defun key-file-string-list (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-string-list keyfile group key) => value}
  @syntax{(setf (g:key-file-string-list keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name}
  @argument[value]{a list of strings}
  @begin{short}
    The @fun{g:key-file-string-list} function returns the values associated
    with @arg{key} under @arg{group}.
  @end{short}
  In the event the key or the group name cannot be found, @code{nil} is
  returned. The @setf{g:key-file-string-list} function associates a list of
  string values for @arg{key} under @arg{group}. If @arg{key} or @arg{group}
  cannot be found then they are created.
  @see-type{g:key-file}"
  (with-ignore-error (err)
    (cffi:with-foreign-object (len :size)
      (%key-file-get-string-list keyfile group key len err))))

(export 'key-file-string-list)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_locale_string_list
;;; g_key_file_get_locale_string_list
;;; ----------------------------------------------------------------------------

(defun (setf key-file-locale-string-list) (value keyfile group key locale)
  (cffi:foreign-funcall "g_key_file_set_locale_string_list"
                        (:pointer (:struct key-file)) keyfile
                        :string group
                        :string key
                        :string locale
                        strv-t value
                        :size (length value))
  value)

(cffi:defcfun ("g_key_file_get_locale_string_list" %key-file-locale-string-list)
    strv-t
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (locale :string)
  (len (:pointer :size))
  (err :pointer))

(defun key-file-locale-string-list (keyfile group key &optional locale)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-locale-string-list keyfile group key locale) => value}
  @syntax{(setf (g:key-file-locale-string-list keyfile group key locale) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name}
  @argument[locale]{a string for the locale identifier}
  @argument[value]{a list of strings}
  @begin{short}
    The @fun{g:key-file-locale-string-list} function returns the values
    associated with @arg{key} under @arg{group} translated in the given
    @arg{locale} if available.
  @end{short}
  If the @arg{locale} argument is @code{nil}, the default, then the current
  locale is assumed.

  The @setf{g:key-file-locale-string-list} function associates a list of string
  values for @arg{key} and @arg{locale} under @arg{group}. If the translation
  for @arg{key} cannot be found then it is created.

  If the @arg{locale} argument is to be not @code{nil}, or if the current locale
  will change over the lifetime of the @symbol{g:key-file} instance, it must be
  loaded with the @code{:keep-translations} value for the
  @symbol{g:key-file-flags} flags in order to load strings for all locales.

  If the @arg{key} argument cannot be found then @code{nil} is returned. If the
  values associated with @arg{key} cannot be interpreted or no suitable
  translations can be found then the untranslated values are returned.
  @see-type{g:key-file}
  @see-symbol{g:key-file-flags}"
  (with-ignore-error (err)
    (cffi:with-foreign-object (len :size)
      (let ((locale (or locale (cffi:null-pointer))))
        (%key-file-locale-string-list keyfile group key locale len err)))))

(export 'key-file-locale-string-list)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_boolean_list
;;; g_key_file_get_boolean_list
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_set_boolean_list" %key-file-set-boolean-list) :void
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (lst :pointer)
  (len :size))

(defun (setf key-file-boolean-list) (lst keyfile group key)
  (let ((len (length lst)))
    (cffi:with-foreign-object (lstptr :boolean len)
      (iter (for i from 0 below len)
            (for value in lst)
            (setf (cffi:mem-aref lstptr :boolean i) value))
      (%key-file-set-boolean-list keyfile group key lstptr len))
    lst))

(cffi:defcfun ("g_key_file_get_boolean_list" %key-file-get-boolean-list)
    :pointer
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (len (:pointer :size))
  (err :pointer))

(defun key-file-boolean-list (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-boolean-list keyfile group key) => value}
  @syntax{(setf (g:key-file-boolean-list keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name}
  @argument[value]{a list of boolean values}
  @begin{return}
    The values associated with the key as a list of boolean values, or
    @code{nil} if the key was not found or could not be parsed.
  @end{return}
  @begin{short}
    The @fun{g:key-file-boolean-list} function returns the values associated
    with @arg{key} under @arg{group} as boolean values.
  @end{short}
  The @setf{g:key-file-boolean-list} function associates a list of boolean
  values with @arg{key} under @arg{group}. If @arg{key} cannot be found then it
  is created. If @arg{group} is @code{nil}, the start group is used.

  If @arg{key} cannot be found then @code{nil} is returned. Likewise, if the
  values associated with @arg{key} cannot be interpreted as booleans then
  @code{nil} is returned.
  @see-symbol{g:key-file}"
  (with-ignore-error (err)
    (cffi:with-foreign-object (len :size)
      (let* ((lstptr (%key-file-get-boolean-list keyfile group key len err)))
        (unwind-protect
          (iter (for i from 0 below (cffi:mem-ref len :size))
                (collect (cffi:mem-aref lstptr :boolean i)))
          (free lstptr))))))

(export 'key-file-boolean-list)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_integer_list
;;; g_key_file_get_integer_list
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_set_integer_list" %key-file-set-integer-list) :void
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (lst :pointer)
  (len :size))

(defun (setf key-file-integer-list) (lst keyfile group key)
  (let ((len (length lst)))
    (cffi:with-foreign-object (lstptr :int len)
      (iter (for i from 0 below len)
            (for value in lst)
            (setf (cffi:mem-aref lstptr :int i) value))
      (%key-file-set-integer-list keyfile group key lstptr len))
    lst))

(cffi:defcfun ("g_key_file_get_integer_list" %key-file-get-integer-list)
    :pointer
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (len (:pointer :size))
  (err :pointer))

(defun key-file-integer-list (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-integer-list keyfile group key) => value}
  @syntax{(setf (g:key-file-integer-list keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name}
  @argument[value]{a list of integers}
  @begin{return}
    The values associated with the key as a list of integers, or @code{nil} if
    the key was not found or could not be parsed.
  @end{return}
  @begin{short}
    The @fun{g:key-file-integer-list} function returns the values associated
    with @arg{key} under @arg{group} as integers.
  @end{short}
  The @setf{g:key-file-integer-list} function associates a list of integer
  values with @arg{key} under @arg{group}. If @arg{key} cannot be found then it
  is created. If @arg{group} is @code{nil}, the start group is used.

  If @arg{key} cannot be found then @code{nil} is returned. Likewise, if the
  values associated with @arg{key} cannot be interpreted as integers then
  @code{nil} is returned.
  @see-symbol{g:key-file}"
  (with-ignore-error (err)
    (cffi:with-foreign-object (len :size)
      (let* ((lstptr (%key-file-get-integer-list keyfile group key len err)))
        (iter (for i from 0 below (cffi:mem-ref len :size))
              (collect (cffi:mem-aref lstptr :int i)))))))

(export 'key-file-integer-list)

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_double_list
;;; g_key_file_get_double_list
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_set_double_list" %key-file-set-double-list) :void
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (lst :pointer)
  (len :size))

(defun (setf key-file-double-list) (lst keyfile group key)
  (let* ((lst (mapcar (lambda (x) (coerce x 'double-float)) lst))
         (len (length lst)))
    (cffi:with-foreign-object (lstptr :double len)
      (iter (for i from 0 below len)
            (for value in lst)
            (setf (cffi:mem-aref lstptr :double i) value))
      (%key-file-set-double-list keyfile group key lstptr len))
    lst))

(cffi:defcfun ("g_key_file_get_double_list" %key-file-get-double-list)
    :pointer
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (len (:pointer :size))
  (err :pointer))

(defun key-file-double-list (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-double-list keyfile group key) => value}
  @syntax{(setf (g:key-file-double-list keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name}
  @argument[value]{a list of numbers coerced to double floats}
  @begin{return}
    The values associated with the key as a list of double floats, or @code{nil}
    if the key was not found or could not be parsed.
  @end{return}
  @begin{short}
    The @fun{g:key-file-double-list} function returns the values associated
    with @arg{key} under @arg{group} as double floats.
  @end{short}
  The @setf{g:key-file-double-list} function associates a list of numbers
  coerced to double floats with @arg{key} under @arg{group}. If @arg{key}
  cannot be found then it is created. If @arg{group} is @code{nil}, the start
  group is used.

  If @arg{key} cannot be found then @code{nil} is returned. Likewise, if the
  values associated with @arg{key} cannot be interpreted as double floats
  then @code{nil} is returned.
  @see-symbol{g:key-file}"
  (with-ignore-error (err)
    (cffi:with-foreign-object (len :size)
      (let* ((lstptr (%key-file-get-double-list keyfile group key len err)))
        (iter (for i from 0 below (cffi:mem-ref len :size))
              (collect (cffi:mem-aref lstptr :double i)))))))

(export 'key-file-double-list)

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_comment
;;; g_key_file_set_comment
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_set_comment" %key-file-set-comment) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (comment :string)
  (err :pointer))

(defun (setf key-file-comment) (value keyfile group key)
  (with-ignore-error (err)
    (%key-file-set-comment keyfile
                           (or group (cffi:null-pointer))
                           (or key (cffi:null-pointer))
                           value
                           err)
    value))

(cffi:defcfun ("g_key_file_get_comment" %key-file-get-comment) :string
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-comment (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @syntax{(g:key-file-comment keyfile group key) => value}
  @syntax{(setf (g:key-file-comment keyfile group key) value)}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name}
  @argument[value]{a string for the comment}
  @begin{short}
    The @fun{g:key-file-comment} retrieves a comment above @arg{key} from
    @arg{group}.
  @end{short}
  If @arg{key} is @code{nil} then the comment will be read from above
  @arg{group}. If both @arg{key} and @arg{group} are @code{nil}, then the
  comment will be read from above the first group in the file.

  The @setf{g:key-file-comment} function places a comment above @arg{key} from
  @arg{group}. If @arg{key} is @code{nil} then the comment will be written
  above @arg{group}. If both @arg{key} and @arg{group} are @code{nil}, then the
  comment will be written above the first group in the file.
  @see-type{g:key-file}"
  (with-ignore-error (err)
    (%key-file-get-comment keyfile
                           (or group (cffi:null-pointer))
                           (or key (cffi:null-pointer))
                           err)))

(export 'key-file-comment)

;;; ----------------------------------------------------------------------------
;;; g_key_file_remove_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_remove_group" %key-file-remove-group) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (err :pointer))

(defun key-file-remove-group (keyfile group)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name to remove}
  @return{@em{True} if the group was removed, @em{false} otherwise.}
  @begin{short}
    Removes the specified group from the key file.
  @end{short}
  @see-type{g:key-file}"
  (with-ignore-error (err)
    (%key-file-remove-group keyfile group err)))

(export 'key-file-remove-group)

;;; ----------------------------------------------------------------------------
;;; g_key_file_remove_key
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_remove_key" %key-file-remove-key) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-remove-key (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name to remove}
  @return{@em{True} if the key was removed, @em{false} otherwise.}
  @begin{short}
    Removes the specified key from the key file.
  @end{short}
  @see-type{g:key-file}"
  (with-ignore-error (err)
    (%key-file-remove-key keyfile group key err)))

(export 'key-file-remove-key)

;;; ----------------------------------------------------------------------------
;;; g_key_file_remove_comment
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_key_file_remove_comment" %key-file-remove-comment) :boolean
  (keyfile (:pointer (:struct key-file)))
  (group :string)
  (key :string)
  (err :pointer))

(defun key-file-remove-comment (keyfile group key)
 #+liber-documentation
 "@version{2025-05-23}
  @argument[keyfile]{a @type{g:key-file} instance}
  @argument[group]{a string for the group name}
  @argument[key]{a string for the key name to remove}
  @return{@em{True} if the comment was removed, @em{false} otherwise.}
  @begin{short}
    Removes a comment above @arg{key} from @arg{group}.
  @end{short}
  If @arg{key} is @code{nil} then the comment will be removed above @arg{group}.
  If both @arg{key} and @arg{group} are @code{nil}, then the comment will be
  removed above the first group in the file.
  @see-type{g:key-file}"
  (with-ignore-error (err)
    (%key-file-remove-comment keyfile
                              (or group (cffi:null-pointer))
                              (or key (cffi:null-pointer))
                              err)))

(export 'key-file-remove-comment)

;;; --- End of file glib.key-value.lisp ----------------------------------------
