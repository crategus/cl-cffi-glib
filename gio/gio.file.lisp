;;; ----------------------------------------------------------------------------
;;; gio.file.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2024 Dieter Kaiser
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
;;; GFile
;;;
;;;     A small subset of types and functions for file and directory handling
;;;
;;; Types and Values
;;;
;;;     GFile
;;;     GFileQueryInfoFlags
;;;
;;; Functions
;;;
;;;     g_file_new_for_path
;;;     g_file_new_for_uri
;;;     g_file_new_for_commandline_arg
;;;     g_file_new_for_commandline_arg_and_cwd
;;;
;;;     g_file_query_info
;;;
;;;     g_file_parse_name
;;;     g_file_get_basename
;;;     g_file_get_path
;;;     g_file_get_uri
;;;     g_file_get_parse_name
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GFile
;;;
;;; Prerequisites
;;;
;;;     GFile requires GObject
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GFileQueryInfoFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GFileQueryInfoFlags" file-query-info-flags
  (:export t
   :type-initializer "g_file_query_info_flags_get_type")
  (:none 0)
  (:nofollow-symlinks #.(ash 1 0)))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-query-info-flags)
      "GFlags"
      (liber:symbol-documentation 'file-query-info-flags)
 "@version{2024-12-14}
  @begin{declaration}
(gobject:define-gflags \"GFileQueryInfoFlags\" file-query-info-flags
  (:export t
   :type-initializer \"g_file_query_info_flags_get_type\")
  (:none 0)
  (:nofollow-symlinks #.(ash 1 0)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No flags set.}
      @entry[:nofollow-symlinks]{Do not follow symlinks.}
    @end{table}
  @end{values}
  @begin{short}
    Flags used when querying a @class{g:file-info} instance.
  @end{short}
  @see-class{g:file}
  @see-class{g:file-info}")

;;; ----------------------------------------------------------------------------
;;; GFile
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GFile" file
  (:export t
   :type-initializer "g_file_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'file)
      "Interface"
      (documentation 'file 'type)
 "@version{2024-10-12}
  @begin{short}
    The @class{g:file} interface is a high level abstraction for manipulating
    files on a virtual file system.
  @end{short}
  The @class{g:file} objects are lightweight, immutable objects that do no I/O
  upon creation. It is necessary to understand that @class{g:file} objects do
  not represent files, merely an identifier for a file. All file content I/O is
  implemented as streaming operations, see @code{GInputStream} and
  @code{GOutputStream}.

  To construct a @class{g:file} object, you can use the
  @fun{g:file-new-for-path} function if you have a path, the
  @fun{g:file-new-for-uri} function if you have a URI, the
  @fun{g:file-new-for-commandline-arg} function for a command line argument,
  the @fun{g:file-parse-name} function from a UTF-8 string gotten from
  the @fun{g:file-get-parse-name} function.

  One way to think of a @class{g:file} object is as an abstraction of a
  pathname. For normal files the system pathname is what is stored internally,
  but as @class{g:file} objects are extensible it could also be something else
  that corresponds to a pathname in a userspace implementation of a filesystem.
  @see-constructor{g:file-new-for-path}
  @see-constructor{g:file-new-for-uri}
  @see-constructor{g:file-new-for-commandline-arg}
  @see-constructor{g:file-new-for-comandline-arg-and-cwd}
  @see-type{g:file-as-namestring}")

;;; ----------------------------------------------------------------------------
;;; gio:file-as-namestring
;;; ----------------------------------------------------------------------------

;; TODO: Improve the documentation

(cffi:define-foreign-type file-as-namestring-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser file-as-namestring))

(defmethod cffi:translate-to-foreign (value (type file-as-namestring-type))
  (if value
      (gobject:object-pointer
          (cffi:foreign-funcall "g_file_parse_name"
                                :string (namestring value)
                                (gobject:object :return)))
      (cffi:null-pointer)))

(defmethod cffi:translate-from-foreign (value (type file-as-namestring-type))
  (when value
    (setf value
          (if (cffi:pointerp value)
              value
              (gobject:object-pointer value)))
    (unless (cffi:null-pointer-p value)
      (cffi:foreign-funcall "g_file_get_parse_name"
                            :pointer value
                            (:string :free-from-foreign t)))))

#+liber-documentation
(setf (liber:alias-for-class 'file-as-namestring)
      "Type"
      (documentation 'file-as-namestring 'type)
 "@version{2025-09-22}
  @begin{short}
    The @class{g:file-as-namestring} type specifier represents and performs
    automatic conversion between a Lisp namestring and a @class{g:file} object
    for files on a virtual file system.
  @end{short}
  A Lisp namestring or pathname is converted to the corresponding @class{g:file}
  object using the @fun{g:file-parse-name} function. The conversion from a
  @class{g:file} object back to a Lisp namestring is done with the
  @fun{g:file-get-parse-name} function.

  The @code{nil} value is converted to a foreign @code{(cffi:null-pointer)}
  value and vice versa.
  @begin[Examples]{dictionary}
    Conversion of a Lisp namestring to @class{g:file} object and back:
    @begin{pre}
(setf path \"/home/lisp/github/glib/gio/gio.file.lisp\")
=> \"/home/lisp/github/glib/gio/gio.file.lisp\"
(cffi:convert-to-foreign path 'g:file-as-namestring)
=> #.(SB-SYS:INT-SAP #X60BCAD6BD5B0)
(cffi:convert-from-foreign * 'g:file-as-namestring)
=> \"/home/lisp/github/glib/gio/gio.file.lisp\"
(cffi:convert-to-foreign nil 'g:file-as-namestring)
=> #.(SB-SYS:INT-SAP #X00000000)
(cffi:convert-from-foreign * 'g:file-as-namestring)
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{g:file}")

(export 'file-as-namestring)

;;; ----------------------------------------------------------------------------
;;; g_file_new_for_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_new_for_path" %file-new-for-path)
    (gobject:object file :return)
  (path :string))

(defun file-new-for-path (path)
 #+liber-documentation
 "@version{2024-10-12}
  @argument[path]{a pathname or namestring containing a relative or absolute
    path, the path must be encoded in the GLib filename encoding}
  @return{The new @class{g:file} object for the given @arg{path}.}
  @begin{short}
    Constructs a @class{g:file} object for a given @arg{path}.
  @end{short}
  This operation never fails, but the returned object might not support any I/O
  operation if the @arg{path} argument is malformed.
  @see-class{g:file}"
  (%file-new-for-path (namestring path)))

(export 'file-new-for-path)

;;; ----------------------------------------------------------------------------
;;; g_file_new_for_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_new_for_uri" file-new-for-uri)
    (gobject:object file :return)
 #+liber-documentation
 "@version{2024-10-12}
  @argument[uri]{a UTF-8 string containing a URI}
  @return{The new @class{g:file} object for the given @arg{uri}.}
  @begin{short}
    Constructs a @class{g:file} object for a given URI.
  @end{short}
  This operation never fails, but the returned object might not support any I/O
  operation if the @arg{uri} argument is malformed or if the URI type is not
  supported.
  @see-class{g:file}"
  (uri :string))

(export 'file-new-for-uri)

;;; ----------------------------------------------------------------------------
;;; g_file_new_for_commandline_arg
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_new_for_commandline_arg" file-new-for-commandline-arg)
    (gobject:object file :return)
 #+liber-documentation
 "@version{2024-10-12}
  @argument[arg]{a command line string}
  @return{The new @class{g:file} object.}
  @begin{short}
    Creates a @class{g:file} object with the given @arg{arg} from the command
    line.
  @end{short}
  The value of the @arg{arg} argument can be either a URI, an absolute path or
  a relative path resolved relative to the current working directory. This
  operation never fails, but the returned object might not support any I/O
  operation if the @arg{arg} argument points to a malformed path.

  Note that on Windows, this function expects its argument to be in UTF-8, not
  the system code page. This means that you should not use this function
  with strings from the @code{argv} parameter as it is passed to the main
  function. The @code{g_win32_get_command_line()} function will return a UTF-8
  version of the command line. The @class{g:application} class also uses UTF-8
  but the @fun{g:application-command-line-create-file-for-arg} function may be
  more useful for you there. It is also always possible to use this function
  with @type{g:option-context} instances of @code{:filename} type.
  @see-class{g:file}
  @see-class{g:application}
  @see-type{g:option-context}
  @see-function{g:application-command-line-create-file-for-arg}"
  (arg :string))

(export 'file-new-for-commandline-arg)

;;; ----------------------------------------------------------------------------
;;; g_file_new_for_commandline_arg_and_cwd
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_new_for_commandline_arg_and_cwd"
               file-new-for-commandline-arg-and-cwd)
    (gobject:object file :return)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[arg]{a command line string}
  @argument[cwd]{a string for the current working directory of the command
    line}
  @return{The new @class{g:file} object.}
  @begin{short}
    Creates a @class{g:file} object with the given @arg{arg} from the command
    line.
  @end{short}
  This function is similar to the @fun{g:file-new-for-commandline-arg}
  function except that it allows for passing the current working directory as
  an argument instead of using the current working directory of the process.
  This is useful if the command line argument was given in a context other than
  the invocation of the current process.

  See also the @fun{g:application-command-line-create-file-for-arg} function.
  @see-class{g:file}
  @see-function{g:file-new-for-commandline-arg}
  @see-function{g:application-command-line-create-file-for-arg}"
  (arg :string)
  (cwd :string))

(export 'file-new-for-commandline-arg-and-cwd)

;;; ----------------------------------------------------------------------------
;;; g_file_query_info
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_query_info" %file-query-info)
    (gobject:object file-info :return)
  (file gobject:object)
  (attributes :string)
  (flags file-query-info-flags)
  (cancellable (gobject:object cancellable))
  (err :pointer))

(defun file-query-info (file attributes flags &optional cancellable)
 #+liber-documentation
 "@version{2024-12-28}
  @argument[file]{a @class{g:file} instance}
  @argument[attributes]{an attribute query string}
  @argument[flags]{a @symbol{g:file-query-info-flags} value}
  @argument[cancellable]{an optional @class{g:cancellable} instance}
  @return{The @symbol{g:file-info} instance for the given @arg{file} or
    @code{nil} on error.}
  @begin{short}
    Gets the requested information about the specified file.
  @end{short}
  The result is a @class{g:file-info} instance that contains key-value
  attributes, such as the type or size of the file.

  The @arg{attributes} value is a string that specifies the file attributes
  that should be gathered. It is not an error if it is not possible to read a
  particular requested attribute from a file, it just will not be set.
  The @arg{attributes} argument should be a comma-separated list of attributes
  or attribute wildcards. The wildcard @code{\"\"} means all attributes, and a
  wildcard like @code{\"standard::\"} means all attributes in the standard
  namespace. An example attribute query be @code{\"standard::*,owner::user\"}.

  If the @arg{cancellable} argument is not @code{nil}, then the operation can
  be cancelled by triggering the @arg{cancellable} instance from another thread.
  If the operation was cancelled, the error @code{G_IO_ERROR_CANCELLED} will be
  returned.

  For symlinks, normally the information about the target of the symlink is
  returned, rather than information about the symlink itself. However if you
  pass @code{:nofollow-symlinks} in @arg{flags} the information about the
  symlink itself will be returned. Also, for symlinks that point to non-existing
  files the information about the symlink itself will be returned.

  If the file does not exist, the @code{G_IO_ERROR_NOT_FOUND} error will be
  returned. Other errors are possible too, and depend on what kind of filesystem
  the file is on.
  @see-class{g:file}
  @see-class{g:file-info}
  @see-symbol{g:file-query-info-flags}"
  (glib:with-error (err)
    (let ((cancellable (or cancellable (cffi:null-pointer))))
      (%file-query-info file attributes flags cancellable err))))

(export 'file-query-info)

;;; ----------------------------------------------------------------------------
;;; g_file_parse_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_parse_name" file-parse-name)
    (gobject:object file :return)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[parsename]{a string for a file name or path to be parsed}
  @return{The new @class{g:file} object.}
  @begin{short}
    Constructs a @class{g:file} object with the given @arg{parsename}, that is,
    something given by the @fun{g:file-get-parse-name} function.
  @end{short}
  This operation never fails, but the returned object might not support any I/O
  operation if @arg{parsename} cannot be parsed.
  @see-class{g:file}
  @see-function{g:file-get-parse-name}"
  (parsename :string))

(export 'file-parse-name)

;;; ----------------------------------------------------------------------------
;;; g_file_get_basename
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_get_basename" file-basename) :string
 #+liber-documentation
 "@version{2024-10-12}
  @argument[file]{a @class{g:file} object}
  @return{The string containing the base name of the @class{g:file} object, or
    @code{nil} if the given @class{g:file} object is invalid.}
  @begin{short}
    Gets the base name, the last component of the path, for a given
    @class{g:file} object.
  @end{short}
  If called for the toplevel of a system, such as the filesystem root or a URI
  like @file{sftp://host/}, it will return a single directory separator, and on
  Windows, possibly a drive letter.

  The base name is a byte string, not UTF-8. It has no defined encoding or rules
  other than it may not contain zero bytes. If you want to use filenames in a
  user interface you should use the display name, see the
  @fun{g:file-get-parse-name} function.
  @see-class{g:file}
  @see-function{g:file-get-parse-name}"
  (file gobject:object))

(export 'file-basename)

;;; ----------------------------------------------------------------------------
;;; g_file_get_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_get_path" file-path) (:string :free-from-foreign t)
 #+liber-documentation
 "@version{2024-12-30}
  @argument[file]{a @class{g:file} object}
  @begin{return}
    The string containing the path for the file, or @code{nil} if no such path
    exists.
  @end{return}
  @begin{short}
    Gets the local pathname for @arg{file}, if one exists.
  @end{short}
  If not @code{nil}, this is guaranteed to be an absolute, canonical path. It
  might contain symlinks. This call does no blocking I/O.
  @see-class{g:file}"
  (file gobject:object))

(export 'file-path)

;;; ----------------------------------------------------------------------------
;;; g_file_get_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_get_uri" file-uri) (:string :free-from-foreign t)
 #+liber-documentation
 "@version{2024-12-30}
  @argument[file]{a @class{g:file} object}
  @begin{return}
    The string containing the URI for the file, or @code{nil} if no such path
    exists.
  @end{return}
  @begin{short}
    Gets the local pathname for @arg{file}, if one exists.
  @end{short}
  If  not @code{nil}, this is guaranteed to be an absolute, canonical path. It
  might contain symlinks. This call does no blocking I/O.
  @see-class{g:file}"
  (file gobject:object))

(export 'file-uri)

;;; ----------------------------------------------------------------------------
;;; g_file_get_parse_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_file_get_parse_name" file-get-parse-name) :string
 #+liber-documentation
 "@version{2024-10-12}
  @argument[file]{a @class{g:file} object}
  @return{The string containing the parse name for the file.}
  @begin{short}
    Gets the parse name for the file.
  @end{short}
  A parse name is a UTF-8 string that describes the file such that one can get
  the @class{g:file} object back using the @fun{g:file-parse-name} function.

  This is generally used to show the @class{g:file} object as a nice
  full-pathname kind of string in a user interface, like in a location entry.

  For local files with names that can safely be converted to UTF-8 the pathname
  is used, otherwise the IRI is used, a form of URI that allows UTF-8 characters
  unescaped.
  @see-class{g:file}
  @see-function{g:file-parse-name}"
  (file gobject:object))

(export 'file-get-parse-name)

;;; --- End of file gio.file.lisp ----------------------------------------------
