;;; ----------------------------------------------------------------------------
;;; gio.resource.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GResource
;;;
;;;     Resource framework
;;;
;;; Types and Values
;;;
;;;     GResource
;;;     GResourceFlags
;;;     GResourceLookupFlags
;;;     GStaticResource
;;;
;;;     G_RESOURCE_ERROR
;;;     GResourceError
;;;
;;; Functions
;;;
;;;     g_resource_load
;;;     g_resource_new_from_data
;;;     g_resource_ref
;;;     g_resource_unref
;;;     g_resource_lookup_data
;;;     g_resource_open_stream
;;;     g_resource_enumerate_children
;;;     g_resource_get_info
;;;
;;;     g_static_resource_init
;;;     g_static_resource_fini
;;;     g_static_resource_get_resource
;;;
;;;     g_resources_register
;;;     g_resources_unregister
;;;     g_resources_lookup_data
;;;     g_resources_open_stream
;;;     g_resources_enumerate_children
;;;     g_resources_get_info
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GResource
;;;
;;;     GEnum
;;;     ╰── GResourceError
;;;
;;;     GFlags
;;;     ├── GResourceFlags
;;;     ╰── GResourceLookupFlags
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GResourceFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GResourceFlags" resource-flags
  (:export t
   :type-initializer "g_resource_flags_get_type")
  (:none 0)
  (:compressed #.(ash 1 0)))

#+liber-documentation
(setf (liber:alias-for-symbol 'resource-flags)
      "GFlags"
      (liber:symbol-documentation 'resource-flags)
 "@version{2024-5-12}
  @begin{declaration}
(gobject:define-g-flags \"GResourceFlags\" resource-flags
  (:export t
   :type-initializer \"g_resource_flags_get_type\")
  (:none 0)
  (:compressed #.(ash 1 0)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No flags set.}
      @entry[:compressed]{The file is compressed.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{g:resource-flags} flags give information about a particular
    file inside a resource bundle.
  @end{short}
  @see-class{g:resource}")

;;; ----------------------------------------------------------------------------
;;; GResourceLookupFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GResourceLookupFlags" resource-lookup-flags
  (:export t
   :type-initializer "g_resource_lookup_flags_get_type")
  (:none 0))

#+liber-documentation
(setf (liber:alias-for-symbol 'resource-lookup-flags)
      "GFlags"
      (liber:symbol-documentation 'resource-lookup-flags)
 "@version{2024-5-12}
  @begin{declaration}
(gobject:define-g-flags \"GResourceLookupFlags\" resource-lookup-flags
  (:export t
   :type-initializer \"g_resource_lookup_flags_get_type\")
  (:none 0))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No flags set.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{g:resource-lookup-flags} flags determine how resource path
    lookups are handled.
  @end{short}
  @see-class{g:resource}")

;;; ----------------------------------------------------------------------------
;;; struct GStaticResource
;;;
;;; GStaticResource is an opaque data structure and can only be accessed using
;;; the following functions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_RESOURCE_ERROR
;;;
;;; #define G_RESOURCE_ERROR (g_resource_error_quark ())
;;;
;;; Error domain for GResource. Errors in this domain will be from the
;;; GResourceError enumeration. See GError for more information on error
;;; domains.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GResourceError
;;;
;;; An error code used with G_RESOURCE_ERROR in a GError returned from a
;;; GResource routine.
;;;
;;; G_RESOURCE_ERROR_NOT_FOUND
;;;     no file was found at the requested path
;;;
;;; G_RESOURCE_ERROR_INTERNAL
;;;     unknown error
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GResource
;;; ----------------------------------------------------------------------------

;; TODO: Rework the documentation for use in the Lisp library

(glib:define-g-boxed-opaque resource "GResource"
  :export t
  :type-initializer "g_resource_get_type"
  :alloc (error "GResource cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'resource)
      "GBoxed"
      (documentation 'resource 'type)
 "@version{2024-5-12}
  @begin{declaration}
(glib:define-g-boxed-opaque resource \"GResource\"
  :export t
  :type-initializer \"g_resource_get_type\"
  :alloc (error \"GResource cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    Applications and libraries often contain binary or textual data that is
    really part of the application, rather than user data.
  @end{short}
  For instance @class{gtk:builder} @code{.ui} files, splashscreen images,
  @class{g:menu} markup XML, CSS files, icons, etc. These are often shipped as
  files in @code{$datadir/appname}, or manually included as literal strings in
  the code.

  The @class{g:resource} API and the @code{glib-compile-resources} program
  provide a convenient and efficient alternative to this which has some nice
  properties. You maintain the files as normal files, so its easy to edit them,
  but during the build the files are combined into a binary bundle that is
  linked into the executable. This means that loading the resource files are
  efficient, as they are already in memory, shared with other instances, and
  simple, no need to check for things like I/O errors or locate the files in
  the filesystem. It also makes it easier to create relocatable applications.

  Resource files can also be marked as compressed. Such files will be included
  in the resource bundle in a compressed form, but will be automatically
  uncompressed when the resource is used. This is very useful e.g. for larger
  text files that are parsed once, or rarely, and then thrown away.

  Resource files can also be marked to be preprocessed, by setting the value of
  the preprocess attribute to a comma-separated list of preprocessing options.
  The only options currently supported are:
  @begin{itemize}
    @begin{item}
      @code{xml-stripblanks} which will use the @code{xmllint} command to strip
      ignorable whitespace from the XML file. For this to work, the
      @code{XMLLINT} environment variable must be set to the full path to the
      @code{xmllint} executable, or @code{xmllint} must be in the @code{PATH}.
      Otherwise the preprocessing step is skipped.
    @end{item}
    @begin{item}
      @code{to-pixdata} which will use the @code{gdk-pixbuf-pixdata} command to
      convert images to the @code{GdkPixdata} format, which allows you to create
      pixbufs directly using the data inside the resource file, rather than an
      uncompressed copy if it. For this, the @code{gdk-pixbuf-pixdata} program
      must be in the @code{PATH}, or the @code{GDK_PIXBUF_PIXDATA} environment
      variable must be set to the full path to the @code{gdk-pixbuf-pixdata}
      executable. Otherwise the resource compiler will abort.
    @end{item}
  @end{itemize}
  Resource files will be exported in the @class{g:resource} namespace using the
  combination of the given prefix and the filename from the file element. The
  alias attribute can be used to alter the filename to expose them at a
  different location in the resource namespace. Typically, this is used to
  include files from a different source directory without exposing the source
  directory in the resource namespace, as in the example below.

  Resource bundles are created by the @code{glib-compile-resources} program
  which takes an XML file that describes the bundle, and a set of files that
  the XML references. These are combined into a binary resource bundle.

  An example resource description:
  @begin{pre}
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<gresources>
  <gresource prefix=\"/org/gtk/Example\">
    <file>data/splashscreen.png</file>
    <file compressed=\"true\">dialog.ui</file>
    <file preprocess=\"xml-stripblanks\">menumarkup.xml</file>
    <file alias=\"example.css\">data/example.css</file>
  </gresource>
</gresources>
  @end{pre}
  This will create a resource bundle with the following files:
  @begin{pre}
/org/gtk/Example/data/splashscreen.png
/org/gtk/Example/dialog.ui
/org/gtk/Example/menumarkup.xml
/org/gtk/Example/example.css
  @end{pre}
  Note that all resources in the process share the same namespace, so use
  Java-style path prefixes, like in the above example, to avoid conflicts.

  You can then use the @code{glib-compile-resources} program to compile the XML
  to a binary bundle that you can load with the @fun{g:resource-load} function.
  However, its more common to use the @code{--generate-source} and
  @code{--generate-header} arguments to create a source file and header to link
  directly into your application. This will generate @code{get_resource()},
  @code{register_resource()} and @code{unregister_resource()} functions,
  prefixed by the @code{--c-name} argument passed to
  @code{glib-compile-resources}. The @code{get_resource()} function returns the
  generated @class{g:resource} instance. The register and unregister functions
  register the resource so its files can be accessed using the
  @fun{g:resources-lookup-data} function.

  Once a @class{g:resource} instance has been created and registered all the
  data in it can be accessed globally in the process by using API calls like
  the @code{g_resources_open_stream()} function to stream the data or the
  @fun{g:resources-lookup-data} function to get a direct pointer to the data.
  You can also use URIs like
  @code{\"resource:///org/gtk/Example/data/splashscreen.png\"} with
  @code{GFile} to access the resource data.

  Some higher-level APIs, such as the @class{gtk:application} class, will
  automatically load resources from certain well-known paths in the resource
  namespace as a convenience. See the documentation for those APIs for details.

  There are two forms of the generated source, the default version uses the
  compiler support for constructor and destructor functions, where available,
  to automatically create and register the @class{g:resource} instance on
  startup or library load time. If you pass @code{--manual-register}, two
  functions to register/unregister the resource are created instead. This
  requires an explicit initialization call in your application/library, but it
  works on all platforms, even on the minor ones where constructors are not
  supported. Constructor support is available for at least Win32, Mac OS and
  Linux.

  Note that resource data can point directly into the data segment of e.g. a
  library, so if you are unloading libraries during runtime you need to be very
  careful with keeping around pointers to data from a resource, as this goes
  away when the library is unloaded. However, in practice this is not generally
  a problem, since most resource accesses are for your own resources, and
  resource data is often used once, during parsing, and then released.

  When debugging a program or testing a change to an installed version, it is
  often useful to be able to replace resources in the program or library,
  without recompiling, for debugging or quick hacking and testing purposes. It
  is possible to use the @code{G_RESOURCE_OVERLAYS} environment variable to
  selectively overlay resources with replacements from the filesystem. It is a
  @code{G_SEARCHPATH_SEPARATOR} separated list of substitutions to perform
  during resource lookups.

  A substitution has the form:
  @begin{pre}
/org/gtk/libgtk=/home/desrt/gtk-overlay
  @end{pre}
  The part before the @code{=} is the resource subpath for which the overlay
  applies. The part after is a filesystem path which contains files and
  subdirectories as you would like to be loaded as resources with the
  equivalent names.

  In the example above, if an application tried to load a resource with the
  resource path @code{/org/gtk/libgtk/ui/gtkdialog.ui} then the
  @class{g:resource} instance would check the filesystem path
  @code{/home/desrt/gtk-overlay/ui/gtkdialog.ui}. If a file was found there, it
  would be used instead. This is an overlay, not an outright replacement, which
  means that if a file is not found at that path, the built-in version will be
  used instead. Whiteouts are not currently supported.

  Substitutions must start with a slash, and must not contain a trailing slash
  before the @code{'='}. The path after the slash should ideally be absolute,
  but this is not strictly required. It is possible to overlay the location of
  a single resource with an individual file.
  @see-class{gtk:application}")

(export 'resource)

;;; ----------------------------------------------------------------------------

;; TODO: Remove the WITH-G-RESOURCES implementation, it is replaced with
;; WITH-RESOURCE and WITH-RESOURCES

(defmacro with-g-resources ((resource path) &body body)
 #+liber-documentation
 "@version{2023-4-22}
  @syntax[]{(with-g-resources (resource path) body) => result}
  @argument[resource]{a @class{g:resource} instance to create and register}
  @argument[path]{a pathname or namestring with the path of a file to load}
  @begin{short}
    The @macro{g:with-g-resources} macro allocates a new @class{g:resource}
    instance, loads the resource from a file, register the resource with the
    process-global set of resources and executes the body that uses the
    resources.
  @end{short}
  After execution of the body the resource is unregistered from the
  process-global set of resources.
  @begin[Warning]{dictionary}
    This macro has been deprecated and will no longer be available in the near
    future. Use the @macro{g:with-resource} macro.
  @end{dictionary}
  @see-class{g:resource}
  @see-function{g:resource-load}
  @see-function{g:resources-register}
  @see-function{g:resources-unregister}"
  `(let ((,resource (resource-load ,path)))
     (resources-register ,resource)
     (unwind-protect
       (progn ,@body)
       (resources-unregister ,resource))))

(export 'with-g-resources)

;;; ----------------------------------------------------------------------------

(defmacro with-resource ((resource path) &body body)
 #+liber-documentation
 "@version{2024-5-12}
  @syntax{(g:with-resource (resource path) body) => result}
  @argument[resource]{a @class{g:resource} instance to create and register}
  @argument[path]{a pathname or namestring with the path of a file to load}
  @argument[body]{a body that uses the binding @arg{resource}}
  @begin{short}
    The @macro{g:with-resource} macro allocates a new @class{g:resource}
    instance, loads the resource from a file, register the resource with the
    process-global set of resources and executes the body that uses the
    resources.
  @end{short}
  After execution of the body the resource is unregistered from the
  process-global set of resources.
  @see-class{g:resource}
  @see-function{g:resource-load}
  @see-function{g:resources-register}
  @see-function{g:resources-unregister}"
  `(let ((,resource (resource-load ,path)))
     (resources-register ,resource)
     (unwind-protect
       (progn ,@body)
       (resources-unregister ,resource))))

(export 'with-resource)

(defmacro with-resources (vars &body body)
 #+liber-documentation
 "@version{2024-5-12}
  @syntax{(g:with-resources ((resource1 path1) ... (resourcen pathn)) body)
    => result}
  @argument[resource1 ... resourcen]{newly created @class{g:resource} instances}
  @argument[path1 ... pathn]{pathnames or namestrings with the path of a file
    to load}
  @argument[body]{a body that uses the bindings @arg{resource1 ... resourcen}}
  @begin{short}
    The @fun{g:with-resources} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.
  See also the @fun{g:with-resource} documentation.
  @see-macro{g:with-resource}"
  (if vars
      (let ((var (glib-sys:mklist (first vars))))
        `(with-resource ,var
           (with-resources ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-resources)

;;; ----------------------------------------------------------------------------
;;; g_resource_load
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resource_load" %resource-load) (glib:boxed resource :return)
  (filename :string)
  (err :pointer))

(defun resource-load (path)
 #+liber-documentation
 "@version{2024-5-12}
  @argument[path]{a pathname or namestring with the path of a file to load, in
    the GLib filenname encoding}
  @return{The new @class{g:resource} instance.}
  @begin{short}
    Loads a binary resource bundle and creates a @class{g:resource} instance
    representation of it, allowing you to query it for data.
  @end{short}
  If you want to use this resource in the global resource namespace you need
  to register it with the @fun{g:resources-register} function. The function
  signals an error if the resource file does not exist.
  @see-class{g:resource}
  @see-function{g:resources-register}"
  (glib:with-g-error (err)
    (%resource-load (namestring path) err)))

(export 'resource-load)

;;; ----------------------------------------------------------------------------
;;; g_resource_new_from_data ()
;;;
;;; GResource *
;;; g_resource_new_from_data (GBytes *data, GError **error);
;;;
;;; Creates a GResource from a reference to the binary resource bundle. This
;;; will keep a reference to data while the resource lives, so the data should
;;; not be modified or freed.
;;;
;;; If you want to use this resource in the global resource namespace you need
;;; to register it with g_resources_register().
;;;
;;; Note: data must be backed by memory that is at least pointer aligned.
;;; Otherwise this function will internally create a copy of the memory since
;;; GLib 2.56, or in older versions fail and exit the process.
;;;
;;; If data is empty or corrupt, G_RESOURCE_ERROR_INTERNAL will be returned.
;;;
;;; data :
;;;     A GBytes
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a new GResource, or NULL on error.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resource_ref                                          not needed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resource_ref" resource-ref) (glib:boxed resource)
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[resource]{a @class{g:resource} instance}
  @return{The passed in @class{g:resource} instance}
  @begin{short}
    Atomically increments the reference count of @arg{resource} by one.
  @end{short}
  This function is MT-safe and may be called from any thread.
  @see-class{g:resource}"
  (resource (glib:boxed resource)))

;;; ----------------------------------------------------------------------------
;;; g_resource_unref                                        not needed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resource_unref" resource-unref) :void
 #+liber-documentation
 "@version{#2022-12-30}
  @argument[resource]{a @class{g:resource} instance}
  @begin{short}
    Atomically decrements the reference count of @arg{resource} by one.
  @end{short}
  If the reference count drops to 0, all memory allocated by the resource is
  released. This function is MT-safe and may be called from any thread.
  @see-class{g:resource}"
  (resource (glib:boxed resource)))

;;; ----------------------------------------------------------------------------
;;; g_resource_lookup_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resource_lookup_data" %resource-lookup-data) :pointer
  (resource (glib:boxed resource))
  (path :string)
  (lookup resource-lookup-flags)
  (err :pointer))

(defun resource-lookup-data (resource path lookup)
 #+liber-documentation
 "@version{2024-5-12}
  @argument[resource]{a @class{g:resource} instance}
  @argument[path]{a string with a pathname inside the resource}
  @argument[lookup]{a @symbol{g:resource-lookup-flags} value}
  @return{The pointer to the data, @code{cffi:null-pointer} on error.}
  @begin{short}
    Looks for a file at the specified path in the resource and returns a pointer
    that lets you directly access the data in memory.
  @end{short}

  The data is always followed by a zero byte, so you can safely use the data
  as a C string. However, that byte is not included in the size of the data.

  For uncompressed resource files this is a pointer directly into the resource
  bundle, which is typically in some readonly data section in the program
  binary. For compressed files we allocate memory on the heap and automatically
  uncompress the data.

  The @arg{lookup} argument controls the behaviour of the lookup.
  @see-class{g:resource}
  @see-symbol{g:resource-lookup-flags}"
  (glib:with-g-error (err)
    (%resource-lookup-data resource path lookup err)))

(export 'resource-lookup-data)

;;; ----------------------------------------------------------------------------
;;; g_resource_open_stream ()
;;;
;;; GInputStream *
;;; g_resource_open_stream (GResource *resource,
;;;                        const char *path,
;;;                        GResourceLookupFlags lookup_flags,
;;;                        GError **error);
;;;
;;; Looks for a file at the specified path in the resource and returns a
;;; GInputStream that lets you read the data.
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; resource :
;;;     A GResource
;;;
;;; path :
;;;     A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     GInputStream or NULL on error. Free the returned object with
;;;     g_object_unref().
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resource_enumerate_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resource_enumerate_children" %resource-enumerate-children)
    glib:strv-t
  (resource (glib:boxed resource))
  (path :string)
  (lookup resource-lookup-flags)
  (err :pointer))

(defun resource-enumerate-children (resource path lookup)
 #+liber-documentation
 "@version{2024-5-12}
  @argument[resource]{a @class{g:resource} instance}
  @argument[path]{a string with a pathname inside the resource}
  @argument[lookup]{a @symbol{g:resource-lookup-flags} value}
  @return{The list of strings.}
  @begin{short}
    Returns all the names of children at the specified path in the resource.
  @end{short}
  The return result is a list of strings. The @arg{lookup} argument controls
  the behaviour of the lookup.
  @see-class{g:resource}
  @see-symbol{g:resource-lookup-flags}"
  (glib:with-g-error (err)
    (%resource-enumerate-children resource path lookup err)))

(export 'resource-enumerate-children)

;;; ----------------------------------------------------------------------------
;;; g_resource_get_info
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resource_get_info" %resource-info) :boolean
  (resource (glib:boxed resource))
  (path :string)
  (lookup resource-lookup-flags)
  (size (:pointer :size))
  (flags (:pointer :uint32))
  (err :pointer))

(defun resource-info (resource path lookup)
 #+liber-documentation
 "@version{2024-5-12}
  @argument[resource]{a @class{g:resource} instance}
  @argument[path]{a string with a pathname inside the resource}
  @argument[lookup]{a @symbol{g:resource-lookup-flags} value}
  @begin{return}
    @arg{size} -- an integer with the length of the contents of the file @br{}
    @arg{flags} -- an unsigned integer with the flags about the file
  @end{return}
  @begin{short}
    Looks for a file at the specified path in the resource and if found returns
    information about it.
  @end{short}
  The @arg{lookup} argument controls the behaviour of the lookup.
  @see-class{g:resource}
  @see-symbol{g:resource-lookup-flags}"
  (glib:with-g-error (err)
    (cffi:with-foreign-objects ((size :size) (flags :uint32))
      (when (%resource-info resource path lookup size flags err)
        (values (cffi:mem-ref size :size)
                (cffi:mem-ref flags :uint32))))))

(export 'resource-info)

;;; ----------------------------------------------------------------------------
;;; g_static_resource_init ()
;;;
;;; void
;;; g_static_resource_init (GStaticResource *static_resource);
;;;
;;; Initializes a GResource from static data using a GStaticResource.
;;;
;;; This is normally used by code generated by glib-compile-resources and is
;;; not typically used by other code.
;;;
;;; static_resource :
;;;     pointer to a static GStaticResource
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_static_resource_fini ()
;;;
;;; void
;;; g_static_resource_fini (GStaticResource *static_resource);
;;;
;;; Finalized a GResource initialized by g_static_resource_init().
;;;
;;; This is normally used by code generated by glib-compile-resources and is
;;; not typically used by other code.
;;;
;;; static_resource :
;;;     pointer to a static GStaticResource
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_static_resource_get_resource ()
;;;
;;; GResource *
;;; g_static_resource_get_resource (GStaticResource *static_resource);
;;;
;;; Gets the GResource that was registered by a call to
;;; g_static_resource_init().
;;;
;;; This is normally used by code generated by glib-compile-resources and is
;;; not typically used by other code.
;;;
;;; static_resource :
;;;     pointer to a static GStaticResource
;;;
;;; Returns :
;;;     a GResource.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resources_register
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resources_register" resources-register) :void
 #+liber-documentation
 "@version{2024-5-12}
  @argument[resource]{a @class{g:resource} instance}
  @begin{short}
    Registers the resource with the process-global set of resources.
  @end{short}
  Once a resource is registered the files in it can be accessed with the global
  resource lookup functions like the @fun{g:resources-lookup-data} function.
  @see-class{g:resource}
  @see-function{g:resources-lookup-data}
  @see-function{g:resources-unregister}"
  (resource (glib:boxed resource)))

(export 'resources-register)

;;; ----------------------------------------------------------------------------
;;; g_resources_unregister
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resources_unregister" resources-unregister) :void
 #+liber-documentation
 "@version{2024-5-12}
  @argument[resource]{a @class{g:resource} instance}
  @begin{short}
    Unregisters the resource from the process-global set of resources.
  @end{short}
  @see-class{g:resource}
  @see-function{g:resources-register}"
  (resource (glib:boxed resource)))

(export 'resources-unregister)

;;; ----------------------------------------------------------------------------
;;; g_resources_lookup_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resources_lookup_data" %resources-lookup-data) :pointer
  (path :string)
  (lookup resource-lookup-flags)
  (err :pointer))

(defun resources-lookup-data (path &optional (lookup :none))
 #+liber-documentation
 "@version{2024-5-12}
  @argument[path]{a string with a pathname inside the resource}
  @argument[lookup]{an optional @symbol{g:resource-lookup-flags} value,
    the default value is @code{:none}}
  @return{The pointer or @code{cffi:null-pointer}.}
  @begin{short}
    Looks for a file at the specified path in the set of globally registered
    resources and returns a pointer that lets you directly access the data in
    memory.
  @end{short}
  The data is always followed by a zero byte, so you can safely use the data
  as a C string.

  For uncompressed resource files this is a pointer directly into the resource
  bundle, which is typically in some readonly data section in the program
  binary. For compressed files we allocate memory on the heap and automatically
  uncompress the data.

  The @arg{lookup} argument controls the behaviour of the lookup.
  @see-class{g:resource}
  @see-symbol{g:resource-lookup-flags}"
  (glib:with-g-error (err)
    (%resources-lookup-data path lookup err)))

(export 'resources-lookup-data)

;;; ----------------------------------------------------------------------------
;;; g_resources_open_stream ()
;;;
;;; GInputStream *
;;; g_resources_open_stream (const char *path,
;;;                          GResourceLookupFlags lookup_flags,
;;;                          GError **error);
;;;
;;; Looks for a file at the specified path in the set of globally registered
;;; resources and returns a GInputStream that lets you read the data.
;;;
;;; lookup_flags controls the behaviour of the lookup.
;;;
;;; path :
;;;     A pathname inside the resource
;;;
;;; lookup_flags :
;;;     A GResourceLookupFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     GInputStream or NULL on error. Free the returned object with
;;;     g_object_unref().
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_resources_enumerate_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resources_enumerate_children" %resources-enumerate-children)
    glib:strv-t
  (path :string)
  (lookup resource-lookup-flags)
  (err :pointer))

(defun resources-enumerate-children (path lookup)
 #+liber-documentation
 "@version{2024-5-12}
  @argument[path]{a string with a pathname inside the resource}
  @argument[lookup]{a @symbol{g:resource-lookup-flags} value}
  @return{The list of strings.}
  @begin{short}
    Returns all the names of children at the specified path in the set of
    globally registered resources.
  @end{short}
  The @arg{lookup} argument controls the behaviour of the lookup.
  @see-class{g:resource}
  @see-symbol{g:resource-lookup-flags}"
  (glib:with-g-error (err)
    (%resources-enumerate-children path lookup err)))

(export 'resources-enumerate-children)

;;; ----------------------------------------------------------------------------
;;; g_resources_get_info
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_resources_get_info" %resources-info) :boolean
  (path :string)
  (lookup resource-lookup-flags)
  (size (:pointer :size))
  (flags (:pointer :uint32))
  (err :pointer))

(defun resources-info (path lookup)
 #+liber-documentation
 "@version{2024-5-12}
  @argument[path]{a string with a pathname inside the resource}
  @argument[lookup]{a @symbol{g:resource-lookup-flags} value}
  @begin{return}
    @arg{size} -- an integer with the length of the contents of the file @br{}
    @arg{flags} -- an unsigned integer with the flags about the file
  @end{return}
  @begin{short}
    Looks for a file at the specified path in the set of globally registered
    resources and if found returns information about it.
  @end{short}
  The @arg{lookup} argument controls the behaviour of the lookup.
  @see-class{g:resource}
  @see-symbol{g:resource-lookup-flags}"
  (glib:with-g-error (err)
    (cffi:with-foreign-objects ((size :size) (flags :uint32))
      (when (%resources-info path lookup size flags err)
        (values (cffi:mem-ref size :size)
                (cffi:mem-ref flags :uint32))))))

(export 'resources-info)

;;; --- End of file gio.resource.lisp ------------------------------------------
