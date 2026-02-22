;;; ----------------------------------------------------------------------------
;;; gio.settings.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.86 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2025 Dieter Kaiser
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
;;;     GSettings
;;;     GSettingsSchema
;;;     GSettingsSchemaSource
;;;     GSettingsBindFlags
;;;
;;; Accessors
;;;
;;;
;;; Functions
;;;
;;;     g_settings_schema_source_default
;;;     g_settings_schema_source_lookup
;;;     g_settings_schema_source_list_schemas
;;;
;;;     g_settings_schema_get_id
;;;     g_settings_schema_get_key
;;;     g_settings_schema_get_path
;;;     g_settings_schema_has_key
;;;     g_settings_schema_list_children
;;;     g_settings_schema_list_keys
;;;
;;;     g_settings_new
;;;     g_settings_new_full
;;;     g_settings_new_with_backend
;;;     g_settings_new_with_backend_and_path
;;;     g_settings_new_with_path
;;;
;;;     g_settings_list_schemas
;;;     g_settings_list_relocatable_schemas
;;;     g_settings_sync
;;;
;;;     g_settings_apply
;;;     g_settings_delay

;;;     g_settings_bind
;;;     g_settings_bind_with_mapping
;;;     g_settings_bind_with_mapping_closures
;;;     g_settings_bind_writable
;;;     g_settings_unbind

;;;     g_settings_create_action

;;;     g_settings_is_writable
;;;     g_settings_list_keys

;;;     g_settings_reset
;;;     g_settings_revert
;;;
;;;     g_settings_list_children
;;;     g_settings_get_child
;;;     g_settings_get_has_unapplied
;;;     g_settings_get_mapped
;;;     g_settings_get_range
;;;     g_settings_range_check

;;;     g_settings_get_value
;;;     g_settings_set_value
;;;     g_settings_get
;;;     g_settings_set

;;;
;;;     g_settings_get_default_value
;;;     g_settings_get_user_value


;;;     g_settings_get_boolean
;;;     g_settings_set_boolean
;;;     g_settings_get_int
;;;     g_settings_set_int
;;;     g_settings_get_int64
;;;     g_settings_set_int64
;;;     g_settings_get_uint
;;;     g_settings_set_uint
;;;     g_settings_get_uint64
;;;     g_settings_set_uint64
;;;     g_settings_get_double
;;;     g_settings_set_double
;;;     g_settings_get_string
;;;     g_settings_set_string
;;;     g_settings_get_strv
;;;     g_settings_set_strv

;;;     g_settings_get_enum
;;;     g_settings_set_enum
;;;     g_settings_get_flags
;;;     g_settings_set_flags
;;;
;;; Signals
;;;
;;;     change-event
;;;     changed
;;;     writable-change-event
;;;     writable-changed
;;;
;;; Properties
;;;
;;;     backend
;;;     delay-apply
;;;     has-unapplied
;;;     path
;;;     schema
;;;     schema-id
;;;     settings-schema
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GSettings
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GioSettingsBindFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GSettingsBindFlags" settings-bind-flags
  (:export t
   :type-initializer "g_settings_bind_flags_get_type")
  (:default 0)
  (:get 1)
  (:set 2)
  (:no-sensitivity 4)
  (:get-no-changes 8)
  (:invert-boolean 16))

#+liber-documentation
(setf (liber:alias-for-symbol 'settings-bind-flags)
      "GFlags"
      (liber:symbol-documentation 'settings-bind-flags)
 "@version{2025-12-25}
  @begin{declaration}
(gobject:define-gflags \"GSettingsBindFlags\" settings-bind-flags
  (:export t
   :type-initializer \"g_settings_bind_flags_get_type\")
  (:default 0)
  (:get 1)
  (:set 2)
  (:no-sensitivity 4)
  (:get-no-changes 8)
  (:invert-boolean 16))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:default]{Equivalent to @code{'(:get :set)}}
      @entry[:get]{Update the @code{GObject} property when the setting changes.
        It is an error to use this flag if the property is not writable.}
      @entry[:set]{Update the setting when the @code{GObject} property changes.
        It is an error to use this flag if the property is not readable.}
      @entry[:no-sensitivity]{Do not try to bind a \"sensitivity\" property to
        the writability of the setting.}
      @entry[:get-no-changes]{When set in addition to @code{:get}, set the
        @code{GObject} property value initially from the setting, but do not
        listen for changes of the setting.}
      @entry[:invert-boolean]{When passed to the @fun{g:settings-bind} function,
        uses a pair of mapping functions that invert the boolean value when
        mapping between the setting and the property. The setting and property
        must both be booleans. You cannot pass this flag to the
        @fun{g:settings-bind-with-mapping} function.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Flags used when creating a binding.
  @end{short}
  These flags determine in which direction the binding works. The default is to
  synchronize in both directions.
  @see-class{g:settings}")

;;; ----------------------------------------------------------------------------
;;; GSettingsSchema
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque settings-schema "GSettingsSchema"
  :export t
  :type-initializer "g_settings_schema_get_type"
  :alloc (error "GSettingsSchema cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'settings-schema)
      "GBoxed"
      (documentation 'settings-schema 'type)
 "@version{2025-12-24}
  @begin{declaration}
(glib:define-gboxed-opaque settings-schema \"GSettingsSchema\"
  :export t
  :type-initializer \"g_settings_schema_get_type\"
  :alloc (error \"GSettingsSchema cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    The GSettingsSchemaSource and GSettingsSchema APIs provide a mechanism for
    advanced control over the loading of schemas and a mechanism for
    introspecting their content.
  @end{short}
  @see-class{g:settings}")

;;;     g_settings_schema_get_id
;;;     g_settings_schema_get_key
;;;     g_settings_schema_get_path
;;;     g_settings_schema_has_key
;;;     g_settings_schema_list_children
;;;     g_settings_schema_list_keys

;;; ----------------------------------------------------------------------------
;;; GSettingsSchemaSource
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque settings-schema-source "GSettingsSchemaSource"
  :export t
  :type-initializer "g_settings_schema_source_get_type"
  :alloc (error "GSettingsSchemaSource cannot be created from the Lisp side"))

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_source_new_from_directory
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_source_new_from_directory"
               %settings-schema-source-new-from-directory)
    (glib:boxed settings-schema-source)
  (dir :string)
  (parent (glib:boxed settings-schema-source))
  (trusted :boolean)
  (err :pointer))

(defun settings-schema-source-new-from-directory (path trusted &key parent)
  "Attempts to create a new schema source corresponding to the contents of the
 given PATH

This function is not required for normal uses of GSettings but it may be useful
to authors of plugin management systems.

The directory should contain a file called gschemas.compiled as produced by the
glib-compile-schemas tool.

If trusted is TRUE then gschemas.compiled is trusted not to be corrupted. This
assumption has a performance advantage, but can result in crashes or
inconsistent behaviour in the case of a corrupted file. Generally, you should
set trusted to TRUE for files installed by the system and to FALSE for files in
the home directory.

TODO: ERROR HANDLING NOT IMPLEMENTED. ---
In either case, an empty file or some types of corruption in the file will
result in G_FILE_ERROR_INVAL being returned.
---

If PARENT is non-NIL then there are two effects.

First, if `settings-schema-source-lookup' is called with the recursive flag set
to TRUE and the schema can not be found in the source, the lookup will recurse
to the parent.

Second, any references to other schemas specified within this source (ie: child
or extends) references may be resolved from the parent.

For this second reason, except in very unusual situations, the parent should
probably be given as the default schema source, as returned by
`settings-schema-source-default'.
"
  (%settings-schema-source-new-from-directory path parent trusted (cffi:null-pointer)))

(export 'settings-schema-source-new-from-directory)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_source_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_source_get_default"
               settings-schema-source-default)
    (glib:boxed settings-schema-source))

(export 'settings-schema-source-default)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_source_lookup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_source_lookup" settings-schema-source-lookup)
    (glib:boxed settings-schema :return)
  (source (glib:boxed settings-schema-source))
  (schema-id :string)
  (recursive :boolean))

(export 'settings-schema-source-lookup)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_source_list_schemas
;;; ----------------------------------------------------------------------------

;; FIXME: Does not work.
;;  --------------------------------
;;  G-SETTINGS-SCHEMA-SOURCE-LIST-SCHEMAS in GIO-SETTINGS []:
;;       Unexpected Error: #<BABEL-ENCODINGS:INVALID-UTF8-STARTER-BYTE {1002934BD3}>
;; Illegal :UTF-8 character starting at position 1..
;;  --------------------------------


(cffi:defcfun ("g_settings_schema_source_list_schemas"
               %settings-schema-source-list-schemas) :void
  (source (glib:boxed settings-schema-source))
  (recursive :boolean)
  (non-relocatable :pointer)
  (relocatable :pointer))

#+nil
(defun settings-schema-source-list-schemas (source recursive)
  (cffi:with-foreign-objects ((ptr1 'glib:strv-t) (ptr2 'glib:strv-t))
    (%settings-schema-source-list-schemas source recursive ptr1 ptr2)
    (values (cffi:convert-from-foreign ptr1 'glib:strv-t)
            ptr2 ;(cffi:convert-from-foreign ptr2 'glib:strv-t))))
)))

#+nil
(export 'settings-schema-source-list-schemas)

;;; ----------------------------------------------------------------------------
;;; GioSettings
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GSettings" settings
  (:superclass gobject:object
   :export t
   :interfaces nil
   :type-initializer "g_settings_get_type")
  ((backend
    settings-backend
    "backend" "GSettingsBackend" t t)
   (delay-apply
    settings-delay-apply
    "delay-apply" "gboolean" t nil)
   (has-unapplied
    settings-has-unapplied
    "has-unapplied" "gboolean" t nil)
   (path
    settings-path
    "path" "gchararray" t t)
   (schema
    settings-schema
    "schema" "gchararry" t t)
   (schema-id
    settings-schema-id
    "schema-id" "gchararray" t t)
   (settings-schema
    settings-settings-schema
    "settings-schema" "GSettingsSchema" t t)))

#+liber-documentation
(setf (documentation 'settings 'type)
 "@version{2025-12-25}
  @begin{short}
    The @class{g:settings} class provides a convenient API for storing and
    retrieving application settings.
  @end{short}

  Reads and writes can be considered to be non-blocking. Reading settings with
  the @class{g:settings} API is typically extremely fast: on approximately the
  same order of magnitude (but slower than) a @code{GHashTable} lookup. Writing
  settings is also extremely fast in terms of time to return to your
  application, but can be extremely expensive for other threads and other
  processes. Many settings backends (including @code{dconf}) have lazy
  initialisation which means in the common case of the user using their computer
  without modifying any settings a lot of work can be avoided. For @code{dconf},
  the D-Bus service does not even need to be started in this case. For this
  reason, you should only ever modify @code{GSettings} keys in response to
  explicit user action. Particular care should be paid to ensure that
  modifications are not made during startup - for example, when setting the
  initial value of preferences widgets. The built-in @fun{g:settings-bind}
  functionality is careful not to write settings in response to notify signals
  as a result of modifications that it makes to widgets.

  When creating a @code{GSettings} instance, you have to specify a schema that
  describes the keys in your settings and their types and default values, as
  well as some other information. Normally, a schema has a fixed path that
  determines where the settings are stored in the conceptual global tree of
  settings. However, schemas can also be \"relocatable\", that is, not equipped
  with a fixed path. This is useful, for example, when the schema describes an
  \"account\", and you want to be able to store a arbitrary number of accounts.

  Paths must start with and end with a forward slash character (/) and must not
  contain two sequential slash characters. Paths should be chosen based on a
  domain name associated with the program or library to which the settings
  belong. Examples of paths are @file{/org/gtk/settings/file-chooser/} and
  @file{/ca/desrt/dconf-editor/}. Paths should not start with @file{/apps/},
  @file{/desktop/} or @file{/system/} as they often did in @code{GConf}.

  Unlike other configuration systems (like @code{GConf}), @code{GSettings} does
  not restrict keys to basic types like strings and numbers. @code{GSettings}
  stores values as @class{g:variant} instances, and allows any
  @class{g:variant-type} instances for keys. Key names are restricted to
  lowercase characters, numbers and -. Furthermore, the names must begin with a
  lowercase character, must not end with a -, and must not contain consecutive
  dashes.

  Similar to @code{GConf}, the default values in @code{GSettings} schemas can be
  localized, but the localized values are stored in @code{gettext} catalogs and
  looked up with the domain that is specified in the @code{gettext}-domain
  attribute of the @code{<schemalist>} or @code{<schema>} elements and the
  category that is specified in the @code{l10n} attribute of the
  @code{<default>} element. The string which is translated includes all text in
  the @code{<default>} element, including any surrounding quotation marks.

  The @code{l10n} attribute must be set to messages or time, and sets the locale
  category for translation. The messages category should be used by default. Use
  time for translatable date or time formats. A translation comment can be added
  as an XML comment immediately above the @code{<default>} element - it is
  recommended to add these comments to aid translators understand the meaning
  and implications of the default value. An optional translation context
  attribute can be set on the @code{<default>} element to disambiguate multiple
  defaults which use the same string.

  For example:
  @begin{pre}
<!-- Translators: A list of words which are not allowed to be typed,
     in GVariant serialization syntax.
     See: https://developer.gnome.org/glib/stable/gvariant-text.html -->
<default l10n='messages' context='Banned words'>['bad', 'words']</default>
  @end{pre}
  Translations of default values must remain syntactically valid serialized
  @code{GVariants}, for example, retaining any surrounding quotation marks, or
  runtime errors will occur.

  @code{GSettings} uses schemas in a compact binary form that is created by the
  @code{glib-compile-schemas} utility. The input is a schema description in an
  XML format. A DTD for the @code{gschema} XML format can be found here:
  @url[https://gitlab.gnome.org/GNOME/glib/-/blob/HEAD/gio/gschema.dtd]{gschema.dtd}.
  The @code{glib-compile-schemas} tool expects schema files to have the
  extension @file{.gschema.xml}.

  At runtime, schemas are identified by their ID (as specified in the ID
  attribute of the @code{<schema>} element). The convention for schema IDs is
  to use a dotted name, similar in style to a D-Bus bus name, for example,
  @file{org.gnome.SessionManager}. In particular, if the settings are for a
  specific service that owns a D-Bus bus name, the D-Bus bus name and schema ID
  should match. For schemas which deal with settings not associated with one
  named application, the ID should not use @code{StudlyCaps}, for example,
  @file{org.gnome.font-rendering}.

  In addition to @code{GVariant} types, keys can have types that have enumerated
  types. These can be described by a @code{<choice>}, @code{<enum>} or
  @code{<flags>} element, as seen in the second example below. The underlying
  type of such a key is string, but you can use the @fun{g:settings-enum} or
  @fun{g:settings-flags} functions to access the numeric values corresponding
  to the string value of enum and flags keys.

  An example for default value:
  @begin{pre}
<schemalist>
  <schema id=\"org.gtk.Test\" path=\"/org/gtk/Test/\" gettext-domain=\"test\">
    <key name=\"greeting\" type=\"s\">
      <default l10n=\"messages\">\"Hello, earthlings\"</default>
      <summary>A greeting</summary>
      <description>
        Greeting of the invading martians
      </description>
    </key>
    <key name=\"box\" type=\"(ii)\">
      <default>(20,30)</default>
    </key>
    <key name=\"empty-string\" type=\"s\">
      <default>\"\"</default>
      <summary>Empty strings have to be provided in GVariant form</summary>
    </key>
  </schema>
</schemalist>
  @end{pre}
  An example for ranges, choices and enumerated types:
  @begin{pre}
<schemalist>
  <enum id=\"org.gtk.Test.myenum\">
    <value nick=\"first\" value=\"1\"/>
    <value nick=\"second\" value=\"2\"/>
  </enum>
  <flags id=\"org.gtk.Test.myflags\">
    <value nick=\"flag1\" value=\"1\"/>
    <value nick=\"flag2\" value=\"2\"/>
    <value nick=\"flag3\" value=\"4\"/>
  </flags>
  <schema id=\"org.gtk.Test\">
    <key name=\"key-with-range\" type=\"i\">
      <range min=\"1\" max=\"100\"/>
      <default>10</default>
    </key>
    <key name=\"key-with-choices\" type=\"s\">
      <choices>
        <choice value='Elisabeth'/>
        <choice value='Annabeth'/>
        <choice value='Joe'/>
      </choices>
      <aliases>
        <alias value='Anna' target='Annabeth'/>
        <alias value='Beth' target='Elisabeth'/>
      </aliases>
      <default>'Joe'</default>
    </key>
    <key name='enumerated-key' enum='org.gtk.Test.myenum'>
      <default>'first'</default>
    </key>
    <key name='flags-key' flags='org.gtk.Test.myflags'>
      <default>[\"flag1\",\"flag2\"]</default>
    </key>
  </schema>
</schemalist>
  @end{pre}
  @subheading{Vendor overrides}
  Default values are defined in the schemas that get installed by an
  application. Sometimes, it is necessary for a vendor or distributor to adjust
  these defaults. Since patching the XML source for the schema is inconvenient
  and error-prone, @code{glib-compile-schemas} reads so-called \"vendor
  override\" files. These are keyfiles in the same directory as the XML schema
  sources which can override default values. The schema ID serves as the group
  name in the key file, and the values are expected in serialized
  @code{GVariant} form, as in the following example:
  @begin{pre}
[org.gtk.Example]
key1='string'
key2=1.5
  @end{pre}
  @code{glib-compile-schemas} expects schema files to have the extension
  @file{.gschema.override}.

  @subheading{Binding}
  A very convenient feature of @code{GSettings} lets you bind @code{GObject}
  properties directly to settings, using the @fun{g:settings-bind} function.
  Once a @code{GObject} property has been bound to a setting, changes on either
  side are automatically propagated to the other side. @code{GSettings} handles
  details like mapping between @code{GObject} and @code{GVariant} types, and
  preventing infinite cycles.

  This makes it very easy to hook up a preferences dialog to the underlying
  settings. To make this even more convenient, @code{GSettings} looks for a
  boolean property with the name sensitivity and automatically binds it to the
  writability of the bound setting. If this \"magic\" gets in the way, it can
  be suppressed with the @val[g:settings-bind-flags]{:no-sensitivity} flag.

  @subheading{Relocatable schemas}
  A relocatable schema is one with no path attribute specified on its
  @code{<schema>} element. By using the @fun{g:settings-new-with-path} function,
  a @code{GSettings} object can be instantiated for a relocatable schema,
  assigning a path to the instance. Paths passed to the
  @fun{g:settings-new-with-path} function will typically be constructed
  dynamically from a constant prefix plus some form of instance identifier, but
  they must still be valid @code{GSettings} paths. Paths could also be constant
  and used with a globally installed schema originating from a dependency
  library.

  For example, a relocatable schema could be used to store geometry information
  for different windows in an application. If the schema ID was
  @file{org.foo.MyApp.Window}, it could be instantiated for paths
  @file{/org/foo/MyApp/main/}, @file{/org/foo/MyApp/document-1/},
  @file{/org/foo/MyApp/document-2/}, and so on. If any of the paths are well
  known they can be specified as @code{<child>} elements in the parent schema,
  for example:
  @begin{pre}
<schema id=\"org.foo.MyApp\" path=\"/org/foo/MyApp/\">
  <child name=\"main\" schema=\"org.foo.MyApp.Window\"/>
</schema>
  @end{pre}
  @subheading{Build system integration}
  @subheading{Meson}
  @code{GSettings} is natively supported by Meson’s GNOME module. You can
  install the schemas as any other data file:
  @begin{pre}
install_data(
  'org.foo.MyApp.gschema.xml',
  install_dir: get_option('datadir') / 'glib-2.0/schemas',
)
  @end{pre}
  You can use the @code{gnome.post_install()} function to compile the schemas
  on installation:
  @begin{pre}
gnome = import('gnome')
gnome.post_install(
  glib_compile_schemas: true,
)
  @end{pre}
  If an enumerated type defined in a C header file is to be used in a
  @code{GSettings} schema, it can either be defined manually using an
  @code{<enum>} element in the schema XML, or it can be extracted automatically
  from the C header. This approach is preferred, as it ensures the two
  representations are always synchronised. To do so, you will need to use the
  @code{gnome.mkenums()} function with the following templates:
  @begin{pre}
schemas_enums = gnome.mkenums('org.foo.MyApp.enums.xml',
  comments: '<!-- @@comment@@ -->',
  fhead: '<schemalist>',
  vhead: '  <@@type@@ id=\"org.foo.MyApp.@@EnumName@@\">',
  vprod: '    <value nick=\"@@valuenick@@\" value=\"@@valuenum@@\"/>',
  vtail: '  </@@type@@>',
  ftail: '</schemalist>',
  sources: enum_sources,
  install_header: true,
  install_dir: get_option('datadir') / 'glib-2.0/schemas',
)
  @end{pre}
  It is recommended to validate your schemas as part of the test suite for your
  application:
  @begin{pre}
test('validate-schema',
  find_program('glib-compile-schemas'),
  args: ['--strict', '--dry-run', meson.current_source_dir()],
)
  @end{pre}
  If your application allows running uninstalled, you should also use the
  @code{gnome.compile_schemas()} function to compile the schemas in the current
  build directory:
  @begin{pre}
gnome.compile_schemas()
  @end{pre}
  @subheading{Autotools}
  @code{GSettings} comes with autotools integration to simplify compiling and
  installing schemas. To add @code{GSettings} support to an application, add
  the following to your @file{configure.ac}:
  @begin{pre}
GLIB_GSETTINGS
  @end{pre}
  In the appropriate @file{Makefile.am}, use the following snippet to compile
  and install the named schema:
  @begin{pre}
gsettings_SCHEMAS = org.foo.MyApp.gschema.xml
EXTRA_DIST = $(gsettings_SCHEMAS)
@@GSETTINGS_RULES@@
  @end{pre}
  If an enumerated type defined in a C header file is to be used in a
  @code{GSettings} schema, it can either be defined manually using an
  @code{<enum>} element in the schema XML, or it can be extracted automatically
  from the C header. This approach is preferred, as it ensures the two
  representations are always synchronised. To do so, add the following to the
  relevant @file{Makefile.am}:
  @begin{pre}
gsettings_ENUM_NAMESPACE = org.foo.MyApp
gsettings_ENUM_FILES = my-app-enums.h my-app-misc.h
  @end{pre}
  @code{gsettings_ENUM_NAMESPACE} specifies the schema namespace for the enum
  files, which are specified in @code{gsettings_ENUM_FILES}. This will generate
  a @file{org.foo.MyApp.enums.xml} file containing the extracted enums, which
  will be automatically included in the schema compilation, install and
  uninstall rules. It should not be committed to version control or included in
  @code{EXTRA_DIST}.

  @subheading{Localization}
  No changes are needed to the build system to mark a schema XML file for
  translation. Assuming it sets the gettext-domain attribute, a schema may be
  marked for translation by adding it to @file{POTFILES.in}, assuming
  @code{gettext} version 0.19 or newer is in use (the preferred method for
  translation):
  @begin{pre}
data/org.foo.MyApp.gschema.xml
  @end{pre}
  Alternatively, if @code{intltool} version 0.50.1 is in use:
  @begin{pre}
[type: gettext/gsettings]data/org.foo.MyApp.gschema.xml
  @end{pre}
  @code{GSettings} will use @code{gettext} to look up translations for the
  @code{<summary>} and @code{<description>} elements, and also any
  @code{<default>} elements which have a @code{l10n} attribute set.

  Translations must not be included in the @file{.gschema.xml} file by the build
  system, for example by using a rule to generate the XML file from a template.
  @begin[Signal Details]{dictionary}
    @begin[settings::change-event]{signal}
      @begin{pre}
lambda (settings keys nkeys)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[settings]{The @class{g:settings} instance that received the
          signal.}
        @entry[keys]{The array of strings for the keys which have changed. The
          argument can be the @code{cffi:null-pointer} value. The length of the
          array is specified in the @arg{nkeys} argument.}
        @entry[nkeys]{The integer for the length of the keys array, or 0.}
      @end{simple-table}
    @end{signal}
    @begin[settings::changed]{signal}
      @begin{pre}
lambda (settings key)     :run-first
      @end{pre}
      @begin{simple-table}
        @entry[settings]{The @class{g:settings} instance that received the
          signal.}
        @entry[key]{The string for the name of the key that changed.}
      @end{simple-table}
      Emitted when a key has potentially changed. You should call one of the
      @fun{g:settings-get} calls to check the new value. This signal supports
      detailed connections. You can connect to the detailed signal
      @code{changed::x } in order to only receive callbacks when key @code{x}
      changes. Note that settings only emits this signal if you have read key
      at least once while a signal handler was already connected for key.
    @end{signal}
    @begin[settings::writable-change-event]{signal}
      @begin{pre}
lambda (settings key)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[settings]{The @class{g:settings} instance that received the
          signal.}
        @entry[key]{The string for the key, or @code{nil}.}
      @end{simple-table}
      Emitted once per writability change event that affects this settings
      object. You should connect to this signal if you are interested in viewing
      groups of changes before they are split out into multiple emissions of the
      GSettings::writable-changed signal. For most use cases it is more
      appropriate to use the GSettings::writable-changed signal. In the event
      that the writability change applies only to a single key, key will be set
      to the GQuark for that key. In the event that the writability change
      affects the entire settings object, key will be 0. The default handler for
      this signal invokes the GSettings::writable-changed and
      GSettings::changed signals for each affected key. This is done because
      changes in writability might also imply changes in value (if for example,
      a new mandatory setting is introduced). If any other connected handler
      returns true then this default functionality will be suppressed.
    @end{signal}
    @begin[settings::writable-changed]{signal}
      @begin{pre}
lambda (settings key)     :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[settings]{The @class{g:settings} instance that received the
          signal.}
        @entry[key]{The string for the key, or @code{nil}.}
      @end{simple-table}
      Emitted when the writability of a key has potentially changed. You should
      call g_settings_is_writable() in order to determine the new status. This
      signal supports detailed connections. You can connect to the detailed
      signal writable-changed::x in order to only receive callbacks when the
      writability of x changes.
    @end{signal}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:settings-backend -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "backend" 'settings) t)
 "The @code{backend} property of type @code{GSettingsBackend} (Read) @br{}
  The context that the settings are stored in.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-backend)
      "Accessor"
      (documentation 'settings-backend 'function)
 "@version{2025-12-24}
  @syntax{(g:settings-backend object) => backend}
  @argument[object]{a @class{g:settings} object}
  @argument[backend]{a @class{g:settings-backend} object}
  @begin{short}
    The accessor for the @slot[g:settings]{backend} slot of the
    @class{g:settings} class gets the context that the settings are stored in.
  @end{short}
  @see-class{g:settings}")

;;; --- g:settings-delay-apply -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "delay-apply" 'settings) t)
 "The @code{delay-apply} property of type @code{:boolean} (Read) @br{}
  Whether the @class{g:settings} object is in \"delay-apply\" mode. See the
  @fun{g:settings-delay} function for details. @br{}
  Default value : @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-delay-apply)
      "Accessor"
      (documentation 'settings-delay-apply 'function)
 "@version{2025-12-25}
  @syntax{(g:settings-delay-apply object) => mode}
  @argument[object]{a @class{g:settings} object}
  @argument[mode]{a boolean whether @arg{object} is in \"delay-apply\" mode}
  @begin{short}
    The accessor for the @slot[g:settings]{delay-apply} slot of the
    @class{g:settings} class gets whether @arg{object} is in \"delay-apply\"
    mode.
  @end{short}
  @see-class{g:settings}")

;;; --- g:settings-has-unapplied -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-unapplied" 'settings) t)
 "The @code{has-unapplied} property of type @code{:boolean} (Read) @br{}
  Whether the @class{g:settings} object has outstanding changes. These changes
  will be applied when the @fun{g:settings-apply} function is called. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-has-unapplied)
      "Accessor"
      (documentation 'settings-has-unapplied 'function)
 "@version{2025-12-25}
  @syntax{(g:settings-has-unapplied object) => setting}
  @argument[object]{a @class{g:settings} object}
  @argument[setting]{a boolean whether @arg{object} has outstanding changes}
  @begin{short}
    The accessor for the @slot[g:settings]{has-unapplied} slot of the
    @class{g:settings} class gets whether @arg{object} has outstanding changes.
  @end{short}
  @see-class{g:settings}")

;;; --- g:settings-path --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "path" 'settings) t)
 "The @code{path} property of type @code{:string} (Read) @br{}
  The path within the backend where the settings are stored.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-path)
      "Accessor"
      (documentation 'settings-path 'function)
 "@version{2025-12-25}
  @syntax{(g:settings-path object) => path}
  @argument[object]{a @class{g:settings} object}
  @argument[path]{a string for the path within the backend}
  @begin{short}
    The accessor for the @slot[g:settings]{path} slot of the @class{g:settings}
    class gets the path within the backend where the settings are stored.
  @end{short}
  @see-class{g:settings}")

;;; --- g:settings-schema ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "schema" 'settings) t)
 "The @code{schema} property of type @code{:string} (Read) @br{}
  The name of the schema that describes the types of keys for this
  @class{g:settings} object. Deprecated 2.32")

#+liber-documentation
(setf (liber:alias-for-function 'settings-schema)
      "Accessor"
      (documentation 'settings-schema 'function)
 "@version{2025-12-25}
  @syntax{(g:settings-schema object) => schema}
  @argument[object]{a @class{g:settings} object}
  @argument[schema]{a string for the name of the schema}
  @begin{short}
    The accessor for the @slot[g:settings]{schema} slot of the
    @class{g:settings} class gets the name of the schema that describes the
    types of keys for @arg{object}.
  @end{short}

  Deprecated 2.23
  @see-class{g:settings}")

;;; --- g:settings-schema-id ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "schema-id" 'settings) t)
 "The @code{schema-id} property of type @code{:string} (Read) @br{}
  The name of the schema that describes the types of keys for this
  @class{g:settings} object.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-schema-id)
      "Accessor"
      (documentation 'settings-schema-id 'function)
 "@version{2025-12-25}
  @syntax{(g:settings-schema-id object) => ID}
  @argument[object]{a @class{g:settings} object}
  @argument[schema]{a string for the name of the schema}
  @begin{short}
    The accessor for the @slot[g:settings]{schema-id} slot of the
    @class{g:settings} class gets the name of the schema that describes the
    types of keys for @arg{object}.
  @end{short}
  @see-class{g:settings}")

;;; --- g:settings-settings-schema ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "settings-schema" 'settings) t)
 "The @code{settings-schema} property of type @code{:string} (Read) @br{}
  The @class{g:settings-schema} instance describing the types of keys for this
  @class{g:settings} object.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-settings-schema)
      "Accessor"
      (documentation 'settings-settings-schema 'function)
 "@version{2025-12-25}
  @syntax{(g:settings-settings-schema object) => schema}
  @argument[object]{a @class{g:settings} object}
  @argument[schema]{a @class{g:settings-schema} instance}
  @begin{short}
    The accessor for the @slot[g:settings]{settings-schema} slot of the
    @class{g:settings} class gets the @class{g:settings-schema} instance
    describing the types of keys for @arg{object}.
  @end{short}
  @see-class{g:settings}")

;;; ----------------------------------------------------------------------------
;;; g_settings_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_new" settings-new) (gobject:object settings :return)
 #+liber-documentation
 "@version{2025-12-29}
  @argument[schema-id]{a string for the ID of the schema}
  @begin{short}
    Creates a new @class{g:settings} object with the schema specified by
    @arg{schema-id}.
  @end{short}
  It is an error for the schema to not exist. Schemas are an essential part of
  a program, as they provide type information. If schemas need to be dynamically
  loaded, for example, from an optional runtime dependency, the
  @fun{g:settings-schema-source-lookup} function can be used to test for their
  existence before loading them.

  Signals on the newly created @class{g:settings} object will be dispatched via
  the thread-default @code{GMainContext} in effect at the time of the call to
  the @fun{g:settings-new} function. The new @class{g:settings} object will hold
  a reference on the context.
  @see-class{g:settings}"
  (schema-id :string))

(export 'settings-new)

;;; ----------------------------------------------------------------------------
;;; g_settings_new_full
;;;
;;; Creates a new GSettings object with a given schema, backend and path.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_new_with_backend
;;;
;;; Creates a new GSettings object with the schema specified by schema_id and a
;;; given GSettingsBackend.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_new_with_backend_and_path
;;;
;;; Creates a new GSettings object with the schema specified by schema_id and a
;;; given GSettingsBackend and path.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_new_with_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_new_with_path" settings-new-with-path)
    (gobject:object settings :return)
 #+liber-documentation
 "@version{#2025-12-25}
  @argument[schema-id]{a string for the ID of the schema}
  @argument[path]{a string for the path to use}
  @begin{short}
    Creates a new @class{g:settings} object with the relocatable schema
    specified by @arg{schema-id} and a given @arg{path}.
  @end{short}
  You only need to do this if you want to directly create a settings object with
  a schema that does not have a specified path of its own. That is quite rare.

  It is a programmer error to call this function for a schema that has an
  explicitly specified path. It is a programmer error if path is not a valid
  path. A valid path begins and ends with / and does not contain two consecutive
  / characters.
  @see-class{g:settings}"
  (schema-id :string)
  (path :string))

(export 'settings-new-with-path)

;;; ----------------------------------------------------------------------------
;;; g_settings_list_schemas
;;;
;;; Deprecated 2.40
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_list_schemas" settings-list-schemas) glib:strv-t)

(export 'settings-list-schemas)

;;; ----------------------------------------------------------------------------
;;; g_settings_list_relocatable_schemas
;;;
;;; Deprecated 2.40
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_list_relocatable_schemas"
               settings-list-relocatable-schemas) glib:strv-t)

(export 'settings-list-relocatable-schemas)

;;; ----------------------------------------------------------------------------
;;; g_settings_sync
;;;
;;; Ensures that all pending operations are complete for the default backend.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_sync" settings-sync) :void)

(export 'settings-sync)

;;; ----------------------------------------------------------------------------
;;; g_settings_apply
;;;
;;; Applies any changes that have been made to the settings. This function does
;;; nothing unless settings is in ‘delay-apply’ mode; see g_settings_delay().
;;; In the normal case settings are always applied immediately.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_apply" settings-apply) :void
  (settings (gobject:object settings)))

(export 'settings-apply)

;;; ----------------------------------------------------------------------------
;;; g_settings_bind
;;;
;;; Create a binding between the key in the settings object and the property
;;; property of object.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_bind" settings-bind) :void
  (settings (gobject:object settings))
  (key :string)
  (object gobject:object)
  (property :string)
  (flags settings-bind-flags))

(export 'settings-bind)

;;; ----------------------------------------------------------------------------
;;; GSettingsBindGetMapping
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcallback settings-bind-get-mapping :boolean
    ((value (:pointer (:struct gobject:value)))
     (variant (:pointer (:struct glib:variant)))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (declare (type function func))
    (unwind-protect
      (funcall func value variant)
      (glib:free-stable-pointer data))))

#+nil
(export 'settings-bind-get-mapping)

;;; ----------------------------------------------------------------------------
;;; GSettingsBindSetMapping
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcallback settings-bind-set-mapping (:pointer (:struct glib:variant))
    ((value (:pointer (:struct gobject:value)))
     (vtype (glib:boxed glib:variant-type))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (declare (type function func))
    (unwind-protect
      (funcall func value vtype)
      (glib:free-stable-pointer data))))

#+nil
(export 'settings-bind-get-mapping)

;;; ----------------------------------------------------------------------------
;;; g_settings_bind_with_mapping_closures
;;;
;;; Version of g_settings_bind_with_mapping() using closures instead of
;;; callbacks for easier binding in other languages.
;;;
;;; since: 2.82
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; g_settings_bind_with_mapping
;;;
;;; Create a binding between the key in the settings object and the property
;;; property of object.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_bind_with_mapping_closures"
               %settings-bind-with-mapping-closures) :void
  (settings (gobject:object settings))
  (key :string)
  (object :pointer)
  (property :string)
  (flags settings-bind-flags)
  (get-mapping :pointer)
  (set-mapping :pointer))

(defun settings-bind-with-mapping
       (settings key object property flags get-mapping set-mapping)
  (let ((object (gobject:object-pointer object)))
    (%settings-bind-with-mapping-closures
            settings
            key
            object
            property
            flags
            (if get-mapping
                (gobject:create-closure-for-instance object get-mapping)
                (cffi:null-pointer))
            (if set-mapping
                (gobject:create-closure-for-instance object set-mapping)
                (cffi:null-pointer)))))

(export 'settings-bind-with-mapping)

;;; ----------------------------------------------------------------------------
;;; g_settings_bind_writable
;;;
;;; Create a binding between the writability of key in the settings object and
;;; the property property of object. The property must be boolean; “sensitive”
;;; or “visible” properties of widgets are the most likely candidates.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_bind_writable" settings-bind-writable) :void
  (settings (gobject:object settings))
  (key :string)
  (object gobject:object)
  (property :string)
  (inverted :boolean))

(export 'settings-bind-writable)

;;; ----------------------------------------------------------------------------
;;; g_settings_unbind
;;;
;;; Removes an existing binding for property on object.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_unbind" settings-unbind) :void
  (object gobject:object)
  (property :string))

(export 'settings-unbind)

;;; ----------------------------------------------------------------------------
;;; g_settings_create_action
;;;
;;; Creates a GAction corresponding to a given GSettings key.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_create_action" settings-create-action)
    (gobject:object action)
  (settings (gobject:object settings))
  (key :string))

(export 'settings-create-action)

;;; ----------------------------------------------------------------------------
;;; g_settings_is_writable
;;;
;;; Finds out if a key can be written or not.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_is_writable" settings-is-writable) :boolean
  (settings (gobject:object settings))
  (name :string))

(export 'settings-is-writable)


;;; ----------------------------------------------------------------------------
;;; g_settings_list_keys
;;;
;;; Introspects the list of keys on settings.
;;;
;;; deprecated: 2.46
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_list_keys" settings-list-keys)
    (glib:strv-t :free-from-foreign t)
  (settings (gobject:object settings)))

(export 'settings-list-keys)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_has_unapplied                            Accessor
;;;
;;; Returns whether the GSettings object has any unapplied changes. This can
;;; only be the case if it is in ‘delayed-apply’ mode.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_get_mapped
;;;
;;; Gets the value that is stored at key in settings, subject to
;;; application-level validation/mapping.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_get_range
;;;
;;; Queries the range of a key.
;;;
;;; deprecated: 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_range_check
;;;
;;; Checks if the given value is of the correct type and within the permitted
;;; range for key.
;;;
;;; deprecated: 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_reset
;;;
;;; Resets key to its default value.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_reset" settings-reset) :void
  (settings (gobject:object settings))
  (key :string))

(export 'settings-reset)

;;; ----------------------------------------------------------------------------
;;; g_settings_revert
;;;
;;; Reverts all non-applied changes to the settings. This function does nothing
;;; unless settings is in ‘delay-apply’ mode; see g_settings_delay(). In the
;;; normal case settings are always applied immediately.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_revert" settings-revert) :void
  (settings (gobject:object settings)))

(export 'settings-revert)

;;; ----------------------------------------------------------------------------
;;; g_settings_delay
;;;
;;; Changes the GSettings object into ‘delay-apply’ mode. In this mode, changes
;;; to settings are not immediately propagated to the backend, but kept locally
;;; until g_settings_apply() is called.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_delay" settings-delay) :void
  (settings (gobject:object settings)))

(export 'settings-delay)

;;; ----------------------------------------------------------------------------
;;; g_settings_list_children
;;;
;;; Gets the list of children on settings.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_list_children" settings-list-children)
    (glib:strv-t :free-from-foreign t)
  (settings (gobject:object settings)))

(export 'settings-list-children)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_child
;;;
;;; Creates a child settings object which has a base path of base-path/name`,
;;; wherebase-pathis the base path ofsettings`.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_get_child" settings-child) (gobject:object settings)
  (settings (gobject:object settings))
  (name :string))

(export 'settings-child)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_value
;;; g_settings_set_value
;;;
;;; Gets the value that is stored in settings for key.
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

(defun (setf settings-value) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_value"
                              (gobject:object settings) settings
                              :string key
                              (:pointer (:struct glib:variant)) value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_value" settings-value)
    (:pointer (:struct glib:variant))
  (settings (gobject:object settings))
  (key :string))

(export 'settings-value)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_default_value
;;;
;;; Gets the “default value” of a key.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_get_default_value" settings-default-value)
    (:pointer (:struct glib:variant))
  (settings (gobject:object settings))
  (key :string))

(export 'settings-default-value)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_user_value
;;;
;;; Checks the “user value” of a key, if there is one.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_get_user_value" settings-user-value)
    (:pointer (:struct glib:variant))
  (settings (gobject:object settings))
  (key :string))

(export 'settings-user-value)

;;; ----------------------------------------------------------------------------
;;; g_settings_get
;;; g_settings_set
;;;
;;; Gets the value that is stored at key in settings.
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_get_boolean
;;; g_settings_set_boolean
;;;
;;; Gets the value that is stored at key in settings.
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

(defun (setf settings-boolean) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_boolean"
                              (gobject:object settings) settings
                              :string key
                              :boolean value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_boolean" settings-boolean) :boolean
  (settings (gobject:object settings))
  (key :string))

(export 'settings-boolean)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_int
;;; g_settings_set_int
;;;
;;; Gets the value that is stored at key in settings.
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

(defun (setf settings-int) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_int"
                              (gobject:object settings) settings
                              :string key
                              :int value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_int" settings-int) :int
  (settings (gobject:object settings))
  (key :string))

(export 'settings-int)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_int64
;;; g_settings_set_int64
;;;
;;; Gets the value that is stored at key in settings.
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

(defun (setf settings-int64) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_int64"
                              (gobject:object settings) settings
                              :string key
                              :int64 value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_int64" settings-int64) :int64
  (settings (gobject:object settings))
  (key :string))

(export 'settings-int64)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_uint
;;; g_settings_set_uint
;;;
;;; Gets the value that is stored at key in settings.
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

(defun (setf settings-uint) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_uint"
                              (gobject:object settings) settings
                              :string key
                              :uint value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_uint" settings-uint) :uint
  (settings (gobject:object settings))
  (key :string))

(export 'settings-uint)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_uint64
;;; g_settings_set_uint64
;;;
;;; Gets the value that is stored at key in settings.
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

(defun (setf settings-uint64) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_uint64"
                              (gobject:object settings) settings
                              :string key
                              :uint64 value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_uint64" settings-uint64) :uint64
  (settings (gobject:object settings))
  (key :string))

(export 'settings-uint64)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_double
;;; g_settings_set_double
;;;
;;; Gets the value that is stored at key in settings.
;;;
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

(defun (setf settings-double) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_double"
                              (gobject:object settings) settings
                              :string key
                              :double value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_double" settings-double) :double
  (settings (gobject:object settings))
  (key :string))

(export 'settings-double)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_string
;;; g_settings_set_string
;;;
;;; Gets the value that is stored at key in settings.
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

(defun (setf settings-string) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_string"
                              (gobject:object settings) settings
                              :string key
                              :string value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_string" settings-string) :string
  (settings (gobject:object settings))
  (key :string))

(export 'settings-string)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_strv
;;; g_settings_set_strv
;;;
;;; A convenience variant of g_settings_get() for string arrays.
;;; Sets key in settings to value.
;;; ----------------------------------------------------------------------------

(defun (setf settings-strv) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_strv"
                              (gobject:object settings) settings
                              :string key
                              glib:strv-t value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_strv" settings-strv)
    (glib:strv-t :free-from-foreign t)
  (settings (gobject:object settings))
  (key :string))

(export 'settings-strv)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_enum
;;; g_settings_set_enum
;;;
;;; Gets the value that is stored in settings for key and converts it to the
;;; enum value that it represents.
;;;
;;; Looks up the enumerated type nick for value and writes it to key, within
;;; settings.
;;; ----------------------------------------------------------------------------

(defun (setf settings-enum) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_enum"
                              (gobject:object settings) settings
                              :string key
                              :int value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_enum" settings-enum) :int
  (settings (gobject:object settings))
  (key :string))

(export 'settings-enum)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_flags
;;; g_settings_set_flags
;;;
;;; Gets the value that is stored in settings for key and converts it to the
;;; flags value that it represents.
;;;
;;; Looks up the flags type nicks for the bits specified by value, puts them in
;;; an array of strings and writes the array to key, within settings.
;;; ----------------------------------------------------------------------------

(defun (setf settings-flags) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_flags"
                              (gobject:object settings) settings
                              :string key
                              :uint value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_flags" settings-flags) :uint
  (settings (gobject:object settings))
  (key :string))

(export 'settings-flags)

;;; --- End of file gio.settings.lisp ------------------------------------------
