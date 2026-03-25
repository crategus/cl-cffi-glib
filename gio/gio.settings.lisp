;;; ----------------------------------------------------------------------------
;;; gio.settings.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.88 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2025 - 2026 Dieter Kaiser
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
;;;     GSettingsBindFlags
;;;
;;; Functions
;;;
;;;     g_settings_new
;;;     g_settings_new_full
;;;     g_settings_new_with_backend
;;;     g_settings_new_with_backend_and_path
;;;     g_settings_new_with_path
;;;
;;;     g_settings_list_schemas                             Deprecated 2.40
;;;     g_settings_list_relocatable_schemas                 Deprecated 2.40
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
;;;     g_settings_list_keys                                Deprecated 2.46

;;;     g_settings_reset
;;;     g_settings_revert
;;;
;;;     g_settings_list_children
;;;     g_settings_get_child
;;;     g_settings_get_has_unapplied                        Accessor
;;;     g_settings_get_mapped                               not implemented
;;;     g_settings_get_range                                Deprecated 2.40
;;;     g_settings_range_check                              Deprecated 2.40

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
 "@version{2026-03-25}
  @argument[id]{a string for the ID of the schema}
  @begin{short}
    Creates a new @class{g:settings} object with the schema specified by
    @arg{id}.
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
  @see-class{g:settings}
  @see-function{g:settings-schema-source-lookup}"
  (id :string))

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
 "@version{#2026-03-25}
  @argument[id]{a string for the ID of the schema}
  @argument[path]{a string for the path to use}
  @begin{short}
    Creates a new @class{g:settings} object with the relocatable schema
    specified by @arg{id} and a given @arg{path}.
  @end{short}
  You only need to do this if you want to directly create a settings object with
  a schema that does not have a specified path of its own. That is quite rare.

  It is a programmer error to call this function for a schema that has an
  explicitly specified path. It is a programmer error if path is not a valid
  path. A valid path begins and ends with / and does not contain two consecutive
  / characters.
  @see-class{g:settings}"
  (id :string)
  (path :string))

(export 'settings-new-with-path)

;;; ----------------------------------------------------------------------------
;;; g_settings_list_schemas                                 Deprecated 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_list_relocatable_schemas                     Deprecated 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_sync
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_sync" settings-sync) :void
 #+liber-documentation
 "@version{#2026-03-25}
  @begin{short}
    Ensures that all pending operations are complete for the default backend.
  @end{short}
  Writes made to a @class{g:settings} object are handled asynchronously. For
  this reason, it is very unlikely that the changes have it to disk by the time
  the @setf{g:settings-value} function returns.

  This call will block until all of the writes have made it to the backend.
  Since the main loop is not running, no change notifications will be dispatched
  during this call, but some may be queued by the time the call is done.
  @see-class{g:settings}
  @see-function{g:settings-value}")

(export 'settings-sync)

;;; ----------------------------------------------------------------------------
;;; g_settings_apply
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_apply" settings-apply) :void
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} object}
  @begin{short}
    Applies any changes that have been made to the settings.
  @end{short}
  This function does nothing unless @arg{settings} is in \"delay-apply\" mode.
  In the normal case settings are always applied immediately.
  @see-class{g:settings}
  @see-function{g:settings-delay}"
  (settings (gobject:object settings)))

(export 'settings-apply)

;;; ----------------------------------------------------------------------------
;;; g_settings_delay
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_delay" settings-delay) :void
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} object}
  @begin{short}
    Changes the @class{g:settings} object into \"delay-apply\" mode.
  @end{short}
  In this mode, changes to settings are not immediately propagated to the
  backend, but kept locally until the @fun{g:settings-apply} function is called.
  @see-class{g:settings}
  @see-function{g:settings-apply}"
  (settings (gobject:object settings)))

(export 'settings-delay)

;;; ----------------------------------------------------------------------------
;;; g_settings_bind
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_bind" settings-bind) :void
 #+liber-documentation
  "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to bind}
  @argument[object]{a @class{g:object} object for the object with the property
    to bind}
  @argument[property]{a string for the name of the property to bind}
  @argument[flags]{a @symbol{g:settings-bind-flags} value for the binding}
  @begin{short}
    Create a binding between @arg{key} in the settings object and the property
    @arg{property} of @arg{object}.
  @end{short}
  The binding uses the default GIO mapping functions to map between the settings
  and property values. These functions handle booleans, numeric types and string
  types in a straightforward way. Use the @fun{g:settings-bind-with-mapping}
  function if you need a custom mapping, or map between types that are not
  supported by the default mapping functions.

  Unless the flags include @val[g:settings-bind-flags]{no-sensitivity}, this
  function also establishes a binding between the writability of key and the
  sensitive property of object, if @arg{object} has a boolean property by that
  name. See the @fun{g:settings-bind-writable} function for more details about
  writable bindings.

  Note that the lifecycle of the binding is tied to @arg{object}, and that you
  can have only one binding per object property. If you bind the same property
  twice on the same object, the second binding overrides the first one.
  @see-class{g:settings}
  @see-symbol{g:settings-bind-flags}
  @see-function{g:settings-bind-with-mapping}
  @see-function{g:settings-bind-writable}"
  (settings (gobject:object settings))
  (key :string)
  (object gobject:object)
  (property :string)
  (flags settings-bind-flags))

(export 'settings-bind)

;;; ----------------------------------------------------------------------------
;;; GSettingsBindGetMapping
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-symbol 'settings-bind-get-mapping)
      "Callback"
      (liber:symbol-documentation 'settings-bind-get-mapping)
 "@version{#2026-03-25}
  @begin{declaration}
lambda (value variant) => result
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[value]{a @symbol{g:value} instance for the property value}
      @entry[variant]{a @symbol{g:variant} instance to map to the property
        value}
      @entry[result]{@em{True} if the conversion succeeded, @em{false} in case
        of an error.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The type for the callback function that is used to convert from the
    @class{g:settings} object to an object property with the
    @fun{g:settings-bind-with-mapping} function.
  @end{short}
  The value is already initialized to hold values of the appropriate type.
  @see-class{g:settings}
  @see-symbol{g:value}
  @see-symbol{g:variant}
  @see-function{g:settings-bind-with-mapping}")

(export 'settings-bind-get-mapping)

;;; ----------------------------------------------------------------------------
;;; GSettingsBindSetMapping
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-symbol 'settings-bind-set-mapping)
      "Callback"
      (liber:symbol-documentation 'settings-bind-set-mapping)
 "@version{#2026-03-25}
  @begin{declaration}
lambda (value vtype) => result
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[value]{a @symbol{g:value} instance for the property value}
      @entry[vtype]{a @class{g:variant-type} instance for the expected type
        of the result}
      @entry[result]{The new @symbol{g:variant} instance holding the date from
        @arg{value}, or @code{nil} in case of an error.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The type for the callback function that is used to convert from the
    @class{g:settings} object to an object property with the
    @fun{g:settings-bind-with-mapping} function.
  @end{short}
  The value is already initialized to hold values of the appropriate type.
  @see-class{g:settings}
  @see-symbol{g:value}
  @see-symbol{g:variant}
  @see-function{g:settings-bind-with-mapping}")

(export 'settings-bind-set-mapping)

;;; ----------------------------------------------------------------------------
;;; g_settings_bind_with_mapping
;;; ----------------------------------------------------------------------------

;; TODO: This implementation requieres GLIB 2.82. The variant without closures
;; is not implemented.

#+glib-2-82
(cffi:defcfun ("g_settings_bind_with_mapping_closures"
               %settings-bind-with-mapping-closures) :void
  (settings (gobject:object settings))
  (key :string)
  (object :pointer)
  (property :string)
  (flags settings-bind-flags)
  (get-mapping :pointer)
  (set-mapping :pointer))

#+glib-2-82
(defun settings-bind-with-mapping
       (settings key object property flags get-mapping set-mapping)
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to bind}
  @argument[object]{a @class{g:object} object with the property to bind}
  @argument[property]{a string for the name of the property to bind}
  @argument[flags]{a @symbol{g:settings-bind-flags} value for the binding}
  @argument[get-mapping]{a @symbol{g:settings-bind-get-mapping} callback
    function that gets called to convert values from @arg{settings} to
    @arg{object}, or @code{nil} to use the default GIO mapping}
  @argument[set-mapping]{a @symbol{g:settings-bind-set-mapping} callback
    function that gets called to convert values from @arg{object} to
    @arg{settings}, or @code{nil} to use the default GIO mapping}
  @begin{short}
    Create a binding between @arg{key} in the settings object and the property
    @arg{property} of @arg{object}.
  @end{short}
  The binding uses the provided mapping functions to map between settings and
  property values.

  Note that the lifecycle of the binding is tied to @arg{object}, and that you
  can have only one binding per object property. If you bind the same property
  twice on the same object, the second binding overrides the first one.
  @begin[Notes]{dictionary}
    This function is available since GIO 2.82. It uses closures to implement
    the mapping. This is not available in ealier version of GIO.
  @end{dictionary}
  @see-class{g:settings}"
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

#+glib-2-82
(export 'settings-bind-with-mapping)

;;; ----------------------------------------------------------------------------
;;; g_settings_bind_writable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_bind_writable" settings-bind-writable) :void
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to bind}
  @argument[object]{a @class{g:object} object with the property to bind}
  @argument[property]{a string for the name of the boolean property to bind}
  @argument[inverted]{a boolean whether to invert the value}
  @begin{short}
    Create a binding between the writability of @arg{key} in the settings object
    and the property property of @arg{object}.
  @end{short}
  The property must be boolean. The @code{\"sensitive\"} or @code{\"visible\"}
  properties of widgets are the most likely candidates.

  Writable bindings are always uni-directional. Changes of the writability of
  the setting will be propagated to the object property, not the other way.

  When the inverted argument is @em{true}, the binding inverts the value as it
  passes from the setting to the object, that is, @arg{property} will be set to
  @em{true} if the key is not writable.

  Note that the lifecycle of the binding is tied to @arg{object}, and that you
  can have only one binding per object property. If you bind the same property
  twice on the same object, the second binding overrides the first one.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string)
  (object gobject:object)
  (property :string)
  (inverted :boolean))

(export 'settings-bind-writable)

;;; ----------------------------------------------------------------------------
;;; g_settings_unbind
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_unbind" settings-unbind) :void
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[object]{a @class{g:object} object with the property to unbind}
  @argument[property]{a string for the property whose binding is removed}
  @begin{short}
    Removes an existing binding for @arg{property} on @arg{object}.
  @end{short}
  Note that bindings are automatically removed when the object is finalized, so
  it is rarely necessary to call this function.
  @see-class{g:settings}"
  (object gobject:object)
  (property :string))

(export 'settings-unbind)

;;; ----------------------------------------------------------------------------
;;; g_settings_create_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_create_action" settings-create-action)
    (gobject:object action)
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} instance}
  @argument[key]{a string for the name of a key in @arg{settings}}
  @return{The new @class{g:action} instance.}
  @begin{short}
    Creates a @class{g:action} instance corresponding to a given @arg{key}.
  @end{short}
  The action has the same name as the key.

  The value of the key becomes the state of the action and the action is enabled
  when the key is writable. Changing the state of the action results in the key
  being written to. Changes to the value or writability of the key cause
  appropriate change notifications to be emitted for the action.

  For boolean valued keys, action activations take no parameter and result in
  the toggling of the value. For all other types, activations take the new value
  for the key, which must have the correct type.
  @see-class{g:settings}
  @see-class{g:action}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-create-action)

;;; ----------------------------------------------------------------------------
;;; g_settings_is_writable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_is_writable" settings-is-writable) :boolean
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} instance}
  @argument[key]{a string for the name of a key in @arg{settings}}
  @return{The boolean whether @arg{key} is writable.}
  @begin{short}
    Finds out if a key can be written.
  @end{short}
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-is-writable)

;;; ----------------------------------------------------------------------------
;;; g_settings_list_keys                                    Deprecated 2.46
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_get_has_unapplied                            Accessor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_get_mapped
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_get_range                                    Deprecated 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_range_check                                  Deprecated 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_reset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_reset" settings-reset) :void
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} instance}
  @argument[key]{a string for the name of a key in @arg{settings}}
  @begin{short}
    Resets @arg{key} to its default value.
  @end{short}
  This call resets the key, as much as possible, to its default value. That
  might be the value specified in the schema or the one set by the
  administrator.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-reset)

;;; ----------------------------------------------------------------------------
;;; g_settings_revert
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_revert" settings-revert) :void
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} instance}
  @begin{short}
    Reverts all unapplied changes to the settings.
  @end{short}
  This function does nothing unless settings is in \"delay-apply\" mode. In the
  normal case settings are always applied immediately.

  Change notifications will be emitted for affected keys.
  @see-class{g:settings}"
  (settings (gobject:object settings)))

(export 'settings-revert)

;;; ----------------------------------------------------------------------------
;;; g_settings_list_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_list_children" settings-list-children)
    (glib:strv-t :free-from-foreign t)
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} instance}
  @begin{short}
    Gets the list of children on @arg{settings}.
  @end{short}
  The list is exactly the list of strings for which it is not an error to call
  the @fun{g:settings-child} function.

  There is little reason to call this function from \"normal\" code, since you
  should already know what children are in your schema. This function may still
  be useful there for introspection reasons, however.
  @see-class{g:settings}
  @see-function{g:settings-child}"
  (settings (gobject:object settings)))

(export 'settings-list-children)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_get_child" settings-child)
    (gobject:object settings :return)
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} instance}
  @argument[name]{a string for the name of the child schema}
  @return{The new @class{g:settings} instance for the chils settings object.}
  @begin{short}
    Creates a child settings object which has a base path of
    @code{base-path/name}, where @code{base-path} is the base path of settings
    and @code{name} is as specified by the caller.
  @end{short}
  The schema for the child settings object must have been declared in the schema
  of @arg{settings} using a @code{<child>} element.

  The created child settings object will inherit the \"delay-apply\" mode from
  @arg{settings}.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (name :string))

(export 'settings-child)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_value
;;; g_settings_set_value
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
 #+liber-documentation
 "@version{#2026-03-25}
  @syntax{(g:settings-value settings key) => value}
  @syntax{(setf (g:settings-value settings key) value)}
  @argument[settings]{a @class{g:settings} instance}
  @argument[key]{a string for the name of a key in @arg{settings}}
  @argument[value]{a @symbol{g:variant} instance of the correct type}
  @begin{short}
    Gets or sets the value that is stored in @arg{settings} for @arg{key}.
  @end{short}
  It is a programmer error to give a key that is not contained in the schema
  for @arg{settings}.
  @see-class{g:settings}
  @see-symbol{g:variant}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-value)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_default_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_get_default_value" settings-default-value)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} instance}
  @argument[key]{a string for the name of a key in @arg{settings}}
  @return{The new @symbol{g:variant} instance for the default value.}
  @begin{short}
    Gets the default value of a key.
  @end{short}
  This is the value that would be read if the @fun{g:settings-reset} function
  were to be called on the key.

  Note that this may be a different value than returned by the
  @fun{g:settings-schema-key-default-value} function if the system administrator
  has provided a default value.

  Comparing the return values of the @fun{g:settings-default-value} function and
  the @fun{g:settings-value} function is not sufficient for determining if a
  value has been set because the user may have explicitly set the value to
  something that happens to be equal to the default. The difference here is that
  if the default changes in the future, the user’s key will still be set.

  This function may be useful for adding an indication to a UI of what the
  default value was before the user set it.

  It is a programmer error to give a key that is not contained in the schema
  for @arg{settings}.
  @see-class{g:settings}
  @see-symbol{g:variant}
  @see-function{g:settings-reset}
  @see-function{g:settings-value}
  @see-function{g:settings-schema-key-default-value}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-default-value)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_user_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_get_user_value" settings-user-value)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{#2026-03-25}
  @argument[settings]{a @class{g:settings} instance}
  @argument[key]{a string for the name of a key in @arg{settings}}
  @return{The new @symbol{g:variant} instance for the user value, if set.}
  @begin{short}
    Checks the user value of a key, if there is one.
  @end{short}
  The user value of a key is the last value that was set by the user.

  After calling the @fun{g:settings-reset} function this function should always
  return @code{nil}, assuming something is not wrong with the system
  configuration.

  It is possible that the @fun{g:settings-value} function will return a
  different value than this function. This can happen in the case that the user
  set a value for a key that was subsequently locked down by the system
  administrator — this function will return the user’s old value.

  This function may be useful for adding a \"reset\" option to a UI or for
  providing indication that a particular value has been changed.

  It is a programmer error to give a key that is not contained in the schema
  for @arg{settings}.
  @see-class{g:settings}
  @see-symbol{g:variant}
  @see-function{g:settings-reset}
  @see-function{g:settings-value}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-user-value)

;;; ----------------------------------------------------------------------------
;;; g_settings_get
;;; g_settings_set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_settings_get_boolean
;;; g_settings_set_boolean
;;; ----------------------------------------------------------------------------

(defun (setf settings-boolean) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_boolean"
                              (gobject:object settings) settings
                              :string key
                              :boolean value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_boolean" settings-boolean) :boolean
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-boolean settings) => value}
  @syntax{(setf (g:settings-boolean settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{a boolean for the value}
  @begin{short}
    Gets or sets the value that is stored at @arg{key} in @arg{settings}.
  @end{short}
  It is a programmer error to give a key that is not specified as having an
  \"b\" type in the schema for settings.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-boolean)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_int
;;; g_settings_set_int
;;; ----------------------------------------------------------------------------

(defun (setf settings-int) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_int"
                              (gobject:object settings) settings
                              :string key
                              :int value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_int" settings-int) :int
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-int settings) => value}
  @syntax{(setf (g:settings-int settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{an integer for the value}
  @begin{short}
    Gets or sets the value that is stored at @arg{key} in @arg{settings}.
  @end{short}
  It is a programmer error to give a key that is not specified as having an
  \"i\" type in the schema for settings.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-int)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_int64
;;; g_settings_set_int64
;;; ----------------------------------------------------------------------------

(defun (setf settings-int64) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_int64"
                              (gobject:object settings) settings
                              :string key
                              :int64 value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_int64" settings-int64) :int64
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-int64 settings) => value}
  @syntax{(setf (g:settings-int64 settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{an unsigned 64-bit integer for the value}
  @begin{short}
    Gets or sets the value that is stored at @arg{key} in @arg{settings}.
  @end{short}
  It is a programmer error to give a key that is not specified as having an
  \"x\" type in the schema for settings.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-int64)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_uint
;;; g_settings_set_uint
;;; ----------------------------------------------------------------------------

(defun (setf settings-uint) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_uint"
                              (gobject:object settings) settings
                              :string key
                              :uint value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_uint" settings-uint) :uint
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-uint settings) => value}
  @syntax{(setf (g:settings-uint settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{an unsigned integer for the value}
  @begin{short}
    Gets or sets the value that is stored at @arg{key} in @arg{settings}.
  @end{short}
  It is a programmer error to give a key that is not specified as having an
  \"u\" type in the schema for settings.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-uint)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_uint64
;;; g_settings_set_uint64
;;; ----------------------------------------------------------------------------

(defun (setf settings-uint64) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_uint64"
                              (gobject:object settings) settings
                              :string key
                              :uint64 value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_uint64" settings-uint64) :uint64
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-unit64 settings) => value}
  @syntax{(setf (g:settings-uint64 settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{an unsigned 64-bit integer for the value}
  @begin{short}
    Gets or sets the value that is stored at @arg{key} in @arg{settings}.
  @end{short}
  It is a programmer error to give a key that is not specified as having an
  @code{\"t\"} type in the schema for settings.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-uint64)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_double
;;; g_settings_set_double
;;; ----------------------------------------------------------------------------

(defun (setf settings-double) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_double"
                              (gobject:object settings) settings
                              :string key
                              :double value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_double" settings-double) :double
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-double settings) => value}
  @syntax{(setf (g:settings-double settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{a number coerced to a double float for the value}
  @begin{short}
    Gets or sets the value that is stored at @arg{key} in @arg{settings}.
  @end{short}
  It is a programmer error to give a key that is not specified as having an
  @code{\"d\"} type in the schema for settings.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-double)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_string
;;; g_settings_set_string
;;; ----------------------------------------------------------------------------

(defun (setf settings-string) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_string"
                              (gobject:object settings) settings
                              :string key
                              :string value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_string" settings-string) :string
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-string settings) => value}
  @syntax{(setf (g:settings-string settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{a string for the value}
  @begin{short}
    Gets or sets the value that is stored at @arg{key} in @arg{settings}.
  @end{short}
  It is a programmer error to give a key that is not specified as having an
  @code{\"s\"} type in the schema for settings.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-string)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_strv
;;; g_settings_set_strv
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
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-strv settings) => value}
  @syntax{(setf (g:settings-strv settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{a list of strings that is stored at @arg{key} in
    @arg{settings}}
  @begin{short}
    Gets or sets the value that is stored at @arg{key} in @arg{settings}.
  @end{short}
  It is a programmer error to give a key that is not specified as having an
  @code{\"as\"} type in the schema for settings.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-strv)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_enum
;;; g_settings_set_enum
;;; ----------------------------------------------------------------------------

(defun (setf settings-enum) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_enum"
                              (gobject:object settings) settings
                              :string key
                              :int value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_enum" settings-enum) :int
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-enum settings) => value}
  @syntax{(setf (g:settings-enum settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{an integer for the value}
  @begin{short}
    Gets or sets the value that is stored in @arg{settings} for @arg{key} and
    converts it to the enum value that it represents.
  @end{short}
  In order to use this function the type of the value must be a string and it
  must be marked in the schema file as an enumerated type.

  It is a programmer error to give a key that is not contained in the schema
  for settings or is not marked as an enumerated type.

  If the value stored in the configuration database is not a valid value for
  the enumerated type then this function will return the default value.

  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-enum)

;;; ----------------------------------------------------------------------------
;;; g_settings_get_flags
;;; g_settings_set_flags
;;; ----------------------------------------------------------------------------

(defun (setf settings-flags) (value settings key)
  (when (cffi:foreign-funcall "g_settings_set_flags"
                              (gobject:object settings) settings
                              :string key
                              :uint value
                              :boolean)
    value))

(cffi:defcfun ("g_settings_get_flags" settings-flags) :uint
 #+liber-documentation
 "@version{#2026-03-24}
  @syntax{(g:settings-flags settings) => value}
  @syntax{(setf (g:settings-flags settings) value)}
  @argument[settings]{a @class{g:settings} object}
  @argument[key]{a string for the key to get the value for}
  @argument[value]{an unsigned integer for the value}
  @begin{short}
    Gets or sets the value that is stored in @arg{settings} for @arg{key} and
    converts it to the flags value that it represents.
  @end{short}
  In order to use this function the type of the value must be an array of
  strings and it must be marked in the schema file as a flags type.

  It is a programmer error to give a key that is not contained in the schema
  for settings or is not marked as a flags type.

  If the value stored in the configuration database is not a valid value for
  the flags type then this function will return the default value.
  @see-class{g:settings}"
  (settings (gobject:object settings))
  (key :string))

(export 'settings-flags)

;;; --- End of file gio.settings.lisp ------------------------------------------
