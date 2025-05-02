;;; ----------------------------------------------------------------------------
;;; gio.settings.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
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

;;; Types and Values
;;;
;;;     GSettings
;;;
;;; Accessors
;;;

;;; Functions
;;;
;;;     g_settings_new
;;;     g_settings_new_full
;;;     g_settings_new_with_backend
;;;     g_settings_new_with_backend_and_path
;;;     g_settings_new_with_path
;;;
;;;     g_settings_list_relocatable_schemas
;;;     g_settings_list_schemas
;;;     g_settings_sync
;;;     g_settings_unbind
;;;
;;;     g_settings_apply
;;;     g_settings_bind
;;;     g_settings_bind_with_mapping
;;;     g_settings_bind_with_mapping_closures
;;;     g_settings_bind_writable
;;;     g_settings_create_action
;;;     g_settings_delay
;;;     g_settings_get
;;;     g_settings_get_boolean
;;;     g_settings_get_child
;;;     g_settings_get_default_value
;;;     g_settings_get_double
;;;     g_settings_get_enum
;;;     g_settings_get_flags
;;;     g_settings_get_has_unapplied
;;;     g_settings_get_int
;;;     g_settings_get_int64
;;;     g_settings_get_mapped
;;;     g_settings_get_range
;;;     g_settings_get_string
;;;     g_settings_get_strv
;;;     g_settings_get_uint
;;;     g_settings_get_uint64
;;;     g_settings_get_user_value
;;;     g_settings_get_value
;;;     g_settings_is_writable
;;;     g_settings_list_children
;;;     g_settings_list_keys
;;;     g_settings_range_check
;;;     g_settings_reset
;;;     g_settings_revert
;;;     g_settings_set
;;;     g_settings_set_boolean
;;;     g_settings_set_double
;;;     g_settings_set_enum
;;;     g_settings_set_flags
;;;     g_settings_set_int
;;;     g_settings_set_int64
;;;     g_settings_set_string
;;;     g_settings_set_strv
;;;     g_settings_set_uint
;;;     g_settings_set_uint64
;;;     g_settings_set_value
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
 "@version{2025-3-12}
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

  When creating a GSettings instance, you have to specify a schema that describes the keys in your settings and their types and default values, as well as some other information.

Normally, a schema has a fixed path that determines where the settings are stored in the conceptual global tree of settings. However, schemas can also be ‘relocatable’, i.e. not equipped with a fixed path. This is useful e.g. when the schema describes an ‘account’, and you want to be able to store a arbitrary number of accounts.

Paths must start with and end with a forward slash character (/) and must not contain two sequential slash characters. Paths should be chosen based on a domain name associated with the program or library to which the settings belong. Examples of paths are /org/gtk/settings/file-chooser/ and /ca/desrt/dconf-editor/. Paths should not start with /apps/, /desktop/ or /system/ as they often did in GConf.

Unlike other configuration systems (like GConf), GSettings does not restrict keys to basic types like strings and numbers. GSettings stores values as GVariant, and allows any GVariantType for keys. Key names are restricted to lowercase characters, numbers and -. Furthermore, the names must begin with a lowercase character, must not end with a -, and must not contain consecutive dashes.

Similar to GConf, the default values in GSettings schemas can be localized, but the localized values are stored in gettext catalogs and looked up with the domain that is specified in the gettext-domain attribute of the <schemalist> or <schema> elements and the category that is specified in the l10n attribute of the <default> element. The string which is translated includes all text in the <default> element, including any surrounding quotation marks.

The l10n attribute must be set to messages or time, and sets the locale category for translation. The messages category should be used by default; use time for translatable date or time formats. A translation comment can be added as an XML comment immediately above the <default> element — it is recommended to add these comments to aid translators understand the meaning and implications of the default value. An optional translation context attribute can be set on the <default> element to disambiguate multiple defaults which use the same string.



")

#|

For example:

 <!-- Translators: A list of words which are not allowed to be typed, in
      GVariant serialization syntax.
      See: https://developer.gnome.org/glib/stable/gvariant-text.html -->
 <default l10n='messages' context='Banned words'>['bad', 'words']</default>
Copy
Translations of default values must remain syntactically valid serialized GVariants (e.g. retaining any surrounding quotation marks) or runtime errors will occur.

GSettings uses schemas in a compact binary form that is created by the glib-compile-schemas utility. The input is a schema description in an XML format.

A DTD for the gschema XML format can be found here: gschema.dtd

The glib-compile-schemas tool expects schema files to have the extension .gschema.xml.

At runtime, schemas are identified by their ID (as specified in the id attribute of the <schema> element). The convention for schema IDs is to use a dotted name, similar in style to a D-Bus bus name, e.g. org.gnome.SessionManager. In particular, if the settings are for a specific service that owns a D-Bus bus name, the D-Bus bus name and schema ID should match. For schemas which deal with settings not associated with one named application, the ID should not use StudlyCaps, e.g. org.gnome.font-rendering.

In addition to GVariant types, keys can have types that have enumerated types. These can be described by a <choice>, <enum> or <flags> element, as seen in the second example below. The underlying type of such a key is string, but you can use g_settings_get_enum(), g_settings_set_enum(), g_settings_get_flags(), g_settings_set_flags() access the numeric values corresponding to the string value of enum and flags keys.

An example for default value:

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
Copy
An example for ranges, choices and enumerated types:

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

Vendor overrides

Default values are defined in the schemas that get installed by an application.
Sometimes, it is necessary for a vendor or distributor to adjust these defaults.
Since patching the XML source for the schema is inconvenient and error-prone,
glib-compile-schemas reads so-called ‘vendor override’ files. These are keyfiles
in the same directory as the XML schema sources which can override default
values. The schema ID serves as the group name in the key file, and the values
are expected in serialized GVariant form, as in the following example:

[org.gtk.Example]
key1='string'
key2=1.5
Copy
glib-compile-schemas expects schema files to have the extension .gschema.override.

Binding
A very convenient feature of GSettings lets you bind GObject properties directly to settings, using g_settings_bind(). Once a GObject property has been bound to a setting, changes on either side are automatically propagated to the other side. GSettings handles details like mapping between GObject and GVariant types, and preventing infinite cycles.

This makes it very easy to hook up a preferences dialog to the underlying settings. To make this even more convenient, GSettings looks for a boolean property with the name sensitivity and automatically binds it to the writability of the bound setting. If this ‘magic’ gets in the way, it can be suppressed with the G_SETTINGS_BIND_NO_SENSITIVITY flag.

Relocatable schemas
A relocatable schema is one with no path attribute specified on its <schema> element. By using g_settings_new_with_path(), a GSettings object can be instantiated for a relocatable schema, assigning a path to the instance. Paths passed to g_settings_new_with_path() will typically be constructed dynamically from a constant prefix plus some form of instance identifier; but they must still be valid GSettings paths. Paths could also be constant and used with a globally installed schema originating from a dependency library.

For example, a relocatable schema could be used to store geometry information for different windows in an application. If the schema ID was org.foo.MyApp.Window, it could be instantiated for paths /org/foo/MyApp/main/, /org/foo/MyApp/document-1/, /org/foo/MyApp/document-2/, etc. If any of the paths are well-known they can be specified as <child> elements in the parent schema, e.g.:

<schema id=\"org.foo.MyApp\" path=\"/org/foo/MyApp/\">
  <child name=\"main\" schema=\"org.foo.MyApp.Window\"/>
</schema>
Copy
Build system integration
Meson
GSettings is natively supported by Meson’s GNOME module.

You can install the schemas as any other data file:

install_data(
  'org.foo.MyApp.gschema.xml',
  install_dir: get_option('datadir') / 'glib-2.0/schemas',
)
Copy
You can use gnome.post_install() function to compile the schemas on installation:

gnome = import('gnome')
gnome.post_install(
  glib_compile_schemas: true,
)
Copy
If an enumerated type defined in a C header file is to be used in a GSettings schema, it can either be defined manually using an <enum> element in the schema XML, or it can be extracted automatically from the C header. This approach is preferred, as it ensures the two representations are always synchronised. To do so, you will need to use the gnome.mkenums() function with the following templates:

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
Copy
It is recommended to validate your schemas as part of the test suite for your application:

test('validate-schema',
  find_program('glib-compile-schemas'),
  args: ['--strict', '--dry-run', meson.current_source_dir()],
)
Copy
If your application allows running uninstalled, you should also use the gnome.compile_schemas() function to compile the schemas in the current build directory:

gnome.compile_schemas()
Copy
Autotools
GSettings comes with autotools integration to simplify compiling and installing schemas. To add GSettings support to an application, add the following to your configure.ac:

GLIB_GSETTINGS
Copy
In the appropriate Makefile.am, use the following snippet to compile and install the named schema:

gsettings_SCHEMAS = org.foo.MyApp.gschema.xml
EXTRA_DIST = $(gsettings_SCHEMAS)

@GSETTINGS_RULES@
Copy
If an enumerated type defined in a C header file is to be used in a GSettings schema, it can either be defined manually using an <enum> element in the schema XML, or it can be extracted automatically from the C header. This approach is preferred, as it ensures the two representations are always synchronised. To do so, add the following to the relevant Makefile.am:

gsettings_ENUM_NAMESPACE = org.foo.MyApp
gsettings_ENUM_FILES = my-app-enums.h my-app-misc.h
Copy
gsettings_ENUM_NAMESPACE specifies the schema namespace for the enum files, which are specified in gsettings_ENUM_FILES. This will generate a org.foo.MyApp.enums.xml file containing the extracted enums, which will be automatically included in the schema compilation, install and uninstall rules. It should not be committed to version control or included in EXTRA_DIST.

Localization
No changes are needed to the build system to mark a schema XML file for translation. Assuming it sets the gettext-domain attribute, a schema may be marked for translation by adding it to POTFILES.in, assuming gettext 0.19 or newer is in use (the preferred method for translation):

data/org.foo.MyApp.gschema.xml
Copy
Alternatively, if intltool 0.50.1 is in use:

[type: gettext/gsettings]data/org.foo.MyApp.gschema.xml
Copy
GSettings will use gettext to look up translations for the <summary> and <description> elements, and also any <default> elements which have a l10n attribute set.

Translations must not be included in the .gschema.xml file by the build system, for example by using a rule to generate the XML file from a template.
|#

;;; ----------------------------------------------------------------------------
;;; g_settings_new
;;;
;;; Creates a new GSettings object with the schema specified by schema_id.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_new" settings-new) (gobject:object settings :return)
  (schema :string))

(export 'settings-new)

#|
g_settings_new_full
Creates a new GSettings object with a given schema, backend and path.

g_settings_new_with_backend
Creates a new GSettings object with the schema specified by schema_id and a given GSettingsBackend.

g_settings_new_with_backend_and_path
Creates a new GSettings object with the schema specified by schema_id and a given GSettingsBackend and path.
|#

;;; ----------------------------------------------------------------------------
;;; g_settings_new_with_path
;;;
;;; Creates a new GSettings object with the relocatable schema specified by
;;; schema_id and a given path.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_new_with_path" settings-new-with-path)
    (gobject:object settings :return)
  (schema :string)
  (path :string))

(export 'settings-new-with-path)

#|
Functions
g_settings_list_relocatable_schemas
Deprecated.

deprecated: 2.40
|#

;;; ----------------------------------------------------------------------------
;;; g_settings_list_schemas
;;;
;;; Deprecated.
;;;
;;; deprecated: 2.40
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_list_schemas" settings-list-schemas)
    (glib:strv-t :free-from-foreign nil))

(export 'settings-list-schemas)

#|
g_settings_sync
Ensures that all pending operations are complete for the default backend.

g_settings_unbind
Removes an existing binding for property on object.

Instance methods
g_settings_apply
Applies any changes that have been made to the settings. This function does nothing unless settings is in ‘delay-apply’ mode; see g_settings_delay(). In the normal case settings are always applied immediately.

g_settings_bind
Create a binding between the key in the settings object and the property property of object.

g_settings_bind_with_mapping
Create a binding between the key in the settings object and the property property of object.

g_settings_bind_with_mapping_closures
Version of g_settings_bind_with_mapping() using closures instead of callbacks for easier binding in other languages.

since: 2.82

g_settings_bind_writable
Create a binding between the writability of key in the settings object and the property property of object. The property must be boolean; “sensitive” or “visible” properties of widgets are the most likely candidates.

g_settings_create_action
Creates a GAction corresponding to a given GSettings key.

g_settings_delay
Changes the GSettings object into ‘delay-apply’ mode. In this mode, changes to settings are not immediately propagated to the backend, but kept locally until g_settings_apply() is called.

g_settings_get
Gets the value that is stored at key in settings.

g_settings_get_boolean
Gets the value that is stored at key in settings.

g_settings_get_child
Creates a child settings object which has a base path of base-path/name`, wherebase-pathis the base path ofsettings`.

g_settings_get_default_value
Gets the “default value” of a key.

g_settings_get_double
Gets the value that is stored at key in settings.

g_settings_get_enum
Gets the value that is stored in settings for key and converts it to the enum value that it represents.

g_settings_get_flags
Gets the value that is stored in settings for key and converts it to the flags value that it represents.

g_settings_get_has_unapplied
Returns whether the GSettings object has any unapplied changes. This can only be the case if it is in ‘delayed-apply’ mode.

g_settings_get_int
Gets the value that is stored at key in settings.

g_settings_get_int64
Gets the value that is stored at key in settings.

g_settings_get_mapped
Gets the value that is stored at key in settings, subject to application-level validation/mapping.

g_settings_get_range
Queries the range of a key.

deprecated: 2.40

g_settings_get_string
Gets the value that is stored at key in settings.

g_settings_get_strv
A convenience variant of g_settings_get() for string arrays.

g_settings_get_uint
Gets the value that is stored at key in settings.

g_settings_get_uint64
Gets the value that is stored at key in settings.

g_settings_get_user_value
Checks the “user value” of a key, if there is one.

g_settings_get_value
Gets the value that is stored in settings for key.

g_settings_is_writable
Finds out if a key can be written or not.

g_settings_list_children
Gets the list of children on settings.

g_settings_list_keys
Introspects the list of keys on settings.

deprecated: 2.46

g_settings_range_check
Checks if the given value is of the correct type and within the permitted range for key.

deprecated: 2.40

g_settings_reset
Resets key to its default value.

g_settings_revert
Reverts all non-applied changes to the settings. This function does nothing unless settings is in ‘delay-apply’ mode; see g_settings_delay(). In the normal case settings are always applied immediately.

g_settings_set
Sets key in settings to value.

g_settings_set_boolean
Sets key in settings to value.

g_settings_set_double
Sets key in settings to value.

g_settings_set_enum
Looks up the enumerated type nick for value and writes it to key, within settings.

g_settings_set_flags
Looks up the flags type nicks for the bits specified by value, puts them in an array of strings and writes the array to key, within settings.

g_settings_set_int
Sets key in settings to value.

g_settings_set_int64
Sets key in settings to value.

g_settings_set_string
Sets key in settings to value.

g_settings_set_strv
Sets key in settings to value.

g_settings_set_uint
Sets key in settings to value.

g_settings_set_uint64
Sets key in settings to value.

g_settings_set_value
Sets key in settings to value.

[+]
Methods inherited from GObject (43)
[−]
Properties
Gio.Settings:backend
The name of the context that the settings are stored in.

Gio.Settings:delay-apply
Whether the GSettings object is in ‘delay-apply’ mode. See g_settings_delay() for details.

Gio.Settings:has-unapplied
If this property is TRUE, the GSettings object has outstanding changes that will be applied when g_settings_apply() is called.

Gio.Settings:path
The path within the backend where the settings are stored.

Gio.Settings:schema
The name of the schema that describes the types of keys for this GSettings object.

deprecated: 2.32

Gio.Settings:schema-id
The name of the schema that describes the types of keys for this GSettings object.

Gio.Settings:settings-schema
The GSettingsSchema describing the types of keys for this GSettings object.

[−]
Signals
Gio.Settings::change-event
The “change-event” signal is emitted once per change event that affects this settings object. You should connect to this signal only if you are interested in viewing groups of changes before they are split out into multiple emissions of the “changed” signal. For most use cases it is more appropriate to use the “changed” signal.

Gio.Settings::changed
The “changed” signal is emitted when a key has potentially changed. You should call one of the g_settings_get() calls to check the new value.

Gio.Settings::writable-change-event
The “writable-change-event” signal is emitted once per writability change event that affects this settings object. You should connect to this signal if you are interested in viewing groups of changes before they are split out into multiple emissions of the “writable-changed” signal. For most use cases it is more appropriate to use the “writable-changed” signal.

Gio.Settings::writable-changed
The “writable-changed” signal is emitted when the writability of a key has potentially changed. You should call g_settings_is_writable() in order to determine the new status.

[+]
Signals inherited from GObject (1)
[+]
Class structure
[−]
Virtual methods
Gio.SettingsClass.change_event
No description available.
Gio.SettingsClass.changed
No description available.
Gio.SettingsClass.writable_change_event
No description available.
Gio.SettingsClass.writable_changed
No description available.
|#
