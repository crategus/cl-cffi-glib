;;; ----------------------------------------------------------------------------
;;; gio.settings-schema.lisp
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
;;
;;; Types and Values
;;;
;;;     GSettingsSchema
;;;     GSettingsSchemaKey
;;;     GSettingsSchemaSource
;;;
;;; Functions
;;;
;;;     g_settings_schema_source_default
;;;     g_settings_schema_source_lookup
;;;     g_settings_schema_source_list_schemas               not implemented
;;;
;;;     g_settings_schema_key_get_default_value
;;;     g_settings_schema_key_get_description
;;;     g_settings_schema_key_get_name
;;;     g_settings_schema_key_get_range
;;;     g_settings_schema_key_get_summary
;;;     g_settings_schema_key_get_value_type
;;;     g_settings_schema_key_range_check
;;;
;;;     g_settings_schema_get_id
;;;     g_settings_schema_get_key
;;;     g_settings_schema_get_path
;;;     g_settings_schema_has_key
;;;     g_settings_schema_list_children
;;;     g_settings_schema_list_keys
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GSettingsSchemaKey
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque settings-schema-key "GSettingsSchemaKey"
  :export t
  :type-initializer "g_settings_schema_key_get_type"
  :alloc (error "GSettingsSchemaKey cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'settings-schema-key)
      "GBoxed"
      (documentation 'settings-schema-key 'type)
 "@version{2026-03-24}
  @begin{declaration}
(glib:define-gboxed-opaque settings-schema-key \"GSettingsSchemaKey\"
  :export t
  :type-initializer \"g_settings_schema_key_get_type\"
  :alloc (error \"GSettingsSchemaKey cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    This is an opaque structure type. You cannot access it directly or create
    it from the Lisp side.
  @end{short}
  @see-class{g:settings-schema}")

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_key_get_default_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_key_get_default_value"
               settings-schema-key-default-value)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{2026-03-24}
  @argument[key]{a @class{g:settings-schema-key} instance}
  @return{The @symbol{g:variant} instance for the default of the key.}
  @begin{short}
    Gets the default value for @arg{key}.
  @end{short}
  Note that this is the default value according to the schema. System
  administrator defaults and lockdown are not visible via this API.
  @see-class{g:settings-schema-key}
  @see-symbol{g:variant}"
  (key (glib:boxed settings-schema-key)))

(export 'settings-schema-key-default-value)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_key_get_description
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_key_get_description"
               settings-schema-key-description) :string
 #+liber-documentation
 "@version{2026-03-24}
  @argument[key]{a @class{g:settings-schema-key} instance}
  @return{The string for the description of @arg{key}.}
  @begin{short}
    Gets the description for @arg{key}.
  @end{short}
  If no description has been provided in the schema for @arg{key}, returns
  @code{nil}.

  The description can be one sentence to several paragraphs in length.
  Paragraphs are delimited with a double newline. Descriptions can be translated
  and the value returned from this function is is the current locale.

  This function is slow. The summary and description information for the schemas
  is not stored in the compiled schema database so this function has to parse
  all of the source XML files in the schema directory.
  @see-class{g:settings-schema-key}"
  (key (glib:boxed settings-schema-key)))

(export 'settings-schema-key-description)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_key_get_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_key_get_name" settings-schema-key-name)
    :string
 #+liber-documentation
 "@version{2026-03-24}
  @argument[key]{a @class{g:settings-schema-key} instance}
  @return{The string for the name of @arg{key}.}
  @begin{short}
    Gets the name of @arg{key}.
  @end{short}
  @see-class{g:settings-schema-key}"
  (key (glib:boxed settings-schema-key)))

(export 'settings-schema-key-name)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_key_get_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_key_get_range" settings-schema-key-range)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{2026-03-24}
  @argument[key]{a @class{g:settings-schema-key} instance}
  @return{The @symbol{g:variant} instance describing the range}
  @begin{short}
    Queries the range of a key.
  @end{short}
  This function will return a @symbol{g:variant} instance that fully describes
  the range of values that are valid for @arg{key}.

  The type of the @symbol{g:variant} instance returned is @code{\"(sv)\"}. The
  string describes the type of range restriction in effect. The type and meaning
  of the value contained in the variant depends on the string.

  If the string is @code{\"type\"} then the variant contains an empty array. The
  element type of that empty array is the expected type of value and all values
  of that type are valid.

  If the string is @code{\"enum\"} then the variant contains an array
  enumerating the possible values. Each item in the array is a possible valid
  value and no other values are valid.

  If the string is @code{\"flags\"} then the variant contains an array. Each
  item in the array is a value that may appear zero or one times in an array to
  be used as the value for this key. For example, if the variant contained the
  array @code{['x', 'y']} then the valid values for the key would be @code{[]},
  @code{['x']}, @code{['y']}, @code{['x', 'y']} and @code{['y', 'x']}.

  Finally, if the string is @code{\"range\"} then the variant contains a pair
  of like-typed values — the minimum and maximum permissible values for this
  key.

  This information should not be used by normal programs. It is considered to
  be a hint for introspection purposes. Normal programs should already know what
  is permitted by their own schema. The format may change in any way in the
  future — but particularly, new forms may be added to the possibilities
  described above.

  You should free the returned value with the @fun{g:variant-unref} function
  when it is no longer needed.
  @see-class{g:settings-schema-key}
  @see-symbol{g:variant}
  @see-function{g:variant-unref}"
  (key (glib:boxed settings-schema-key)))

(export 'settings-schema-key-range)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_key_get_summary
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_key_get_summary" settings-schema-key-summary)
    :string
 #+liber-documentation
 "@version{2026-03-24}
  @argument[key]{a @class{g:settings-schema-key} instance}
  @return{The string for the summary.}
  @begin{short}
    Gets the summary for @arg{key}.
  @end{short}
  If no summary has been provided in the schema for @arg{key}, returns
  @code{nil}.

  The summary is a short description of the purpose of the key. Usually one
  short sentence. Summaries can be translated and the value returned from this
  function is is the current locale.

  This function is slow. The summary and description information for the schemas
  is not stored in the compiled schema database so this function has to parse
  all of the source XML files in the schema directory.
  @see-class{g:settings-schema-key}"
  (key (glib:boxed settings-schema-key)))

(export 'settings-schema-key-summary)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_key_get_value_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_key_get_value_type"
               settings-schema-key-value-type) (glib:boxed glib:variant-type)
 #+liber-documentation
 "@version{2026-03-24}
  @argument[key]{a @class{g:settings-schema-key} instance}
  @return{The @class{g:variant-type} instance for the type of @arg{key}.}
  @begin{short}
    Gets the @class{g:variant-type} instance of @arg{key}.
  @end{short}
  @see-class{g:settings-schema-key}
  @see-class{g:variant-type}"
  (key (glib:boxed settings-schema-key)))

(export 'settings-schema-key-value-type)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_key_range_check
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_key_range_check"
               settings-schema-key-range-check) :boolean
 #+liber-documentation
 "@version{2026-03-24}
  @argument[key]{a @class{g:settings-schema-key} instance}
  @argument[value]{a @symbol{g:variant} instance for the value}
  @return{The boolean whether @arg{value} is valid for @arg{key}.}
  @begin{short}
    Checks if the given value is within the permitted range for @arg{key}.
  @end{short}
  It is a programmer error if @arg{value} is not of the correct type — you must
  check for this first.
  @see-class{g:settings-schema-key}
  @see-symbol{g:variant}"
  (key (glib:boxed settings-schema-key))
  (value (:pointer (:struct glib:variant))))

(export 'settings-schema-key-range-check)

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

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_get_id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_get_id" settings-schema-get-id) :string
 #+liber-documentation
 "@version{2026-03-24}
  @argument[schema]{a @class{g:settings-schema} instance}
  @return{The string for the ID.}
  @begin{short}
    Gets the ID of @arg{schema}.
  @end{short}
  @see-class{g:settings-schema}"
  (schema (glib:boxed settings-schema)))

(export 'settings-schema-get-id)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_get_key
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_get_key" settings-schema-key)
    (glib:boxed settings-schema-key :return)
 "@version{2026-03-24}
  @argument[schema]{a @class{g:settings-schema} instance}
  @argument[name]{a string for the name of a key}
  @return{The @class{g:settings-schema-key} instance}
  @begin{short}
    Gets the key named @arg{name} from @arg{schema}.
  @end{short}
  It is a programmer error to request a key that does not exist. See the
  @fun{g:settings-schema-list-keys} function.
  @see-class{g:settings-schema}
  @see-class{g:settings-schema-key}
  @see-function{g:settings-schema-list-keys}"
  (schema (glib:boxed settings-schema))
  (name :string))

(export 'settings-schema-key)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_get_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_get_path" settings-schema-path) :string
 "@version{2026-03-24}
  @argument[schema]{a @class{g:settings-schema} instance}
  @return{The string for the path of the schema, or @code{nil}.}
  @begin{short}
    Gets the path associated with schema, or @code{nil}.
  @end{short}
  Schemas may be single-instance or relocatable. Single-instance schemas
  correspond to exactly one set of keys in the backend database, those located
  at the path returned by this function.

  Relocatable schemas can be referenced by other schemas and can therefore
  describe multiple sets of keys at different locations. For relocatable
  schemas, this function will return @code{nil}.
  @see-class{g:settings-schema}"
  (schema (glib:boxed settings-schema)))

(export 'settings-schema-path)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_has_key
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_has_key" settings-schema-has-key) :boolean
 "@version{2026-03-24}
  @argument[schema]{a @class{g:settings-schema} instance}
  @argument[name]{a string for the name of a key.}
  @return{The boolean whether such a key exists.}
  @begin{short}
    Checks if @arg{schema} has a key named @arg{name}.
  @end{short}
  @see-class{g:settings-schema}"
  (schema (glib:boxed settings-schema))
  (name :string))

(export 'settings-schema-has-key)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_list_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_list_children" settings-schema-list-children)
    (glib:strv-t :free-from-foreign t)
 "@version{2026-03-24}
  @argument[schema]{a @class{g:settings-schema} instance}
  @return{The list of strings for the children in @arg{schema}.}
  @begin{short}
    Gets the list of children in @arg{schema}.
  @end{short}
  @see-class{g:settings-schema}"
  (schema (glib:boxed settings-schema)))

(export 'settings-schema-list-children)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_list_keys
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_list_keys" settings-schema-list-keys)
    (glib:strv-t :free-from-foreign t)
 "@version{2026-03-24}
  @argument[schema]{a @class{g:settings-schema} instance}
  @return{The list of strings for the keys in @arg{schema}.}
  @begin{short}
    Introspects the list of keys in @arg{schema}.
  @end{short}
  You should probably not be calling this function from \"normal\" code, since
  you should already know what keys are in your schema. This function is
  intended for introspection reasons.
  @see-class{g:settings-schema}"
  (schema (glib:boxed settings-schema)))

(export 'settings-schema-list-keys)

;;; ----------------------------------------------------------------------------
;;; GSettingsSchemaSource
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque settings-schema-source "GSettingsSchemaSource"
  :export t
  :type-initializer "g_settings_schema_source_get_type"
  :alloc (error "GSettingsSchemaSource cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'settings-schema-source)
      "GBoxed"
      (documentation 'settings-schema-source 'type)
 "@version{2026-03-24}
  @begin{declaration}
(glib:define-gboxed-opaque settings-schema-source \"GSettingsSchemaSource\"
  :export t
  :type-initializer \"g_settings_schema_source_get_type\"
  :alloc (error \"GSettingsSchemaSource cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    This is an opaque structure type.
  @end{short}
  You cannot access it directly or create it from the Lisp side.
  @see-class{g:settings}")

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_source_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_source_get_default"
               settings-schema-source-default)
    (glib:boxed settings-schema-source)
 #+liber-documentation
 "@version{2026-03-24}
  @begin{return}
    The @class{g:settings-schema-source} instance for the default schema source.
  @end{return}
  @begin{short}
    Gets the default system schema source.
  @end{short}
  If no schemas are installed, @code{nil} will be returned.

  This function is not required for normal uses of @code{GSettings} but it may
  be useful to authors of plugin management systems or to those who want to
  introspect the content of schemas.

  The returned source may actually consist of multiple schema sources from
  different directories, depending on which directories were given in
  @code{XDG_DATA_DIRS} and @code{GSETTINGS_SCHEMA_DIR}. For this reason, all
  lookups performed against the default source should probably be done
  recursively.
  @see-class{g:settings-schema-source}")

(export 'settings-schema-source-default)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_source_lookup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_settings_schema_source_lookup" settings-schema-source-lookup)
    (glib:boxed settings-schema :return)
 #+liber-documentation
 "@version{2026-03-24}
  @argument[source]{a @class{g:settings-schema-source} instance}
  @argument[id]{a string for the schema ID}
  @argument[recursive]{a boolean whether the lookup should be recursive}
  @begin{short}
    Looks up a schema with the identifier @arg{id} in @arg{source}.
  @end{short}
  If the schema is not found directly in @arg{source} and recursive is @em{true}
  then the parent sources will also be checked. If the schema is not found,
  @code{nil} is returned.

  This function is not required for normal uses of @code{GSettings} but it may
  be useful to authors of plugin management systems or to those who want to
  introspect the content of schemas.
  @see-class{g:settings-schema-source}"
  (source (glib:boxed settings-schema-source))
  (id :string)
  (recursive :boolean))

(export 'settings-schema-source-lookup)

;;; ----------------------------------------------------------------------------
;;; g_settings_schema_source_list_schemas                   not implemented
;;; ----------------------------------------------------------------------------

;; No longer works with GLIB 2.88. We get a memory default.
;; The array of pointer to strings no longer ends with NULL.

#+nil
(cffi:defcfun ("g_settings_schema_source_list_schemas"
               %settings-schema-source-list-schemas) :void
  (source (glib:boxed settings-schema-source))
  (recursive :boolean)
  (non-relocatable :pointer)
  (relocatable :pointer))

#+nil
(defun settings-schema-source-list-schemas (source recursive)
 #+liber-documentation
 "@version{2026-03-24}
  @syntax{(g:settings-schema-source-list-schemas source recursive) =>
    (list non-relocatable relocatable)}
  @argument[source]{a @class{g:settings-schema-source} instance}
  @argument[recursive]{a boolean whether the lookup should be recursive}
  @argument[non-relocatable]{a list of strings for the non-relocatble schemas}
  @argument[relocatable]{a list of strings for the relocatable schemas}
  @begin{short}
    Lists the schemas in a given @arg{source}.
  @end{short}
  If @arg{recursive} is @em{true} then include parent sources. If @em{false}
  then only include the schemas from one source, that is one directory. You
  probably want @em{true}.

  Non-relocatable schemas are those for which you can call the
  @fun{g:settings-new} function. Relocatable schemas are those for which you
  must use the @fun{g:settings-new-with-path} function.

  Do not call this function from normal programs. This is designed for use by
  database editors, commandline tools, and so on.
  @see-class{g:settings-schema-source}
  @see-function{g:settings-new}
  @see-function{g:settings-new-with-path}"
  (cffi:with-foreign-objects ((ptr1 :pointer) (ptr2 :pointer))
    (%settings-schema-source-list-schemas source recursive ptr1 ptr2)
    (values ;; List of non-relocatabel schemas
            (iter (for i from 0)
                  (for strptr = (cffi:mem-aref ptr1 :pointer i))
                  (while (not (cffi:null-pointer-p strptr)))
                  (collect (cffi:convert-from-foreign
                               (cffi:mem-ref strptr :pointer) :string)))
            ;; List of relocatable schemas
            (iter (for i from 0)
                  (for strptr = (cffi:mem-aref ptr2 :pointer i))
                  (while (not (cffi:null-pointer-p strptr)))
                  (collect (cffi:convert-from-foreign
                               (cffi:mem-ref strptr :pointer) :string))))))

#+nil
(export 'settings-schema-source-list-schemas)

;;; --- End of file gio.settings-schema.lisp -----------------------------------
