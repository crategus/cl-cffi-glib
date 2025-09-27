;;; ----------------------------------------------------------------------------
;;; gobject.binding.lisp
;;;
;;; The documentation in this file is taken from the GObject Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GObject
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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
;;; GBinding
;;;
;;;     Bind two object properties
;;;
;;; Types and Values
;;;
;;;     GBinding
;;;     GBindingFlags
;;;
;;; Accessors
;;;
;;;     g_binding_get_source                                Deprecated 2.68
;;;     g_binding_get_source_property
;;;     g_binding_get_target                                Deprecated 2.68
;;;     g_binding_get_target_property
;;;     g_binding_get_flags
;;;
;;; Functions
;;;
;;;     g_binding_dup_source                                Since 2.68
;;;     g_binding_dup_target                                Since 2.68
;;;     g_binding_unbind
;;;     g_object_bind_property
;;;
;;;     GBindingTransformFunc
;;;     g_object_bind_property_full
;;;     g_object_bind_property_with_closures
;;;
;;; Properties
;;;
;;;     flags
;;;     source
;;;     source-property
;;;     target
;;;     target-property
;;;
;;; Object Hierarchy
;;;
;;;     GFlags
;;;     ╰── GBindingFlags
;;;
;;;     GObject
;;;     ╰── GBinding
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; GBindingFlags
;;; ----------------------------------------------------------------------------

(define-gflags "GBindingFlags" binding-flags
  (:export t
   :type-initializer "g_binding_flags_get_type")
  (:default 0)
  (:bidirectional 1)
  (:sync-create 2)
  (:invert-boolean 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'binding-flags)
      "GFlags"
      (liber:symbol-documentation 'binding-flags)
 "@version{2024-06-18}
  @begin{declaration}
(define-gflags \"GBindingFlags\" binding-flags
  (:export t
   :type-initializer \"g_binding_flags_get_type\")
  (:default 0)
  (:bidirectional 1)
  (:sync-create 2)
  (:invert-boolean 4))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:default]{The default binding. If the source property changes, the
        target property is updated with its value.}
      @entry[:bidirectional]{Bidirectional binding. If either the property of
        the source or the property of the target changes, the other is updated.}
      @entry[:sync-create]{Synchronize the values of the source and target
        properties when creating the binding. The direction of the
        synchronization is always from the source to the target.}
      @entry[:invert-boolean]{If the two properties being bound are booleans,
        setting one to @em{true} will result in the other being set to
        @em{false} and vice versa. This flag will only work for boolean
        properties, and cannot be used when passing custom transformation
        functions to the @fun{g:object-bind-property-full} function.}
    @end{table}
  @end{values}
  @begin{short}
    Flags to be passed to the @fun{g:object-bind-property} or
    @fun{g:object-bind-property-full} functions.
  @end{short}
  @see-class{g:binding}
  @see-function{g:object-bind-property}
  @see-function{g:object-bind-property-full}")

;;; ----------------------------------------------------------------------------
;;; GBinding
;;; ----------------------------------------------------------------------------

(define-gobject "GBinding" binding
  (:superclass object
   :export t
   :interfaces nil
   :type-initializer "g_binding_get_type")
  ((flags
    binding-flags
    "flags" "GBindingFlags" t t)
   (source
    binding-source
    "source" "GObject" t t)
   (source-property
    binding-source-property
    "source-property" "gchararray" t t)
   (target
    binding-target
    "target" "GObject" t t)
   (target-property
    binding-target-property
    "target-property" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'binding 'type)
 "@version{2024-12-07}
  @begin{short}
    The @class{g:binding} object is the representation of a binding between a
    property on a @class{g:object} instance, or source, and another property on
    another @class{g:object} instance, or target.
  @end{short}
  Whenever the source property changes, the same value is applied to the target
  property. For instance, the following binding:
  @begin{pre}
(g:object-bind-property object1 \"property-a\"
                        object2 \"property-b\"
                        :default)
  @end{pre}
  will cause the property named @code{property-b} of @code{object2} to be
  updated every time the @fun{g:object-property} function or the specific
  accessor changes the value of the property @code{property-a} of
  @code{object1}.

  It is possible to create a bidirectional binding between two properties of two
  @class{g:object} instances, so that if either property changes, the other is
  updated as well, for instance:
  @begin{pre}
(g:object-bind-property object1 \"property-a\"
                        object2 \"property-b\"
                        :bidirectional)
  @end{pre}
  will keep the two properties in sync.

  It is also possible to set a custom transformation function, in both
  directions, in case of a bidirectional binding, to apply a custom
  transformation from the source value to the target value before applying it.
  For instance, the following binding:
  @begin{pre}
(g:object-bind-property-full adjustment1 \"value\"
                             adjustment2 \"value\"
                             :bidirectional
                             #'celsius-to-fahrenheit
                             #'fahrenheit-to-celsius)
  @end{pre}
  will keep the @code{value} property of the two adjustments in sync. The
  @code{celsius-to-fahrenheit} function will be called whenever the @code{value}
  property of @code{adjustment1} changes and will transform the current value
  of the property before applying it to the @code{value} property of
  @code{adjustment2}. Vice versa, the @code{fahrenheit-to-celsius} function
  will be called whenever the @code{value} property of @code{adjustment2}
  changes, and will transform the current value of the property before applying
  it to the @code{value} property of @code{adjustment1}.

  Note that the @class{g:binding} object does not resolve cycles by itself. A
  cycle like
  @begin{pre}
object1:propertyA -> object2:propertyB
object2:propertyB -> object3:propertyC
object3:propertyC -> object1:propertyA
  @end{pre}
  might lead to an infinite loop. The loop, in this particular case, can be
  avoided if the objects emit the @code{\"notify\"} signal only if the value has
  effectively been changed. A binding is implemented using the @code{\"notify\"}
  signal, so it is susceptible to all the various ways of blocking a signal
  emission, like the @fun{g:signal-stop-emission} or
  @fun{g:signal-handler-block} functions.

  A binding will be severed, and the resources it allocates freed, whenever
  either one of the @class{g:object} instances it refers to are finalized, or
  when the @class{g:binding} object loses its last reference. Bindings for
  languages with garbage collection can use the @fun{g:binding-unbind} function
  to explicitly release a binding between the source and target properties,
  instead of relying on the last reference on the binding, source, and target
  instances to drop.
  @see-slot{g:binding-flags}
  @see-slot{g:binding-source}
  @see-slot{g:binding-source-property}
  @see-slot{g:binding-target}
  @see-slot{g:binding-target-poperty}
  @see-class{g:object}")

;;; ----------------------------------------------------------------------------
;;; Propery and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:binding-flags --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "flags" 'binding) t)
 "The @code{flags} property of type @symbol{g:binding-flags}
  (Read / Write / Construct Only) @br{}
  The flags to be used to control the binding.")

#+liber-documentation
(setf (liber:alias-for-function 'binding-flags)
      "Accessor"
      (documentation 'binding-flags 'function)
 "@version{2025-09-27}
  @syntax{(g:binding-flags object) => flags}
  @argument[object]{a @class{g:binding} object}
  @argument[flags]{a @symbol{g:binding-flags} value used by the binding}
  @begin{short}
    The accessor for the @slot[g:binding]{flags} slot of the @class{g:binding}
    class retrieves the flags passed when constructing the @class{g:binding}
    object.
  @end{short}
  @see-class{g:binding}
  @see-symbol{g:binding-flags}")

;;; --- g:binding-source -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "source" 'binding) t)
 "The @code{source} property of type @class{g:object}
  (Read / Write / Construct Only) @br{}
  The object that should be used as the source of the binding.")

#+liber-documentation
(setf (liber:alias-for-function 'binding-source)
      "Accessor"
      (documentation 'binding-source 'function)
 "@version{2025-09-27}
  @syntax{(g:binding-source object) => source}
  @argument[object]{a @class{g:binding} object}
  @argument[source]{a @class{g:object} source instance}
  @begin{short}
    The accessor for the @slot[g:binding]{source} slot of the @class{g:binding}
    class retrieves the @class{g:object} instance used as the source of the
    binding.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 2.68. Use the @fun{g:binding-dup-source}
    function for a safer version of this function.
  @end{dictionary}
  @see-class{g:binding}
  @see-class{g:object}
  @see-function{g:binding-dup-source}")

;;; --- g:binding-source-property ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "source-property" 'binding) t)
 "The @code{source-property} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The name of the property of the source that should be used as the source of
  the binding. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'binding-source-property)
      "Accessor"
      (documentation 'binding-source-property 'function)
 "@version{2025-09-27}
  @syntax{(g:binding-source-property object) => property}
  @argument[object]{a @class{g:binding} object}
  @argument[property]{a string for the name of the source property}
  @begin{short}
    The accessor for the @slot[g:binding]{source-property} slot of the
    @class{g:binding} class retrieves the name of the property of the source
    used for the binding.
  @end{short}
  @see-class{g:binding}")

;;; --- g:binding-target -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "target" 'binding) t)
 "The @code{target} property of type @class{g:object}
  (Read / Write / Construct Only) @br{}
  The object that should be used as the target of the binding.")

#+liber-documentation
(setf (liber:alias-for-function 'binding-target)
      "Accessor"
      (documentation 'binding-target 'function)
 "@version{2025-09-27}
  @syntax{(g:binding-target object) => target}
  @argument[object]{a @class{g:binding} object}
  @argument[target]{a @class{g:object} target instance}
  @begin{short}
    The accessor for the @slot[g:binding]{target} slot of the @class{g:binding}
    class retrieves the @class{g:object} instance used as the target of the
    binding.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 2.68. Use the @fun{g:binding-dup-target}
    function for a safer version of this function.
  @end{dictionary}
  @see-class{g:binding}
  @see-class{g:object}
  @see-function{g:binding-dup-target}")

;;; --- g:binding-target-property ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "target-property" 'binding) t)
 "The @code{target-property} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The name of the property of the target that should be used for the binding.
  @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'binding-target-property)
      "Accessor"
      (documentation 'binding-target-property 'function)
 "@version{2025-09-27}
  @syntax{(g:binding-target-property object) => property}
  @argument[object]{a @class{g:binding} object}
  @argument[property]{a string for the name of the target property}
  @begin{short}
    The accessor for the @slot[g:binding]{target-property} slot of the
    @class{g:binding} class retrieves the name of the property of the target
    used for the binding.
  @end{short}
  @see-class{g:binding}")

;;; ----------------------------------------------------------------------------
;;; g_binding_dup_source
;;; ----------------------------------------------------------------------------

#+glib-2-68
(cffi:defcfun ("g_binding_dup_source" binding-dup-source)
    (object binding :return)
 #+liber-documentation
 "@version{2024-12-07}
  @argument[binding]{a @class{g:binding} object}
  @return{The @class{g:object} source instance, or @code{nil} if the source
    does not exist any more.}
  @begin{short}
    Retrieves the @class{g:object} instance used as the source of the binding.
  @end{short}
  A @class{g:binding} object can outlive the @class{g:object} source instance
  as the binding does not hold a strong reference to the source. If the source
  is destroyed before the binding then this function will return @code{nil}.

  Since 2.68
  @see-class{g:binding}
  @see-class{g:object}"
  (binding (object binding)))

#+glib-2-68
(export 'binding-dup-source)

;;; ----------------------------------------------------------------------------
;;; g_binding_dup_target
;;; ----------------------------------------------------------------------------

#+glib-2-68
(cffi:defcfun ("g_binding_dup_target" binding-dup-target)
    (object binding :return)
 #+liber-documentation
 "@version{2024-12-07}
  @argument[binding]{a @class{g:binding} object}
  @return{The @class{g:object} target instance, or @code{nil} if the target
    does not exist any more.}
  @begin{short}
    Retrieves the @class{g:object} instance used as the target of the binding.
  @end{short}
  A @class{g:binding} object can outlive the @class{g:object} target instance
  as the binding does not hold a strong reference to the target. If the target
  is destroyed before the binding then this function will return @code{nil}.

  Since 2.68
  @see-class{g:binding}
  @see-class{g:object}"
  (binding (object binding)))

#+glib-2-68
(export 'binding-dup-target)

;;; ----------------------------------------------------------------------------
;;; g_binding_unbind
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_binding_unbind" binding-unbind) :void
 #+liber-documentation
 "@version{2024-12-07}
  @argument[binding]{a @class{g:binding} object}
  @begin{short}
    Explicitly releases the binding between the source and the target property
    expressed by @arg{binding}.
  @end{short}
  This function will release the reference that is being held on the
  @arg{binding} instance. If you want to hold on to the @arg{binding} instance
  after calling the @fun{g:binding-unbind} function, you will need to hold a
  reference to it.
  @see-class{g:binding}"
  (binding (object binding)))

(export 'binding-unbind)

;;; ----------------------------------------------------------------------------
;;; g_object_bind_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_bind_property" object-bind-property) (object binding)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[source]{a @class{g:object} source instance}
  @argument[source-prop]{a string for the property on @arg{source} to bind}
  @argument[target]{a @class{g:object} target instance}
  @argument[target-prop]{a string for the property on @arg{target} to bind}
  @argument[flags]{a @symbol{g:binding-flags} value to pass to the binding}
  @begin{return}
    The @class{g:binding} instance representing the binding between the two
    @class{g:object} instances.
  @end{return}
  @begin{short}
    Creates a binding between @arg{source-prop} on @arg{source} and
    @arg{target-prop} on @arg{target}.
  @end{short}
  Whenever the @arg{source-prop} is changed the @arg{target-prop} is
  updated using the same value.

  If the @arg{flags} argument contains the @code{:bidirectional} value then the
  binding will be mutual. If the @arg{target-prop} property on @arg{target}
  changes then the @arg{source-prop} property on @arg{source} will be updated
  as well.

  The binding will automatically be removed when either the source or the
  target instances are finalized. To remove the binding without affecting the
  source and the target you can call the @fun{g:binding-unbind} function on
  the returned @class{g:binding} object.

  A @class{g:object} instance can have multiple bindings.
  @begin[Examples]{dictionary}
    This example will result in the @code{sensitive} property of the widget to
    be updated with the same value of the @code{active} property of the action.
  @begin{pre}
(g:object-bind-property action \"active\" widget \"sensitive\" :default)
  @end{pre}
  @end{dictionary}
  @see-class{g:binding}
  @see-class{g:object}
  @see-symbol{g:binding-flags}
  @see-function{g:binding-unbind}"
  (source object)
  (source-prop :string)
  (target object)
  (target-prop :string)
  (flags binding-flags))

(export 'object-bind-property)

;;; ----------------------------------------------------------------------------
;;; GBindingTransformFunc
;;; ----------------------------------------------------------------------------

;; This callback is not needed. Binding is implemented with closures.

#+liber-documentation
(setf (liber:alias-for-symbol 'binding-transform-func)
      "Callback"
      (liber:symbol-documentation 'binding-transform-func)
 "@version{2024-12-07}
  @syntax{lambda (binding from to) => result}
  @argument[binding]{a @class{g:binding} object}
  @argument[from]{a @symbol{g:value} instance in which to store the value to
    transform}
  @argument[to]{a @symbol{g:value} instance in which to store the transformed
    value}
  @argument[result]{@em{True} if the transformation was successful,
    and @em{false} otherwise.}
  @begin{short}
    The callback function for the @fun{g:object-bind-property-full} function to
    be called to transform @arg{from} to @arg{to}.
  @end{short}
  If this is the @code{transform-to} function of a binding, then @arg{from} is
  the source property on the source object, and @arg{to} is the target property
  on the target object. If this is the @code{transform-from} function of a
  @code{:bidirectional} binding, then those roles are reversed.
  @see-class{g:binding}
  @see-symbol{g:value}
  @see-function{g:object-bind-property-full}")

(export 'binding-transform-func)

;;; ----------------------------------------------------------------------------
;;; g_object_bind_property_with_closures                    internal use only
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_object_bind_property_with_closures"
               %object-bind-property-with-closures) (object binding)
  (source object)
  (source-prop :string)
  (target object)
  (target-prop :string)
  (flags binding-flags)
  (transform-to :pointer)
  (transfrom-from :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_bind_property_full
;;; ----------------------------------------------------------------------------

(defun object-bind-property-full (source
                                  source-prop
                                  target
                                  target-prop
                                  flags
                                  transform-to
                                  transform-from)
 #+liber-documentation
 "@version{2025-08-30}
  @argument[source]{a @class{g:object} source instance}
  @argument[source-prop]{a string for the property on @arg{source} to bind}
  @argument[target]{a @class{g:object} target instance}
  @argument[target-prop]{a string for the property on @arg{target} to bind}
  @argument[flags]{a @sym{g:binding-flags} value to pass to the binding}
  @argument[transform-to]{a @sym{g:binding-transform-func} callback function
    from the source to the target, or @code{nil} to use the default}
  @argument[transform-from]{a @sym{g:binding-transform-func} callback function
    from the target to the source, or @code{nil} to use the default}
  @begin{return}
    The @class{g:binding} object representing the binding between @arg{source}
    and @arg{target}.
  @end{return}
  @begin{short}
    Complete version of the @fun{g:object-bind-property} function.
  @end{short}
  Creates a binding between @arg{source-prop} on @arg{source} and
  @arg{target-prop} on @arg{target}, allowing you to set the transformation
  functions to be used by the binding.

  If @arg{flags} contains the @code{:birectional} value then the binding will
  be mutual: if @arg{target-prop} on @arg{target} changes then the
  @arg{source-prop} on @arg{source} will be updated as well. The
  @arg{transform-from} function is only used in case of bidirectional bindings,
  otherwise it will be ignored.

  The binding will automatically be removed when either the source or the target
  instances are finalized. This will release the reference that is being held on
  the @class{g:binding} object. If you want to hold on to the @class{g:binding}
  object, you will need to hold a reference to it. To remove the binding, call
  the @fun{g:binding-unbind} function.

  A @class{g:object} instance can have multiple bindings.
  @see-class{g:binding}
  @see-class{g:object}
  @see-symbol{g:binding-flags}
  @see-symbol{g:binding-transform-func}
  @see-function{g:object-bind-property}
  @see-function{g:binding-unbind}"
  (let ((source (object-pointer source)))
    (%object-bind-property-with-closures
            source
            source-prop
            target
            target-prop
            flags
            (if transform-to
                (create-closure-for-instance source transform-to)
                (cffi:null-pointer))
            (if transform-from
                (create-closure-for-instance source transform-from)
                (cffi:null-pointer)))))

(export 'object-bind-property-full)

;;; --- gobject.binding.lisp ---------------------------------------------------
