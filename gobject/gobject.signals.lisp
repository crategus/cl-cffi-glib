;;; ----------------------------------------------------------------------------
;;; gobject.signals.lisp
;;;
;;; The documentation in this file is taken from the GObject Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GObject
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; Signals
;;;
;;;     A means for customization of object behaviour and a general purpose
;;;     notification mechanism.
;;;
;;; Types and Values
;;;
;;;     GSignalInvocationHint                               not implemented
;;;     GSignalCMarshaller                                  not implemented
;;;     GSignalCVaMarshaller                                not implemented
;;;
;;;     GSignalFlags
;;;     GConnectFlags
;;;     GSignalQuery
;;;
;;; Functions
;;;
;;;     GSignalAccumulator
;;;     GSignalEmissionHook
;;;
;;;     g_signal_new
;;;     g_signal_newv
;;;     g_signal_new_valist
;;;     g_signal_set_va_marshaller
;;;     g_signal_query                                      exported
;;;     g_signal_lookup                                     exported
;;;     g_signal_name                                       exported
;;;     g_signal_list_ids                                   exported
;;;     g_signal_emitv                                      internal
;;;     g_signal_emit                                       exported
;;;     g_signal_emit_by_name
;;;     g_signal_emit_valist
;;;     g_signal_connect                                    exported
;;;     g_signal_connect_after                              exported
;;;     g_signal_connect_swapped
;;;     g_signal_connect_object
;;;     g_signal_connect_data
;;;     g_signal_connect_closure                            internal
;;;     g_signal_connect_closure_by_id
;;;     g_signal_handler_block                              exported
;;;     g_signal_handler_unblock                            exported
;;;     g_signal_handler_disconnect                         exported
;;;     g_signal_handler_find                               exported
;;;     g_signal_handlers_block_matched
;;;     g_signal_handlers_unblock_matched
;;;     g_signal_handlers_disconnect_matched
;;;     g_signal_handler_is_connected                       exported
;;;     g_signal_handlers_block_by_func
;;;     g_signal_handlers_unblock_by_func
;;;     g_signal_handlers_disconnect_by_func
;;;     g_signal_handlers_disconnect_by_data
;;;     g_signal_has_handler_pending                        exported
;;;     g_signal_stop_emission                              exported
;;;     g_signal_stop_emission_by_name                      exported
;;;     g_signal_override_class_closure
;;;     g_signal_chain_from_overridden
;;;     g_signal_new_class_handler
;;;     g_signal_override_class_handler
;;;     g_signal_chain_from_overridden_handler
;;;     g_signal_add_emission_hook
;;;     g_signal_remove_emission_hook
;;;     g_signal_is_valid_name                              Since 2.66
;;;     g_signal_parse_name                                 internal
;;;     g_signal_get_invocation_hint
;;;     g_signal_type_cclosure_new
;;;     g_signal_accumulator_first_wins
;;;     g_signal_accumulator_true_handled
;;;     g_clear_signal_handler
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------

;; Store, retrieve and delete a Lisp function in a hash table for an instance

(let ((function-table (make-hash-table :test 'equal)))

  (defun get-handlers-for-instance (instance)
    (gethash (cffi:pointer-address instance) function-table))

  (defun list-handlers-for-instance ()
    (iter (for (key value) in-hashtable function-table)
          (collect (list key value))))

  (defun find-free-function-id (instance)
    (iter (with handlers = (get-handlers-for-instance instance))
          (for i from 0 below (length handlers))
          (finding i such-that (null (aref handlers i)))))

  (defun save-handler-to-instance (instance handler)
    (let ((id (find-free-function-id instance)))
      (if id
          (progn
            (setf (aref (get-handlers-for-instance instance) id) handler)
            id)
          (let ((handlers (get-handlers-for-instance instance)))
            (if handlers
                (progn
                  (vector-push-extend handler handlers)
                  (1- (length handlers)))
                (let ((handlers (make-array 0 :adjustable t :fill-pointer t)))
                  (setf (gethash (cffi:pointer-address instance) function-table)
                        handlers)
                  (vector-push-extend handler handlers)
                  (1- (length handlers))))))))

  (defun retrieve-handler-from-instance (instance function-id)
    (let ((handlers (get-handlers-for-instance instance)))
      (aref handlers function-id)))

  (defun delete-handler-from-instance (instance function-id)
    (let ((handlers (get-handlers-for-instance instance)))
      (setf (aref handlers function-id) nil)
      (iter (while (plusp (length handlers)))
            (while (null (aref handlers (1- (length handlers)))))
            (vector-pop handlers))
      nil)))

;;; ----------------------------------------------------------------------------

(cffi:defcstruct closure
  (:private-data :uint32)
  (:marshal :pointer)
  (:data :pointer)
  (:notifiers :pointer))

;; Specialization of CLOSURE for Lisp function callbacks
(cffi:defcstruct lisp-closure
  (:parent-instance (:struct closure))
  (:object :pointer)
  (:function-id :int))

;; Accessors for the slots of LISP-CLOSURE
(declaim (inline lisp-closure-instance))
(defun lisp-closure-instance (closure)
  (cffi:foreign-slot-value closure '(:struct lisp-closure) :object))
(defun (setf lisp-closure-instance) (value closure)
  (setf (cffi:foreign-slot-value closure
                                 '(:struct lisp-closure) :object) value))

(declaim (inline lisp-closure-function-id))
(defun lisp-closure-function-id (closure)
  (cffi:foreign-slot-value closure '(:struct lisp-closure) :function-id))
(defun (setf lisp-closure-function-id) (value closure)
  (setf (cffi:foreign-slot-value closure
                                 '(:struct lisp-closure) :function-id) value))

;;; ----------------------------------------------------------------------------

;; Functions to implemented callbacks with CREATE-CLOSURE

(cffi:defcfun ("g_closure_new_simple" closure-new-simple)
    (:pointer (:struct closure))
  (sizeof-closure :uint)
  (data :pointer))

(cffi:defcfun ("g_closure_add_finalize_notifier" closure-add-finalize-notifier)
    :void
  (closure (:pointer (:struct closure)))
  (notify-data :pointer)
  (notify-func :pointer))

(cffi:defcfun ("g_closure_set_marshal" closure-set-marshal) :void
  (closure (:pointer (:struct closure)))
  (marshal :pointer))

;;; ----------------------------------------------------------------------------

;; Finalization notifier function registered from CREATE-CLOSURE

#+nil
(defun finalize-lisp-closure (closure)
  (let* ((function-id (lisp-closure-function-id closure))
         (ptr (lisp-closure-instance closure))
         (object (get-gobject-for-pointer ptr)))
    (when object
      (delete-handler-from-object object function-id))))

#+nil
(cffi:defcallback lisp-closure-finalize :void
    ((data :pointer)
     (closure (:pointer (:struct lisp-closure))))
  (declare (ignore data))
  (finalize-lisp-closure closure))

;;; ----------------------------------------------------------------------------

(defun finalize-lisp-closure-for-instance (closure)
  (let ((function-id (lisp-closure-function-id closure))
        (instance (lisp-closure-instance closure)))
    (when instance
      (delete-handler-from-instance instance function-id))))

(cffi:defcallback lisp-closure-finalize-for-instance :void
    ((data :pointer)
     (closure (:pointer (:struct lisp-closure))))
  (declare (ignore data))
  (finalize-lisp-closure-for-instance closure))

;;; ----------------------------------------------------------------------------

;; GClosureMarshal function used for CREATE-CLOSURE

;; Helper function to collect arguments from gvalues
(defun parse-closure-arguments (count-of-args args)
  (iter (for i from 0 below count-of-args)
        (collect (value-get (cffi:mem-aptr args '(:struct value) i)))))

(defun call-with-restarts (func args)
  (restart-case
    (apply func args)
    (return-from-closure (&optional v)
                         :report "Return value from closure" v)))

(cffi:defcallback lisp-closure-marshal-for-instance :void
    ((closure (:pointer (:struct lisp-closure)))
     (return-value (:pointer (:struct value)))
     (count-of-args :uint)
     (args (:pointer (:struct value)))
     (invocation-hint :pointer)
     (marshal-data :pointer))
  (declare (ignore invocation-hint marshal-data))
  (let* ((args (parse-closure-arguments count-of-args args))
         (function-id (lisp-closure-function-id closure))
         (instance (lisp-closure-instance closure))
         (return-type (and (not (cffi:null-pointer-p return-value))
                           (value-type return-value)))
         (func (retrieve-handler-from-instance instance function-id))
         (result (call-with-restarts func args)))
    (when return-type
      (setf (value-get return-value return-type) result))))

;;; ----------------------------------------------------------------------------

;; Called from SIGNAL-CONNECT to create the callback function

#+nil
(defun create-closure (object fn)
  (let ((function-id (save-handler-to-object object fn))
        (closure (closure-new-simple
                     (cffi:foreign-type-size '(:struct lisp-closure))
                     (cffi:null-pointer))))
    (setf (lisp-closure-function-id closure)
          function-id
          (lisp-closure-instance closure)
          (object-pointer object))
    (closure-add-finalize-notifier closure
                                   (cffi:null-pointer)
                                   (cffi:callback lisp-closure-finalize))
    (closure-set-marshal closure (cffi:callback lisp-closure-marshal))
    closure))

(defun create-closure-for-instance (instance func)
  (let* ((function-id (save-handler-to-instance instance func))
         (size (cffi:foreign-type-size '(:struct lisp-closure)))
         (closure (closure-new-simple size (cffi:null-pointer))))
    (setf (lisp-closure-function-id closure)
          function-id
          (lisp-closure-instance closure)
          instance)
    (closure-add-finalize-notifier
            closure
            (cffi:null-pointer)
            (cffi:callback lisp-closure-finalize-for-instance))
    (closure-set-marshal
            closure
            (cffi:callback lisp-closure-marshal-for-instance))
    closure))

;;; ----------------------------------------------------------------------------
;;; GSignalFlags
;;; ----------------------------------------------------------------------------

(cffi:defbitfield signal-flags
  :run-first
  :run-last
  :run-cleanup
  :no-recurse
  :detailed
  :action
  :no-hooks
  :must-collect
  :deprecated)

#+liber-documentation
(setf (liber:alias-for-symbol 'signal-flags)
      "Bitfield"
      (liber:symbol-documentation 'signal-flags)
 "@version{2025-07-12}
  @begin{declaration}
(cffi:defbitfield signal-flags
  :run-first
  :run-last
  :run-cleanup
  :no-recurse
  :detailed
  :action
  :no-hooks
  :must-collect
  :deprecated)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:run-first]{Invoke the object method handler in the first emission
        stage.}
      @entry[:run-last]{Invoke the object method handler in the third emission
        stage.}
      @entry[:run-cleanup]{Invoke the object method handler in the last emission
        stage.}
      @entry[:no-recurse]{Signals being emitted for an object while currently
        being in emission for this very object will not be emitted recursively,
        but instead cause the first emission to be restarted.}
      @entry[:detailed]{The signal supports @code{::detail} appendices to the
        signal name upon handler connections and emissions.}
      @entry[:action]{Action signals are signals that may freely be emitted on
        alive objects from user code via the @fun{g:signal-emit} function and
        friends, without the need of being embedded into extra code that
        performs pre or post emission adjustments on the object. They can also
        be thought of as object methods which can be called generically by
        third-party code.}
      @entry[:no-hooks]{No emissions hooks are supported for this signal.}
      @entry[:must-collect]{Varargs signal emission will always collect the
        arguments, even if there are no signal handlers connected.}
      @entry[:deprecated]{The signal is deprecated and will be removed in a
        future version.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The signal flags are used to specify the behaviour of the signal, the
    overall signal description outlines how the flags control the stages of a
    signal emission.
  @end{short}
  @see-function{g:signal-query}
  @see-function{g:signal-emit}")

(export 'signal-flags)

;;; ----------------------------------------------------------------------------
;;; GConnectFlags
;;; ----------------------------------------------------------------------------

(cffi:defbitfield connect-flags
  :after
  :swapped)

#+liber-documentation
(setf (liber:alias-for-symbol 'connect-flags)
      "Bitfield"
      (liber:symbol-documentation 'connect-flags)
 "@version{2025-07-12}
  @begin{declaration}
(cffi:defbitfield connect-flags
  :after
  :swapped)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:after]{Whether the handler should be called before or after the
        default handler of the signal.}
      @entry[:swapped]{Whether the instance and data should be swapped when
        calling the handler.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The connection flags are used to specify the behaviour of the connection
    of the signal.
  @end{short}
  @see-function{g:signal-connect}")

(export 'connect-flags)

;;; ----------------------------------------------------------------------------
;;; GSignalQuery
;;; ----------------------------------------------------------------------------

;; TODO: The documentation for the Lisp structure does not work as expected.
;; Improve this.

(cffi:defcstruct %signal-query
  (signal-id :uint)
  (signal-name :string)
  (owner-type type-t)
  (signal-flags signal-flags)
  (return-type (type-t :mangled-p t))
  (n-params :uint)
  (param-types (:pointer (type-t :mangled-p t))))

(defstruct signal-query
  signal-id
  signal-name
  owner-type
  signal-flags
  return-type
  param-types
  signal-detail)

#+liber-documentation
(setf (documentation 'signal-query 'type)
 "@version{2025-09-27}
  @begin{declaration}
(defstruct signal-query
  signal-id
  signal-name
  owner-type
  signal-flags
  return-type
  param-types
  signal-detail)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[signal-id]{The unsinged integer for the signal ID of the signal
        being queried, or 0 if the signal to be queried was unknown.}
      @entry[signal-name]{The string for the signal name.}
      @entry[owner-type]{The interface/instance @class{g:object} type that this
        signal can be emitted for.}
      @entry[signal-flags]{The signal flags of type @symbol{g:signal-flags}.}
      @entry[return-type]{The return @class{g:type-t} type ID for user
        callbacks.}
      @entry[param-types]{The list with the individual parameter types for user
        callbacks.}
      @entry[signal-detail]{The string for the signal detail.}
    @end{table}
  @end{values}
  @begin{short}
    A structure holding in-depth information for a specific signal.
  @end{short}
  It is filled in by the @fun{g:signal-query} function.
  @see-constructor{g:signal-query}
  @see-slot{g:signal-query-signal-id}
  @see-slot{g:signal-query-signal-name}
  @see-slot{g:signal-query-owner-type}
  @see-slot{g:signal-query-signal-flags}
  @see-slot{g:signal-query-return-type}
  @see-slot{g:signal-query-param-types}
  @see-slot{g:signal-query-signal-detail}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-symbol{g:signal-flags}
  @see-function{g:signal-query}")

(defmethod print-object ((instance signal-query) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (instance stream)
         (format stream
                 "Signal [#~A] ~A ~A.~A~@[::~A~] (~{~A~^, ~})~@[ [~{~A~^, ~}]~]"
                 (signal-query-signal-id instance)
                 (glib:gtype-name (signal-query-return-type instance))
                 (glib:gtype-name (signal-query-owner-type instance))
                 (signal-query-signal-name instance)
                 (signal-query-signal-detail instance)
                 (mapcar #'glib:gtype-name (signal-query-param-types instance))
                 (signal-query-signal-flags instance)))))

(export 'signal-query)

;;; ----------------------------------------------------------------------------
;;; Accessor details
;;; ----------------------------------------------------------------------------

;;; --- g:signal-query-signal-id -----------------------------------------------

;; Slot documentation does not work for Lisp structures. Improve this.

#+liber-documentation
(setf (documentation (liber:slot-documentation "signal-id" 'signal-query) t)
 "The @code{signal-id} slot (Read) @br{}
  The unsigned integer for the signal ID of the signal being queried, or 0
  if the signal to be queried was unknown.")

#+liber-documentation
(setf (liber:alias-for-function 'signal-query-signal-id)
      "Accessor"
      (documentation 'signal-query-signal-id 'function)
 "@version{2025-09-27}
  @syntax{(g:signal-query-signal-id instance) => id}
  @argument[instance]{a @struct{g:signal-query} instance}
  @argument[id]{an unsigned integer for the signal ID of the signal being
    queried, or 0 if the signal to be queried was unknown}
  @begin{short}
    The accessor for the @slot[g:signal-query]{signal-id} slot of the
    @class{g:signal-query} structure.
  @end{short}
  See the @fun{g:signal-query} function.
  @see-function{g:signal-query}")

(export 'signal-query-signal-id)

;;; --- g:signal-query-signal-name ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'signal-query-signal-name)
      "Accessor"
      (documentation 'signal-query-signal-name 'function)
 "@version{2025-09-27}
  @syntax{(g:signal-query-signal-name instance) => name}
  @argument[instance]{a @struct{g:signal-query} instance}
  @argument[name]{a string for the signal name}
  @begin{short}
    The accessor for the @slot[g:signal-query]{signal-name} slot of the
    @class{g:signal-query} structure.
  @end{short}
  See the @fun{g:signal-query} function.
  @see-function{g:signal-query}")

(export 'signal-query-signal-name)

;;; --- g:signal-query-owner-type ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'signal-query-owner-type)
      "Accessor"
      (documentation 'signal-query-owner-type 'function)
 "@version{2025-09-27}
  @syntax{(g:signal-query-owner-type instance) => owner-type}
  @argument[instance]{a @struct{g:signal-query} instance}
  @argument[owner-type]{a @class{g:type-t} type ID that this signal can be
    emitted for}
  @begin{short}
    The accessor for the @slot[g:signal-query]{owner-type} slot of the
    @class{g:signal-query} structure.
  @end{short}
  See the @fun{g:signal-query} function.
  @see-class{g:type-t}
  @see-function{g:signal-query}")

(export 'signal-query-owner-type)

;;; --- g:signal-query-signal-flags --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'signal-query-signal-flags)
      "Accessor"
      (documentation 'signal-query-signal-flags 'function)
 "@version{2025-09-27}
  @syntax{(g:signal-query-signal-flags instance) => flags}
  @argument[instance]{a @struct{g:signal-query} instance}
  @argument[flags]{a @symbol{g:signal-flags} value for the signal flags}
  @begin{short}
    The accessor for the @slot[g:signal-query]{signal-flags} slot of the
    @class{g:signal-query} structure.
  @end{short}
  See the @fun{g:signal-query} function.
  @see-symbol{g:signal-flags}
  @see-function{g:signal-query}")

(export 'signal-query-signal-flags)

;;; --- g:signal-query-return-type ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'signal-query-return-type)
      "Accessor"
      (documentation 'signal-query-return-type 'function)
 "@version{2025-09-27}
  @syntax{(g:signal-query-return-type instance) => return-type}
  @argument[instance]{a @struct{g:signal-query} instance}
  @argument[return-type]{a @class{g:type-t} type ID with the return type for
    user callbacks}
  @begin{short}
    The accessor for the @slot[g:signal-query]{return-type} slot of the
    @class{g:signal-query} structure.
  @end{short}
  See the @fun{g:signal-query} function.
  @see-class{g:type-t}
  @see-function{g:signal-query}")

(export 'signal-query-return-type)

;;; --- g:signal-query-param-types ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'signal-query-param-types)
      "Accessor"
      (documentation 'signal-query-param-types 'function)
 "@version{2025-09-27}
  @syntax{(g:signal-query-param-types instance) => param-types}
  @argument[instance]{a @struct{g:signal-query} instance}
  @argument[param-types]{a list with the individual parameter @class{g:type-t}
    type IDs for user callbacks}
  @begin{short}
    The accessor for the @slot[g:signal-query]{param-types} slot of the
    @class{g:signal-query} structure.
  @end{short}
  See the @fun{g:signal-query} function.
  @see-class{g:type-t}
  @see-function{g:signal-query}")

(export 'signal-query-param-types)

;;; --- g:signal-query-signal-detail -------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'signal-query-signal-detail)
      "Accessor"
      (documentation 'signal-query-signal-detail 'function)
 "@version{2025-09-27}
  @syntax{(g:signal-query-signal-detail instance) => detail}
  @argument[instance]{a @struct{g:signal-query} instance}
  @argument[detail]{a string for the signal detail}
  @begin{short}
    The accessor for the @slot[g:signal-query]{signal-detail} slot of the
    @class{g:signal-query} structure.
  @end{short}
  See the @fun{g:signal-query} function.
  @see-function{g:signal-query}")

(export 'signal-query-signal-detail)

;;; ----------------------------------------------------------------------------
;;; GSignalAccumulator
;;;
;;; The signal accumulator is a special callback function that can be used to
;;; collect return values of the various callbacks that are called during a
;;; signal emission.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GSignalEmissionHook
;;;
;;; A simple function pointer to get invoked when the signal is emitted.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_new
;;;
;;; Creates a new signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_newv                                           not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_new_valist
;;;
;;; Creates a new signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_set_va_marshaller
;;;
;;; Change the GSignalCVaMarshaller used for a given signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_query
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_query" %signal-query) :void
  (signal-id :uint)
  (query (:pointer (:struct %signal-query))))

(defun signal-query (id)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[id]{an unsigned integer for the signal ID of the signal to query
    information for}
  @return{The @struct{g:signal-query} instance with the signal info.}
  @begin{short}
    Returns the signal info.
  @end{short}
  Queries the signal system for in-depth information about a specific signal.
  This function will return signal-specific information. If an invalid signal
  ID is passed in the @code{nil} value is returned.
  @see-struct{g:signal-query}"
  (cffi:with-foreign-object (query '(:struct %signal-query))
    (%signal-query id query)
    (cffi:with-foreign-slots ((signal-id
                               signal-name
                               owner-type
                               signal-flags
                               return-type
                               n-params
                               param-types)
                              query (:struct %signal-query))
      (unless (= 0 signal-id)
        (make-signal-query :signal-id signal-id
                           :signal-name signal-name
                           :owner-type owner-type
                           :signal-flags signal-flags
                           :return-type return-type
                           :param-types
                           (iter (for i from 0 below n-params)
                                 (for param-type =
                                      (cffi:mem-aref param-types
                                                     '(type-t :mangled-p t)
                                                     i))
                                 (collect param-type)))))))

(export 'signal-query)

;;; ----------------------------------------------------------------------------
;;; g_signal_lookup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_lookup" signal-lookup) :uint
 #+liber-documentation
 "@version{2025-09-27}
  @argument[name]{a string for the name of the signal}
  @argument[itype]{a @class{g:type-t} type ID that the signal operates on}
  @return{The unsigned integer for the identifying number of the signal, or 0
    if no signal was found.}
  @begin{short}
    Given the name of the signal and the type of object it connects to, gets
    the identifying integer of the signal.
  @end{short}
  Emitting the signal by number is somewhat faster than using the name each
  time. Also tries the ancestors of the given type.
  @begin[Examples]{dictionary}
    @begin{pre}
(g:signal-lookup \"notify\" \"GObject\")
=> 1
(g:signal-lookup \"notify\" \"GtkWidget\")
=> 1
(g:signal-lookup \"unknown\" \"GObject\")
=> 0
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:signal-name}
  @see-function{g:signal-query}
  @see-function{g:signal-list-ids}"
  (name :string)
  (itype type-t))

(export 'signal-lookup)

;;; ----------------------------------------------------------------------------
;;; g_signal_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_name" signal-name) :string
 #+liber-documentation
 "@version{2025-09-27}
  @argument[id]{an unsigned integer for the identifying number of the signal}
  @return{The string for the signal name, or @code{nil} if the signal number
    was invalid.}
  @begin{short}
    Given the identifier of the signal, finds its name.
  @end{short}
  Two different signals may have the same name, if they have differing types.
  @begin[Examples]{dictionary}
    Get the signal ID for the @code{\"startup\"} signal and then get the name
    for the ID:
    @begin{pre}
(g:signal-lookup \"startup\" \"GApplication\")
=> 95
(g:signal-name *)
=> \"startup\"
    @end{pre}
    List the IDs for an application object and retrieves the names of the
    signals:
    @begin{pre}
(g:signal-list-ids \"GApplication\")
=> (97 95 96 98 99 100 101)
(mapcar #'g:signal-name *)
=> (\"activate\" \"startup\" \"shutdown\" \"open\" \"command-line\"
    \"handle-local-options\" \"name-lost\")
    @end{pre}
  @end{dictionary}
  @see-function{g:signal-query}
  @see-function{g:signal-lookup}
  @see-function{g:signal-list-ids}"
  (id :uint))

(export 'signal-name)

;;; ----------------------------------------------------------------------------
;;; g_signal_list_ids
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_list_ids" %signal-list-ids) (:pointer :uint)
  (itype type-t)
  (n-ids (:pointer :uint)))

(defun signal-list-ids (itype)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[itype]{a @class{g:type-t} type ID}
  @return{The list of unsigned integer for the signal IDs.}
  @begin{short}
    Lists the signals by ID that a certain instance or interface type created.
  @end{short}
  Further information about the signals can be acquired through the
  @fun{g:signal-query} function.
  @begin[Examples]{dictionary}
    Get the IDs for a window widget in and show the names of the signals:
    @begin{pre}
(mapcar #'g:signal-name (g:signal-list-ids \"GApplication\"))
=> (\"activate\" \"startup\" \"shutdown\" \"open\" \"command-line\"
    \"handle-local-options\" \"name-lost\")
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-function{g:signal-query}"
  (when (or (type-is-object itype)
            (type-is-interface itype))
    (cffi:with-foreign-object (n-ids :uint)
    (let ((ids (%signal-list-ids itype n-ids)))
      (unwind-protect
        (iter (for i from 0 below (cffi:mem-ref n-ids :uint))
              (collect (cffi:mem-aref ids :uint i)))
        (glib:free ids))))))

(export 'signal-list-ids)

;;; ----------------------------------------------------------------------------
;;; g_signal_emitv                                          internal use only
;;; ----------------------------------------------------------------------------

;; Called from signal-emit. For internal use and not exported.

(cffi:defcfun ("g_signal_emitv" %signal-emitv) :void
  (instance-and-params (:pointer (:struct value)))
  (signal-id :uint)
  (detail glib:quark-as-string)
  (return-value (:pointer (:struct value))))

;;; ----------------------------------------------------------------------------
;;; g_signal_emit
;;; ----------------------------------------------------------------------------

#+nil
(defun signal-emit (instance detailed &rest args)
  (let* ((gtype (type-from-instance (object-pointer instance)))
         (query (signal-parse-name gtype detailed)))
    (unless query
      (error "Signal ~A not found on instance ~A" detailed instance))
    (let ((count (length (signal-query-param-types query))))
      (assert (= count (length args)))
      (cffi:with-foreign-object (params '(:struct value) (1+ count))

        (set-gvalue (cffi:mem-aptr params '(:struct value) 0)
                    instance
                    gtype)

        (iter (for i from 0 below count)
              (for arg in args)
              (for gtype in (signal-query-param-types query))
              (set-gvalue (cffi:mem-aptr params '(:struct value) (1+ i))
                          arg
                          gtype))
        (prog1
          (if (eq (signal-query-return-type query) (glib:gtype "void"))
              ;; Emit a signal which has no return value
              (let ((detail (signal-query-signal-detail query)))
                (%signal-emitv params
                               (signal-query-signal-id query)
                               (if detail detail (cffi:null-pointer))
                               (cffi:null-pointer)))

              ;; Emit a signal which has a return value
              (cffi:with-foreign-object (return-value '(:struct value))
                (value-init return-value
                            (signal-query-return-type query))
                (let ((detail (signal-query-signal-detail query)))
                  (%signal-emitv params
                                 (signal-query-signal-id query)
                                 (if detail detail (cffi:null-pointer))
                                 return-value))
                (prog1
                  ;; Return value of the signal
                  (value-get return-value)
                  (value-unset return-value))))
          (iter (for i from 0 below (1+ count))
                (value-unset (cffi:mem-aptr params '(:struct value) i))))))))

(defun signal-emit (instance detailed &rest args)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[instance]{a @class{g:object} instance the signal is being emitted
    on}
  @argument[detailed]{a string for the detailed signal name}
  @argument[args]{parameters to be passed to the signal}
  @return{The return value of the signal.}
  @short{Emits a signal.}
  Note that the @fun{g:signal-emit} function resets the return value to the
  default if no handlers are connected.
  @begin[Lisp implementation]{dictionary}
    In the Lisp implementation this function takes not the signal ID but the
    detailed signal name as an argument. For this case the C library has the
    @code{g_signal_emit_by_name()} function, which is not implemented in the
    Lisp binding.

    At this time, setting a @code{GParam} value is not implemented in the
    Lisp binding. Therefore, you can not emit a @code{\"notify::<property>\"}
    signal on an instance.
  @end{dictionary}
  @see-class{g:object}"
  (let* ((gtype (type-from-instance (object-pointer instance)))
         (query (signal-parse-name gtype detailed)))
    (unless query
      (error "Signal ~A not found on instance ~A" detailed instance))
    (let ((nparams (length (signal-query-param-types query))))
      (assert (= nparams (length args)))
      (cffi:with-foreign-object (params '(:struct value) (1+ nparams))
        (set-gvalue (cffi:mem-aptr params '(:struct value) 0)
                    instance
                    gtype)
        (iter (for i from 0 below nparams)
              (for arg in args)
              (for gtype in (signal-query-param-types query))
              (set-gvalue (cffi:mem-aptr params '(:struct value) (1+ i))
                          arg
                          gtype))
        (let ((detail (signal-query-signal-detail query))
              (return-type (signal-query-return-type query)))
          (unwind-protect
            (if (eq return-type (glib:gtype "void"))
                ;; Emit a signal which has no return value
                (%signal-emitv params
                               (signal-query-signal-id query)
                               (or detail (cffi:null-pointer))
                               (cffi:null-pointer))
                ;; Emit a signal which has a return value
                (with-value (return-value return-type)
                  (%signal-emitv params
                                 (signal-query-signal-id query)
                                 (or detail (cffi:null-pointer))
                                 return-value)
                  ;; Return value of the signal
                  (get-gvalue return-value return-type)))
            (iter (for i from 0 below (1+ nparams))
                  (value-unset (cffi:mem-aptr params '(:struct value) i)))))))))

(export 'signal-emit)

;;; ----------------------------------------------------------------------------
;;; g_signal_emit_by_name
;;;
;;; Emits a signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_emit_valist
;;;
;;; Emits a signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_connect
;;; ----------------------------------------------------------------------------

(defun signal-connect (instance signal handler &key after)
 #+liber-documentation
 "@version{2025-08-31}
  @argument[instance]{a @class{g:object} instance to connect to}
  @argument[signal]{a string of the form @code{\"signal-name::detail\"}}
  @argument[handler]{a Lisp callback function to connect}
  @argument[after]{if @em{true} the handler is called after the default handler}
  @return{The unsigned long integer for the handler ID.}
  @begin{short}
    Connects a Lisp callback function to a signal for a particular object.
  @end{short}
  The handler will be called before the default handler of the signal. If the
  @arg{after} keyword argument is @em{true}, the handler will be called after
  the default handler of the signal.
  @begin[Lisp implmentation]{dictionary}
    The C library knows in addition the @code{g_signal_connect_after()}
    function, which is implemented with the @arg{after} keyword argument.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Connect a Lisp lambda function to the @code{\"toggled\"} signal of a toggle
    button:
    @begin{pre}
(g:signal-connect button \"toggled\"
   (lambda (widget)
     (if (gtk:toggle-button-active widget)
         (progn
           ;; If control reaches here, the toggle button is down
         )
        (progn
           ;; If control reaches here, the toggle button is up
         ))))
    @end{pre}
    If it is necessary to have a separate function which needs user data, the
    following implementation is possible:
    @begin{pre}
(defun separate-event-handler (widget arg1 arg2 arg3)
  [ here is the code of the event handler ] )

(g:signal-connect window \destroy\"
                  (lambda (widget)
                    (separate-event-handler widget arg1 arg2 arg3)))
    @end{pre}
    If no extra data is needed, but the callback function should be separated
    out than it is also possible to implement something like:
    @begin{pre}
(g:signal-connect window \"destroy\" #'separate-event-handler)
    @end{pre}
  @end{dictionary}
  @see-class{g:object}"
  (let ((instance (if (cffi:pointerp instance)
                      instance
                      (object-pointer instance))))
    (%signal-connect-closure instance
                             signal
                             (create-closure-for-instance instance handler)
                             after)))

(export 'signal-connect)

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_after                                  not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_swapped
;;;
;;; Connects a GCallback function to a signal for a particular object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_object
;;;
;;; This is similar to g_signal_connect_data(), but uses a closure which ensures
;;; that the gobject stays alive during the call to c_handler by temporarily
;;; adding a reference count to gobject.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_data
;;;
;;; Connects a GCallback function to a signal for a particular object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_closure
;;; ----------------------------------------------------------------------------

;; Called from G:SIGNAL-CONNECT. For internal use and not exported.

(cffi:defcfun ("g_signal_connect_closure" %signal-connect-closure) :ulong
  (instance :pointer)
  (detailed-signal :string)
  (closure (:pointer (:struct closure)))
  (after :boolean))

;;; ----------------------------------------------------------------------------
;;; g_signal_connect_closure_by_id
;;;
;;; Connects a closure to a signal for a particular object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_block
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_handler_block" signal-handler-block) :void
 #+liber-documentation
 "@version{2024-06-19}
  @argument[instance]{a @class{g:object} instance to block the signal handler
    of}
  @argument[handler-id]{an unsigned integer handler ID of the handler to be
    blocked}
  @begin{short}
    Blocks a handler of an instance so it will not be called during any signal
    emissions unless it is unblocked again.
  @end{short}
  Thus \"blocking\" a signal handler means to temporarily deactive it. A signal
  handler has to be unblocked exactly the same amount of times it has been
  blocked before to become active again.

  The @arg{handler-id} has to be a valid signal handler ID, connected to a
  signal of @arg{instance}.
  @see-class{g:object}
  @see-function{g:signal-handler-unblock}"
  (instance object)
  (handler-id :ulong))

(export 'signal-handler-block)

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_unblock
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_handler_unblock" signal-handler-unblock) :void
 #+liber-documentation
 "@version{2024-06-19}
  @argument[instance]{a @class{g:object} instance to unblock the signal
    handler of}
  @argument[handler-id]{an unsigned integer handler ID of the handler to be
    unblocked}
  @begin{short}
    Undoes the effect of a previous call of the @fun{g:signal-handler-block}
    function.
  @end{short}
  A blocked handler is skipped during signal emissions and will not be invoked,
  unblocking it (for exactly the amount of times it has been blocked before)
  reverts its \"blocked\" state, so the handler will be recognized by the signal
  system and is called upon future or currently ongoing signal emissions, since
  the order in which handlers are called during signal emissions is
  deterministic, whether the unblocked handler in question is called as part
  of a currently ongoing emission depends on how far that emission has
  proceeded yet.

  The @arg{handler-id} has to be a valid ID of a signal handler that is
  connected to a signal of @arg{instance} and is currently blocked.
  @see-class{g:object}
  @see-function{g:signal-handler-block}"
  (instance object)
  (handler-id :ulong))

(export 'signal-handler-unblock)

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_disconnect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_handler_disconnect" signal-handler-disconnect) :void
 #+liber-documentation
 "@version{2025-09-27}
  @argument[instance]{a @class{g:object} instance to remove the signal handler
    from}
  @argument[handler-id]{an unsigned long integer for the handler ID of the
    handler to be disconnected}
  @begin{short}
    Disconnects a handler from an instance so it will not be called during any
    future or currently ongoing emissions of the signal it has been connected
    to.
  @end{short}
  The @arg{handler-id} becomes invalid and may be reused.

  The @arg{handler-id} has to be a valid signal handler ID, connected to a
  signal of @arg{instance}.
  @see-class{g:object}
  @see-function{g:signal-connect}"
  (object object)
  (handler-id :ulong))

(export 'signal-handler-disconnect)

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_find
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_handler_find" %signal-handler-find) :ulong
  (instance object)
  (mask :int)
  (signal-id :uint)
  (detail glib:quark-as-string)
  (closure (:pointer (:struct closure)))
  (func :pointer)
  (data :pointer))

(defun signal-handler-find (instance signal-id)
 #+liber-documentation
 "@version{2025-09-27}
  @argument[instance]{a @class{g:object} instance owning the signal handler
    to be found}
  @argument[signal-id]{an unsigned integer for a signal ID the handler has to
    be connected to}
  @return{The unsigned integer for a valid non-0 signal handler ID for a
    successful match.}
  @begin{short}
    Finds the first signal handler that matches the given @arg{signal-id}.
  @end{short}
  If no handler was found, 0 is returned.
  @begin[Lisp implementation]{dictionary}
    In the Lisp implementation only the search for a signal ID is implemented.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar action (make-instance 'g:simple-action)) => ACTION
;; Get the signal ID
(g:signal-lookup \"activate\" \"GSimpleAction\") => 139
;; No signal handler connected
(g:signal-handler-find action 139) => 0
;; Connect signal handler
(g:signal-connect action \"activate\"
                  (lambda (action param) action param))
=> 118534
(g:signal-handler-find action 139) => 118534
;; Disconnect signal handler
(g:signal-handler-disconnect action *)
(g:signal-handler-find action 139) => 0
    @end{pre}
  @end{dictionary}
  @see-class{g:object}"
  (%signal-handler-find instance
                        1                   ; for :id
                        signal-id
                        (cffi:null-pointer)
                        (cffi:null-pointer)
                        (cffi:null-pointer)
                        (cffi:null-pointer)))

(export 'signal-handler-find)

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_block_matched
;;;
;;; Blocks all handlers on an instance that match a certain selection criteria.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_unblock_matched
;;;
;;; Unblocks all handlers on an instance that match a certain selection
;;; criteria.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_disconnect_matched
;;;
;;; Disconnects all handlers on an instance that match a certain selection
;;; criteria.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handler_is_connected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_handler_is_connected" signal-handler-is-connected)
    :boolean
 #+liber-documentation
 "@version{2025-09-27}
  @argument[instance]{a @class{g:object} instance where a signal handler is
    sought}
  @argument[handler-id]{an unsigned long integer for the handler ID}
  @return{The boolean whether @arg{handler-id} identifies a handler connected to
    @arg{instance}.}
  @begin{short}
    Returns whether @arg{handler-id} is the ID of a handler connected to
    @arg{instance}.
  @end{short}
  @see-class{g:object}
  @see-function{g:signal-connect}"
  (instance object)
  (handler-id :ulong))

(export 'signal-handler-is-connected)

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_block_by_func
;;;
;;; Blocks all handlers on an instance that match func and data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_unblock_by_func
;;;
;;; Unblocks all handlers on an instance that match func and data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_disconnect_by_func
;;;
;;; Disconnects all handlers on an instance that match func and data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_handlers_disconnect_by_data
;;;
;;; Disconnects all handlers on an instance that match data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_has_handler_pending
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_signal_has_handler_pending" signal-has-handler-pending)
    :boolean
 #+liber-documentation
 "@version{2025-09-27}
  @argument[instance]{a @class{g:object} instance whose signal handlers are
    sought}
  @argument[id]{an unsigned integer for the signal ID}
  @argument[detail]{a string for the detail of a signal name}
  @argument[may-be-blocked]{a boolean whether blocked handlers should count as
    match}
  @return{@em{True} if a handler is connected to the signal, @em{false}
    otherwise.}
  @begin{short}
    Returns whether there are any handlers connected to @arg{instance} for the
    given signal ID and @arg{detail}.
  @end{short}

  If @arg{detail} is @code{nil} then it will only match handlers that were
  connected without detail. If @arg{detail} is non-@code{nil} then it will
  match handlers connected both without detail and with the given @arg{detail}.
  This is consistent with how a signal emitted with detail would be delivered
  to those handlers.

  This also checks for a non-default class closure being installed, as this is
  basically always what you want.

  One example of when you might use this is when the arguments to the signal
  are difficult to compute. A class implementor may opt to not emit the signal
  if no one is attached anyway, thus saving the cost of building the arguments.
  @see-class{g:object}
  @see-type{g:quark-as-string}
  @see-function{g:signal-connect}"
  (instance object)
  (id :uint)
  (detail glib:quark-as-string)
  (may-be-blocked :boolean))

(export 'signal-has-handler-pending)

;;; ----------------------------------------------------------------------------
;;; g_signal_stop_emission
;;; ----------------------------------------------------------------------------

;; This version is not implemented. See g:signal-stop-emission

;;; ----------------------------------------------------------------------------
;;; g_signal_stop_emission_by_name
;;; ----------------------------------------------------------------------------

;; This function is implemented as g:signal-stop-emission

(cffi:defcfun ("g_signal_stop_emission_by_name" signal-stop-emission) :void
 #+liber-documentation
 "@version{2024-06-19}
  @argument[instance]{a @class{g:object} instance whose signal handlers you
    wish to stop}
  @argument[detailed]{a string of the form @code{\"signal-name::detail\"}}
  @begin{short}
    Stops a current emission of the signal.
  @end{short}
  This will prevent the default method from running, if the signal was
  @code{:run-last} and you connected normally, for example, without the
  @code{:after} flag for the @fun{g:signal-connect} function.

  Prints a warning if used on a signal which is not being emitted.
  @begin[Examples]{dictionary}
    As an example of the usage, by connecting the following handler to the
    @code{\"insert-text\"} signal, an application can convert all entry into a
    widget into uppercase.
    @begin{pre}
;; Handler for the \"insert-text\" signal
(setf handlerid
      (g:signal-connect entry \"insert-text\"
          (lambda (editable text length position)
            (g:signal-handler-block editable handlerid)
            (gtk:editable-insert-text editable
                                      (string-upcase text)
                                      (cffi:mem-ref position :intptr))
            (g:signal-stop-emission editable \"insert-text\")
            (g:signal-handler-unblock editable handlerid))))
    @end{pre}
  @end{dictionary}
  @see-class{g:object}
  @see-function{g:signal-connect}"
  (instance object)
  (detailed :string))

(export 'signal-stop-emission)

;;; ----------------------------------------------------------------------------
;;; g_signal_override_class_closure
;;;
;;; Overrides the class closure, that is the default handler, for the given
;;; signal for emissions on instances of instance_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_chain_from_overridden
;;;
;;; Calls the original class closure of a signal. This function should only be
;;; called from an overridden class closure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_new_class_handler
;;;
;;; Creates a new signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_override_class_handler
;;;
;;; Overrides the class closure, that is the default handler, for the given
;;; signal for emissions on instances of instance_type with callback
;;; class_handler.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_chain_from_overridden_handler
;;;
;;; Calls the original class closure of a signal. This function should only be
;;; called from an overridden class closure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_add_emission_hook
;;;
;;; Adds an emission hook for a signal, which will get called for any emission
;;; of that signal, independent of the instance.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_remove_emission_hook
;;;
;;; Deletes an emission hook.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_is_valid_name
;;;
;;; Validate a signal name.
;;;
;;; Since 2.66
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_parse_name
;;;
;;; Internal function to parse a signal name into its signal_id and detail
;;; quark.
;;; ----------------------------------------------------------------------------

;; Returns a G:SIGNAL-QUERY instance for the given GTYPE and DETAILED signal
;; name. The Lisp function does not work as documented. The function is
;; used in G:SIGNAL-EMIT and not exported.

(cffi:defcfun ("g_signal_parse_name" %signal-parse-name) :boolean
 (detailed :string)
 (gtype type-t)
 (signal-id-p (:pointer :uint))
 (detail (:pointer glib:quark-as-string))
 (force-detail-quark :boolean))

(defun signal-parse-name (gtype detailed)
  (cffi:with-foreign-objects ((signal-id :uint) (detail 'glib:quark-as-string))
    (when (%signal-parse-name detailed gtype signal-id detail t)
      (let ((query (signal-query (cffi:mem-ref signal-id :uint))))
        (setf (signal-query-signal-detail query)
              (cffi:mem-ref detail 'glib:quark-as-string))
        query))))

;;; ----------------------------------------------------------------------------
;;; g_signal_get_invocation_hint
;;;
;;; Returns the invocation hint of the innermost signal emission of instance.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_type_cclosure_new
;;;
;;; Creates a new closure which invokes the function found at the offset
;;; struct_offset in the class structure of the interface or classed type
;;; identified by itype.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_accumulator_first_wins
;;;
;;; A predefined GSignalAccumulator for signals intended to be used as a hook
;;; for application code to provide a particular value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_signal_accumulator_true_handled
;;;
;;; A predefined GSignalAccumulator for signals that return a boolean values.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_clear_signal_handler
;;;
;;; Disconnects a handler from instance so it will not be called during any
;;; future or currently ongoing emissions of the signal it has been connected
;;; to.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.signals.lisp ---------------------------------------
