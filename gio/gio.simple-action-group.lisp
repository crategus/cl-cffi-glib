;;; ----------------------------------------------------------------------------
;;; gio.simple-action-group.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;; GSimpleActionGroup
;;;
;;;     A simple GActionGroup implementation
;;;
;;; Types and Values
;;;
;;;     GSimpleActionGroup
;;;
;;; Functions
;;;
;;;     g_simple_action_group_new
;;;     g_simple_action_group_lookup                       deprecated
;;;     g_simple_action_group_insert                       deprecated
;;;     g_simple_action_group_remove                       deprecated
;;;     g_simple_action_group_add_entries                  deprecated
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GSimpleActionGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GSimpleActionGroup implements GActionGroup and GActionMap.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GSimpleActionGroup
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GSimpleActionGroup" simple-action-group
  (:superclass gobject:object
   :export t
   :interfaces ("GActionGroup"
                "GActionMap")
   :type-initializer "g_simple_action_group_get_type")
  nil)

#+liber-documentation
(setf (documentation 'simple-action-group 'type)
 "@version{#2024-12-27}
  @begin{short}
    The @class{g:simple-action-group} class is a hash table filled with
    @class{g:action} instances, implementing the @class{g:action-group} and
    @class{g:action-map} interfaces.
  @end{short}
  @see-class{g:action}
  @see-class{g:action-map}
  @see-class{g:action-group}")

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_new
;;; ----------------------------------------------------------------------------

(declaim (inline simple-action-group-new))

(defun simple-action-group-new ()
 #+liber-documentation
 "@version{2024-12-27}
  @return{The new @class{g:simple-action-group} instance.}
  @short{Creates a new, empty, action group.}
  @see-class{g:simple-action-group}"
  (make-instance 'simple-action-group))

(export 'simple-action-group-new)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_lookup                            deprecated
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_simple_action_group_lookup" simple-action-group-lookup)
    (gobject:object action)
 #+liber-documentation
 "@version{#2024-12-27}
  @argument[group]{a @class{g:simple-action-group} instance}
  @argument[name]{a string with the name of an action}
  @return{The @class{g:action} instance, or @code{nil}.}
  @begin{short}
    Looks up the action with the name @arg{name} in the action group.
  @end{short}
  If no such action exists, returns @code{nil}.
  @begin[Warning]{dictionary}
    The @fun{g:simple-action-group-lookup} function has been deprecated since
    version 2.38 and should not be used in newly written code. Use the
    @fun{g:action-map-lookup-action} function.
  @end{dictionary}
  @see-class{g:action}
  @see-class{g:simple-action-group}
  @see-function{g:action-map-lookup-action}"
  (group (gobject:object simple-action-group))
  (name :string))

(export 'simple-action-group-lookup)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_insert                            deprecated
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_simple_action_group_insert" simple-action-group-insert) :void
 #+liber-documentation
 "@version{#2024-12-27}
  @argument[group]{a @class{g:simple-action-group} instance}
  @argument[action]{a @class{g:action} instance}
  @begin{short}
    Adds an action to the action group.
  @end{short}
  If the action group already contains an action with the same name as
  @arg{action} then the old action is dropped from the action group. The action
  group takes its own reference on @arg{action}.
  @begin[Warning]{dictionary}
    The @fun{g:simple-action-group-insert} function has been deprecated since
    version 2.38 and should not be used in newly written code. Use the
    @fun{g:action-map-add-action} function.
  @end{dictionary}
  @see-class{g:action}
  @see-class{g:simple-action-group}
  @see-function{g:action-map-add-action}"
  (group (gobject:object simple-action-group))
  (action (gobject:object action)))

(export 'simple-action-group-insert)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_remove                            deprecated
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_simple_action_group_remove" simple-action-group-remove) :void
 #+liber-documentation
 "@version{#2024-12-27}
  @argument[group]{a @class{g:simple-action-group} instance}
  @argument[name]{a string with the name of the action}
  @begin{short}
    Removes the named action from the action group.
  @end{short}
  If no action of this name is in the action group then nothing happens.
  @begin[Warning]{dictionary}
    The @fun{g:simple-action-group-remove} function has been deprecated since
    version 2.38 and should not be used in newly written code. Use the
    @fun{g:action-map-remove-action} function.
  @end{dictionary}
  @see-class{g:action}
  @see-class{g:simple-action-group}
  @see-function{g:action-map-remove-action}"
  (group (gobject:object simple-action-group))
  (name :string))

(export 'simple-action-group-remove)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_group_add_entries                       deprecated
;;; ----------------------------------------------------------------------------

(declaim (inline simple-action-group-add-entries))

(defun simple-action-group-add-entries (group entries)
 #+liber-documentation
 "@version{#2024-12-27}
  @argument[group]{a @class{g:simple-action-group} instance}
  @argument[entries]{a list of descriptions for the actions}
  @begin{short}
    A convenience function for creating multiple @class{g:simple-action}
    instances and adding them to the action group.
  @end{short}
  @begin[Note]{dictionary}
    In the Lisp implementation this function calls the
    @fun{g:action-map-add-action-entries} function. See the documentation of the
    @fun{g:action-map-add-action-entries} function for more information about
    the parameters in the list to describe an action.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{g:simple-action-group-add-entries} function has been deprecated
    since version 2.38 and should not be used in newly written code. Use the
    @fun{g:action-map-add-action-entries} function.
  @end{dictionary}
  @see-class{g:action}
  @see-class{g:simple-action-group}
  @see-function{g:action-map-add-action-entries}"
  (action-map-add-action-entries group entries))

(export 'simple-action-group-add-entries)

;;; --- End of file gio.simple-action-group.lisp -------------------------------
