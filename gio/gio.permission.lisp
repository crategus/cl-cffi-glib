;;; ----------------------------------------------------------------------------
;;; gio.permission.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; version 2.84 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2021 - 2025 Dieter Kaiser
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
;;; GPermission
;;;
;;;     An object representing the permission to perform a certain action
;;;
;;; Types and Values
;;;
;;;     GPermission
;;;
;;; Functions
;;;
;;;     g_permission_get_allowed
;;;     g_permission_get_can_acquire
;;;     g_permission_get_can_release
;;;     g_permission_acquire
;;;     g_permission_acquire_async
;;;     g_permission_acquire_finish
;;;     g_permission_release
;;;     g_permission_release_async
;;;     g_permission_release_finish
;;;     g_permission_impl_update                            not implemented
;;;
;;; Properties
;;;
;;;     allowed
;;;     can-acquire
;;;     can-release
;;;
;;; Object Hierarchy
;;;
;;;    GObject
;;;    ╰── GPermission
;;;        ╰── GSimplePermission
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GPermission
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GPermission" permission
  (:superclass gobject:object
   :export t
   :interfaces ()
   :type-initializer "g_permission_get_type")
  ((allowed
    permission-allowed
    "allowed" "gboolean" t nil)
   (can-acquire
    permission-can-acquire
    "can-acquire" "gboolean" t nil)
   (can-release
    permission-can-release
    "can-release" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'permission 'type)
 "@version{2025-05-26}
  @begin{short}
    The @class{g:permission} class represents the status of the permission of
    the caller to perform a certain action.
  @end{short}
  You can query if the action is currently allowed and if it is possible to
  acquire the permission so that the action will be allowed in the future.
  There is also an API to actually acquire the permission and one to release it.

  As an example, a @class{g:permission} object might represent the ability for
  the user to write to a @class{g:settings} object. This @class{g:permission}
  object could then be used to decide if it is appropriate to show a \"Click
  here to unlock\" button in a dialog and to provide the mechanism to invoke
  when that button is clicked.
  @see-slot{g:permission-allowed}
  @see-slot{g:permission-can-acquire}
  @see-slot{g:permission-can-release}
  @see-class{g:simple-permission}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:permission-allowed ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "allowed" 'permission) t)
 "The @code{allowed} property of type @code{:boolean} (Read) @br{}
  @em{True} if the caller currently has permission to perform the action that
  the @class{g:permission} object represents the permission to perform. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'permission-allowed)
      "Accessor"
      (documentation 'permission-allowed 'function)
 "@version{2025-05-26}
  @syntax{(g:permission-allowed object) => allowed}
  @argument[object]{a @class{g:permission} object}
  @argument[allowed]{a boolean whether the caller currently has permission to
    perform the action}
  @begin{short}
    Accessor of the @slot[g:permission]{allowed} slot of the
    @class{g:permission} class.
  @end{short}
  The @fun{g:permission-allowed} function gets the value of the property. This
  property is @em{true} if the caller currently has permission to perform the
  action that the @class{g:permission} object represents the permission to
  perform.
  @see-class{g:permission}")

;;; --- g:permission-can-acquire -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-acquire" 'permission) t)
 "The @code{can-acquire} property of type @code{:boolean} (Read) @br{}
  @em{True} if it is generally possible to acquire the permission by calling
  the @fun{g:permission-acquire} function. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'permission-can-acquire)
      "Accessor"
      (documentation 'permission-can-acquire 'function)
 "@version{2025-05-26}
  @syntax{(g:permission-can-acquire object) => can-acquire}
  @argument[object]{a @class{g:permission} object}
  @argument[can-acquire]{a boolean whether it is generally possible to acquire
    the permission}
  @begin{short}
    Accessor of the @slot[g:permission]{can-acquire} slot of the
    @class{g:permission} class.
  @end{short}
  The @fun{g:permission-can-acquire} function gets the value of the property.
  This property is @em{true} if it is generally possible to acquire the
  permission by calling the @fun{g:permission-acquire} function.
  @see-class{g:permission}
  @see-function{g:permission-acquire}")

;;; --- g:permission-can-release -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-release" 'permission) t)
 "The @code{can-release} property of type @code{:boolean} (Read) @br{}
  @em{True} if it is generally possible to release the permission by calling
  the @fun{g:permission-release} function. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'permission-can-release)
      "Accessor"
      (documentation 'permission-can-release 'function)
 "@version{2025-06-26}
  @syntax{(g:permission-can-release object) => can-release}
  @argument[object]{a @class{g:permission} object}
  @argument[can-release]{a boolean whether it is generally possible to release
    the permission}
  @begin{short}
    Accessor of the @slot[g:permission]{can-release} slot of the
    @class{g:permission} class.
  @end{short}
  The @fun{g:permission-can-release} function gets the value of the property.
  This property is @em{true} if it is generally possible to release the
  permission by calling the @fun{g:permission-release} function.
  @see-class{g:permission}
  @see-function{g:permission-release}")

;;; ----------------------------------------------------------------------------
;;; g_permission_acquire
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_permission_acquire" %permission-acquire) :boolean
  (permission (gobject:object permission))
  (cancellable :pointer)
  (err :pointer))

(defun permission-acquire (permission &optional (cancellable nil))
 #+liber-documentation
 "@version{#2025-05-26}
  @argument[permission]{a @class{g:permission} object}
  @argument[cancellable]{a @class{g:cancellable} instance, or @code{nil}}
  @return{@em{True} if the permission was successfully acquired.}
  @begin{short}
    Attempts to acquire the permission represented by @arg{permission}.
  @end{short}
  The precise method by which this happens depends on the permission and the
  underlying authentication mechanism. A simple example is that a dialog may
  appear asking the user to enter their password.

  You should check with the @fun{g:permission-can-acquire} function before
  calling this function. If the permission is acquired then @em{true} is
  returned. Otherwise, @em{false} is returned.

  This call is blocking, likely for a very long time, in the case that user
  interaction is required.
  @see-class{g:permission}
  @see-class{g:cancellable}
  @see-function{g:permission-can-acquire}"
  (let ((cancellable (or cancellable (cffi:null-pointer))))
    (glib:with-error (err)
      (%permission-acquire permission cancellable err))))

(export 'permission-acquire)

;;; ----------------------------------------------------------------------------
;;; g_permission_acquire_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_permission_acquire_async" %permission-acquire-async) :void
  (permission (gobject:object permission))
  (cancellable (gobject:object cancellable))
  (func :pointer)
  (data :pointer))

(defun permission-acquire-async (permission cancellable func)
 #+liber-documentation
 "@version{#2025-05-26}
  @argument[permission]{a @class{g:permission} object}
  @argument[cancellable]{a @class{g:cancellable} object, can be @code{nil}}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to
    call when the request is done}
  @begin{short}
    Attempts to acquire the permission represented by @arg{permission}.
  @end{short}
  This is the first half of the asynchronous version of the
  @fun{g:permission-acquire} function.
  @see-class{g:permission}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{g:permission-acquire}
  @see-function{g:permission-acquire-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%permission-acquire-async permission
                               (or cancellable (cffi:null-pointer))
                               (cffi:callback async-ready-callback)
                               ptr)))

(export 'permission-acquire-async)

;;; ----------------------------------------------------------------------------
;;; g_permission_acquire_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_permission_acquire_finish" %permission-acquire-finish)
    :boolean
  (permission (gobject:object permission))
  (result (gobject:object async-result))
  (err :pointer))

(defun permission-acquire-finish (permission result)
 #+liber-documentation
 "@version{#2025-05-26}
  @argument[permission]{a @class{g:permission} object}
  @argument[result]{a @class{g:async-result} object}
  @return{@em{True} if the permission was successfully acquired.}
  @begin{short}
    Collects the result of attempting to acquire the permission represented by
    @arg{permission}.
  @end{short}
  This is the second half of the asynchronous version of the
  @fun{g:permission-acquire} function.
  @see-class{g:permission}
  @see-class{g:async-result}
  @see-function{g:permission-acquire}
  @see-function{g:permission-acquire-async}"
  (glib:with-ignore-error (err)
    (%permission-acquire-finish permission result err)))

(export 'permission-acquire-finish)

;;; ----------------------------------------------------------------------------
;;; g_permission_release
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_permission_release" %permission-release) :boolean
  (permission (gobject:object permission))
  (cancellable :pointer)
  (err :pointer))

(defun permission-release (permission &optional (cancellable nil))
 #+liber-documentation
 "@version{#2025-05-26}
  @argument[permission]{a @class{g:permission} object}
  @argument[cancellable]{a @class{g:cancellable} instance, or @code{nil}}
  @return{@em{True} if the permission was successfully released.}
  @begin{short}
    Attempts to release the permission represented by @arg{permission}.
  @end{short}
  The precise method by which this happens depends on the permission and the
  underlying authentication mechanism. In most cases the permission will be
  dropped immediately without further action.

  You should check with the @fun{g:permission-can-release} function before
  calling this function. If the permission is released then @em{true} is
  returned. Otherwise, @em{false} is returned and error is set appropriately.

  This call is blocking, likely for a very long time, in the case that user
  interaction is required.
  @see-class{g:permission}
  @see-class{g:cancellable}
  @see-function{g:permission-can-release}"
  (let ((cancellable (or cancellable (cffi:null-pointer))))
    (glib:with-error (err)
      (%permission-release permission cancellable err))))

(export 'permission-release)

;;; ----------------------------------------------------------------------------
;;; g_permission_release_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_permission_release_async" %permission-release-async) :void
  (permission (gobject:object permission))
  (cancellable (gobject:object cancellable))
  (func :pointer)
  (data :pointer))

(defun permission-release-async (permission cancellable func)
 #+liber-documentation
 "@version{#2025-05-26}
  @argument[permission]{a @class{g:permission} object}
  @argument[cancellable]{a @class{g:cancellable} object, can be @code{nil}}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to
    call when the request is done}
  @begin{short}
    Attempts to release the permission represented by @arg{permission}.
  @end{short}
  This is the first half of the asynchronous version of the
  @fun{g:permission-release} function.
  @see-class{g:permission}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{g:permission-release}
  @see-function{g:permission-release-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%permission-release-async permission
                               (or cancellable (cffi:null-pointer))
                               (cffi:callback async-ready-callback)
                               ptr)))

(export 'permission-release-async)

;;; ----------------------------------------------------------------------------
;;; g_permission_release_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_permission_release_finish" %permission-release-finish)
    :boolean
  (permission (gobject:object permission))
  (result (gobject:object async-result))
  (err :pointer))

(defun permission-release-finish (permission result)
 #+liber-documentation
 "@version{#2025-05-26}
  @argument[permission]{a @class{g:permission} object}
  @argument[result]{a @class{g:async-result} object}
  @return{@em{True} if the permission was successfully released.}
  @begin{short}
    Collects the result of attempting to release the permission represented by
    @arg{permission}.
  @end{short}
  This is the second half of the asynchronous version of the
  @fun{g:permission-release} function.
  @see-class{g:permission}
  @see-class{g:async-result}
  @see-function{g:permission-release}
  @see-function{g:permission-release-async}"
  (glib:with-ignore-error (err)
    (%permission-release-finish permission result err)))

(export 'permission-release-finish)

;;; ----------------------------------------------------------------------------
;;; g_permission_impl_update ()
;;;
;;; void
;;; g_permission_impl_update (GPermission *permission,
;;;                           gboolean allowed,
;;;                           gboolean can_acquire,
;;;                           gboolean can_release);
;;;
;;; This function is called by the GPermission implementation to update the
;;; properties of the permission. You should never call this function except
;;; from a GPermission implementation.
;;;
;;; GObject notify signals are generated, as appropriate.
;;;
;;; permission :
;;;     a GPermission instance
;;;
;;; allowed :
;;;     the new value for the 'allowed' property
;;;
;;; can_acquire :
;;;     the new value for the 'can-acquire' property
;;;
;;; can_release :
;;;     the new value for the 'can-release' property
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.permission.lisp ----------------------------------------
