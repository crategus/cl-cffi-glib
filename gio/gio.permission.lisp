;;; ----------------------------------------------------------------------------
;;; gio.permission.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.74 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2021 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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
;;;     g_permission_impl_update
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

(define-g-object-class "GPermission" permission
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
 "@version{#2022-12-31}
  @begin{short}
    A @sym{g:permission} object represents the status of the permission of the
    caller to perform a certain action.
  @end{short}

  You can query if the action is currently allowed and if it is possible to
  acquire the permission so that the action will be allowed in the future.

  There is also an API to actually acquire the permission and one to release it.

  As an example, a @sym{g:permission} object might represent the ability for the
  user to write to a @class{gtk:settings} object. This @sym{g:permission} object
  could then be used to decide if it is appropriate to show a \"Click here to
  unlock\" button in a dialog and to provide the mechanism to invoke when that
  button is clicked.
  @see-slot{g:permission-allowed}
  @see-slot{g:permission-can-acquire}
  @see-slot{g:permission-can-release}
  @see-class{g:simple-permission}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- permission-allowed -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "allowed" 'permission) t)
 "The @code{allowed} property of type @code{:boolean} (Read) @br{}
  @em{True} if the caller currently has permission to perform the action that
  the @sym{g:permission} object represents the permission to perform. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'permission-allowed)
      "Accessor"
      (documentation 'permission-allowed 'function)
 "@version{#2022-12-31}
  @syntax[]{(g:permission-allowed object) => allowed}
  @argument[object]{a @class{g:permission} object}
  @argument[allowed]{a boolean whether the caller currently has permission to
    perform the action}
  @begin{short}
    Accessor of the @slot[permission]{allowed} slot of the
    @class{g:permission} class.
  @end{short}

  The @sym{g:permission-allowed} function gets the value of the property. This
  property is @em{true} if the caller currently has permission to perform the
  action that the @class{g:permission} object represents the permission to
  perform.
  @see-class{g:permission}")

;;; --- permission-can-acquire -------------------------------------------------

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
 "@version{#2022-12-31}
  @syntax[]{(g:permission-can-acquire object) => can-acquire}
  @argument[object]{a @class{g:permission} object}
  @argument[can-acquire]{a boolean whether it is generally possible to acquire
    the permission}
  @begin{short}
    Accessor of the @slot[permission]{can-acquire} slot of the
    @class{g:permission} class.
  @end{short}

  The @sym{g:permission-can-acquire} function gets the value of the property.
  This property is @em{true} if it is generally possible to acquire the
  permission by calling the @fun{g:permission-acquire} function.
  @see-class{g:permission}
  @see-function{g:permission-acquire}")

;;; --- permission-can-release -------------------------------------------------

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
 "@version{#2022-12-31}
  @syntax[]{(g:permission-can-release object) => can-release}
  @argument[object]{a @class{g:permission} object}
  @argument[can-release]{a boolean whether it is generally possible to release
    the permission}
  @begin{short}
    Accessor of the @slot[permission]{can-release} slot of the
    @class{g:permission} class.
  @end{short}

  The @sym{g:permission-can-release} function gets the value of the property.
  This property is @em{true} if it is generally possible to release the
  permission by calling the @fun{g:permission-release} function.
  @see-class{g:permission}
  @see-function{g:permission-release}")

;;; ----------------------------------------------------------------------------
;;; g_permission_acquire ()
;;;
;;; gboolean
;;; g_permission_acquire (GPermission *permission,
;;;                       GCancellable *cancellable,
;;;                       GError **error);
;;;
;;; Attempts to acquire the permission represented by permission .
;;;
;;; The precise method by which this happens depends on the permission and the
;;; underlying authentication mechanism. A simple example is that a dialog may
;;; appear asking the user to enter their password.
;;;
;;; You should check with g_permission_get_can_acquire() before calling this
;;; function.
;;;
;;; If the permission is acquired then TRUE is returned. Otherwise, FALSE is
;;; returned and error is set appropriately.
;;;
;;; This call is blocking, likely for a very long time (in the case that user
;;; interaction is required). See g_permission_acquire_async() for the
;;; non-blocking version.
;;;
;;; permission :
;;;     a GPermission instance
;;;
;;; cancellable :
;;;     a GCancellable, or NULL.
;;;
;;; error :
;;;     a pointer to a NULL GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the permission was successfully acquired
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_permission_acquire_async ()
;;;
;;; void
;;; g_permission_acquire_async (GPermission *permission,
;;;                             GCancellable *cancellable,
;;;                             GAsyncReadyCallback callback,
;;;                             gpointer user_data);
;;;
;;; Attempts to acquire the permission represented by permission .
;;;
;;; This is the first half of the asynchronous version of
;;; g_permission_acquire().
;;;
;;; permission :
;;;     a GPermission instance
;;;
;;; cancellable :
;;;     a GCancellable, or NULL.
;;;
;;; callback :
;;;     the GAsyncReadyCallback to call when done
;;;
;;; user_data :
;;;     the user data to pass to callback
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_permission_acquire_finish ()
;;;
;;; gboolean
;;; g_permission_acquire_finish (GPermission *permission,
;;;                              GAsyncResult *result,
;;;                              GError **error);
;;;
;;; Collects the result of attempting to acquire the permission represented by
;;; permission .
;;;
;;; This is the second half of the asynchronous version of
;;; g_permission_acquire().
;;;
;;; permission :
;;;     a GPermission instance
;;;
;;; result :
;;;     the GAsyncResult given to the GAsyncReadyCallback
;;;
;;; error :
;;;     a pointer to a NULL GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the permission was successfully acquired
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_permission_release ()
;;;
;;; gboolean
;;; g_permission_release (GPermission *permission,
;;;                       GCancellable *cancellable,
;;;                       GError **error);
;;;
;;; Attempts to release the permission represented by permission .
;;;
;;; The precise method by which this happens depends on the permission and the
;;; underlying authentication mechanism. In most cases the permission will be
;;; dropped immediately without further action.
;;;
;;; You should check with g_permission_get_can_release() before calling this
;;; function.
;;;
;;; If the permission is released then TRUE is returned. Otherwise, FALSE is
;;; returned and error is set appropriately.
;;;
;;; This call is blocking, likely for a very long time (in the case that user
;;; interaction is required). See g_permission_release_async() for the
;;; non-blocking version.
;;;
;;;
;;; permission :
;;;     a GPermission instance
;;;
;;; cancellable :
;;;     a GCancellable, or NULL.
;;;
;;; error :
;;;     a pointer to a NULL GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the permission was successfully released
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_permission_release_async ()
;;;
;;; void
;;; g_permission_release_async (GPermission *permission,
;;;                             GCancellable *cancellable,
;;;                             GAsyncReadyCallback callback,
;;;                             gpointer user_data);
;;;
;;; Attempts to release the permission represented by permission .
;;;
;;; This is the first half of the asynchronous version of
;;; g_permission_release().
;;;
;;; permission :
;;;     a GPermission instance
;;;
;;; cancellable :
;;;     a GCancellable, or NULL.
;;;
;;; callback :
;;;     the GAsyncReadyCallback to call when done
;;;
;;; user_data :
;;;     the user data to pass to callback
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_permission_release_finish ()
;;;
;;; gboolean
;;; g_permission_release_finish (GPermission *permission,
;;;                              GAsyncResult *result,
;;;                              GError **error);
;;;
;;; Collects the result of attempting to release the permission represented by
;;; permission .
;;;
;;; This is the second half of the asynchronous version of
;;; g_permission_release().
;;;
;;; permission :
;;;     a GPermission instance
;;;
;;; result :
;;;     the GAsyncResult given to the GAsyncReadyCallback
;;;
;;; error :
;;;     a pointer to a NULL GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the permission was successfully released
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

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
;;;
;;; Since: 2.26
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.permission.lisp ----------------------------------------
