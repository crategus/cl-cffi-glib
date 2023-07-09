;;; ----------------------------------------------------------------------------
;;; gio.loadable-icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2021 - 2023 Dieter Kaiser
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
;;; GLoadableIcon
;;;
;;;     Loadable Icons
;;;
;;; Types and Values
;;;
;;;     GLoadableIcon
;;;
;;; Functions
;;;
;;;     g_loadable_icon_load
;;;     g_loadable_icon_load_async
;;;     g_loadable_icon_load_finish
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GLoadableIcon
;;;
;;; Prerequisites
;;;
;;;     GLoadableIcon requires GIcon and GObject.
;;;
;;; Known Implementations
;;;
;;;     GLoadableIcon is implemented by GBytesIcon and GFileIcon.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GLoadableIcon
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GLoadableIcon" loadable-icon
  (:export t
   :type-initializer "g_loadable_icon_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'loadable-icon)
      "Interface"
      (documentation 'loadable-icon 'type)
 "@version{#2022-12-30}
  @begin{short}
    Extends the @class{g:icon} interface and adds the ability to load icons
    from streams.
  @end{short}
  @see-class{g:icon}
  @see-class{g:themed-icon}")

;;; ----------------------------------------------------------------------------
;;; g_loadable_icon_load ()
;;;
;;; GInputStream *
;;; g_loadable_icon_load (GLoadableIcon *icon,
;;;                       int size,
;;;                       char **type,
;;;                       GCancellable *cancellable,
;;;                       GError **error);
;;;
;;; Loads a loadable icon. For the asynchronous version of this function, see
;;; g_loadable_icon_load_async().
;;;
;;; icon :
;;;     a GLoadableIcon
;;;
;;; size :
;;;     an integer
;;;
;;; type :
;;;     a location to store the type of the loaded icon, NULL to ignore
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore
;;;
;;; Returns :
;;;     A GInputStream to read the icon from.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_loadable_icon_load_async ()
;;;
;;; void
;;; g_loadable_icon_load_async (GLoadableIcon *icon,
;;;                             int size,
;;;                             GCancellable *cancellable,
;;;                             GAsyncReadyCallback callback,
;;;                             gpointer user_data);
;;;
;;; Loads an icon asynchronously. To finish this function, see
;;; g_loadable_icon_load_finish(). For the synchronous, blocking version of this
;;; function, see g_loadable_icon_load().
;;;
;;; icon :
;;;     a GLoadableIcon
;;;
;;; size :
;;;     an integer
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;;
;;; callback .
;;;     a GAsyncReadyCallback to call when the request is satisfied
;;;
;;; user_data :
;;;     the data to pass to callback function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_loadable_icon_load_finish ()
;;;
;;; GInputStream *
;;; g_loadable_icon_load_finish (GLoadableIcon *icon,
;;;                              GAsyncResult *res,
;;;                              char **type,
;;;                              GError **error);
;;;
;;; Finishes an asynchronous icon load started in g_loadable_icon_load_async().
;;;
;;; icon :
;;;     a GLoadableIcon
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; type :
;;;     a location to store the type of the loaded icon, NULL to ignore
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore
;;;
;;; Returns :
;;;     A GInputStream to read the icon from.
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.loadable-icon.lisp -------------------------------------
