;;; ----------------------------------------------------------------------------
;;; gio.themed-icon.lisp
;;;
;;; The documentation in this file is taken from the GIO Reference Manual
;;; Version 2.82 and modified to document the Lisp binding to the GIO library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; GThemedIcon
;;;
;;;     Icon theming support
;;;
;;; Types and Values
;;;
;;;     GThemedIcon
;;;
;;; Functions
;;;
;;;     g_themed_icon_new
;;;     g_themed_icon_new_from_names
;;;     g_themed_icon_new_with_default_fallbacks
;;;     g_themed_icon_prepend_name
;;;     g_themed_icon_append_name
;;;     g_themed_icon_get_names
;;;
;;; Properties
;;;
;;;     name
;;;     names
;;;     use-default-fallbacks
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GThemedIcon
;;;
;;; Implemented Interfaces
;;;
;;;     GThemedIcon implements GIcon.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GThemedIcon
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GThemedIcon" themed-icon
  (:superclass gobject:object
   :export t
   :interfaces ("GIcon")
   :type-initializer "g_themed_icon_get_type")
  ((name
    themed-icon-name
    "name" "gchararray" nil t)
   (names
    themed-icon-names
    "names" "GStrv" t t)
   (use-default-fallbacks
    themed-icon-use-default-fallbacks
    "use-default-fallbacks" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'themed-icon 'type)
 "@version{2024-10-23}
  @begin{short}
    The @class{g:themed-icon} class is an implementation of the @class{g:icon}
    interface that supports icon themes.
  @end{short}
  The @class{g:themed-icon} class contains a list of all of the icons present
  in an icon theme, so that icons can be looked up quickly. The
  @class{g:themed-icon} class does not provide actual pixmaps for icons, just
  the icon names.
  @see-constructor{g:themed-icon-new}
  @see-constructor{g:themed-icon-new-from-names}
  @see-constructor{g:themed-icon-new-with-default-fallbacks}
  @see-slot{g:themed-icon-name}
  @see-slot{g:themed-icon-names}
  @see-slot{g:themed-icon-use-default-fallbacks}
  @see-class{g:icon}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g:themed-icon-name -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'themed-icon) t)
 "The @code{name} property of type @code{:string} (Write / Construct Only) @br{}
  The icon name. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'themed-icon-name)
      "Accessor"
      (documentation 'themed-icon-name 'function)
 "@version{2024-10-23}
  @syntax{(g:themed-icon-name object) => name}
  @syntax{(setf (g:themed-icon-name object) name)}
  @argument[object]{a @class{g:themed-icon} object}
  @argument[name]{a string with the icon name}
  @begin{short}
    Accessor of the @slot[g:themed-icon]{name} slot of the @class{g:themed-icon}
    class.
  @end{short}
  @see-class{g:themed-icon}")

;;; --- g:themed-icon-names ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "names" 'themed-icon) t)
 "The @code{names} property of type @type{g:strv-t}
  (Read / Write / Construct Only) @br{}
  The list of strings with the icon names.")

#+liber-documentation
(setf (liber:alias-for-function 'themed-icon-names)
      "Accessor"
      (documentation 'themed-icon-names 'function)
 "@version{2024-10-23}
  @syntax{(g:themed-icon-names object) => names}
  @syntax{(setf (g:themed-icon-names object) names)}
  @argument[object]{a @class{g:themed-icon} object}
  @argument[names]{a list of strings with icon names}
  @begin{short}
    Accessor of the @slot[g:themed-icon]{names} slot of the
    @class{g:themed-icon} class.
  @end{short}
  Gets the names of icons from within @arg{object}.
  @see-class{g:themed-icon}")

;;; --- g:themed-icon-use-default-fallbacks ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-default-fallbacks"
                                               'themed-icon) t)
 "The @code{use-default-fallbacks} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  Whether to use the default fallbacks found by shortening the icon name at '-'
  characters. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'themed-icon-use-default-fallbacks)
      "Accessor"
      (documentation 'themed-icon-use-default-fallbacks 'function)
 "@version{2024-10-23}
  @syntax{(g:themed-icon-use-default-fallbacks object) => setting}
  @syntax{(setf (g:themed-icon-use-default-fallbacks object) setting)}
  @argument[object]{a @class{g:themed-icon} object}
  @argument[setting]{a boolean whether to use default fallbacks}
  @begin{short}
    Accessor of the @slot[g:themed-icon]{use-default-fallbacks} slot of the
    @class{g:themed-icon} class.
  @end{short}
  Whether to use the default fallbacks found by shortening the icon name at '-'
  characters. If the @code{names} list has more than one element, ignores
  any past the first.
  @begin[Examples]{dictionary}
    For example, if the icon name was @code{\"gnome-dev-cdrom-audio\"}, the list
    would become
    @begin{pre}
'(\"gnome-dev-cdrom-audio\"
  \"gnome-dev-cdrom\"
  \"gnome-dev\"
  \"gnome\")
    @end{pre}
  @end{dictionary}
  @see-class{g:themed-icon}")

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_new
;;; ----------------------------------------------------------------------------

(declaim (inline themed-icon-new))

(defun themed-icon-new (name)
 #+liber-documentation
 "@version{2024-10-23}
  @argument[name]{a string containing an icon name}
  @return{The new @class{g:themed-icon} object.}
  @begin{short}
    Creates a new themed icon for the icon name.
  @end{short}
  @see-class{g:themed-icon}
  @see-function{g:themed-icon-new-from-names}
  @see-function{g:themed-icon-new-with-default-fallbacks}"
  (make-instance 'themed-icon
                 :name name))

(export 'themed-icon-new)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_new_from_names
;;; ----------------------------------------------------------------------------

(declaim (inline themed-icon-new-from-names))

(defun themed-icon-new-from-names (&rest names)
 #+liber-documentation
 "@version{2024-10-23}
  @argument[names]{strings containing icon names}
  @return{The new @class{g:themed-icon} object.}
  @begin{short}
    Creates a new themed icon for @arg{names}.
  @end{short}
  @see-class{g:themed-icon}
  @see-function{g:themed-icon-new}
  @see-function{g:themed-icon-new-with-default-fallbacks}"
  (make-instance 'themed-icon
                 :names (if (listp (first names))
                            (first names)
                            names)))

(export 'themed-icon-new-from-names)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_new_with_default_fallbacks
;;; ----------------------------------------------------------------------------

(defun themed-icon-new-with-default-fallbacks (name)
 #+liber-documentation
 "@version{2025-3-2}
  @argument[name]{a string containing an icon name}
  @return{The new @class{g:themed-icon} object.}
  @begin{short}
    Creates a new themed icon for @arg{name}, and all the names that can
    be created by shortening the icon name at '-' characters.
  @end{short}
  @begin[Examples]{dictionary}
    In the following example, @code{icon1} and @code{icon2} are equivalent:
    @begin{pre}
(let* ((names (list \"gnome-dev-cdrom-audio\"
                    \"gnome-dev-cdrom\"
                    \"gnome-dev\"
                    \"gnome\"))
       (icon1 (g:themed-icon-new-from-names names))
       (icon2 (g:themed-icon-new-with-default-fallbacks
                \"gnome-dev-cdrom-audio\")))
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{g:themed-icon}
  @see-function{g:themed-icon-new}
  @see-function{g:themed-icon-new-from-names}"
  (make-instance 'themed-icon
                 :name name
                 :use-default-fallbacks t))

(export 'themed-icon-new-with-default-fallbacks)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_prepend_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_themed_icon_prepend_name" themed-icon-prepend-name) :void
 #+liber-documentation
 "@version{2024-10-23}
  @argument[icon]{a @class{g:themed-icon} object}
  @argument[name]{a string with the name of the icon to prepend to list of
    icons from within @arg{icon}}
  @begin{short}
    Prepend a name to the list of icons from within @arg{icon}.
  @end{short}
  Note that doing so invalidates the hash computed by prior calls to the
  @fun{g:icon-hash} function.
  @see-class{g:themed-icon}
  @see-function{g:icon-hash}
  @see-function{g:themed-icon-append-name}"
  (icon (gobject:object themed-icon))
  (name :string))

(export 'themed-icon-prepend-name)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_append_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("g_themed_icon_append_name" themed-icon-append-name) :void
 #+liber-documentation
 "@version{2024-10-23}
  @argument[icon]{a @class{g:themed-icon} object}
  @argument[name]{a string with the name of the icon to append to list of icons
    from within @arg{icon}}
  @begin{short}
    Append a name to the list of icons from within @arg{icon}.
  @end{short}
  Note that doing so invalidates the hash computed by prior calls to the
  @fun{g:icon-hash} function.
  @see-class{g:themed-icon}
  @see-function{g:icon-hash}
  @see-function{g:themed-icon-prepend-name}"
  (icon (gobject:object themed-icon))
  (name :string))

(export 'themed-icon-append-name)

;;; --- gio.themed-icon.lisp ---------------------------------------------------
