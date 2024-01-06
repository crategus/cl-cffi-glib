;;; ----------------------------------------------------------------------------
;;; glib.package.lisp
;;;
;;; The documentation of this file is taken from the GLIB Reference Manual
;;; Version 2.76 and modified to document the Lisp binding to the GLIB library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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

(defpackage :glib
  (:use :iterate :common-lisp)
  (:shadow #:error)
  (:import-from :cffi)
  (:export ;; Symbols from glib.stable-pointer.lisp
           #:allocate-stable-pointer
           #:free-stable-pointer
           #:get-stable-pointer-value
           #:stable-pointer-destroy-notify
           #:with-stable-pointer
           ;; Symbols from glib.gtype.lisp
           #:gtype
           #:gtype-name
           #:gtype-id
           #:symbol-for-gtype
           ;; Symbols from glib.boxed-type.lisp
           #:boxed-opaque-info
           #:boxed-opaque-pointer
           #:boxed-cstruct-info
           #:boxed-variant-info
           #:define-g-boxed-opaque
           #:define-g-boxed-cstruct
           #:define-g-boxed-variant-cstruct
           #:boxed-copy-fn
           #:make-boxed-type
           #:get-boxed-info
           #:with-foreign-boxed-array
           #:with-g-boxed-array
           #:pointer
           #:cleanup-translated-object-for-callback
           ;; Symbols from glib.error.lisp
           #:error
           #:with-g-error
           #:with-ignore-g-error
           #:with-catching-to-g-error))

(in-package :glib)

#+sbcl
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :glib) t)
 "GLib is a general-purpose utility library, which provides many useful data
  types, macros, type conversions, string utilities, file utilities, a main
  loop abstraction, and so on. It works on many UNIX-like platforms, as well
  as Windows and OS X. GLib is released under the GNU Library General Public
  License (GNU LGPL).

  This is the API documentation of a Lisp binding to the library GLib. Only a
  small part of GLib is implemented in Lisp which is necessary to implement
  GTK in Lisp.
  @begin[Version Information]{section}
    Variables and functions to check the GLib version.
    @about-symbol{+glib-major-version+}
    @about-symbol{+glib-minor-version+}
    @about-symbol{+glib-micro-version+}
    @about-function{check-version}
    @about-function{cl-cffi-glib-build-info}
  @end{section}
  @begin[Miscellaneous]{section}
    Documentation of several type definitions and functions, which are
    needed for the implementation of the GTK library. Only a small part of the
    GLib library is implemented.
    @begin[String Utility Functions]{subsection}
      String Utility Functions. The following type is implemented:
      @about-type{strv-t}
    @end{subsection}
    @begin[Doubly-Linked Lists]{subsection}
      Linked lists containing pointers to data, with the ability to iterate over
      the list in both directions. Implemented is the type:
      @about-type{list-t}
    @end{subsection}
    @begin[Singly-Linked Lists]{subsection}
      Linked lists containing pointers to data, limited to iterating over the
      list in one direction. Implemented is the type:
      @about-type{slist-t}
    @end{subsection}
    @begin[Quarks]{subsection}
      A 2-way association between a string and a unique integer identifier.
      @about-type{quark-as-string}
    @end{subsection}
    @begin[GDateTime]{subsection}
      @about-type{date-time}
    @end{subsection}
    @begin[Unicode manipulation]{subsection}
      @about-class{unichar}
    @end{subsection}
    @begin[GError]{subsection}
      @about-class{error}
    @end{subsection}
    @begin[Memory Allocation]{subsection}
      The following functions for general memory handling are implemented:
      @about-function{malloc}
      @about-function{free}
    @end{subsection}
    @begin[Utility Functions]{subsection}
      A selection of portable utility functions. Two functions are implemened.
      @about-function{application-name}
      @about-function{prgname}
    @end{subsection}
  @end{section}
  @begin[The Main Event Loop]{section}
    The Main Event Loop manages all available sources of events.
    @about-variable{+g-priority-high+}
    @about-variable{+g-priority-default+}
    @about-variable{+g-priority-high-idle+}
    @about-variable{+g-priority-default-idle+}
    @about-variable{+g-priority-low+}
    @about-variable{+g-source-continue+}
    @about-variable{+g-source-remove+}
    @about-type{main-loop}
    @about-function{main-loop-new}
    @about-function{main-loop-ref}
    @about-function{main-loop-unref}
    @about-function{main-loop-run}
    @about-function{main-loop-quit}
    @about-function{main-loop-is-running}
    @about-function{main-loop-context}
    @about-type{main-context}
    @about-function{main-context-new}
    @about-function{main-context-ref}
    @about-function{main-context-unref}
    @about-function{main-context-default}
    @about-function{main-context-iteration}
    @about-function{main-context-pending}
    @about-function{main-context-find-source-by-id}
    @about-function{main-context-wakeup}
    @about-function{main-context-acquire}
    @about-function{main-context-release}
    @about-function{main-context-is-owner}
    @about-function{main-context-dispatch}
    @about-function{main-depth}
    @about-function{main-current-source}
    @about-type{source}
    @about-function{timeout-source-new}
    @about-function{timeout-source-new-seconds}
    @about-function{timeout-add}
    @about-function{timeout-add-seconds}
    @about-function{idle-source-new}
    @about-function{idle-add}
    @about-function{source-ref}
    @about-function{source-unref}
    @about-function{source-attach}
    @about-function{source-destroy}
    @about-function{source-is-destroyed}
    @about-function{source-priority}
    @about-function{source-can-recurse}
    @about-function{source-id}
    @about-function{source-name}
    @about-function{source-set-name-by-id}
    @about-function{source-context}
    @about-symbol{source-func}
    @about-function{source-set-callback}
    @about-function{source-time}
    @about-function{source-ready-time}
    @about-function{source-remove}
  @end{section}
  @begin[GBytes]{section}
    A simple refcounted data type representing an immutable sequence of zero or
    more bytes from an unspecified origin.
    @about-class{bytes}
    @about-function{bytes-new}
    @about-function{bytes-new-take}
    @about-function{bytes-new-static}
    @about-function{bytes-new-with-free-func}
    @about-function{bytes-new-from-bytes}
    @about-function{bytes-data}
    @about-function{bytes-size}
    @about-function{bytes-hash}
    @about-function{bytes-equal}
    @about-function{bytes-compare}
    @about-function{bytes-ref}
    @about-function{bytes-unref}
    @about-function{bytes-unref-to-data}
    @about-function{bytes-unref-to-array}
  @end{section}
  @begin[Command line option parser]{section}
    Parses command line options.
    @about-symbol{option-arg}
    @about-symbol{option-flags}
    @about-type{option-context}
    @about-macro{with-g-option-context}
    @about-function{option-context-new}
    @about-function{option-context-summary}
    @about-function{option-context-description}
    @about-symbol{translate-func}
    @about-function{option-context-set-translate-func}
    @about-function{option-context-set-translation-domain}
    @about-function{option-context-free}
    @about-function{option-context-parse}
    @about-function{option-context-parse-strv}
    @about-function{option-context-help-enabled}
    @about-function{option-context-ignore-unknown-options}
    @about-function{option-context-help}
    @about-function{option-context-strict-posix}
    @about-function{option-context-add-main-entries}
    @about-function{option-context-add-group}
    @about-function{option-context-main-group}
    @about-type{option-group}
    @about-macro{with-g-option-group}
    @about-function{option-group-new}
    @about-function{option-group-ref}
    @about-function{option-group-unref}
    @about-function{option-group-free}
    @about-function{option-group-add-entries}
    @about-symbol{OptionParseFunc}
    @about-function{option-group-set-parse-hooks}
    @about-symbol{OptionErrorFunc}
    @about-function{option-group-set-error-hook}
    @about-function{option-group-set-translate-func}
    @about-function{option-group-set-translation-domain}
  @end{section}
  @begin[Key-value file parser]{section}
    Parses .ini-like config files.
    @about-symbol{key-file-error}
    @about-symbol{key-file-flags}
    @about-type{key-file}
    @about-macro{with-g-key-file}
    @about-function{key-file-new}
    @about-function{key-file-free}
    @about-function{key-file-ref}
    @about-function{key-file-unref}
    @about-function{key-file-set-list-separator}
    @about-function{key-file-load-from-file}
    @about-function{key-file-load-from-data}
    @about-function{key-file-load-from-bytes}
    @about-function{key-file-load-from-data-dirs}
    @about-function{key-file-load-from-dirs}
    @about-function{key-file-to-data}
    @about-function{key-file-save-to-file}
    @about-function{key-file-start-group}
    @about-function{key-file-groups}
    @about-function{key-file-keys}
    @about-function{key-file-has-group}
    @about-function{key-file-has-key}
    @about-function{key-file-value}
    @about-function{key-file-string}
    @about-function{key-file-locale-string}
    @about-function{key-file-locale-for-key}
    @about-function{key-file-boolean}
    @about-function{key-file-integer}
    @about-function{key-file-int64}
    @about-function{key-file-uint64}
    @about-function{key-file-double}
    @about-function{key-file-string-list}
    @about-function{key-file-locale-string-list}
    @about-function{key-file-boolean-list}
    @about-function{key-file-integer-list}
    @about-function{key-file-double-list}
    @about-function{key-file-comment}
    @about-function{key-file-remove-group}
    @about-function{key-file-remove-key}
    @about-function{key-file-remove-comment}
  @end{section}
  @begin[GVariantType]{section}
    Introduction to the GVariant type system.
    @about-class{variant-type}
    @about-function{variant-type-checked}
    @about-function{variant-type-copy}
    @about-function{variant-type-new}
    @about-function{variant-type-string-is-valid}
    @about-function{variant-type-string-scan}
    @about-function{variant-type-string-length}
    @about-function{variant-type-peek-string}
    @about-function{variant-type-dup-string}
    @about-function{variant-type-is-definite}
    @about-function{variant-type-is-container}
    @about-function{variant-type-is-basic}
    @about-function{variant-type-is-maybe}
    @about-function{variant-type-is-array}
    @about-function{variant-type-is-tuple}
    @about-function{variant-type-is-dict-entry}
    @about-function{variant-type-is-variant}
    @about-function{variant-type-hash}
    @about-function{variant-type-equal}
    @about-function{variant-type-is-subtype-of}
    @about-function{variant-type-new-maybe}
    @about-function{variant-type-new-array}
    @about-function{variant-type-new-tuple}
    @about-function{variant-type-new-dict-entry}
    @about-function{variant-type-element}
    @about-function{variant-type-n-items}
    @about-function{variant-type-first}
    @about-function{variant-type-next}
    @about-function{variant-type-key}
    @about-function{variant-type-value}
  @end{section}
  @begin[GVariant]{section}
    Strongly typed value datatype.
    @about-type{variant}
    @about-symbol{variant-class}
    @about-symbol{variant-iter}
    @about-symbol{variant-builder}
    @about-symbol{variant-parse-error}
    @about-function{variant-unref}
    @about-function{variant-ref}
    @about-function{variant-ref-sink}
    @about-function{variant-is-floating}
    @about-function{variant-take-ref}
    @about-function{variant-type}
    @about-function{variant-type-string}
    @about-function{variant-is-of-type}
    @about-function{variant-is-container}
    @about-function{variant-compare}
    @about-function{variant-classify}
    @about-function{variant-check-format-string}
    @about-function{variant-get}
    @about-function{variant-get-va}
    @about-function{variant-new}
    @about-function{variant-new-va}
    @about-function{variant-new-boolean}
    @about-function{variant-new-byte}
    @about-function{variant-new-int16}
    @about-function{variant-new-uint16}
    @about-function{variant-new-int32}
    @about-function{variant-new-uint32}
    @about-function{variant-new-int64}
    @about-function{variant-new-uint64}
    @about-function{variant-new-handle}
    @about-function{variant-new-double}
    @about-function{variant-new-string}
    @about-function{variant-new-take-string}
    @about-function{variant-new-printf}
    @about-function{variant-new-object-path}
    @about-function{variant-is-object-path}
    @about-function{variant-new-signature}
    @about-function{variant-is-signature}
    @about-function{variant-new-variant}
    @about-function{variant-new-strv}
    @about-function{variant-new-objv}
    @about-function{variant-new-bytestring}
    @about-function{variant-new-bytestring-array}
    @about-function{variant-boolean}
    @about-function{variant-byte}
    @about-function{variant-int16}
    @about-function{variant-uint16}
    @about-function{variant-int32}
    @about-function{variant-uint32}
    @about-function{variant-int64}
    @about-function{variant-int64}
    @about-function{variant-uint64}
    @about-function{variant-handle}
    @about-function{variant-double}
    @about-function{variant-string}
    @about-function{variant-variant}
    @about-function{variant-strv}
    @about-function{variant-dup-strv}
    @about-function{variant-objv}
    @about-function{variant-dup-objv}
    @about-function{variant-bytestring}
    @about-function{variant-dup-bytestring}
    @about-function{variant-bytestring_array}
    @about-function{variant-dup-bytestring-array}
    @about-function{variant-new-maybe}
    @about-function{variant-new-array}
    @about-function{variant-new-tuple}
    @about-function{variant-new-dict-entry}
    @about-function{variant-new-new-fixed-array}
    @about-function{variant-get-maybe}
    @about-function{variant-n-children}
    @about-function{variant-get-child-value}
    @about-function{variant-get-child}
    @about-function{variant-lookup-value}
    @about-function{variant-lookup}
    @about-function{variant-get-fixed-array}
    @about-function{variant-get-size}
    @about-function{variant-get-data}
    @about-function{variant-get-data-as-bytes}
    @about-function{variant-store}
    @about-function{variant-new-from-data}
    @about-function{variant-new-from-bytes}
    @about-function{variant-byteswap}
    @about-function{variant-get-normal-form}
    @about-function{variant-is-normal-form}
    @about-function{variant-hash}
    @about-function{variant-equal}
    @about-function{variant-print}
    @about-function{variant-print-string}
    @about-function{variant-iter-copy}
    @about-function{variant-iter-free}
    @about-function{variant-iter-init}
    @about-function{variant-iter-n-children}
    @about-function{variant-iter-new}
    @about-function{variant-iter-next-value}
    @about-function{variant-iter-next}
    @about-function{variant-iter-loop}
    @about-function{variant-builder-unref}
    @about-function{variant-builder-ref}
    @about-function{variant-builder-new}
    @about-function{variant-builder-init}
    @about-function{variant-builder-clear}
    @about-function{variant-builder-add-value}
    @about-function{variant-builder-add}
    @about-function{variant-builder-add-parsed}
    @about-function{variant-builder-end}
    @about-function{variant-builder-open}
    @about-function{variant-builder-close}
    @about-class{variant-dict}
    @about-function{variant-dict-unref}
    @about-function{variant-dict-ref}
    @about-function{variant-dict-new}
    @about-function{variant-dict-init}
    @about-function{variant-dict-clear}
    @about-function{variant-dict-contains}
    @about-function{variant-dict-lookup}
    @about-function{variant-dict-lookup-value}
    @about-function{variant-dict-insert}
    @about-function{variant-dict-insert-value}
    @about-function{variant-dict-remove}
    @about-function{variant-dict-end}
    @about-function{variant-parse}
    @about-function{variant-new-parsed-va}
    @about-function{variant-new-parsed}
    @about-function{variant-parse-error-print-context}
  @end{section}")

;;; --- End of file glib.package.lisp ------------------------------------------
