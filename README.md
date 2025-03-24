## cl-cffi-glib

### General information

`cl-cffi-glib` is a Lisp binding of parts of the GLib, GObject and GIO libraries
that forms the basics for projects such as GTK and Pango. The library is loaded
from the `cl-cffi-gtk3` library for a Lisp binding to GTK 3, the `cl-cffi-gtk4`
library for a Lisp binding to GTK 4, and the `cl-cffi-pango` library  for a Lisp
binding to Pango.

The `cl-cffi-glib` library was part of the `cl-cffi-gtk` library and was
separated out to be used independently. The former `cl-cffi-gtk` library is
replaced with the `cl-cffi-gtk3` and `cl-cffi-gtk4` libraries for GTK 3 and
GTK 4, respectivly.

This work is based on the `cl-gtk2` library developed by Kalyanov Dmitry.

### License

The `cl-cffi-glib` library is licensed under the MIT license.

### Installation

The `cl-cffi-glib` library is ASDF installable:
```
(asdf:load-system :cl-cffi-glib)
```
After loading the library information about the installed version is available:
```
* (glib:cl-cffi-glib-build-info)
cl-cffi-glib build date: 13:13 4/6/2023
GLIB version: 2.72.4
Machine type: X86-64
Machine version: Intel(R) Core(TM) i5-5200U CPU @ 2.20GHz
Software type: Linux
Software version: 5.19.0-38-generic
Lisp implementation type: SBCL
Lisp implementation version: 2.1.11.debian
```

The `cl-cffi-glib` library depends on the following Lisp libraries:

* `cffi`

    The Common Foreign Function Interface, purports to be a portable foreign
    function interface for Common Lisp.
    See [common-lisp.net/project/cffi/](http://common-lisp.net/project/cffi/).

    **Warning:** Yout must use the version 0.22.0 or newer of the CFFI library.
    Older versions of CFFI are not compatible with the implementation of the
    `cl-cffi-glib` library.

* `iterate`

    Is a lispy and extensible replacement for the LOOP macro.
    See [common-lisp.net/project/iterate/](http://common-lisp.net/project/iterate/).

* `bordeaux-threads`

    Lets you write multi-threaded applications in a portable way.
    See [common-lisp.net/project/bordeaux-threads/](http://common-lisp.net/project/bordeaux-threads/).

* `closer-mop`

    Closer to MOP is a compatibility layer that rectifies many of the absent or
    incorrect MOP features as detected by MOP Feature Tests.
    See [common-lisp.net/project/closer/closer-mop.html](http://common-lisp.net/project/closer/closer-mop.html)

* `trivial-garbage`

    Provides a portable API to finalizers, weak hash-tables and weak pointers
    on all major CL implementations.
    See [common-lisp.net/project/trivial-garbage/](http://common-lisp.net/project/trivial-garbage/).

* `trivial-features`

    Ensures consistent `*features*` across multiple Common Lisp implementations.
    See [cliki.net/trivial-features](https://www.cliki.net/trivial-features).

### Packages

The `cl-cffi-glib` library defines the following packages:

* `glib` package

    Contains the exported symbols of the Lisp binding to GLib.

* `gobject` package

    Contains the exported symbols of the Lisp binding to GObject.

* `gio` package

    Contains the exported symbols of the Lisp binding to GIO.

* `glib-user` package with nickname `g`

    Imports all symbols from the `glib`, `gobject`, and `gio` packages and
    exports the symbols for usage with the nickname `g`. Therefore you can
    access a symbol, e.g. the `gobject:signal-connect` function in the `gobject`
    package as `g:signal-connect`, or the `gio:application-run` function in the
    `gio` package as `g:application-run`.

### Documentation

The API documentation is part of the
[GTK 3 API documentation](https://crategus.com/books/cl-cffi-gtk3) or
[GTK 4 API documentation](https://crategus.com/books/cl-cffi-gtk4).
